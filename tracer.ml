(** Fold over a trace and partition I/O pairs by Instruction code *)

open Core_kernel.Std
open Bap.Std
open Bap_traces.Std
open Format

let verbose = false

module Cmdline = Tracer_cmdline
module Db = Tracer_db

module Disasm = struct
  module Dis = Disasm_expert.Basic
  open Dis
  type t = (asm, kinds) Dis.t

  let insn dis mem =
    match Dis.insn_of_mem dis mem with
    | Error err -> Error err
    | Ok r -> match r with
      | mem', Some insn, `finished -> Ok (mem',insn)
      | _, None, _ ->
        Error (Error.of_string "nothing was disasmed")
      | _, _, `left _ -> Error (Error.of_string "Overloaded chunk")

  let insn_name = Dis.Insn.name
end

let string_of_error = function
  | `Protocol_error er ->
    Printf.sprintf "protocol error: %s"
      (Info.to_string_hum (Error.to_info er))
  | `System_error er ->
    Printf.sprintf "system error: %s" (Unix.error_message er)
  | `No_provider -> "no provider"
  | `Ambiguous_uri -> "ambiguous uri"

let read_dir path =
  let dir = Unix.opendir path in
  let fullpath file = String.concat ~sep:"/" [path; file] in
  let is_trace file = Filename.check_suffix file ".frames" in
  let next () =
    try
      Some (Unix.readdir dir)
    with End_of_file -> None in
  let rec folddir acc =
    match next () with
    | Some file ->
      if is_trace file then folddir (fullpath file :: acc)
      else folddir acc
    | None -> acc in
  let files = folddir [] in
  Unix.closedir dir;
  files

type event = Trace.event

let get_insn_of_chunk chunk dis endian =
  let open Or_error in
  let insn_bigstring = Bigstring.of_string (Chunk.data chunk) in
  Memory.create endian (Chunk.addr chunk) insn_bigstring >>= fun mem ->
  Disasm.insn dis mem >>= fun (_,insn) ->
  Or_error.return (insn,insn_bigstring)

let update_db
    db
    key
    (new_events : Db.event list)
    new_operands
    insn_bigstring
  : Db.t =
  let open Db in
  match Db.find db key with
  | Some (
      { events = events_for_key
      ; operands = operands_for_key
      ; _
      } as value) ->
    Db.add db key
      { value with
        events = new_events::events_for_key
      ; operands = new_operands::operands_for_key
      }
  | None ->
    Db.add db key
      { insn = insn_bigstring
      ; events = [new_events]
      ; operands = [new_operands]
      }

let get_previous_code_exec index (trace : event Array.t) dis endian =
  let open Option in
  let open Value.Match in
  let rec aux index =
    (try Some trace.(index) with | _ -> None)
    >>= fun event ->
    event
    |> select @@
    case Event.code_exec (fun code_chunk ->
        begin match get_insn_of_chunk code_chunk dis endian with
          | Ok (insn,_) -> Some (insn,index)
          | Error _ -> None
        end)
    @@ default (fun () -> aux (index-1))
  in
  aux index

let delay_slot_insns index trace dis endian =
  let open Option in
  let open Value.Match in
  get_previous_code_exec (index-1) trace dis endian
  >>= fun (previous_insn,index') ->
  get_previous_code_exec (index'-1) trace dis endian
  >>= fun (previous_previous_insn,_) ->
  match Disasm.Dis.Insn.kinds previous_previous_insn with
  | #Kind.affecting_control :: _ ->
    Some (previous_previous_insn,previous_insn)
  | _ -> None

let info_of_insn insn =
  let name = Disasm.insn_name insn in
  let code = Disasm.Dis.Insn.code insn in
  let asm = Disasm.Dis.Insn.asm insn in
  let ops = Disasm.Dis.Insn.ops insn in
  let num_ops = Array.length ops in
  name,code,asm,num_ops,ops

(**
Delay slot correction. E.g.,
Recording events for BGEZAL:
-=-=-=-=
   pc-update: 0x4011C8:32
   pc-update: 0x4011CC:32
Recording events for SLL:
-=-=-=-=
   pc-update: 0x4011CC:32
   pc-update: 0x4011D0:32

The jump pc update occurs for the event in SLL. It needs to be swapped
with the pc-update recorded for the jump, which advanced to the delay slot.
*)
let fix_up_events branch_events delay_slot_events =
  let open Option in
  let get_first = function
    | [] -> None
    | hd :: _ -> Some hd in
  let get_last l = List.nth l ((List.length l)-1) in
  let update_last l event =
    List.rev l
    |> function
    | [] -> []
    | _ :: tl -> event::tl |> List.rev
  in
  let update_first l event =
    match l with
    | [] -> []
    | _ :: tl -> event::tl
  in
  get_first branch_events >>= fun last_branch_events ->
  get_last last_branch_events >>= fun last_branch_pc_update ->
  get_first delay_slot_events >>= fun last_delay_slot_events ->
  get_last last_delay_slot_events >>= fun last_delay_slot_pc_update ->
  if verbose then
    (Format.printf "Found last delay slot pc update:@.";
     Format.printf "\t %a@." Value.pp last_delay_slot_pc_update;
     Format.printf "Last branch pc update:@.";
     Format.printf "\t %a@." Value.pp last_branch_pc_update);
  Some (update_first branch_events
          (update_last last_branch_events last_delay_slot_pc_update),
        update_first delay_slot_events
          (update_last last_delay_slot_events last_branch_pc_update))

let record_new db events_so_far current_insn =
  match current_insn with
  | Some (insn,insn_bigstring) ->
    info_of_insn insn |> fun (name,code,asm,num_ops,ops) ->
    let key = code,num_ops in
    if verbose then
      (Format.printf "Recording events for %s,%s:@." name asm;
       Db.print_events events_so_far;
       Format.printf "-----@.");
    update_db db key events_so_far ops insn_bigstring
  | None -> db

let record_with_fixup db delay_slot_insns index trace dis endian =
  let open Option in
  let key_of_info insn =
    let _,code,_,num_ops,_ = info_of_insn insn in
    (code,num_ops) in
  (delay_slot_insns index trace dis endian
   >>= fun (branch_insn,delay_slot_insn) ->
   let branch_key = key_of_info branch_insn in
   let delay_slot_key = key_of_info delay_slot_insn in
   if verbose then
     (Format.printf "delay slot triggered.@.";
      let delay_name,_,delay_asm,_,_ = info_of_insn delay_slot_insn in
      Format.printf "after branch, previous insn: %s,%s@."
        delay_name delay_asm;
      let branch_name,_,branch_asm,_,_ = info_of_insn branch_insn in
      Format.printf "branch, previous previous: %s,%s@."
        branch_name branch_asm);
   Db.find db branch_key
   >>= fun ({ Db.events = branch_events; _ } as branch_value) ->
   Db.find db delay_slot_key
   >>= fun ({ Db.events = delay_slot_events; _ } as delay_slot_value) ->
   fix_up_events branch_events delay_slot_events
   >>= fun (branch_events, delay_slot_events) ->
   let add db key value events = Db.add db key { value with Db.events} in
   add db branch_key branch_value branch_events
   |> fun db ->
   add db delay_slot_key delay_slot_value delay_slot_events
   |> return)
  |> Option.value ~default:db

let record trace dis endian index (db,events_so_far,current_insn) event =
  let open Value.Match in
  let open Option in
  if verbose then
    (Format.printf "Event: %d@." index;
     Format.printf "\t %a@." Value.pp event);
  event |>
  select @@
  case Event.code_exec (fun code_chunk ->
      let db = record_new db events_so_far current_insn in
      let events_so_far = [] in
      (* do fixup if delay slot just finished executing *)
      let db =
        record_with_fixup db delay_slot_insns index trace dis endian in
      (* start new *)
      match get_insn_of_chunk code_chunk dis endian with
      | Ok (insn,insn_bigstring) ->
        if verbose then
          (let name,code,asm,num_ops,ops = info_of_insn insn in
           Format.printf
             "Starting new event recording for %s,%s@." name asm);
        let pc_is = Value.create Event.pc_update (Chunk.addr code_chunk) in
        let events_so_far = [pc_is] in
        (db,events_so_far,Some (insn,insn_bigstring))
      | Error _ -> (db,events_so_far,None)
    ) @@
  default (fun () -> (db,events_so_far @ [event],current_insn))

let process file : (Db.t, Error.t) Result.t =
  let uri = Uri.of_string ("file:" ^ file) in
  match Trace.load uri with
  | Error e ->
    Error
      (Error.of_string
       @@ Format.sprintf "Error loading trace: %s@." (string_of_error e));
  | Ok trace ->
    match Dict.find (Trace.meta trace) Meta.arch with
    | Some arch ->
      let events : event Seq.t = Trace.read_events trace in
      let endian = Arch.endian arch in
      let run dis =
        let dis = Disasm.Dis.store_asm dis |> Disasm.Dis.store_kinds in
        let lookup = Seq.to_array events in
        let trace_db,_,_ =
          List.foldi
            ~init:([],[],None)
            ~f:(record lookup dis endian)
            (Array.to_list lookup)
        in
        Or_error.return trace_db
      in
      Disasm.Dis.with_disasm ~backend:"llvm" (Arch.to_string arch) ~f:run
    | None ->
      Error (Error.of_string "Unrecognized architecture")

(* TODO: fix up merge dbs *)
let merge_db options db =
  let open Cmdline.Options in
  match options.with_db with
  | Some fname ->
    (try Db.serialize db ~fname with
     | Sys_error s ->
       Format.printf "Could not update db: %s" s)
  | None -> ()

let main project argv =
  let options = Cmdline.parse (List.to_array argv) in
  let open Cmdline.Options in
  let files =
    if Sys.is_directory options.trace_path
    then read_dir options.trace_path
    else [options.trace_path] in
  let process_one file =
    match process file with
    | Error e ->
      Format.eprintf
        "Error processing trace: %s@."
        (Error.to_string_hum e)
    | Ok db ->
      if options.pp_db then
        Db.print db;
      merge_db options db
  in
  List.iter ~f:process_one files
