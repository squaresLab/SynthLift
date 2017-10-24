open Core_kernel.Std
open Bap.Std
open Format
open Bap_c.Std

include Self()

open Synth_err

module Db = Lifter_db
module Cmdline = Lifter_cmdline

(** Synthesis target *)
let arch = `mips
module Target = Synth_mips

let debug = false

(** Use the IDA-provided symbol lookup to resolve the real function target
    address--brancher emits .extern destinations where we really want the true
    address *)
let resolve_true_function_address symbol_lookup extern_addr =
  let open Option in
  begin
    let find_entry_of_start_addr addr =
      List.find symbol_lookup ~f:(fun (_,start_addr,_) ->
          Word.(addr = Word.of_int64 ~width:32 start_addr))
    in
    let find_entry_of_name name extern_addr =
      List.find symbol_lookup ~f:(fun (name',addr,_) ->
          name = name' &&
          (not Word.(extern_addr = Word.of_int64 ~width:32 addr)))
    in
    (* get entry in symbol lookup for extern_addr and get name from that entry*)
    find_entry_of_start_addr extern_addr >>= fun (name,_,_) ->
    (* use name to find the 'other' analog entry with real addr *)
    find_entry_of_name name extern_addr >>= fun (_,addr,_) ->
    return (Word.of_int64 ~width:32 addr)
  end |> function
  | Some addr -> addr
  | None -> extern_addr

let relocate branch_lookup symbol_lookup lift mem insn =
  let open Or_error in
  let find_jump dsts =
    List.find_map dsts ~f:(function
        | Some addr, `Jump -> Some addr
        | _ -> None) in
  let replace_jump bil addr =
    List.map bil ~f:(function
        | Bil.Jmp (Bil.Int _) ->
          let true_addr =
            resolve_true_function_address symbol_lookup addr in
          if debug then
              Format.printf "Replacing addr %a with true addr %a@."
              Word.pp addr
              Word.pp true_addr;
          Bil.Jmp (Bil.Int true_addr)
        | s -> s) in
  lift mem insn >>= fun bil ->
  match find_jump (branch_lookup mem insn) with
  | None -> return bil
  | Some addr -> return (replace_jump bil addr)

class map_failures = object(self)
  inherit Term.mapper as super

  method! map_def def =
    super#map_def def |> fun def ->
    match Def.lhs def with
    | v when Var.name v = "nolift" ->
      Def.create Synth_mips.CPU.v0 (Bil.var Synth_mips.CPU.v0)
    | v when Var.name v = "operr" ->
      Def.create Synth_mips.CPU.v0 (Bil.var Synth_mips.CPU.v0)
    | _ -> Term.set_attr def background `green

  method! map_term cls t =
    super#map_term cls t |> fun t ->
    match Term.get_attr t Disasm.insn with
    | Some insn ->
      let asm = Insn.asm insn in
      Term.set_attr t comment asm
    | None -> t
end

let to_bil_ops ops =
  Array.to_list ops |> fun ops ->
  List.fold ~init:([],[]) ~f:(fun (vars,imms) ->
      function
      | Op.Reg r ->
        ((Bil.var @@ Target.var_of_reg (Bap.Std.Reg.name r))::vars),imms
      | Op.Imm w ->
        (match Imm.to_word ~width:32 w with
         | Some w ->  (vars,(Bil.int w)::imms)
         | None -> failwith "Could not convert word of imm op")
      | Op.Fmm _ -> (vars,imms)) ops |> fun (l1,l2) ->
  (List.rev l1, List.rev l2)

let populate_ops ops (order : int) (bil : stmt list) : bil =
  let filler =
    to_bil_ops ops |> fun (vars,imms) ->
    Synth_util.swap vars |> fun vars_ops ->
    Synth_util.swap imms |> fun imms_ops ->
    List.cartesian_product vars_ops imms_ops |> fun l ->
    List.nth_exn l order in
  List.map ~f:Synth_stmt.destruct_of_stmt bil |>
  List.map ~f:(fun s -> Option.map s ~f:(fun s ->
      Synth_stmt.fill filler s)) |>
  List.fold ~init:[] ~f:(fun acc -> function
      | Some x -> x::acc
      | None -> acc)

module Lifter (Input : sig val db : Db.t end) : Target = struct
  module CPU = Target.CPU

  let lift_helper : lifter = fun mem insn ->
    let code = Disasm_expert.Basic.Insn.code insn in
    let ops = Disasm_expert.Basic.Insn.ops insn in
    let ops = Target.filter_ops ops in
    match code with
    | _ ->
      match Db.find Input.db code with
      | Some ((bil, (permutation_index::_))::_) ->
        begin try Ok (populate_ops ops permutation_index bil) with
          | Bad_number_args ->
            Ok [Bil.move (Var.create "operr" reg32_t)
                  (Bil.unknown "operr" bool_t)]
          | Invalid_argument _ ->
            Ok [Bil.move (Var.create "badlist" reg32_t)
                  (Bil.unknown "badlist" bool_t)]
        end
      | Some []
      | None ->
        Ok [Bil.move (Var.create "nolift" reg32_t)
              (Bil.unknown "nolift" bool_t)]
      | _ -> failwith "Invariant broken: bil exists without permutation index"

  let lift : lifter =
    fun mem insn ->
      let branch_lookup =
        match !New_brancher.branch_lookup with
        | Some branch_lookup -> branch_lookup
        | None -> failwith "Branch lookup not resolved by lift time"
      in
      let symbol_lookup =
        match !New_symbolizer.symbol_lookup with
        | Some symbol_lookup -> symbol_lookup
        | None -> failwith "Symbolizer lookup not resolved by lift time"
      in
      relocate branch_lookup symbol_lookup lift_helper mem insn
end

module Mips_abi = struct

let size = object(self)
  inherit C.Size.base `ILP32
  method! enum s = self#integer `uint
end

module Stack = C.Abi.Stack

let stack n = Stack.create `mips n

let ret (c_return : C.Type.t) =
  match c_return with
  | `Void -> None
  | other as t ->
    let data = C.Abi.data size t in
    Some (data, (Bil.var Synth_mips.CPU.v0))

let args sub _ {C.Type.Proto.return; args=params} =
  info "Populating API for %s@." (Sub.name sub);
  let return = ret return in
  let params =
    List.foldi params ~init:[] ~f:(fun index args (n,t) ->
        match List.nth (Synth_mips.CPU.[a0;a1;a2;a3;t1;t2]) index with
        | Some register ->
          (C.Abi.data size t, Bil.var register)::args
        | None -> args) in
  Some C.Abi.
         { return
         ; hidden = []
         ; params = List.rev params
         }

let abi =
  C.Abi.
    { insert_args = args
    ; apply_attrs = fun _ -> ident
    }

let api arch =
  C.Abi.create_api_processor arch abi
end


module MipsBrancher = struct
  open Bap_ida.Std
  open Regular.Std
  open Bap_future.Std

  let brancher_command =
    let script =
      {|from bap.utils.ida import *
ida.service.request(service='brancher', output='$output')
idc.Exit(0)
|} in
    Command.create
      `python
      ~script
      ~parser:(fun name ->
          let branch_of_sexp x =
            [%of_sexp: int64 * int64 option * int64 list] x in
          In_channel.with_file name ~f:(fun ch ->
              Sexp.input_sexps ch |> List.map ~f:branch_of_sexp))

  module Brancher_info = struct
    type t = (int64 * int64 option * int64 list) list

    include Data.Make(struct
        type t = (int64 * int64 option * int64 list) list
        let version = "0.1"
      end)
  end

  let addr_of_mem mem =
    Memory.min_addr mem
    |> Bitvector.to_int64
    |> function
    | Ok addr -> Some addr
    | Error _ -> None

  let handle_normal_flow =
    let (!) = Word.of_int64 ~width:32 in
    function
    | Some fall -> [Some !fall, `Fall]
    | None -> []

  let handle_other_flow default rest =
    let (!) = Word.of_int64 ~width:32 in
    match rest with
    | [] -> []
    | l -> List.fold l ~init:[] ~f:(fun acc dest_addr ->
        match List.find default ~f:(fun (addr,_) -> addr = Some !dest_addr) with
        (* If IDA dest addr is in default BIL brancher, use same edge info *)
        | Some (Some addr,kind) -> (Some !dest_addr, kind)::acc
        (* XXX heuristic: we are assuming `Jump always (when it could be cond) *)
        | _ -> (Some !dest_addr, `Jump)::acc)


  let resolve_dests memory insn lookup arch =
    let open Option in
    addr_of_mem memory >>= fun addr ->
    (* Only process further if this addr is in our lookup: i.e., we know
       that we have branch information for it. *)
    List.find lookup ~f:(fun (needle,_,_) -> needle = addr) >>=
    (* provide the default information to help with other_flow *)
    let default_brancher = Brancher.of_bil arch in
    let default = Brancher.resolve default_brancher memory insn in
    fun (_,opt,(rest : int64 list)) ->
      let normal_flow = handle_normal_flow opt in
      let other_flow = handle_other_flow default rest in
      other_flow@normal_flow |> return

  let register filename arch =
    let lookup =
      let lookup = Ida.with_file filename brancher_command in
      match lookup with
      | [] ->
        warning "didn't find any branches";
        info "this plugin doesn't work with IDA free";
        fun mem insn -> []
      | lookup -> fun mem insn ->
        match resolve_dests mem insn lookup arch with
        | None -> []
        | Some dests -> dests
    in
    let source =
      Stream.merge
        Project.Info.file
        Project.Info.arch
        ~f:(fun file arch ->
            Or_error.try_with (fun () -> Brancher.create lookup))
    in
    Brancher.Factory.register "mips" source

end

let main project argv =
  let open Cmdline.Options in
  let open Option in
  let options = Cmdline.parse (List.to_array argv) in
  (options.lifter_db >>= fun lifter_db_filename ->
   let db = Db.deserialize ~fname:lifter_db_filename in
   register_target arch (module Lifter(struct let db = db end));
   let filename =
     match Project.get project filename with
     | None -> failwith "No filename"
     | Some filename ->
       Format.printf "Processing filename: %s@." filename;
       filename
   in
   New_brancher.set_ida_branches filename arch;
   New_symbolizer.save_full_symbols filename;
   MipsBrancher.register filename arch;
   info "Registered Mips brancher";
   let file_input = Project.Input.file ~loader:"ida" ~filename in
   let rooter = Rooter.Factory.find "ida" in
   let symbolizer = Symbolizer.Factory.find "ida" in
   let brancher = Brancher.Factory.find "mips" in
   let project =
     Project.create
       ~disassembler:"llvm"
       ?brancher
       ?rooter
       ?symbolizer
       file_input
   in
   match project with
   | Ok project ->
     (* register the abi with api *)
     Bap_api.process (Mips_abi.api Mips_abi.size);
     (* run the api pass *)
     let api_pass = Project.find_pass "api" |> Option.value_exn in
     let project = Project.Pass.run api_pass project in
     let project = match project with
       | Ok project -> project
       | Error Project.Pass.Unsat_dep (_,e) ->
         failwith @@ sprintf "API pass error: %s" e
       | Error Project.Pass.Runtime_error (pass,exn) ->
         failwith @@ sprintf "Pass %s error: %s" (Project.Pass.name pass)
         @@ Exn.to_string exn
     in
     let program = Project.program project in
     let mapper = new map_failures in
     let program = mapper#run program in
     Some (Project.with_program project program)
   | Error e ->
     failwith
     @@ Format.sprintf "Some error setting up: %s@."
     @@ Error.to_string_hum e)
  |> function
  | Some project -> project
  | None -> failwith "Missing arguments"
