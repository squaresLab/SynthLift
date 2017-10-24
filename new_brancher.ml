open Bap.Std
open Bap_ida.Std
open Core_kernel.Std
open Regular.Std

type dest = addr option * edge [@@deriving sexp]

type dests = dest list [@@deriving sexp]

let branch_lookup :
(mem -> Disasm_expert.Basic.full_insn -> dests) option ref = ref None


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

(** Other flow assumes its a jump, when it may be jump cond *)
let handle_other_flow rest =
  let (!) = Word.of_int64 ~width:32 in
  match rest with
  | [] -> []
  | l -> List.fold l ~init:[] ~f:(fun acc dest_addr ->
      (Some !dest_addr, `Jump)::acc)

(** Resolve the destinations for a memory (address) *)
let resolve_dests memory insn lookup arch =
  let open Option in
  addr_of_mem memory >>= fun addr ->
  List.find lookup ~f:(fun (needle,_,_) -> needle = addr) >>=
  fun (_,opt,(rest : int64 list)) ->
  let normal_flow = handle_normal_flow opt in
  let other_flow = handle_other_flow rest in
  other_flow@normal_flow |> return

let set_ida_branches filename arch =
  let lookup = Ida.with_file filename brancher_command in
  (* lookup creates the function for getting destinations of a piece of memory
     (i.e., instruction address) *)
  let lookup = fun mem insn ->
    match resolve_dests mem insn lookup arch with
    | None -> []
    | Some dests -> dests
  in
  branch_lookup := Some lookup
