open Bap.Std

module List = Core_kernel.Std.List

(* So, this stores, for each code, the possible bil, and the events
   associated with that bil that passed. *)

module Synth_result = struct
  open Core_kernel.Std

  (** vars * imms bil ops *)
  type operands = (exp list * exp list)
  [@@deriving sexp]

  type t =
    { name : string (* insn mnemonic name *)
    ; evs : (operands * Bap.Std.Value.Set.t) list
    }
  [@@deriving sexp]
end

type db_value =
  (int * bil,      (* index in the adjacency permutation list, bil *)
   Synth_result.t) (* constraints *)
    Hashtbl.t

type t = (int,  (* code *)
          db_value) Hashtbl.t

let create _ : t = Hashtbl.create 13

(** Turns a specific bil to normalized 'dummy' bil *)
let to_normal (l : stmt list) : stmt list =
  let open Core_kernel.Std in
  List.map ~f:Synth_stmt.destruct_of_stmt l
  |> List.map ~f:(fun synth_value_opt ->
      match synth_value_opt with
      | Some v -> Some (Synth_stmt.fill_dummy v)
      | None -> None)
  |> List.fold ~init:[] ~f:(fun acc -> function
      | Some x -> x::acc
      | None -> acc)

(** transform each stmt in the bil to a destruct version, fill it with dummy
    values, and insert. this is so that we can perform dedup

    bil'' is NOT ordered *)
let store insn (bil'' : bil) (order:int) evs_update db =
  let module List = Core_kernel.Std.List in
  let open Synth_result in
  let sketch_bil = to_normal bil'' in
  let code = Bap.Std.Disasm_expert.Basic.Insn.code insn in
  let name = Bap.Std.Disasm_expert.Basic.Insn.name insn in
  try
    (* A list of different bil candidates. e.g., move, store... *)
    let results_table = Hashtbl.find db code in
    let constraints = Hashtbl.find results_table (order,sketch_bil) in
    Hashtbl.replace results_table (order,sketch_bil)
      {constraints with evs = evs_update::constraints.evs}
  with | Not_found ->
    let new_table = Hashtbl.create 13 in
    Hashtbl.add new_table (order,sketch_bil) {name;evs = []};
    Hashtbl.add db code new_table

let cmp =
  let open Synth_result in
  fun {evs = evs1} {evs = evs2} ->
  (List.length evs2) - (List.length evs1)

let iter f db = Hashtbl.iter f db

let constraints_of_code code (db : t) =
  try Some (Hashtbl.find db code) with
  | Not_found -> None

(** The function that returns the SAT bil for the code. *)
let get code (db : t) : (int*bil) option =
  try
    let open Synth_result in
    let res = Hashtbl.find db code in
    (* XXX return the first entry in the table *)
    Hashtbl.iter (fun key valu -> ()) res;
    Hashtbl.fold (fun key valu ((i,res) as acc) ->
        match i with
        | 0 -> (1,Some key)
        | _ -> acc) res (0,None) |> snd
  with Not_found -> None

let serialize ~fname:filename (db :t) : unit =
  let add db ch =
    Hashtbl.iter (fun code table ->
        Hashtbl.iter (fun k v ->
            let open Core_kernel.Std in
            let line =
              [%sexp_of: int * (int*bil) * Synth_result.t] (code,k,v) in
            Sexp.output_hum ch line) table) db in
  Core_kernel.Std.Out_channel.with_file filename ~f:(add db)

let deserialize ~fname:filename : t =
  let db = create () in
  let add_entry_of_sexp i entry =
    let code,k,v =
      Core_kernel.Std.([%of_sexp: int * (int*bil) * Synth_result.t]) entry in
    try
      let table = Hashtbl.find db code in
      Hashtbl.add table k v
    with | Not_found ->
      let table = Hashtbl.create 13 in
      Hashtbl.add table k v;
      Hashtbl.add db code table in
  Core_kernel.Std.(In_channel.with_file filename ~f:(fun ch ->
      Sexp.input_sexps ch |> List.iteri ~f:add_entry_of_sexp));
  db

let pp (db : t) =
  let open Bap.Std in
  let open Synth_result in
  Hashtbl.iter (fun code table ->
      Format.printf "Insn code: %d@." code;
      Hashtbl.iter (fun (order,bil_sketch) {evs;name} ->
          Format.printf "\tNat: %s@." name;
          Format.printf "\tBil: %a@." Bil.pp bil_sketch;
          Format.printf "\tId : %d@." order;
          Format.printf "\tEvs: %d@." @@ List.length evs
        ) table
    ) db
