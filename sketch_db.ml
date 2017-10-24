open Bap.Std

module Db = Synth_learn_db
module Sketch = Synth_sketch

open Sketch

type t = (int * int, Sketch.t list) Hashtbl.t

let create _ : t = Hashtbl.create 13

let add db _ ({wrapper;_} as value) =
  let key = (Synth_stmt.vars wrapper, Synth_stmt.imms wrapper) in
  try
    let ws = Hashtbl.find db key in
    let cmp = fun {freq = f1;_} {freq = f2;_} -> f2-f1 in
    let ws' = value::ws |> List.sort cmp in
    Hashtbl.replace db key ws'
  with
  | Not_found -> Hashtbl.add db key [value]

let get db key =
  try Hashtbl.find db key with
  | Not_found -> []

let key_of_ops (ops : op array) : (int * int) =
  let open Core_kernel.Std in
  Array.fold ~init:(0,0) ~f:(fun (vars,imms) op ->
    match op with
    | Op.Reg _ -> (vars+1,imms)
    | Op.Imm _ -> (vars,imms+1)
    | Op.Fmm _ -> (vars,imms)) ops |> fun (l,r) ->
  (l,r)

let of_db (db : Db.t) =
  let sketch_db = create () in
  Hashtbl.iter (add sketch_db) db;
  sketch_db

let pp db =
  Format.printf "----------------------@.";
  Format.printf "(#var,#imm) | #cands@.";
  Format.printf "----------------------@.";
  Hashtbl.iter (fun (k1,k2) l ->
      Format.printf "(%d,%d) : %d@." k1 k2 @@ List.length l) db
