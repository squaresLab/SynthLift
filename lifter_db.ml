open Core_kernel.Std
open Bap.Std

(** (bil * sat permutation indices) candidates *)
type value' =
  (bil * int list) list
  [@@deriving sexp]

(** opcode key *)
type key' = int [@@deriving sexp]

type t = (key', value') List.Assoc.t

let add db key value = List.Assoc.add db key value

let find db key = List.Assoc.find db key

(** XXX Copy pasta of tracer_db. *)
let serialize (db : t) ~fname:filename : unit =
  let add channel =
    List.iter ~f:(fun (key,value) ->
        let line = [%sexp_of: key' * value'] (key,value) in
        Sexp.output_hum channel line) db in
  Out_channel.with_file filename ~f:add

let deserialize ~fname:filename : t =
  let add_entry_of_sexp db entry =
    let key,value = [%of_sexp: key' * value'] entry in
    List.Assoc.add db key value in
  In_channel.with_file filename ~f:(fun channel ->
      Sexp.input_sexps channel
      |> List.fold ~init:[] ~f:add_entry_of_sexp)

let print (db : t) =
  let print_bils (bils : bil list) =
    List.iter ~f:(fun bil -> Format.printf "\t%a@." Bil.pp bil) bils in
  List.iter db ~f:(fun (code,value) ->
      Format.printf "Code: %d Lifts to candidates: " code;
      print_bils (List.map ~f:(fun (bil,_) -> bil) value))
