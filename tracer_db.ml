(** Tracer DB:
    Include all static information from disassembling instructions,
    and all recorded input output pairs
*)

open Core_kernel.Std
open Bap.Std
open Bap_traces.Std

type event = Trace.event

type operands = Op.t array
[@@deriving sexp]

(** We save the events with associated operands.
    Invariant: events length is same as operand length.
*)
type value' =
  { insn : Bigstring.t
  ; events : Trace.event list list
  ; operands : operands list
  }
  [@@deriving sexp]

(** key is [instruction code] and [number of instruction ops as per
    disassembly] *)
type key' =
  (int * int)
  [@@deriving sexp]

type t = (key', value') List.Assoc.t

let add db key value = List.Assoc.add db key value

let find db key = List.Assoc.find db key

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

let print_events evs =
  Format.printf "-=-=-=-=@.";
  List.iter ~f:(fun ev -> Format.printf "\t %a@." Value.pp ev) evs

let print db =
  List.iter db ~f:(fun ((code,num_ops),{insn;events}) ->
      Format.printf "Events for instruction %d,#%d@." code num_ops;
      List.iter ~f:print_events events)
