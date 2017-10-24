open Core_kernel.Std
open Bap.Std
open Bap_traces.Std

type event = Trace.event

type operands = Op.t array
[@@deriving sexp]

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

val add : t -> key' -> value' -> t

val find : t -> key' -> value' option

val serialize : t -> fname:string -> unit

val deserialize : fname:string -> t

val print_events : Trace.event list -> unit

val print : t -> unit
