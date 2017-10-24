open Core_kernel.Std
open Bap.Std

(** (bil * sat permutation indices) candidates *)
type value' =
  (bil * int list) list
  [@@deriving sexp]

(** opcode key *)
type key' = int [@@deriving sexp]

type t = (key', value') List.Assoc.t

val add : t -> key' -> value' -> t

val find : t -> key' -> value' option

val serialize : t -> fname:string -> unit

val deserialize : fname:string -> t

val print : t -> unit
