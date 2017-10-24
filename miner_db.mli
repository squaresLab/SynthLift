open Bap.Std

module Sketch = Synth_sketch

type t = (stmt, Sketch.t) Hashtbl.t

val create : unit -> t

val store : Synth_stmt.t -> t -> unit

val filter : (Synth_stmt.t -> bool) -> t -> Synth_stmt.t list

(** Iter provides the statement and count in int *)
val iter : (Synth_stmt.t -> int -> unit) -> t -> unit

val count : Synth_stmt.t -> t -> int option

val size : t -> int

val serialize : fname:string -> t -> unit

val deserialize : fname:string -> t

val pp : t -> unit
