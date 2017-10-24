open Bap.Std

module Hole = Synth_hole

type hole = Hole.t

type t = (hole -> hole -> hole -> hole -> hole -> hole -> hole -> exp)

(** Pick a positional argument in the partial function (hole). Kind
    will be a 'kind' hint when filling the closure *)
val pick : int -> Hole.kind -> t

(** just a default closure*)
val default : t

(** Maximum supported size of closure *)
val max_size : t -> int
