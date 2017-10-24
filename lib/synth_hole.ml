open Bap.Std

type kind = [`Var | `Int]

(** hole used to be just exp. But, we want to hint a kind. But we can't populate
    a record, because that would mean we 'supply' the kind when invoking.
    Instead, have a callback that provides kind when we need it. This is the
    callback.*)
type t = kind -> exp
