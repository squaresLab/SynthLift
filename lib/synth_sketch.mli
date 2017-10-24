open Bap.Std

type t = {
  wrapper : Synth_stmt.t
; freq    : int
}

val to_string : t -> string
