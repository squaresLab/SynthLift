open Bap.Std

type t = {
  wrapper : Synth_stmt.t
; freq    : int
}

let to_string {wrapper;freq} =
  let s = Stmt.to_string (Synth_stmt.fill_dummy wrapper) in
  Format.sprintf "%s:%d" s freq
