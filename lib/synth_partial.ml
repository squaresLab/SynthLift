open Bap.Std
open Format

module Err = Synth_err
open Err

module Hole = Synth_hole

type hole = Hole.t

type t = (hole -> hole -> hole -> hole -> hole -> hole -> hole -> exp)

(** chooses the nth arg, which is a function. Performs a callback
    with the appropriate 'kind' hint *)
let pick n (v : Hole.kind) : t =
  match n with
  | 0 -> (fun f _ _ _ _ _ _ -> f v)
  | 1 -> (fun _ f _ _ _ _ _ -> f v)
  | 2 -> (fun _ _ f _ _ _ _ -> f v)
  | 3 -> (fun _ _ _ f _ _ _ -> f v)
  | 4 -> (fun _ _ _ _ f _ _ -> f v)
  | 5 -> (fun _ _ _ _ _ f _ -> f v)
  | 6 -> (fun _ _ _ _ _ _ f -> f v)
  | _ ->
    Format.fprintf err_formatter "Warning: partial encountered > 7 pos@.";
    raise Destruct_fail

let default = pick 0 `Var

let max_size _ = 7
