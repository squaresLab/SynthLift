open Bap.Std

type t

(** Number of arguments (holes) in the Destruct closure *)
val size : t -> int

val vars : t -> int

val imms : t -> int

(** Destruct a Bil exp. Target module is necessary to check mem var *)
val destruct_exp : ?mem:var -> exp -> t option

(** Fills a destruct with dummy variables. Important for Db. The (int * int)
    pair indicates the count of variables (like registers, in fst, and imms in
    snd
*)
val fill_dummy : ?mem:var -> ?offset_var:int -> ?offset_imm:int -> t -> exp

val fill :
  ?mem:var ->
  ?offset_var:int ->
  ?offset_imm:int ->
  (exp list * exp list) ->
  t ->
  exp
