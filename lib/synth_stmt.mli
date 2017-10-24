open Bap.Std

type t

val move : Synth_exp.t -> Synth_exp.t -> t
val jmp : Synth_exp.t -> t
val while_ : Synth_exp.t -> t list -> t
val if_ : Synth_exp.t -> t list -> t list -> t

val destruct_of_stmt : ?mem:var -> stmt -> t option

val fill_dummy : ?mem:var -> t -> stmt

val fill : ?mem:var -> (exp list * exp list) -> t -> stmt

val vars : t -> int

val imms : t -> int
