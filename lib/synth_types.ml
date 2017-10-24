open Bap.Std

module Sketch_db = Synth_sketch_db
module Lift_db = Synth_lift_db

(** A base synthesizer defines a CPU, var_of_reg conversion
    (so that synthesizer
    can convert operands to bil operands) and filter ops
    (so synthesizer can
    remove annoying operands) *)
module type Base = sig
  module CPU : CPU

  val var_of_reg : string -> var

  val filter_ops : op array -> op array
end

(** A synthesizer that learns *)
module type Synthesizer = sig
  (** include Base because it exposes CPU, so that Synthesizers can be
     registered as a new target*)
  include Base

  (*val lift : lifter*)

  (** register a sketch_db *)
  val register_sketch_db : Sketch_db.t -> unit

  (** return all candidates for an instruction, by (#regs,#imms) *)
  val candidates :  Disasm_expert.Basic.full_insn ->
    (unit -> int * bil option * (exp list * exp list))

  (** record an association an insn with a sketch s.t. events constraint *)
  val record :
    Bap.Std.Disasm_expert.Basic.full_insn ->
    bil ->
    int ->
    (exp list * exp list) ->
    Value.Set.t ->
    unit

  (*val save_lift_db : unit -> unit*)
    val get_lift_db : unit -> Lift_db.t option
end

(** A synthesized lifter. Subsumes Target signature *)
module type Lifter = sig

  include Base

  val register_lift_db : Lift_db.t -> unit

  val lift : lifter
end
