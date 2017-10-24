open Core_kernel.Std
open Bap.Std

(** A convenience module that defines register for MIPS. These can be inferred
    from the trace. But, we must satisfy the CPU module type for BAP at compile
    tyime currently, until the CPU module is deprecated, as planned. *)

module CPU : sig
  include Bap.Std.CPU

  (* expose vars for ABI *)
  val a0 : var
  val a1 : var
  val a2 : var
  val a3 : var
  val t1 : var
  val t2 : var

  val v0 : var
  val v1 : var
end

val var_of_reg : string -> var

val filter_ops : op array -> op array
