open Core_kernel.Std
open Regular.Std
open Bap.Std

module Reg = struct
  type t = [
    | `R0 | `AT
    | `V0 | `V1
    | `A0 | `A1 | `A2 | `A3
    | `T0 | `T1 | `T2 | `T3 | `T4 | `T5 | `T6 | `T7
    | `S0 | `S1 | `S2 | `S3 | `S4 | `S5 | `S6 | `S7
    | `T8 | `T9 | `K0 | `K1 | `GP | `SP | `FP | `S8
    | `RA
    | `LO
    | `HI
    | `PC
    | `Nil
  ] [@@deriving bin_io, compare, sexp, enumerate]
  let create reg : t option =
    let sexpable_of_string t_of_sexp name =
      try Some (t_of_sexp @@ Sexp.of_string name)
      with Sexp.Of_sexp_error _ -> None in
    sexpable_of_string t_of_sexp (Reg.name reg)

include Regular.Make(struct
    type nonrec t = t [@@deriving bin_io, compare, sexp]
    let hash (reg : t) = Hashtbl.hash reg
    let module_name = Some "MIPS_SYNTH.Reg"
    let version = "0.1"
    let pp fmt reg =
      let s = Sexp.to_string (sexp_of_t reg) |> String.lowercase in
      Format.fprintf fmt "%s" s
  end)

end

let (%:) name typ = Var.create name typ
let make_register reg ty = Reg.to_string reg %: ty
let reg32 reg = make_register reg reg32_t

(** Satisfy default BAP CPU module *)
module CPU = struct
  (* BAP CPU SIG *)
  let mem = Var.create "mem" (mem32_t `r32)
  let pc = reg32 `PC
  let sp = reg32 `SP

  (* MIPS SPECIFIC *)
  let zero = reg32 `R0
  let at = reg32 `AT
  let v0 = reg32 `V0
  let v1 = reg32 `V1
  let a0 = reg32 `A0
  let a1 = reg32 `A1
  let a2 = reg32 `A2
  let a3 = reg32 `A3
  let t0 = reg32 `T0
  let t1 = reg32 `T1
  let t2 = reg32 `T2
  let t3 = reg32 `T3
  let t4 = reg32 `T4
  let t5 = reg32 `T5
  let t6 = reg32 `T6
  let t7 = reg32 `T7
  let s0 = reg32 `S0
  let s1 = reg32 `S1
  let s2 = reg32 `S2
  let s3 = reg32 `S3
  let s4 = reg32 `S4
  let s5 = reg32 `S5
  let s6 = reg32 `S6
  let s7 = reg32 `S7
  let t8 = reg32 `T8
  let t9 = reg32 `T9
  let k0 = reg32 `K0
  let k1 = reg32 `K1
  let gp = reg32 `GP
  let sp = reg32 `SP
  let fp = reg32 `FP
  let s8 = reg32 `S8
  let ra = reg32 `RA
  let lo = reg32 `LO
  let hi = reg32 `HI

  let gpr = Var.Set.of_list [
      zero; at; v0; v1;
      a0; a1; a2; a3;
      t0; t1; t2; t3; t4; t5; t6; t7;
      s0; s1; s2; s3; s4; s5; s6; s7;
      t8; t9;
      k0; k1;
      gp; sp;
      fp; s8;
      ra;
      lo; hi]


  let nil = reg32 `Nil

  (** MIPS does not have flags registers, but BAP CPU
      requires it. This is a known limitation and will
      be deprecated in the near future.
      cf. https://github.com/BinaryAnalysisPlatform/bap/pull/673 *)
  let nf = "NF" %: bool_t
  let zf = "ZF" %: bool_t
  let cf = "CF" %: bool_t
  let vf = "VF" %: bool_t

  let is = Var.same

  let is_reg r = Set.mem gpr (Var.base r)
  let is_sp = is sp
  let is_bp = is fp
  let is_pc = is pc

  let addr_of_pc m = Addr.(Memory.min_addr m ++ 8)

  let flags = Var.Set.of_list @@ [nf; zf; cf; vf;]

  let is_flag r = Set.mem flags (Var.base r)
  let is_zf = is zf
  let is_cf = is cf
  let is_vf = is vf
  let is_nf = is nf

  let is_mem = is mem

  let var_of_reg : string -> var = function
    | "ZERO" -> zero
    | "AT" -> at
    | "V0" -> v0
    | "V1" -> v1
    | "A0" -> a0
    | "A1" -> a1
    | "A2" -> a2
    | "A3" -> a3
    | "T0" -> t0
    | "T1" -> t1
    | "T2" -> t2
    | "T3" -> t3
    | "T4" -> t4
    | "T5" -> t5
    | "T6" -> t6
    | "T7" -> t7
    | "S0" -> s0
    | "S1" -> s1
    | "S2" -> s2
    | "S3" -> s3
    | "S4" -> s4
    | "S5" -> s5
    | "S6" -> s6
    | "S7" -> s7
    | "T8" -> t8
    | "T9" -> t9
    | "K0" -> k0
    | "K1" -> k1
    | "GP" -> gp
    | "SP" -> sp
    | "FP" -> s8
    | "S8" -> s8
    | "RA" -> ra
    | "LO" -> lo
    | "HI" -> hi
    | s -> Format.printf "Warning: no reg for %s@." s; nil

end

let filter_ops (ops : Op.t array) =
  Array.filter ~f:(function
      | Op.Reg r when Bap.Std.Reg.name r = "Nil"
        -> false
      | Op.Imm w ->
        (match Imm.to_word ~width:32 w with
         | None -> false
         | Some w -> true)
      | Op.Fmm _ -> false
      | _ -> true) ops

let var_of_reg = CPU.var_of_reg
