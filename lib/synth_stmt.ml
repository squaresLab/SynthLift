open Bap.Std
open Format
open Core_kernel.Std
open Option

(** This is the wrapper class for Bil statements. Basically, for every Bil
    statement, the destruct operation will store the result in this form,
    using these types *)

type partial =
  | Move of  Synth_exp.t * Synth_exp.t
  | Jmp of   Synth_exp.t
  | If of    Synth_exp.t * t list * t list
  | While of Synth_exp.t * t list
and t =
  { partial : partial
  ; vars    : int
  ; imms    : int
  }

let move d1 d2 =
  { partial = Move (d1,d2)
  ; vars = (Synth_exp.vars d1) + (Synth_exp.vars d2)
  ; imms = (Synth_exp.imms d1) + (Synth_exp.imms d2)
  }

let jmp d1 =
  { partial = Jmp d1
  ; vars = Synth_exp.vars d1
  ; imms = Synth_exp.imms d1
  }

let f1 acc {vars;_} = acc+vars
let f2 acc {imms;_} = acc+imms
let sum f = List.fold ~init:0 ~f

let if_ d1 (d2l : t list) (d3l : t list) =
  let vars = (sum f1 d2l) + (sum f1 d3l) in
  let imms = (sum f2 d2l) + (sum f2 d3l) in
  { partial = If (d1, d2l, d3l)
  ; vars = (Synth_exp.vars d1) + vars
  ; imms = (Synth_exp.imms d1) + imms
  }

let while_ d1 d2l =
  let vars = sum f1 d2l in
  let imms = sum f2 d2l in
  { partial = While (d1,d2l)
  ; vars = (Synth_exp.vars d1) + vars
  ; imms = (Synth_exp.imms d1) + imms
  }

let vars {vars;_} = vars

let imms {imms;_} = imms

let destruct_of_stmt ?(mem=Var.create "mem" (mem32_t `r32))
    (stmt : stmt) : t option =
  match stmt with
  | Bil.Move (v,e) ->
    Synth_exp.destruct_exp ~mem (Bil.var v) >>= fun d1 ->
    Synth_exp.destruct_exp ~mem e >>= fun d2 ->
    return @@ move d1 d2
  | Bil.Jmp e -> Synth_exp.destruct_exp ~mem e >>= fun e ->
    return @@ jmp e
  | Bil.If (_,_,_) (* TODO import from master *)
  | Bil.While (_,_)
  | Bil.Special _
  | Bil.CpuExn _ -> None

let fill_dummy
    ?(mem=Var.create "mem" (mem32_t `r32))
    ({partial;_} as _' : t) : stmt =
  match partial with
  | Move (f1,f2) ->
    let res1 = Synth_exp.fill_dummy ~mem f1 in
    (match res1 with
     (* if we substituted mem, don't increment the offset var *)
     | Bil.Var v when v = mem ->
       let exp = Synth_exp.fill_dummy ~mem f2 in
       Bil.move v exp
     | Bil.Var v ->
       (* offset_var:1 means that a var is already inserted (of the total
          counts), so start at index one of default var list when filling *)
       let exp = Synth_exp.fill_dummy ~offset_var:1 ~mem f2 in
       Bil.move v exp
     | _ -> failwith "Expected Var")
  | Jmp f ->
    let exp = Synth_exp.fill_dummy ~mem f in
    Bil.jmp exp
  (* TODO import from master *)
  | If (_,_,_) -> failwith "Not handling right now"
  | While (_,_) -> failwith "Not handling right now"

let fill ?(mem=Var.create "mem" (mem32_t `r32)) fillers {partial;_} =
  match partial with
  | Move (f1,f2) ->
    let res1 = Synth_exp.fill ~mem fillers f1 in
    (* filling f1 *should* just be a var *)
    (match res1 with
     (* if we substituted mem, don't increment the offset var *)
     | Bil.Var v when v = mem ->
       let exp = Synth_exp.fill ~mem fillers f2 in
       Bil.move v exp
     | Bil.Var v ->
       (* offset_var:1 means that a var is already inserted (of the total
          counts), so start at index one of default var list when filling *)
       let exp = Synth_exp.fill ~mem ~offset_var:1 fillers f2 in
       Bil.move v exp
     | _ -> failwith "Expected Var")
  | Jmp f ->
    let exp = Synth_exp.fill ~mem fillers f in
    Bil.jmp exp
  | If (_,_,_) -> failwith "Not handling right now"
  | While (_,_) -> failwith "Not handling right now"
