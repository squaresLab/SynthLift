open Core_kernel.Std
open Bap.Std
open Format

module Err = Synth_err
open Err
module Partial = Synth_partial
module Hole = Synth_hole

type partial = Partial.t

type t =
  { partial : partial
  ; vars  : int
  ; imms  : int
  ; sz    : int
  }

let destruct_rec (e : exp) pos : t =
  let vars = ref 0 in
  let imms = ref 0 in
  let rec destruct (e: exp) pos : partial * int =
    try
      match e with
      (* return the 'hole', could be var or int*)
      | Bil.Var _ ->
        vars := !vars + 1;
        Partial.pick pos `Var,(pos+1)
      (* return the 'hole', could be var or int*)
      | Bil.Int _ ->
        imms := !imms + 1;
        Partial.pick pos `Int,(pos+1)
      | Bil.UnOp (op, e) ->
        destruct e pos |> fun (cl1,pos) ->
        (fun _1 _2 _3 _4 _5 _6 _7 ->
           cl1 _1 _2 _3 _4 _5 _6 _7 |> fun e' ->
           Bil.unop op e'),pos
      | Bil.BinOp (b, e1, e2) ->
        destruct e1 pos |> fun (cl1,pos) ->
        destruct e2 pos |> fun (cl2,pos) ->
        (fun _1 _2 _3 _4 _5 _6 _7 ->
           cl1 _1 _2 _3 _4 _5 _6 _7 |> fun e1' ->
           cl2 _1 _2 _3 _4 _5 _6 _7 |> fun e2' ->
           Bil.binop b e1' e2'),pos
      | Bil.Load (mem,addr,en,sz) ->
        destruct addr pos |> fun (cl2,pos) ->
        (fun _1 _2 _3 _4 _5 _6 _7 ->
           cl2 _1 _2 _3 _4 _5 _6 _7 |> fun addr ->
           Bil.load ~mem ~addr en sz),pos
      | Bil.Store (mem,addr,e,en,sz) ->
        destruct addr pos |> fun (cl2,pos) ->
        destruct e pos    |> fun (cl3,pos) ->
        (fun _1 _2 _3 _4 _5 _6 _7 ->
           cl2 _1 _2 _3 _4 _5 _6 _7 |> fun addr ->
           cl3 _1 _2 _3 _4 _5 _6 _7 |> fun e' ->
           Bil.store ~mem ~addr e' en sz),pos
      | Bil.Cast (c,i,e) ->
        (* We can't destruct i because it is an int and not an expression. We
            should use propositions for the types we care about, at some
            point *)
        destruct e pos |> fun (cl1,pos) ->
        (fun _1 _2 _3 _4 _5 _6 _7 ->
           cl1 _1 _2 _3 _4 _5 _6 _7 |> fun e' ->
           Bil.cast c i e'),pos
      | Bil.Ite (if_,then_,else_) ->
        destruct if_ pos   |> fun (cl1,pos) ->
        destruct then_ pos |> fun (cl2,pos) ->
        destruct else_ pos |> fun (cl3,pos) ->
        (fun _1 _2 _3 _4 _5 _6 _7 ->
           cl1 _1 _2 _3 _4 _5 _6 _7 |> fun if_ ->
           cl2 _1 _2 _3 _4 _5 _6 _7 |> fun then_ ->
           cl3 _1 _2 _3 _4 _5 _6 _7 |> fun else_ ->
           Bil.ite ~if_ ~then_ ~else_),pos
      | Bil.Extract (hi,lo,e) ->
        destruct e pos |> fun (cl1,pos) ->
        (fun _1 _2 _3 _4 _5 _6 _7 ->
           cl1 _1 _2 _3 _4 _5 _6 _7 |> fun e' ->
           Bil.extract hi lo e'),pos
      | Bil.Concat (e1,e2) ->
        destruct e1 pos |> fun (cl1,pos) ->
        destruct e2 pos |> fun (cl2,pos) ->
        (fun _1 _2 _3 _4 _5 _6 _7 ->
           cl1 _1 _2 _3 _4 _5 _6 _7 |> fun e1' ->
           cl2 _1 _2 _3 _4 _5 _6 _7 |> fun e2' ->
           Bil.concat e1' e2'),pos
      | Bil.Let (v',e1,e2) ->
        destruct e1 pos |> fun (cl1,pos) ->
        destruct e2 pos |> fun (cl2,pos) ->
        (fun _1 _2 _3 _4 _5 _6 _7 ->
           cl1 _1 _2 _3 _4 _5 _6 _7 |> fun e1' ->
           cl2 _1 _2 _3 _4 _5 _6 _7 |> fun e2' ->
           Bil.let_ v' e1' e2'),pos
      | Bil.Unknown (s,t) -> raise Destruct_fail
    with | _ -> raise Destruct_fail in
  let partial,sz = destruct e pos in
  { partial
  ; sz
  ; vars = !vars
  ; imms = !imms
  }

let size {sz;_} = sz

let vars {vars;_} = vars

let imms {imms;_} = imms

(** There is one exception to using move: if we see a store then
    mem := mem[v1 + i1] <- v2, and
    lhs needs to be filled with mem and not a hole. similarly, update
    counts.
    The convention is to case out on sz = 0 for a exp_wrapper: that
    means always substitute with var mem
*)

let destruct_exp ?(mem=Var.create "mem" (mem32_t `r32)) exp : t option =
  try match exp with
    | Bil.Var v when v = mem ->
      Option.return @@
      {partial = Partial.default;
       vars = 0;
       imms = 0;
       sz = 0}
    | _ -> Option.return @@ destruct_rec exp 0
  with | Destruct_fail ->
    Format.fprintf err_formatter
      "Warning: unknown in destruct OR too many args\
       in expression %a@." Exp.pp exp;
    None

let _v1 = Var.create "_v1" bool_t |> Bil.var
let _v2 = Var.create "_v2" bool_t |> Bil.var
let _v3 = Var.create "_v3" bool_t |> Bil.var
let _v4 = Var.create "_v4" bool_t |> Bil.var
let _v5 = Var.create "_v5" bool_t |> Bil.var
let _v6 = Var.create "_v6" bool_t |> Bil.var
let _v7 = Var.create "_v7" bool_t |> Bil.var

let _i1 = Bil.int (Word.of_int ~width:32 0x0)
let _i2 = Bil.int (Word.of_int ~width:32 0x1)
let _i3 = Bil.int (Word.of_int ~width:32 0x2)
let _i4 = Bil.int (Word.of_int ~width:32 0x3)
let _i5 = Bil.int (Word.of_int ~width:32 0x4)
let _i6 = Bil.int (Word.of_int ~width:32 0x5)
let _i7 = Bil.int (Word.of_int ~width:32 0x6)

let vars_ = [_v1;_v2;_v3;_v4;_v5;_v6;_v7]
let ints_ = [_i1;_i2;_i3;_i4;_i5;_i6;_i7]

let fill_dummy ?(mem=Var.create "mem" (mem32_t `r32))
    ?offset_var ?offset_imm
    ({partial;sz;_} : t) : exp =
  let open Hole in
  let init = function
    | Some v -> ref v
    | None -> ref 0 in
  let pos_var = init offset_var in
  let pos_int = init offset_imm in
  let fill_one pos l =
    match List.nth l !pos with
    | Some v -> pos := !pos + 1; v
    | None ->
      printf "Warning: fill_dummy unable to pick var or int at pos %d@." !pos;
      raise Bad_number_args
  in
  let _' = function
    | `Var -> fill_one pos_var vars_
    | `Int -> fill_one pos_int ints_ in
  match sz with
  | 0 ->  partial
            (fun _ -> fill_one pos_var [Bil.Var mem]) _' _' _' _' _' _'
  | _ ->    partial _' _' _' _' _' _' _'

let fill ?(mem=Var.create "mem" (mem32_t `r32))
    ?offset_var ?offset_imm (vars,imms)
    ({partial;sz;_} : t) : exp =
  let open Hole in
  let init = function
    | Some v -> ref v
    | None -> ref 0 in
  let pos_var = init offset_var in
  let pos_int = init offset_imm in
  let fill_one pos l =
    match List.nth l !pos with
    | Some v -> pos := !pos + 1; v
    | None ->
      printf "Warning: fill_dummy unable to pick var or int at pos %d@." !pos;
      raise Bad_number_args
  in
  let _' = function
    | `Var -> fill_one pos_var vars
    | `Int -> fill_one pos_int imms in
  match sz with
  (* Fill first part with mem *)
  | 0 ->  partial
            (fun _ -> fill_one pos_var [Bil.Var mem]) _' _' _' _' _' _'
  (* fill the argument at pos in l *)
  | _ ->    partial _' _' _' _' _' _' _'
