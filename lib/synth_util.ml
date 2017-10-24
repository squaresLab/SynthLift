open Bap.Std
open Core_kernel.Std

module Hole = Synth_hole

let _v1 = Var.create "_1:v" bool_t |> Bil.var
let _v2 = Var.create "_2:v" bool_t |> Bil.var
let _v3 = Var.create "_3:v" bool_t |> Bil.var
let _v4 = Var.create "_4:v" bool_t |> Bil.var
let _v5 = Var.create "_5:v" bool_t |> Bil.var
let _v6 = Var.create "_6:v" bool_t |> Bil.var
let _v7 = Var.create "_7:v" bool_t |> Bil.var

let _i1 = Word.of_int ~width:32 0x1 |> Bil.int
let _i2 = Word.of_int ~width:32 0x2 |> Bil.int
let _i3 = Word.of_int ~width:32 0x3 |> Bil.int
let _i4 = Word.of_int ~width:32 0x4 |> Bil.int
let _i5 = Word.of_int ~width:32 0x5 |> Bil.int
let _i6 = Word.of_int ~width:32 0x6 |> Bil.int
let _i7 = Word.of_int ~width:32 0x7 |> Bil.int

let vars = [_v1;_v2;_v3;_v4;_v5;_v6;_v7]
let ints = [_i1;_i2;_i3;_i4;_i5;_i6;_i7]

(** TODO: use max_size *)
let n_holes ?(offset=0) n : Hole.t list =
  let (!) x = fun _ -> x in
  if n > 7 || offset > 7 || n + offset > 7
  then failwith "Maximum 7 holes supported";
  match offset with
  | 0 -> List.take (List.map ~f:(!) vars) n
  | _n -> List.split_n vars _n |> fun
            (_,vars) -> List.take (List.map ~f:(!) vars) n

(** Adjacent swap permutation *)
(**# swap [1;2;3;4;5;6];;
- : int list list =
[[2; 1; 3; 4; 5; 6]; [1; 3; 2; 4; 5; 6]; [1; 2; 4; 3; 5; 6];
 [1; 2; 3; 5; 4; 6]; [1; 2; 3; 4; 6; 5]]**)
let swap l =
  let rec aux (front : 'a list) (back : 'a list) : 'a list list =
    match back with
    | [] -> []
    | a::[] -> []
    | a::b::tl ->
      (front@b::a::tl)::(aux (front@[a]) (b::tl))
  in l::(aux [] l)

let save_lift_db ~fname lift_db =
  match lift_db with
  | Some db -> Synth_lift_db.serialize ~fname db
  | None -> ()
