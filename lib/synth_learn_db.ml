open Bap.Std

module Sketch = Synth_sketch
module Util = Synth_util

open Sketch

type t = (stmt, Sketch.t) Hashtbl.t

let strip s = Stringext.replace_all s ~pattern:"\n" ~with_:""

let create _ : t = Hashtbl.create 13

(** Make a Synth_stmt containing partials into a
    concrete stmt by substituting dummy values *)
let mk_cmp = Synth_stmt.fill_dummy

(** Utility to store a given frequency. *)
let store_with_frequency stmt (s : Synth_stmt.t) n db =
  Hashtbl.add db stmt (s,n)

(** Synth_stmts internally contain Destructs which are functional values and
    cannot be compared. For comparison purposes we populate the structures with
    dummy values. (_1,...) for vars, (0x1, 0x2,...) for ints, which is what
    Destruct.fill does *)
let store (s : Synth_stmt.t) db =
  let stmt = mk_cmp s in
  try
    let w = Hashtbl.find db stmt in
    Hashtbl.replace db stmt {w with freq = w.freq+1}
  with
  | Not_found -> Hashtbl.add db stmt {wrapper = s;freq = 1}

let filter f (db : t) =
  Hashtbl.fold (fun _ {wrapper = s;_} acc ->
      match f s with
      | true -> s::acc
      | false -> acc) db []

let iter f (db : t) : unit =
  Hashtbl.iter (fun _ {wrapper = w; freq} -> f w freq) db

let count (s : Synth_stmt.t) db =
  let stmt = mk_cmp s in
  try Some (match Hashtbl.find db stmt with | {freq;_} -> freq)
  with | Not_found -> None

let size = Hashtbl.length

let serialize ~fname:filename (db : t) : unit =
  let add db ch =
    Hashtbl.iter (fun stmt {freq = i;_} ->
        let open Core_kernel.Std in
        let line = ([%sexp_of: stmt * int] (stmt,i)) in
        Sexp.output_hum ch line (** XXX for human readable debugging *)
      ) db in
  let open Core_kernel.Std in
  Out_channel.with_file filename ~f:(add db)

let deserialize ~fname:filename : t =
  let db = create () in
  let add_entry_of_sexp entry =
    let stmt,freq = Core_kernel.Std.([%of_sexp: stmt * int] entry) in
    match Synth_stmt.destruct_of_stmt stmt with
    | Some wrapper ->
      Hashtbl.add db stmt {wrapper;freq}
    | None -> () in
  Core_kernel.Std.(In_channel.with_file filename ~f:(fun ch ->
      Sexp.input_sexps ch |> List.iter ~f:add_entry_of_sexp));
  db

let add_percent l =
  let open Core_kernel.Std in
  let sum = List.fold ~init:0. ~f:(fun acc (_,{freq;_}) ->
      acc +.(float_of_int freq)) l in
  List.fold ~init:[] ~f:(fun acc (key,{wrapper = s;freq}) ->
      (key,(s,freq,((float_of_int freq)/.sum)*.100.))::acc) l
  |> List.rev

let to_list table =
  Hashtbl.fold (fun key valu acc -> (key,valu)::acc) table []

let sort_by_freq =
  let open Core_kernel.Std in
  List.sort ~cmp:(fun (_,{freq = cnt1;_}) (_,{freq = cnt2;_}) -> cnt2-cnt1)

let pp db =
  let open Core_kernel.Std in
  Format.printf "------------------------------------------------@.";
  Format.printf "Abs freq  | %% freq    | AST @.";
  Format.printf "------------------------------------------------@.";
  List.iter ~f:(fun (key,((s : Synth_stmt.t),freq,freq_percent)) ->
      let s = Stmt.pps () key |> strip in
      Format.printf "[%08d] % 10f   %s@." freq freq_percent s)
    (to_list db |> sort_by_freq |> add_percent)
