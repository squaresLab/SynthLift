open Bap.Std

(** for each native insn, fst is the frequency, snd is the size of bil *)
type t = (string, int * int) Hashtbl.t

let create _ : t = Hashtbl.create 13

let store (s : insn) bil_size db =
  let s = Format.sprintf "%a" Insn.pps s in
  try
    let (cnt,bil_size) = Hashtbl.find db s in
    Hashtbl.replace db s (cnt+1,bil_size)
  with | Not_found -> Hashtbl.add db s (1,bil_size)

let to_list table =
  Hashtbl.fold (fun key valu acc -> (key,valu)::acc) table []

let sort_by_freq =
  let open Core_kernel.Std in
  List.sort ~cmp:(fun (_,(cnt1,_)) (_,(cnt2,_)) -> cnt2-cnt1)

let add_percentage l =
  let open Core_kernel.Std in
  let sum =
    List.fold ~init:0. ~f:(fun acc (_,freq) -> acc+.(float_of_int freq)) l in
  List.fold ~init:[] ~f:(fun acc (bil_size,freq) ->
      (bil_size,freq,((float_of_int freq)/.sum)*.100.)::acc) l

let add_percent2 l =
  let open Core_kernel.Std in
  let sum = List.fold ~init:0. ~f:(fun acc (_,(freq,_)) ->
      acc+.(float_of_int freq)) l in
  List.fold ~init:[] ~f:(fun acc (k,(freq,bil_sz)) ->
      (k,(freq,(((float_of_int freq)/.sum)*.100.),bil_sz))::acc) l

let sort_by_bil_size =
  let open Core_kernel.Std in
  List.sort ~cmp:(fun (bil_size1,_,_) (bil_size2,_,_) -> bil_size1-bil_size2)

let aggregate_by_bil_size db =
  let agg = Hashtbl.create 13 in
  Hashtbl.iter (fun key (freq,bil_size) ->
      try
        let freq' = Hashtbl.find agg bil_size in
        Hashtbl.replace agg bil_size (freq+freq')
      with | Not_found -> Hashtbl.add agg bil_size freq) db;
  agg

let serialize ~fname:filename (db : t) : unit =
  let add db ch =
    Hashtbl.iter (fun k v ->
        let open Core_kernel.Std in
        let line = ([%sexp_of: string * (int * int)] (k,v)) in
        Sexp.output_hum ch line) db in
  let open Core_kernel.Std in
  Out_channel.with_file filename ~f:(add db)

let deserialize ~fname:filename : t =
  let db = create () in
  let add_entry_of_sexp entry =
    let k,v = Core_kernel.Std.([%of_sexp: string * (int * int)] entry) in
    Hashtbl.add db k v in
  Core_kernel.Std.(In_channel.with_file filename ~f:(fun ch ->
      Sexp.input_sexps ch |> List.iter ~f:add_entry_of_sexp));
  db

let pp db =
  let open Core_kernel.Std in
  Format.printf "--------------------------------@.";
  Format.printf "Bil sz | Abs freq | %% freq | Insn name@.";
  Format.printf "--------------------------------@.";
  List.iter ~f:(fun (key,(freq,freq_percent,bil_size)) ->
      Format.printf "[%02d] [%07d] [%10f] %s@."
        bil_size freq freq_percent key)
    (to_list db |> sort_by_freq |> add_percent2 |> List.rev);

    Format.printf "--------------------------------@.";
  Format.printf "Bil Sz | Abs freq | %% freq@.";
  Format.printf "--------------------------------@.";
  List.iter ~f:(fun (bil_size,freq,freq_percent) ->
      Format.printf "% 6d % 10d % 10f@." bil_size freq freq_percent)
    (aggregate_by_bil_size db |> to_list
     |> add_percentage |> sort_by_bil_size);
  Format.printf "--------------------------------@."
