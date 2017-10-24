open Core_kernel.Std
open Bap.Std
open Graphlib.Std
open Format

module Cmdline = Miner_cmdline
module Stats = Miner_stats
module Db = Miner_db

class bil_visitor db arch (state : unit) =
  object(self)
    inherit [unit] Stmt.visitor as super

    method! enter_stmt stmt state =
      let module Target = (val target_of_arch arch) in
      super#enter_stmt stmt state |> fun _ ->
      Option.(Synth_stmt.destruct_of_stmt stmt >>= fun s ->
              Db.store s db; return s) |> ignore
  end

class block_visitor ?(filter = Fn.id) db arch =
  let add_bil exps = (new bil_visitor db arch ())#run exps () in
  object(self)
    inherit [block, Graphs.Cfg.edge, unit] Graphlib.dfs_identity_visitor

    method! enter_node _ b m =
      Block.insns b |>
      List.map ~f:snd |>
      List.map ~f:Insn.bil |>
      List.map ~f:filter |>
      List.iter ~f:add_bil
  end

let (>|=) x f = Option.(x >>= fun x' -> Option.return (f x'))

class block_visitor_with_stats ?(filter = Fn.id) db arch stats = object(self)
  inherit block_visitor ~filter db arch as super

  method! enter_node i b m =
    super#enter_node i b m |> fun _ ->
    Block.insns b |>
    List.map ~f:snd |> fun (insns : insn list) ->
    List.map ~f:Insn.bil insns |> fun bils ->
    List.zip insns bils >|=
    List.iter ~f:(fun (insn,bils) ->
        Stats.store insn (List.length bils) stats)
    |> ignore
end

let l1 = function
  | [_] as ok -> ok
  | _ -> []

let apply_filter options =
  match options.Cmdline.Options.bil_size with
  | 0 -> Fn.id
  | 1 -> l1
  | _ -> Fn.id

let run db proj options stats : unit =
  let arch = Project.arch proj in
  let filter = apply_filter options in
  let visitor =
    match options.Cmdline.Options.with_stats with
    | Some _ -> new block_visitor_with_stats ~filter db arch stats
    | None -> new block_visitor ~filter db arch
  in
  let syms = Project.symbols proj in
  Symtab.to_sequence syms |> Seq.iter ~f:(fun (name,_,cfg) ->
      Graphlib.depth_first_visit (module Graphs.Cfg) cfg ~init:() visitor)

(** Either get a db from existing file, or create a new one *)
let get_learn_db options =
  match options.Cmdline.Options.with_db with
  | Some fname ->
    (try Db.deserialize ~fname with
     | Sys_error s ->
       Format.printf "File does not exist, creating it...@.";
       let _' = open_out fname in
       Out_channel.close _';
       Db.create ())
  | None -> Db.create ()

let get_stats options =
  match options.Cmdline.Options.with_stats with
  | Some fname ->
    (try Stats.deserialize ~fname with
     | Sys_error s ->
       Format.printf "Stats file does not exist, creating it...@.";
       let _' = open_out fname in
       Out_channel.close _';
       Stats.create ())
  | None -> Stats.create ()

(** If we were given with_db, update it with enriched db *)
let merge_db options db =
  match options.Cmdline.Options.with_db with
  | Some fname ->
    (try Db.serialize fname db with
     | Sys_error s ->
       Format.printf "Could not update db: %s" s)
  | None -> ()

let merge_stats options stats =
  match options.Cmdline.Options.with_stats with
  | Some fname ->
    (try Stats.serialize fname stats with
     | Sys_error s ->
       Format.printf "Could not update db: %s" s)
  | None -> ()

let print_learn_db options db =
  match options.Cmdline.Options.pp_db with
  | true -> Db.pp db
  | false -> ()

let print_stats options stats =
  let open Cmdline.Options in
  match options.pp_stats,options.with_stats with
  | true,Some _ -> Stats.pp stats
  | true,None -> Format.printf "No stats collected. Use --with-stats@."
  | false,_ -> ()

let main project argv =
  let options = Cmdline.parse (List.to_array argv) in
  let learn_db = get_learn_db options in
  let stats = get_stats options in
  run learn_db project options stats;
  merge_db options learn_db;
  merge_stats options stats;
  print_learn_db options learn_db;
  print_stats options stats
