open Core_kernel.Std
open Format

module Options = struct
  type t =
    { with_db : string option
    ; with_stats : string option
    ; pp_db : bool
    ; pp_stats : bool
    ; bil_size : int
    }
end

include Cmdliner

let with_db : string option Term.t =
  let doc =
    "If not specified, will not save the results to a file.\
     If specified but file does not exist, will create and save.\
     If non-empty, with-miner-db will merge the results of the \
     current binary with those in the file." in
  Arg.(value & opt (some string) None & info ["with-miner-db"] ~doc)

let with_stats : string option Term.t =
  let doc = "Same as with_db, but for stats. Probably not super useful." in
  Arg.(value & opt (some string) None & info ["with-miner-stats"] ~doc)

let pp_db : bool Term.t =
  let doc = "Pretty print a db" in
  Arg.(value & flag & info ["pp-miner-db"] ~doc)

let pp_stats : bool Term.t =
  let doc =
    "Print stats of image \
     (number of Bil statements per asm insn)" in
  Arg.(value & flag & info ["pp-miner-stats"] ~doc)

let bil_size : int Term.t =
  let doc =
    "Number of bil statements to include for synthesis \
     (default = all sizes). Recommend 1 for synthesis." in
  Arg.(value & opt int 0 & info ["bil-sz"] ~doc)

let process_args with_db with_stats pp_db pp_stats bil_size =
  let open Options in
  { with_db
  ; with_stats
  ; pp_db
  ; pp_stats
  ; bil_size
  }

let info = Term.info ~doc:"" "Sketch Miner"

let parse argv =
  let args =
    Term.(pure process_args $ with_db $ with_stats
          $ pp_db $ pp_stats $ bil_size) in
  match Term.eval ~argv (args,info) with
  | `Ok res -> res
  | `Error err -> exit 1
  | `Version | `Help -> exit 0
