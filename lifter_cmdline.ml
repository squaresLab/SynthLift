open Core_kernel.Std
open Format

module Options = struct
  type t =
    { lifter_db : string option }
end

include Cmdliner

let lifter_db : string option Term.t =
  let doc = "Input lifter db" in
  Arg.(value & opt (some string) None & info ["lifter-db"] ~doc)

let process_args lifter_db =
  let open Options in
  { lifter_db }

let info = Term.info ~doc:"" "Lift with synthesized lifter"

let parse argv =
  let args = Term.(pure process_args $ lifter_db) in
  match Term.eval ~argv (args,info) with
  | `Ok res -> res
  | `Error err -> exit 1
  | `Version | `Help -> exit 0
