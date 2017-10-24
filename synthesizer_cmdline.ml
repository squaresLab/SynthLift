open Core_kernel.Std
open Format

module Options = struct
  type t =
    { with_db : string option
    ; io_db : string option
    ; sketch_db : string option
    }
end

include Cmdliner

let with_db : string option Term.t =
  let doc = "Output synthesizer rules to db" in
  Arg.(value & opt (some string) None & info ["with-synthesizer-db"] ~doc)

let io_db : string option Term.t =
  let doc = "Input IO db" in
  Arg.(value & opt (some string) None & info ["synthesizer-io-db"] ~doc)

let sketch_db : string option Term.t =
  let doc = "Input sketch db" in
  Arg.(value & opt (some string) None & info ["synthesizer-sketch-db"] ~doc)

let process_args with_db io_db sketch_db =
  let open Options in
  { with_db
  ; io_db
  ; sketch_db
  }

let info = Term.info ~doc:"" "Lifter Synthesizer"

let parse argv =
  let args = Term.(pure process_args $ with_db $ io_db $ sketch_db) in
  match Term.eval ~argv (args,info) with
  | `Ok res -> res
  | `Error err -> exit 1
  | `Version | `Help -> exit 0
