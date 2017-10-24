open Core_kernel.Std
open Format

module Options = struct
  type t =
    { with_db : string option
    ; pp_db : bool
    ; trace_path : string
    }
end

include Cmdliner

let trace_path : string option Term.t =
  let doc =
    "Input file with extension .frames of directory with .frames files" in
  Arg.(value & opt (some string) None & info ["trace"] ~doc)

let with_db : string option Term.t =
  let doc =
    "If not specified, will not save the results to a file.\
     If specified but file does not exist, will create and save.\
     If non-empty, with-db will merge the results of the \
     current binary with those in the file." in
  Arg.(value & opt (some string) None & info ["with-tracer-db"] ~doc)

let pp_db : bool Term.t =
  let doc = "Pretty print a db of traces" in
  Arg.(value & flag & info ["pp-tracer-db"] ~doc)

let process_args with_db pp_db trace_path =
  let open Options in
  match trace_path with
  | Some trace_path ->
    { with_db
    ; pp_db
    ; trace_path
    }
  | None -> failwith "You must provide a trace path"

let info = Term.info ~doc:"" "Trace processing"

let parse argv =
  let args =
    Term.(pure process_args $ with_db $ pp_db $ trace_path) in
  match Term.eval ~argv (args, info) ~catch:false with
  | `Ok opts -> opts
  | `Error `Parse -> exit 64
  | `Error _ -> exit 2
  | _ -> exit 1
