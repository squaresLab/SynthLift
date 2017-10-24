open Core_kernel.Std
open Bap.Std
open Format
include Self()

let main project =
  match Array.to_list argv with
  | x :: "--miner" :: argv ->
    Miner.main project (x::argv);
    project
  | x :: "--tracer" :: argv ->
    Tracer.main project (x::argv);
    project
  | x :: "--synthesizer" :: argv ->
    Synthesizer.main project (x::argv);
    project
  | x :: "--lifter" :: argv ->
    Lifter.main project (x::argv)
  | _ ->
    failwith "Unrecognized subcommand, use one of \
              '--miner', '--tracer', '--synthesizer', or '--lifter'"

(** pass through project, even though it's only important for --lifter *)
let () = Project.register_pass main
