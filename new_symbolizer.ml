open Bap.Std
open Bap_ida.Std
open Core_kernel.Std
open Regular.Std


let symbolizer_command =
  let script =
    {|from bap.utils import ida
ida.service.request(service='symbols', output='$output')
idc.Exit(0)
|} in
  Command.create
    `python
    ~script
    ~parser:(fun name ->
        let blk_of_sexp x = [%of_sexp:string*int64*int64] x in
        In_channel.with_file name ~f:(fun ch ->
            Sexp.input_sexps ch |> List.map ~f:blk_of_sexp))

let symbol_lookup : 'a option ref = ref None

let save_full_symbols filename =
  symbol_lookup := Some (Ida.with_file filename symbolizer_command)
