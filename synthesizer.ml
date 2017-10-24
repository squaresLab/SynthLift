open Core_kernel.Std
open Bap.Std
open Bap_traces.Std

module Cmdline = Synthesizer_cmdline
module Sketch = Synth_sketch
module Db = Lifter_db

module SM = Monad.State
open SM.Monad_infix

let verbose = false

type event = Trace.event

module Disasm = struct
  module Dis = Disasm_expert.Basic
  open Dis
  type t = (asm, kinds) Dis.t

  let insn dis mem =
    match Dis.insn_of_mem dis mem with
    | Error err -> Error err
    | Ok r -> match r with
      | mem', Some insn, `finished -> Ok (mem',insn)
      | _, None, _ ->
        Error (Error.of_string "nothing was disasmed")
      | _, _, `left _ -> Error (Error.of_string "Overloaded chunk")

  let insn_name = Dis.Insn.name
end

let dump_context ctxt =
  Format.printf "\t\t\t\t==============================@.";
  Format.printf "\t\t\t\t===========CTXT===============@.";
  ctxt#bindings |> Seq.iter ~f:(fun (v,bil_result) ->
      let result = Bil.Result.value bil_result in
      match result with
      | Bil.Imm w ->
        Format.printf "\t\t\t\t\tVar: %a:%a = %a@."
          Var.pp v Type.pp (Var.typ v) Word.pp w;
      | Bil.Mem s ->
        Format.printf "\t\t\t\t\tMemory:@.";
        Set.iter ctxt#addrs ~f:(fun addr ->
            (match s#load addr with
             | Some word ->
               Format.printf "\t\t\t\t\t\t%a -> %a\n" Addr.pp addr Word.pp word
             | None -> ()))
      | Bil.Bot -> ());
  Format.printf "\t\t\t\t==============================@.@."

let print_events ?(indentmore=false) evs =
  List.iter ~f:(fun ev ->
      if indentmore then
        Format.printf "\t\t\t\t\t %a@." Value.pp ev
      else
        Format.printf "\t %a@." Value.pp ev
    ) evs

class context =
  object(self)
    inherit Bili.context as super
    val events : event list = []
    val addrs : (addr,Addr.comparator_witness) Set.t =
      Set.empty ~comparator:Addr.comparator

    method add_event event = {< events = events @ [event] >}
    method drop_events = {< events = [] >}
    method drop_pc = self#with_pc Bil.Bot
    method add_addr addr = {< addrs = Set.add addrs addr >}
    method events = events
    method addrs = addrs
end

let create_move_event tag cell' data' =
  Value.create tag Move.({ cell = cell'; data = data' })

let create_memory_store = create_move_event Event.memory_store
let create_memory_load  = create_move_event Event.memory_load
let create_register_read  = create_move_event Event.register_read
let create_register_write = create_move_event Event.register_write

(** An interpreter that records events for Bil execution *)
class ['a] interpreter =
  object(self)
    constraint 'a = #Bili.context
    inherit ['a] Bili.t as super

    method private add_event event =
      SM.update (fun context -> context#add_event event)

    method private add_addr addr =
      SM.update (fun context -> context#add_addr addr)

    (** emit reg read *)
    method! lookup var =
      super#lookup var >>= fun result ->
      if verbose then
        Format.printf "\tLookup for Var: %a:%a@." Var.pp var Type.pp (Var.typ var);
      SM.get () >>= fun ctxt ->
      (*
      let manual_var_lookup = ctxt#lookup var in
      (match manual_var_lookup with
      | Some res ->
        begin
          match Bil.Result.value res with
          | Bil.Imm data -> Format.printf "YESsuccess@."
          | _ -> Format.printf "faile@."
        end
      | None -> Format.printf "no2@.");
         *)
      match Bil.Result.value result with
      | Bil.Mem _ -> SM.return result (* Mem lookup is handled in load *)
      | Bil.Bot ->
        if verbose then
          Format.printf
            "Tried to lookup for var %s but no data is there@." (Var.name var);
        SM.return result
      | Bil.Imm data ->
        match not (Var.is_virtual var) with
        | false ->
          Format.printf "Virtual var in reg read";
          SM.return result
        | true ->
          if verbose then
              Format.printf "Successful lookup for var, data is: %a@." Addr.pp data;
          self#add_event (create_register_read var data)
          >>= fun () -> SM.return result

    (** emit reg write *)
    method! update var result =
      super#update var result >>= fun () ->
      match Bil.Result.value result with
      | Bil.Mem _ -> SM.return () (* Mem write is handled in store *)
      | Bil.Bot -> SM.return ()
      | Bil.Imm data ->
        match not (Var.is_virtual var) with
        | false ->
          Format.printf "Virtual var in reg write";
          SM.return ()
        | true ->
          self#add_event (create_register_write var data)

    method! eval_store ~mem ~addr data endian size =
      super#eval_store ~mem ~addr data endian size >>= fun result ->
      self#eval_exp addr >>= fun addr ->
      self#eval_exp data >>= fun data ->
      match Bil.Result.value addr, Bil.Result.value data with
      | Bil.Imm addr, Bil.Imm data ->
        self#add_event (create_memory_store addr data)
        >>= fun () ->
        self#add_addr addr
        >>= fun () ->
        SM.return result
      | _ -> SM.return result

    method! eval_load ~mem ~addr endian size =
      (* reduce lookup to memory value *)
      super#eval_load ~mem ~addr endian size
      >>= fun result ->
      (* reduce lookup expression to address *)
      self#eval_exp addr >>= fun addr_result ->
      match Bil.Result.value addr_result with
      | Bil.Imm addr ->
        begin match Bil.Result.value result with
          | Bil.Imm data ->
            Format.printf "Eval load for %a resolved to %a@."
              Addr.pp addr Word.pp data;
            self#add_event (create_memory_load addr data)
            >>= fun () ->
            SM.return result
          | _ ->
            Format.printf "Failed to lookup value of memory addr %a@."
              Addr.pp addr;
            SM.return result
        end
      | _ ->
        Format.printf "Value of address load is an Imm: %a but the \
                       thing the expression I tried to reduce to \
                       addr was %a@." Exp.pp addr Bil.Result.pp addr_result;
        SM.return result

    method! eval_jmp addr =
      super#eval_jmp addr >>= fun () ->
      self#add_pc_update

    method private add_pc_update =
      SM.get () >>= fun ctxt ->
      match ctxt#pc with
      | Bil.Mem _ | Bil.Bot -> SM.return ()
      | Bil.Imm pc ->
        let pc_event = Value.create Event.pc_update pc in
        self#add_event pc_event
end

let partition_to_bil_ops (module Target : Synth_types.Base) ops =
  Array.to_list ops |> fun ops ->
  List.fold ~init:([],[]) ~f:(fun (vars,imms) ->
      function
      | Op.Reg r ->
        ((Bil.var @@ Target.var_of_reg (Bap.Std.Reg.name r))::vars),imms
      | Op.Imm w ->
        (match Imm.to_word ~width:32 w with
         | Some w ->  (vars,(Bil.int w)::imms)
         | None -> failwith "Could not convert word of imm op")
      | Op.Fmm _ -> (vars,imms)) ops |> fun (l1,l2) ->
  (List.rev l1, List.rev l2)

let info_of_insn insn =
  let name = Disasm.insn_name insn in
  let code = Disasm.Dis.Insn.code insn in
  let asm = Disasm.Dis.Insn.asm insn in
  let num_ops = Array.length (Disasm.Dis.Insn.ops insn) in
  name,code,asm,num_ops

let run_generator base_sketch_candidates bil_ops =
  let open Sequence.Generator in
  let init = return () in
  let generator =
    Sequence.of_list base_sketch_candidates |>
    Sequence.fold ~init ~f:(fun gen sketch ->
        let vars,imms = bil_ops in
        (* swap [1;2;3] -> [[1;2;3]; [2;1;3]; [1;3;2]] *)
        Synth_util.swap vars |> fun vars_ops ->
        Synth_util.swap imms |> fun imms_ops ->
        (* cartesian product basically to try combinations of
           swaps in the partition *)
        List.cartesian_product vars_ops imms_ops |>
        Sequence.of_list |>
        Sequence.foldi ~init:gen ~f:(fun permutation_index gen ops ->
            (* putting gen here is just right.
               if it's only outside this loop,
               it returns the first element for each of the inner lists.
               If there are two gens, it will give the inner list
               composed with each element in outer list.
               if in here, as expected. with none, it
               returns only the first of list of lists *)
            gen >>= fun () ->
            let statement = Synth_stmt.fill ops sketch in
            (* we need i to get back the right combination *)
            yield (permutation_index,[statement],ops)))
  in
  run generator

let mem_of_context (context : context) =
  let module Target = Synth_mips in (* XXX *)
  context#bindings
  |> Seq.find_map ~f:(fun (var,result) ->
      match Bil.Result.value result with
      | Bil.Mem storage -> Some (var,storage)
      | _ ->
        Some (Target.CPU.mem, new Bil.Storage.sparse))

let to_32_bit_var var =
  Var.create (Var.name var) reg32_t

let cast_register_size_of_events events =
  let open Value.Match in
  let (!) = to_32_bit_var in
  List.map events ~f:(fun event ->
        event |>
        select
        @@ case Event.register_read (fun event ->
            let var = !(Move.cell event) in
            let value = Move.data event in
            create_register_read var value)
        @@ case Event.register_write (fun event ->
            let var = !(Move.cell event) in
            let value = Move.data event in
            create_register_write var value)
        @@ default (fun _ -> event))

let input_context_of_events events : context =
  let open Value.Match in
  (* Vars from trace are size 1. Cast all the reg
     read/writes to 32 *)
  List.foldi events ~init:(new context) ~f:(fun i context event ->
      if i = 0 then
        event
        |> select
        @@ case Event.pc_update (fun addr ->
            let context = context#add_event event in
            context#with_pc (Bil.Imm addr))
        @@ default (fun _ -> failwith "expected pc_update")
      else
        event
        |> select
        @@ case Event.register_read (fun event ->
            let var = Move.cell event in
            let context,value = context#create_word (Move.data event) in
            context#update var value)
        @@ case Event.memory_load (fun event ->
            let addr = Move.cell event in
            let value = Move.data event in
            (* get memory from context *)
            let mem_var,storage =
              Option.value_exn (mem_of_context context) in
            (* 32 bit memory so save each byte. It is far too annoying to
               reimplement Expi.store_word so we'll just have the interpreter
               do this for us. We're replacing this borken part:

               let storage = storage#save addr value in
               let context,storage_result = context#create_storage storage in

               Annoyingly, everything has to be cast to expressions now that can
               be evaluated *)
            let interp = new bili in
            let storage_result,context =
              SM.run
                (interp#eval_store
                   ~mem:(Bil.var mem_var)
                   ~addr:(Bil.int addr)
                   (Bil.int value)
                   LittleEndian (* XXX Executable is actually BigEndian I think *)
                   `r32)
                context
            in
            let context = context#update mem_var storage_result in
            let context = context#add_addr addr in
            context)
        (* pc_update events after the first one
           are ignored (it's not input) *)
        @@ default (fun _ -> context)
    )

let post_process_events ground_truth (test : event list) =
  let open Value.Match in
  match List.rev test with
  | (event :: _) as tl ->
    let event' : event option =
      event
      |> select
      (* insn was a jump or update pc *)
      @@ case Event.pc_update (fun _ -> None)
      @@ default (fun _ -> (* no pc after execution, add it *)
          begin match List.rev ground_truth with
            | hd_event :: _ -> Some hd_event
            | [] -> failwith "Ground truth doesn't contain events"
          end)
    in
    begin match event' with
      | Some event' ->
        (* although order doesn't matter at this point, it's
           good (but inefficient) for debugging to reverse *)
        event'::tl |> List.rev
      | None -> test
    end
  | [] -> []


(** Dismiss spurious events generated by QEMU. We cannot dismiss writes. Reads
    are OK. *)
let dismiss_spurious ground_truth =
  let open Value.Match in
  Set.for_all ground_truth ~f:(fun event ->
      event
        |> select
        (* we cannot dismiss writes in ground truth *)
        @@ case Event.register_write (fun _ -> false)
        @@ case Event.memory_store (fun _ -> false)
        @@ default (fun _ -> true))

(** verify one event 'list' *)
let verify_one bil (events : event list) : bool =
  (* load context *)
  Format.printf "\t\t\t\tPopulating context with events@.";
  print_events ~indentmore:true events;
  let events = cast_register_size_of_events events in
  let context = input_context_of_events events in
  dump_context context;
  (* run bil and collect events *)
  let interpreter = new interpreter in
  let context = SM.exec (interpreter#eval bil) context in
  let emitted_events = context#events in
  let module TraceComparator =
  struct
    type t = Trace.event [@@deriving compare, sexp_of]
  end in
  let module C = Comparator.Make(TraceComparator) in
  let ground_truth_set = Set.of_list C.comparator events in
  let emitted_events = post_process_events events emitted_events in
  let test_set = Set.of_list C.comparator emitted_events in
  match Set.equal ground_truth_set test_set with
  | true ->
    Format.printf "Success:@.";
    Format.printf "\t+++Bil generated these:++@.";
    print_events emitted_events;
    Format.printf "\t+++Expected are these:+++@.";
    print_events events;
    Format.printf "\t+++++++++++++++++++++++++@.";
    true
  | false ->
    let result_set = Set.symmetric_diff ground_truth_set test_set in
    (*Format.printf "Failure:@.";
      Format.printf "\t---Bil generated these:--@.";
      print_events emitted_events;
      Format.printf "\t---Expected are these:---@.";
      print_events events;
      Format.printf "\t-------------------------@.";*)


    let generated_contains_things_not_in_ground =
      Seq.exists result_set ~f:(function
          | First event ->
            Format.printf "\t\tEvent %a in GROUND not in TEST@."
              Value.pp event;
            false
          | Second event ->
            Format.printf "\t\tEvent %a in TEST not in GROUND@."
              Value.pp event;
            true
        ) in
    match generated_contains_things_not_in_ground with
    (* if generated things contain spurious events not in ground,
       reject *)
    | true -> false
    | false ->
      match dismiss_spurious ground_truth_set with
      | true ->
        Format.printf "\t\t OK if dismiss spurious@.";
        true
      | _ -> false

(** Give the indices that you would like to include in
    the permutation generator *)
let permutation_generator_of_selections
    ( base_sketch_candidates : Synth_stmt.t list)
    bil_ops
    indices_selection =
  let open Sequence.Generator in
  let init = return () in
  let generator =
    Sequence.of_list base_sketch_candidates
    |> Sequence.fold ~init ~f:(fun gen sketch ->
        let vars,imms = bil_ops in
        (* swap [1;2;3] -> [[1;2;3]; [2;1;3]; [1;3;2]] *)
        Synth_util.swap vars |> fun vars_ops ->
        Synth_util.swap imms |> fun imms_ops ->
        (* cartesian product basically to try combinations of
           swaps in the partition *)
        List.cartesian_product vars_ops imms_ops
        (* filter operands from cartesian product, but save the index so that we
           can propagate it *)
        |> List.filter_mapi ~f:(fun index ops ->
            match List.exists indices_selection ~f:((=) index) with
            | true -> Some (index,ops)
            | false -> None)
        |> Sequence.of_list
        |> Sequence.fold ~init:gen ~f:(fun gen (permutation_index,ops) ->
            (* putting gen here is just right.
               if it's only outside this loop,
               it returns the first element for each of the inner lists.
               If there are two gens, it will give the inner list
               composed with each element in outer list.
               if in here, as expected. with none, it
               returns only the first of list of lists *)
            gen >>= fun () ->
            let statement = Synth_stmt.fill ops sketch in
            (* we need i to get back the right combination *)
            yield (permutation_index,[statement],sketch,ops)))
  in
  run generator


let to_string stmt =
  Synth_sketch.to_string { Synth_sketch.wrapper = stmt; freq = 0 }

(** For a single bil candidate, run over the events and
    return how many events it satisfied and how many it didnt *)
let verify_events
    (module Target : Synth_types.Base)
    (base_sketch_candidates : Synth_stmt.t list)
    insn
    (all_events : event list list)
    (all_operands : Op.t array list) =
  let name,code,asm,num_ops = info_of_insn insn in
  let all_indices =
    match all_operands with
    | ops :: _ ->
      let vars,imms = partition_to_bil_ops (module Target) ops in
      Synth_util.swap vars |> fun vars_ops ->
      Synth_util.swap imms |> fun imms_ops ->
      List.cartesian_product vars_ops imms_ops
      |> List.mapi ~f:(fun i _ -> i)
    | _ -> []
  in
  let all_selections =
    (* compose all_indices with all base_sketch_candidates *)
    List.concat_map
      base_sketch_candidates
      ~f:(fun sketch -> [sketch,all_indices])
  in
  (* aux processes the next event *)
  let rec aux event_pairs (selections : (Synth_stmt.t * int list) list) iteration =
    (* strict mode: when selections is 0,
       don't bother continuing *)
    if selections = [] then [],iteration
    else match event_pairs with
      | [],[] -> selections,iteration
      | events::tl_events,operands::tl_operands ->
        let bil_ops = partition_to_bil_ops (module Target) operands in
        let generator =
          selections
          |> List.map
            ~f:(fun (base_sketch_candidate,permutation_selections) ->
                permutation_generator_of_selections
                  [base_sketch_candidate]
                  bil_ops
                  permutation_selections)
          |> Seq.of_list
          |> Seq.concat
        in
        let (sat_selections : (Synth_stmt.t * int list) list),_ =
          Seq.fold
            generator
            ~init:([],[])
            (* XXX dont really need unsat*)
            ~f:(fun
                 (sat,unsat)
                 (permutation_index,bil,sketch,(reg_ops,imm_ops)) ->
                 Format.printf "[[[Ops: ( %a ) ( %a ) ]]]@.@."
                   Sexp.pp (sexp_of_list sexp_of_exp reg_ops)
                   Sexp.pp (sexp_of_list sexp_of_exp imm_ops);
                 Format.printf "Bil candidate: %a@." Bil.pp bil;
                 match verify_one bil events with
                 (* Optimize: associate multiple indices with one sketch *)
                 | true -> (sketch,[permutation_index])::sat,unsat
                 | false -> sat,permutation_index::unsat)
        in
        Format.printf
          "Forwarding indices and sketches %a@."
          Sexp.pp (sexp_of_list (fun (sketch,indices) ->
              [%sexp_of: string * int list]
                ((to_string sketch),indices))
              sat_selections);
        aux (tl_events,tl_operands) sat_selections (iteration+1)
      | _ -> failwith "Invariant broken: events and ops list not equal size"
  in
  let result = aux (all_events,all_operands) all_selections 0 in
  match result with
  | [],iteration ->
    Format.printf
      "HARD FAIL for %s. Successful iterations (#events sat) %d@."
      name
      iteration;
    []
  | sat,iteration ->
    (* for some bil there is an operand permutation(s) that is sat for all
       events *)
    Format.printf
      "SUCCESS FOR ALL iterations (#events sat) %d : %s : %a@."
      iteration
      name
      Sexp.pp (sexp_of_list (fun (sketch,indices) ->
          [%sexp_of: string * int list]
            ((to_string sketch),indices))
          sat);
    sat

let synthesize_one_insn
    (module Target : Synth_types.Base)
    (sketch_db : Sketch_db.t)
    insn
    (events : event list list)
    (operands : Op.t array list) =
  let name,code,asm,num_ops = info_of_insn insn in
  Format.printf "Synthesizing for instruction %s,%s (CODE %d)@." name asm code;
  (* Use GENERAL ops to get sketches *)
  let ops = Disasm_expert.Basic.Insn.ops insn in
  let ops = Target.filter_ops ops in
  let base_sketch_candidates =
    Sketch_db.key_of_ops ops
    |> Sketch_db.get sketch_db
    |> List.map ~f:(fun { Sketch.wrapper; _ } -> wrapper)
  in
  Format.printf
    "\tNumber-of-base-sketch-candidates: %d@."
    (List.length base_sketch_candidates);
  Format.printf
    "\tNumber-of-events: %d@."
    (List.length events);
  verify_events (module Target) base_sketch_candidates insn events operands

let insn_of_bigstring insn dis endian =
  let open Or_error in
  insn
  |> Memory.create endian (Word.of_int ~width:32 0) >>= fun mem ->
  Disasm.insn dis mem >>= fun (_,insn) ->
  Or_error.return insn

let synthesize dis endian io_db (sketch_db : Sketch_db.t) =
  let open Tracer_db in
  let module Target = Synth_mips in (* XXX *)
  List.fold ~init:[]
    ~f:(fun lifter_db ((code,num_ops), {insn; events; operands}) ->
        let insn = insn_of_bigstring insn dis endian in
        match insn with
        | Ok insn ->
          let sat =
            synthesize_one_insn
              (module Target)
              sketch_db
              insn
              events
              operands
          in
          let sat =
            List.map
              ~f:(fun (sketch,indices) ->
              ([Synth_stmt.fill_dummy sketch],indices))
              sat
          in
          Db.add lifter_db code sat
        | Error _ -> Format.printf "Disassembly failed"; lifter_db)
    io_db

(** Does not use project *)
let main _ argv =
  let open Cmdline.Options in
  let open Option in
  let options = Cmdline.parse (List.to_array argv) in
  let arch = value_exn (Arch.of_string "mips") in (* XXX *)
  (options.io_db >>= fun io_db_file ->
   options.sketch_db >>= fun sketch_db_file ->
   options.with_db >>= fun with_db_filename ->
   let io_db = Tracer_db.deserialize ~fname:io_db_file in
   let miner_db = Miner_db.deserialize ~fname:sketch_db_file in
   (* partition all mined sketches into the sketch db *)
   let sketch_db = Sketch_db.of_db miner_db in
   let endian = Arch.endian arch in
   begin match Disasm.Dis.create ~backend:"llvm" (Arch.to_string arch) with
     | Ok dis ->
       let dis = Disasm.Dis.store_asm dis |> Disasm.Dis.store_kinds in
       let db = synthesize dis endian io_db sketch_db in
       Db.serialize db ~fname:with_db_filename;
       return ()
     | Error _ -> return ()
   end)
  |> function
  | Some _
  | None -> ()
