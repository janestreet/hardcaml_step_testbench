open! Import
open Hardcaml_waveterm

module Source = struct
  type 'a t =
    { valid : 'a
    ; data : 'a [@bits 32]
    ; first : 'a
    ; last : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module I = struct
  type 'a t =
    { clk : 'a
    ; source : 'a Source.t [@rtlprefix "i_"]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { source : 'a Source.t [@rtlprefix "o_"] } [@@deriving sexp_of, hardcaml]
end

open Signal

let make_circuit (i : Signal.t I.t) =
  let reg_spec = Reg_spec.create () ~clock:i.clk in
  let enable = i.source.valid in
  let count = reg_fb reg_spec ~enable ~w:16 (fun d -> d +:. 1) in
  { O.source = { i.source with data = count @: select i.source.data 15 0 } }
;;

module Tb_source = Hardcaml_step_testbench.Make (Source) (Source)
module Tb = Hardcaml_step_testbench.Make (I) (O)

(* Run an example with 2 tasks.  One task generates a packet framed by a randomly
   generated [valid] signal.  The second task receives the packet.  The data is
   modified by the hardware model by inserting a word count within the upper 16 bits.
   The received packet is shown. *)
let%expect_test "testbench" =
  let module Simulator = Cyclesim.With_interface (I) (O) in
  let simulator = Simulator.create make_circuit in
  let open! Tb.Let_syntax in
  let rec send_data ~first ~num_words _ : unit Tb_source.t =
    if num_words = 0
    then return ()
    else if Random.bool ()
    then (
      let%bind source = Tb_source.cycle Tb_source.input_zero in
      send_data ~first ~num_words source)
    else (
      let%bind source =
        Tb_source.cycle
          { valid = Bits.vdd
          ; data = Bits.of_int ~width:32 num_words
          ; first = (if first then Bits.vdd else Bits.gnd)
          ; last = (if num_words = 1 then Bits.vdd else Bits.gnd)
          }
      in
      send_data ~first:false ~num_words:(num_words - 1) source)
  in
  let rec recv_data data (output : Tb.O_data.t) : Bits.t list Tb.t =
    let source = (Tb.O_data.after_edge output).source in
    if Bits.to_int source.valid <> 1
    then wait_for_next_cycle data
    else if Bits.to_int source.first = 1
    then wait_for_next_cycle [ source.data ]
    else (
      let data = source.data :: data in
      if Bits.to_int source.last = 1
      then return (List.rev data)
      else wait_for_next_cycle data)
  and wait_for_next_cycle data =
    let%bind output = Tb.cycle Tb.input_hold in
    recv_data data output
  in
  let testbench _ =
    (* Demonstrate refining I/O types. *)
    let%bind send_finished =
      Tb_source.spawn_io
        ~inputs:(fun ~(parent : _ I.t) ~child ->
          { parent with source = Tb_source.merge_inputs ~parent:parent.source ~child })
        ~outputs:(fun ~parent ->
          Hardcaml_step_testbench.Before_and_after_edge.map parent ~f:(fun parent ->
            parent.O.source))
        (send_data ~first:true ~num_words:5)
    in
    let%bind recv_finished = Tb.spawn (recv_data []) in
    let%bind () = Tb.wait_for send_finished in
    let%bind recv_packet = Tb.wait_for recv_finished in
    return recv_packet
  in
  let recv_packet = Tb.run_until_finished () ~simulator ~testbench in
  print_s [%sexp (recv_packet : Bits.t list)];
  [%expect
    {|
    (00000000000000010000000000000101
     00000000000000100000000000000100
     00000000000000110000000000000011
     00000000000001000000000000000010
     00000000000001010000000000000001) |}]
;;

let%expect_test "input setting hierarchy" =
  let module Data = struct
    type 'a t =
      { a : 'a [@bits 8]
      ; b : 'a [@bits 8]
      }
    [@@deriving sexp_of, hardcaml]
  end
  in
  let module Data_o = struct
    module T = struct
      include Data

      let t = map t ~f:(fun (n, b) -> n ^ "_o", b)
    end

    include (T : Hardcaml.Interface.S with type 'a t = 'a T.t)
    include Hardcaml.Interface.Make (T)
  end
  in
  let module Tb = Hardcaml_step_testbench.Make (Data) (Data_o) in
  let module Simulator = Cyclesim.With_interface (Data) (Data_o) in
  let simulator = Simulator.create Fn.id in
  let open! Tb.Let_syntax in
  let rec get_outputs count data =
    if count = 0
    then return (List.rev data)
    else (
      let%bind o = Tb.cycle Tb.input_hold in
      let o_after_edge = Tb.O_data.after_edge o in
      get_outputs (count - 1) (Data.map o_after_edge ~f:Bits.to_int :: data))
  in
  (* All combinations of setting ports at 4 levels.  Deepest child should always win.  The
     [a] and [b] ports are set identically, but treated differently by the [input_default]
     argument to [run]. *)
  let levels = 4 in
  let width = 8 in
  let data =
    List.init levels ~f:(fun level ->
      Array.init (1 lsl levels) ~f:(fun index ->
        let x = Bits.of_int ~width:levels index in
        if Bits.(bit x level |> to_int) = 1
        then Bits.of_int ~width (level + 1)
        else Bits.empty))
  in
  let rec set_level data _ =
    match data with
    | [] -> return ()
    | h :: t ->
      let%bind _ = Tb.spawn (set_level t) in
      Tb.for_
        0
        (Array.length h - 1)
        (fun i ->
           let%bind _ = Tb.cycle { a = h.(i); b = h.(i) } in
           return ())
  in
  let data =
    Tb.run_until_finished
      ()
      ~input_default:{ a = Bits.empty; b = Bits.of_int ~width (-1) }
      ~simulator
      ~testbench:(fun _ ->
        let%bind get_outputs_finished =
          Tb.spawn (fun _ -> get_outputs ((1 lsl levels) + 1) [])
        in
        let%bind _ = Tb.spawn (set_level data) in
        let%bind get_outputs_finished = Tb.wait_for get_outputs_finished in
        return get_outputs_finished)
  in
  print_s [%message (data : int Data.t list)];
  [%expect
    {|
    (data (
      ((a 0) (b 255))
      ((a 1) (b 1))
      ((a 2) (b 2))
      ((a 2) (b 2))
      ((a 3) (b 3))
      ((a 3) (b 3))
      ((a 3) (b 3))
      ((a 3) (b 3))
      ((a 4) (b 4))
      ((a 4) (b 4))
      ((a 4) (b 4))
      ((a 4) (b 4))
      ((a 4) (b 4))
      ((a 4) (b 4))
      ((a 4) (b 4))
      ((a 4) (b 4))
      ((a 4) (b 255)))) |}]
;;

let%expect_test "[run] - returns result as option, but only if ready" =
  let module Tb = Hardcaml_step_testbench.Make (Interface.Empty) (Interface.Empty) in
  let module Simulator = Cyclesim.With_interface (Interface.Empty) (Interface.Empty) in
  let simulator = Simulator.create Fn.id in
  let open! Tb.Let_syntax in
  let testbench _ =
    print_s [%message "testbench started"];
    let%bind _ = Tb.cycle ~num_cycles:3 Tb.input_hold in
    print_s [%message "testbench finished"];
    return "Finished"
  in
  let test num_cycles =
    let result =
      Tb.run_with_timeout () ~show_steps:true ~simulator ~timeout:num_cycles ~testbench
    in
    print_s [%message (result : string option)]
  in
  test 2;
  [%expect
    {|
    (step_number 0)
    "testbench started"
    (step_number 1)
    (result ()) |}];
  test 3;
  [%expect
    {|
    (step_number 0)
    "testbench started"
    (step_number 1)
    (step_number 2)
    (result ()) |}];
  test 4;
  [%expect
    {|
    (step_number 0)
    "testbench started"
    (step_number 1)
    (step_number 2)
    (step_number 3)
    "testbench finished"
    (result (Finished)) |}];
  test 5;
  [%expect
    {|
    (step_number 0)
    "testbench started"
    (step_number 1)
    (step_number 2)
    (step_number 3)
    "testbench finished"
    (result (Finished)) |}]
;;

let%expect_test "Spawn tasks sequentially which set the same input." =
  let module I = struct
    type 'a t = { d : 'a [@bits 8] } [@@deriving sexp_of, hardcaml]
  end
  in
  let module O = struct
    type 'a t = { q : 'a [@bits 8] } [@@deriving sexp_of, hardcaml]
  end
  in
  let module Tb = Hardcaml_step_testbench.Make (I) (O) in
  let module Simulator = Cyclesim.With_interface (I) (O) in
  let test normal_spawn =
    let simulator = Simulator.create (fun (x : _ I.t) -> { O.q = x.d }) in
    let waves, simulator = Waveform.create simulator in
    let open! Tb.Let_syntax in
    let set_d _ =
      let%bind _ = Tb.cycle { d = Bits.ones 8 } in
      Tb.cycle { d = Bits.zero 8 }
    in
    let setters _ =
      let spawn task =
        if normal_spawn
        then Tb.spawn task
        else Tb.spawn_io task ~inputs:Tb.merge_inputs ~outputs:(fun ~parent -> parent)
      in
      let%bind h = spawn set_d in
      let%bind _ = Tb.wait_for h in
      let%bind _ = Tb.cycle { d = Bits.one 8 } in
      let%bind h = spawn set_d in
      let%bind _ = Tb.wait_for h in
      return ()
    in
    let testbench _ =
      let%bind _ = Tb.spawn setters in
      let%bind _ = Tb.cycle ~num_cycles:10 { d = Bits.empty } in
      return ()
    in
    Tb.run_until_finished () ~simulator ~testbench;
    Waveform.print ~display_height:9 waves
  in
  test true;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │               ││────────┬───────────────┬───────┬───────┬──────────│
    │d              ││ FF     │00             │01     │FF     │00        │
    │               ││────────┴───────────────┴───────┴───────┴──────────│
    │               ││────────┬───────────────┬───────┬───────┬──────────│
    │q              ││ FF     │00             │01     │FF     │00        │
    │               ││────────┴───────────────┴───────┴───────┴──────────│
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘ |}];
  test false;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │               ││────────┬───────────────┬───────┬───────┬──────────│
    │d              ││ FF     │00             │01     │FF     │00        │
    │               ││────────┴───────────────┴───────┴───────┴──────────│
    │               ││────────┬───────────────┬───────┬───────┬──────────│
    │q              ││ FF     │00             │01     │FF     │00        │
    │               ││────────┴───────────────┴───────┴───────┴──────────│
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;

let%expect_test "Timeout works as expected" =
  let module Simulator = Cyclesim.With_interface (I) (O) in
  let simulator = Simulator.create make_circuit in
  let open! Tb.Let_syntax in
  let testbench ~timeout_in_cycles _ =
    let%bind send_finished =
      Tb.spawn (fun _ ->
        let%bind _ = Tb.cycle ~num_cycles:1 Tb.input_hold in
        return `Finished)
    in
    let%bind result = Tb.wait_for_with_timeout ~timeout_in_cycles send_finished in
    print_s ([%sexp_of: [ `Finished ] option] result);
    return ()
  in
  let test ~timeout_in_cycles =
    let testbench = testbench ~timeout_in_cycles in
    Tb.run_until_finished () ~simulator ~testbench
  in
  test ~timeout_in_cycles:1;
  [%expect {| () |}];
  test ~timeout_in_cycles:2;
  [%expect {| (Finished) |}];
  test ~timeout_in_cycles:3;
  [%expect {| (Finished) |}];
  test ~timeout_in_cycles:4;
  [%expect {| (Finished) |}]
;;
