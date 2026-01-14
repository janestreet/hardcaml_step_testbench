open! Import
open Hardcaml_waveterm

(* Run an example with 2 tasks. One task generates a packet framed by a randomly generated
   [valid] signal. The second task receives the packet. The data is modified by the
   hardware model by inserting a word count within the upper 16 bits. The received packet
   is shown. *)
let%expect_test ("testbench" [@tags "runtime5-only"]) =
  let module Effectful = Send_and_receive_testbench.Effectful in
  let module Monadic = Send_and_receive_testbench.Monadic in
  let module Tb =
    Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make
      (Send_and_receive_testbench.I)
      (Send_and_receive_testbench.O)
  in
  let module Simulator =
    Cyclesim.With_interface (Send_and_receive_testbench.I) (Send_and_receive_testbench.O)
  in
  let testbenches =
    [ (fun o -> Monadic.testbench o)
    ; (fun o -> Tb.run_effectful_computation (fun h -> Effectful.testbench o h))
    ]
  in
  List.iter testbenches ~f:(fun testbench ->
    let simulator = Simulator.create Send_and_receive_testbench.make_circuit in
    let recv_packet = Tb.run_until_finished () ~simulator ~testbench in
    print_s [%sexp (recv_packet : Bits.t list)];
    [%expect
      {|
      (00000000000000010000000000000101
       00000000000000100000000000000100
       00000000000000110000000000000011
       00000000000001000000000000000010
       00000000000001010000000000000001)
      |}])
;;

let%expect_test "input setting hierarchy" =
  let module Data = struct
    type 'a t =
      { a : 'a [@bits 8]
      ; b : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end
  in
  let module Data_o = struct
    module T = struct
      include Data

      let port_names_and_widths = map port_names_and_widths ~f:(fun (n, b) -> n ^ "_o", b)
    end

    include (T : Hardcaml.Interface.S with type 'a t = 'a T.t)
    include Hardcaml.Interface.Make (T)
  end
  in
  let module Tb = Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make (Data) (Data_o)
  in
  let module Simulator = Cyclesim.With_interface (Data) (Data_o) in
  let simulator = Simulator.create Fn.id in
  let open! Tb.Let_syntax in
  let rec get_outputs count data =
    if count = 0
    then return (List.rev data)
    else (
      let%bind o = Tb.cycle Tb.input_hold in
      let o_after_edge = Tb.O_data.after_edge o in
      get_outputs (count - 1) (Data.map o_after_edge ~f:Bits.to_int_trunc :: data))
  in
  (* All combinations of setting ports at 4 levels. Deepest child should always win. The
     [a] and [b] ports are set identically, but treated differently by the [input_default]
     argument to [run]. *)
  let levels = 4 in
  let width = 8 in
  let data =
    List.init levels ~f:(fun level ->
      Array.init (1 lsl levels) ~f:(fun index ->
        let x = Bits.of_int_trunc ~width:levels index in
        if Bits.(x.:(level) |> to_int_trunc) = 1
        then Bits.of_int_trunc ~width (level + 1)
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
          let%bind () = Tb.delay { a = h.(i); b = h.(i) } ~num_cycles:1 in
          return ())
  in
  let data =
    Tb.run_until_finished
      ()
      ~input_default:{ a = Bits.empty; b = Bits.of_int_trunc ~width (-1) }
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
      ((a 4) (b 255))))
    |}]
;;

let%expect_test "[run] - returns result as option, but only if ready" =
  let module Tb =
    Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make
      (Interface.Empty)
      (Interface.Empty)
  in
  let module Simulator = Cyclesim.With_interface (Interface.Empty) (Interface.Empty) in
  let simulator = Simulator.create Fn.id in
  let open! Tb.Let_syntax in
  let testbench _ =
    print_s [%message "testbench started"];
    let%bind () = Tb.delay Tb.input_hold ~num_cycles:3 in
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
    (result ())
    |}];
  test 3;
  [%expect
    {|
    (step_number 0)
    "testbench started"
    (step_number 1)
    (step_number 2)
    (result ())
    |}];
  test 4;
  [%expect
    {|
    (step_number 0)
    "testbench started"
    (step_number 1)
    (step_number 2)
    (step_number 3)
    "testbench finished"
    (result (Finished))
    |}];
  test 5;
  [%expect
    {|
    (step_number 0)
    "testbench started"
    (step_number 1)
    (step_number 2)
    (step_number 3)
    "testbench finished"
    (result (Finished))
    |}]
;;

let%expect_test "Spawn tasks sequentially which set the same input." =
  let module I = struct
    type 'a t = { d : 'a [@bits 8] } [@@deriving hardcaml]
  end
  in
  let module O = struct
    type 'a t = { q : 'a [@bits 8] } [@@deriving hardcaml]
  end
  in
  let module Tb = Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make (I) (O) in
  let module Simulator = Cyclesim.With_interface (I) (O) in
  let test normal_spawn =
    let simulator = Simulator.create (fun (x : _ I.t) -> { O.q = x.d }) in
    let waves, simulator = Waveform.create simulator in
    let open! Tb.Let_syntax in
    let set_d _ =
      let%bind () = Tb.delay { d = Bits.ones 8 } ~num_cycles:1 in
      Tb.cycle { d = Bits.zero 8 }
    in
    let setters _ =
      let spawn task =
        if normal_spawn
        then Tb.spawn task
        else Tb.spawn_io task ~inputs:Tb.merge_inputs ~outputs:Fn.id
      in
      let%bind h = spawn set_d in
      let%bind _ = Tb.wait_for h in
      let%bind () = Tb.delay { d = Bits.one 8 } ~num_cycles:1 in
      let%bind h = spawn set_d in
      let%bind _ = Tb.wait_for h in
      return ()
    in
    let testbench _ =
      let%bind _ = Tb.spawn setters in
      let%bind () = Tb.delay { d = Bits.empty } ~num_cycles:10 in
      return ()
    in
    Tb.run_until_finished () ~simulator ~testbench;
    Waveform.expect waves
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
    └───────────────┘└───────────────────────────────────────────────────┘
    a8fdb200e2f8f0bcfadf037cdab1139d
    |}];
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
    └───────────────┘└───────────────────────────────────────────────────┘
    a8fdb200e2f8f0bcfadf037cdab1139d
    |}]
;;

let%expect_test "Timeout works as expected" =
  let module Tb =
    Hardcaml_step_testbench.Monadic.Functional.Cyclesim.Make
      (Send_and_receive_testbench.I)
      (Send_and_receive_testbench.O)
  in
  let module Simulator =
    Cyclesim.With_interface (Send_and_receive_testbench.I) (Send_and_receive_testbench.O)
  in
  let simulator = Simulator.create Send_and_receive_testbench.make_circuit in
  let open! Tb.Let_syntax in
  let testbench ~timeout_in_cycles _ =
    let%bind send_finished =
      Tb.spawn (fun _ ->
        let%bind () = Tb.delay Tb.input_hold ~num_cycles:1 in
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
