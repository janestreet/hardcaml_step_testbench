open! Import

module Event_simulator =
  Hardcaml_step_testbench.Event_driven_sim.Simulator.Event_simulator

module _ (* Basic test *) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { q : 'a [@bits 8] } [@@deriving sexp_of, hardcaml]
  end

  let create ({ clock; enable } : _ I.t) =
    let spec = Reg_spec.create ~clock () in
    { O.q = Signal.reg_fb spec ~enable ~width:8 ~f:(fun d -> Signal.( +:. ) d 1) }
  ;;

  module Step = Hardcaml_step_testbench.Event_driven_sim.Make (I) (O)

  module Evsim =
    Hardcaml_event_driven_sim.With_interface
      (Hardcaml_event_driven_sim.Two_state_logic)
      (I)
      (O)

  open Step.Let_syntax
  open Hardcaml.Bits

  let testbench _ =
    let time () =
      Hardcaml_step_testbench.Event_driven_sim.Simulator.Event_simulator.Async
      .current_time
        ()
    in
    let%bind _ = Step.cycle { Step.input_hold with enable = vdd } in
    print_s [%message "Stepping 1" (time () : int)];
    let%bind _ = Step.cycle { Step.input_hold with enable = gnd } in
    print_s [%message "Stepping 2" (time () : int)];
    let%bind _ = Step.cycle Step.input_hold in
    print_s [%message "Stepping 3" (time () : int)];
    let%bind _ = Step.cycle { Step.input_hold with enable = vdd } in
    print_s [%message "Stepping 4" (time () : int)];
    let%bind _ = Step.cycle Step.input_hold in
    print_s [%message "Stepping 5" (time () : int)];
    let%bind _ = Step.cycle { Step.input_hold with enable = gnd } in
    return ()
  ;;

  let run () =
    let { Evsim.processes; input; output; internal = _ } = Evsim.create create in
    let step_process =
      Step.process
        ()
        ~clock:input.clock.signal
        ~inputs:(I.map input ~f:(fun i -> i.signal))
        ~outputs:(O.map output ~f:(fun o -> o.signal))
        ~testbench
    in
    let clock = Evsim.create_clock input.clock.signal ~time:5 in
    let trace =
      Event_simulator.Process.create
        [ Event_simulator.Signal.id output.q.signal ]
        (fun () ->
          print_s [%message "q" (Event_simulator.( !! ) output.q.signal : Bits.t)])
    in
    let sim = Event_simulator.create (clock :: step_process :: trace :: processes) in
    Event_simulator.run ~time_limit:100 sim
  ;;

  let%expect_test "" =
    run ();
    [%expect
      {|
    (q ("Event_simulator.(!!) (output.q).signal" 00000000))
    (q ("Event_simulator.(!!) (output.q).signal" 00000001))
    ("Stepping 1" ("time ()" 10))
    ("Stepping 2" ("time ()" 20))
    ("Stepping 3" ("time ()" 30))
    (q ("Event_simulator.(!!) (output.q).signal" 00000010))
    ("Stepping 4" ("time ()" 40))
    (q ("Event_simulator.(!!) (output.q).signal" 00000011))
    ("Stepping 5" ("time ()" 50)) |}]
  ;;
end

module _ (* Multiple spawned things *) = struct
  let test_multi_spawns () =
    let module Test = Send_and_receive_testbench in
    let module Send_and_receive_testbench =
      Test.Make (Hardcaml_step_testbench.Event_driven_sim)
    in
    let module Evstep = Hardcaml_step_testbench.Event_driven_sim.Make (Test.I) (Test.O) in
    let module Evsim =
      Hardcaml_event_driven_sim.With_interface
        (Hardcaml_event_driven_sim.Two_state_logic)
        (Test.I)
        (Test.O)
    in
    let { Evsim.processes; input; output; internal = _ } =
      Evsim.create Test.make_circuit
    in
    let testbench =
      Evstep.deferred
        ()
        ~clock:input.clk.signal
        ~inputs:(Test.I.map input ~f:(fun i -> i.signal))
        ~outputs:(Test.O.map output ~f:(fun o -> o.signal))
        ~testbench:Send_and_receive_testbench.testbench
    in
    let testbench_process =
      Event_simulator.Async.create_process (fun () ->
        let open Event_simulator.Async.Deferred.Let_syntax in
        let%bind result = testbench () in
        print_s [%message (result : Bits.t list option)];
        Event_simulator.Async.wait_forever ())
    in
    let clock = Evsim.create_clock input.clk.signal ~time:5 in
    let sim = Event_simulator.create (clock :: testbench_process :: processes) in
    Event_simulator.run ~time_limit:1000 sim
  ;;

  let%expect_test "multiple spawns" =
    test_multi_spawns ();
    [%expect
      {|
      (result ((
        00000000000000010000000000000101
        00000000000000100000000000000100
        00000000000000110000000000000011
        00000000000001000000000000000010
        00000000000001010000000000000001))) |}]
  ;;
end

module _ (* Test different ways outputs are affected *) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; d : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { input_passed_thru : 'a
      ; comb_logic_of_input_only : 'a
      ; registered : 'a
      ; comb_logic_after_register : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  (* This tries to model the various ways inputs, registers, comb logic etc affect outputs
     in the simulation step. *)
  let create_fn { I.clock; clear; d } =
    let open Signal in
    let spec = Reg_spec.create ~clock ~clear () in
    let q = reg spec d in
    { O.input_passed_thru = d
    ; comb_logic_of_input_only = ~:d
    ; registered = q
    ; comb_logic_after_register = ~:q
    }
  ;;

  module Testbench (Monads : Hardcaml_step_testbench.Api.Monads) = struct
    module Step = Hardcaml_step_testbench.Api.Make (Monads) (I) (O)

    let testbench (o : Step.O_data.t) =
      let open Step.Let_syntax in
      print_s [%message (o : Step.O_data.t)];
      let%bind o = Step.cycle { Step.input_hold with d = Bits.vdd } in
      print_s [%message (o : Step.O_data.t)];
      let%bind o = Step.cycle { Step.input_hold with d = Bits.gnd } in
      print_s [%message (o : Step.O_data.t)];
      let%bind o = Step.cycle { Step.input_hold with d = Bits.vdd } in
      print_s [%message (o : Step.O_data.t)];
      let%bind o = Step.cycle { Step.input_hold with d = Bits.gnd } in
      print_s [%message (o : Step.O_data.t)];
      return ()
    ;;
  end

  let test_evsim () =
    let module Testbench = Testbench (Hardcaml_step_testbench.Event_driven_sim) in
    let module Evstep = Hardcaml_step_testbench.Event_driven_sim.Make (I) (O) in
    let module Evsim =
      Hardcaml_event_driven_sim.With_interface
        (Hardcaml_event_driven_sim.Two_state_logic)
        (I)
        (O)
    in
    let { Evsim.processes; input; output; internal = _ } = Evsim.create create_fn in
    let testbench =
      Evstep.deferred
        ()
        ~clock:input.clock.signal
        ~inputs:(I.map input ~f:(fun i -> i.signal))
        ~outputs:(O.map output ~f:(fun o -> o.signal))
        ~testbench:Testbench.testbench
    in
    let testbench_process =
      Event_simulator.Async.create_process (fun () ->
        let open Event_simulator.Async.Deferred.Let_syntax in
        let%bind _result = testbench () in
        Event_simulator.Async.wait_forever ())
    in
    let clock = Evsim.create_clock input.clock.signal ~time:5 in
    let sim = Event_simulator.create (clock :: testbench_process :: processes) in
    Event_simulator.run ~time_limit:1000 sim
  ;;

  let test_cyclesim () =
    let module Testbench = Testbench (Hardcaml_step_testbench.Cyclesim) in
    let module Step = Hardcaml_step_testbench.Cyclesim.Make (I) (O) in
    let module Sim = Cyclesim.With_interface (I) (O) in
    let simulator = Sim.create create_fn in
    Step.run_until_finished () ~simulator ~testbench:Testbench.testbench
  ;;

  module Compare (Test : sig
    val test : unit -> unit
  end) =
  struct
    let%expect_test "basic tests " =
      Test.test ();
      [%expect
        {|
      (o (
        (before_edge (
          (input_passed_thru         0)
          (comb_logic_of_input_only  0)
          (registered                0)
          (comb_logic_after_register 0)))
        (after_edge (
          (input_passed_thru         0)
          (comb_logic_of_input_only  0)
          (registered                0)
          (comb_logic_after_register 0)))))
      (o (
        (before_edge (
          (input_passed_thru         1)
          (comb_logic_of_input_only  0)
          (registered                0)
          (comb_logic_after_register 1)))
        (after_edge (
          (input_passed_thru         1)
          (comb_logic_of_input_only  0)
          (registered                1)
          (comb_logic_after_register 0)))))
      (o (
        (before_edge (
          (input_passed_thru         0)
          (comb_logic_of_input_only  1)
          (registered                1)
          (comb_logic_after_register 0)))
        (after_edge (
          (input_passed_thru         0)
          (comb_logic_of_input_only  1)
          (registered                0)
          (comb_logic_after_register 1)))))
      (o (
        (before_edge (
          (input_passed_thru         1)
          (comb_logic_of_input_only  0)
          (registered                0)
          (comb_logic_after_register 1)))
        (after_edge (
          (input_passed_thru         1)
          (comb_logic_of_input_only  0)
          (registered                1)
          (comb_logic_after_register 0)))))
      (o (
        (before_edge (
          (input_passed_thru         0)
          (comb_logic_of_input_only  1)
          (registered                1)
          (comb_logic_after_register 0)))
        (after_edge (
          (input_passed_thru         0)
          (comb_logic_of_input_only  1)
          (registered                0)
          (comb_logic_after_register 1))))) |}]
    ;;
  end

  (* Run both backends.  This will fail if they dont generate the same output values. *)
  module _ = Compare (struct
    let test = test_cyclesim
  end)

  module _ = Compare (struct
    let test = test_evsim
  end)
end
