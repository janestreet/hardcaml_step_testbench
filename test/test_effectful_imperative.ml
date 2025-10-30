open! Core
open Hardcaml
open Hardcaml_waveterm

module%test [@tags "runtime5-only"] _ = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; step : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { q : 'a [@bits 8] } [@@deriving hardcaml]
  end

  let create ({ I.clock; enable; step } : _ I.t) =
    { O.q =
        Signal.(
          reg_fb (Reg_spec.create ~clock ()) ~enable ~width:8 ~f:(fun d -> d +: step))
    }
  ;;

  module Sim = Cyclesim.With_interface (I) (O)
  module Step = Hardcaml_step_testbench_effectful.Imperative.Cyclesim

  (* A thread which controls the [step]

   returns a string to show we can.
  *)
  let rec incr_step i step (inputs : _ I.t) handler () =
    inputs.step := Bits.of_int_trunc ~width:8 step;
    if step = 4
    then (
      inputs.step := Bits.of_int_trunc ~width:8 0;
      "hello!!!")
    else if i = step
    then incr_step 0 (step + 1) inputs handler ()
    else (
      Step.cycle handler ();
      incr_step (i + 1) step inputs handler ())
  ;;

  (* master thread which controls [enable] *)
  let testbench (inputs : _ I.t) (outputs : _ O.t) handler =
    let hello = Step.spawn handler (fun handler () -> incr_step 0 0 inputs handler ()) in
    Step.cycle handler ();
    inputs.enable := Bits.vdd;
    Step.cycle handler ~num_cycles:3 ();
    inputs.enable := Bits.gnd;
    Step.cycle handler ~num_cycles:1 ();
    inputs.enable := Bits.vdd;
    let hello = Step.wait_for handler hello in
    Bits.to_int_trunc !(outputs.q), hello
  ;;

  let%expect_test "" =
    let simulator = Sim.create create in
    let waves, simulator = Waveform.create simulator in
    let result =
      Step.run_until_finished () ~simulator ~testbench:(fun handler ->
        testbench (Cyclesim.inputs simulator) (Cyclesim.outputs simulator) handler)
    in
    print_s [%message (result : int * string)];
    Waveform.print ~wave_width:2 waves;
    [%expect
      {|
      (result (10 hello!!!))
      ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
      │clock          ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
      │               ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
      │enable         ││      ┌─────────────────┐     ┌─────────────────   │
      │               ││──────┘                 └─────┘                    │
      │               ││──────┬───────────┬─────────────────┬───────────   │
      │step           ││ 01   │02         │03               │00            │
      │               ││──────┴───────────┴─────────────────┴───────────   │
      │               ││────────────┬─────┬─────┬───────────┬───────────   │
      │q              ││ 00         │02   │04   │07         │0A            │
      │               ││────────────┴─────┴─────┴───────────┴───────────   │
      └───────────────┘└───────────────────────────────────────────────────┘
      |}]
  ;;

  module%test Spawn_functional_step = struct
    module Functional_step =
      Hardcaml_step_testbench_effectful.Functional.Cyclesim.Make (I) (O)

    let testbench (sim : Sim.t) (h : Step.Handler.t) =
      let io_ports =
        Functional_step.create_io_ports_for_imperative sim ~inputs:Fn.id ~outputs:Fn.id
      in
      let task =
        Functional_step.spawn_from_imperative
          io_ports
          h
          (fun (h : Functional_step.Handler.t) (_ : Functional_step.O_data.t) ->
             List.map (List.range 0 5) ~f:(fun i ->
               let o =
                 Functional_step.cycle
                   h
                   { Functional_step.input_hold with
                     enable = Bits.vdd
                   ; step = Bits.of_int_trunc ~width:8 i
                   }
               in
               Bits.to_int_trunc o.after_edge.q)
             [@nontail])
      in
      let results = Step.wait_for h task in
      [%test_result: int list] ~expect:[ 0; 1; 3; 6; 10 ] results
    ;;

    let%expect_test "Spans a task from imperative and run it to completion" =
      let simulator = Sim.create create in
      let waves, simulator = Waveform.create simulator in
      Step.run_until_finished () ~simulator ~testbench:(fun h -> testbench simulator h);
      Waveform.print ~wave_width:2 waves;
      [%expect
        {|
        ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
        │clock          ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
        │               ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
        │enable         ││──────────────────────────────────────────         │
        │               ││                                                   │
        │               ││──────┬─────┬─────┬─────┬─────────────────         │
        │step           ││ 00   │01   │02   │03   │04                        │
        │               ││──────┴─────┴─────┴─────┴─────────────────         │
        │               ││────────────┬─────┬─────┬─────┬─────┬─────         │
        │q              ││ 00         │01   │03   │06   │0A   │0E            │
        │               ││────────────┴─────┴─────┴─────┴─────┴─────         │
        └───────────────┘└───────────────────────────────────────────────────┘
        |}]
    ;;

    let mk_testbench (sim : Sim.t) h =
      let io_ports =
        Functional_step.create_io_ports_for_imperative sim ~inputs:Fn.id ~outputs:Fn.id
      in
      Functional_step.exec_never_returns_from_imperative io_ports h (fun h _ ->
        let ctr = ref 1 in
        Functional_step.forever h (fun h () ->
          Functional_step.delay
            h
            ~num_cycles:1
            { Functional_step.input_hold with
              enable = Bits.vdd
            ; step = Bits.of_int_trunc ~width:8 !ctr
            };
          ctr := !ctr + 1;
          if !ctr > 100
          then failwith "BUG: infinite loop in testbench when expecting task to timeout"))
    ;;

    let%expect_test "Executes a forever task from imperative" =
      let simulator = Sim.create create in
      let waves, simulator = Waveform.create simulator in
      (match
         Step.run_with_timeout ~timeout:5 () ~simulator ~testbench:(fun h ->
           mk_testbench simulator h)
       with
       | None -> ()
       | Some _ -> failwith "expected timeout from a forever task!");
      Waveform.print ~wave_width:2 waves;
      [%expect
        {|
        ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
        │clock          ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
        │               ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
        │enable         ││──────────────────────────────                     │
        │               ││                                                   │
        │               ││──────┬─────┬─────┬─────┬─────                     │
        │step           ││ 01   │02   │03   │04   │05                        │
        │               ││──────┴─────┴─────┴─────┴─────                     │
        │               ││──────┬─────┬─────┬─────┬─────                     │
        │q              ││ 00   │01   │03   │06   │0A                        │
        │               ││──────┴─────┴─────┴─────┴─────                     │
        └───────────────┘└───────────────────────────────────────────────────┘
        |}]
    ;;
  end

  (* Event driven sim *)

  module%test Basic_test = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; enable : 'a
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t = { q : 'a [@bits 8] } [@@deriving hardcaml]
    end

    let create ({ clock; enable } : _ I.t) =
      let spec = Signal.Reg_spec.create ~clock () in
      { O.q = Signal.reg_fb spec ~enable ~width:8 ~f:(fun d -> Signal.( +:. ) d 1) }
    ;;

    module Event_simulator =
      Hardcaml_step_testbench_effectful.Imperative.Event_driven_sim.Simulator

    module Step = Hardcaml_step_testbench_effectful.Imperative.Event_driven_sim

    module Evsim =
      Hardcaml_event_driven_sim.With_interface
        (Hardcaml_event_driven_sim.Two_state_logic)
        (I)
        (O)

    let testbench (input : _ Hardcaml_event_driven_sim.Port.t I.t) h =
      let time () = Event_simulator.Async.current_time () in
      let ( <-- ) = Event_simulator.( <-- ) in
      input.enable.signal <-- Bits.vdd;
      Step.cycle h ();
      print_s [%message "Stepping 1" (time () : int)];
      input.enable.signal <-- Bits.gnd;
      Step.cycle h ();
      print_s [%message "Stepping 2" (time () : int)];
      Step.cycle h ();
      print_s [%message "Stepping 3" (time () : int)];
      input.enable.signal <-- Bits.vdd;
      Step.cycle h ();
      print_s [%message "Stepping 4" (time () : int)];
      Step.cycle h ();
      print_s [%message "Stepping 5" (time () : int)];
      input.enable.signal <-- Bits.gnd;
      Step.cycle h ()
    ;;

    let run () =
      let { Evsim.processes; input; output; internal = _; memories = _ } =
        Evsim.create create
      in
      let step_process =
        Step.process () ~clock:input.clock.signal ~testbench:(fun h -> testbench input h)
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
        ("Stepping 1" ("time ()" 5))
        (q ("Event_simulator.(!!) (output.q).signal" 00000001))
        ("Stepping 2" ("time ()" 15))
        ("Stepping 3" ("time ()" 25))
        ("Stepping 4" ("time ()" 35))
        (q ("Event_simulator.(!!) (output.q).signal" 00000010))
        ("Stepping 5" ("time ()" 45))
        (q ("Event_simulator.(!!) (output.q).signal" 00000011))
        |}]
    ;;
  end
end
