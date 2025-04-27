open! Core
open Hardcaml
open Hardcaml_waveterm

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
      Signal.(reg_fb (Reg_spec.create ~clock ()) ~enable ~width:8 ~f:(fun d -> d +: step))
  }
;;

module Sim = Cyclesim.With_interface (I) (O)
module Step = Hardcaml_step_testbench.Imperative.Cyclesim
open Step.Let_syntax

(* A thread which controls the [step]

   returns a string to show we can.
*)
let rec incr_step i step (inputs : _ I.t) () =
  inputs.step := Bits.of_int_trunc ~width:8 step;
  if step = 4
  then (
    inputs.step := Bits.of_int_trunc ~width:8 0;
    return "hello!!!")
  else if i = step
  then incr_step 0 (step + 1) inputs ()
  else (
    let%bind () = Step.cycle () in
    incr_step (i + 1) step inputs ())
;;

(* master thread which controls [enable] *)
let testbench (inputs : _ I.t) (outputs : _ O.t) () =
  let%bind hello = Step.spawn (incr_step 0 0 inputs) in
  let%bind () = Step.cycle () in
  inputs.enable := Bits.vdd;
  let%bind () = Step.cycle ~num_cycles:3 () in
  inputs.enable := Bits.gnd;
  let%bind () = Step.cycle ~num_cycles:1 () in
  inputs.enable := Bits.vdd;
  let%bind hello = Step.wait_for hello in
  return (Bits.to_int_trunc !(outputs.q), hello)
;;

let%expect_test "" =
  let simulator = Sim.create create in
  let waves, simulator = Waveform.create simulator in
  let result =
    Step.run_until_finished
      ()
      ~simulator
      ~testbench:(testbench (Cyclesim.inputs simulator) (Cyclesim.outputs simulator))
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

(* Event driven sim *)

module _ (* Basic test *) = struct
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
    Hardcaml_step_testbench.Imperative.Event_driven_sim.Simulator.Event_simulator

  module Step = Hardcaml_step_testbench.Imperative.Event_driven_sim

  module Evsim =
    Hardcaml_event_driven_sim.With_interface
      (Hardcaml_event_driven_sim.Two_state_logic)
      (I)
      (O)

  open Step.Let_syntax

  let testbench (input : _ Hardcaml_event_driven_sim.Port.t I.t) () =
    let time () = Event_simulator.Async.current_time () in
    let ( <-- ) = Event_simulator.( <-- ) in
    input.enable.signal <-- Bits.vdd;
    let%bind _ = Step.cycle () in
    print_s [%message "Stepping 1" (time () : int)];
    input.enable.signal <-- Bits.gnd;
    let%bind _ = Step.cycle () in
    print_s [%message "Stepping 2" (time () : int)];
    let%bind _ = Step.cycle () in
    print_s [%message "Stepping 3" (time () : int)];
    input.enable.signal <-- Bits.vdd;
    let%bind _ = Step.cycle () in
    print_s [%message "Stepping 4" (time () : int)];
    let%bind _ = Step.cycle () in
    print_s [%message "Stepping 5" (time () : int)];
    input.enable.signal <-- Bits.gnd;
    let%bind _ = Step.cycle () in
    return ()
  ;;

  let run () =
    let { Evsim.processes; input; output; internal = _; memories = _ } =
      Evsim.create create
    in
    let step_process =
      Step.process () ~clock:input.clock.signal ~testbench:(testbench input)
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
