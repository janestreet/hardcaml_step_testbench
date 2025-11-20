open Core
open Hardcaml
open Hardcaml_waveterm

module Bit = struct
  type 'a t = { x : 'a } [@@deriving hardcaml]
end

module I = struct
  type 'a t =
    { clock : 'a
    ; i0 : 'a Bit.t
    }
  [@@deriving hardcaml ~rtlmangle:"_"]
end

module O = struct
  type 'a t = { o : 'a } [@@deriving hardcaml]
end

let create (i : _ I.t) =
  { O.o = Signal.reg (Signal.Reg_spec.create ~clock:i.clock ()) i.i0.x }
;;

module Sim = Cyclesim.With_interface (I) (O)

let sim () = Sim.create create

let run f ~count =
  for _ = 0 to count - 1 do
    let results = f () in
    print_endline [%string {| ---> %{results#Bits}|}];
    print_endline ""
  done
;;

module Functional = struct
  module Step = Hardcaml_step_testbench_effectful.Functional.Cyclesim.Make (I) (O)

  type finished_event = (unit, Step.I_data.t) Step.finished_event

  let run_test testbench ~count ~print_waves =
    let simulator = sim () in
    let waves, simulator = Waveform.create simulator in
    let f () = Step.run_until_finished () ~show_steps:true ~simulator ~testbench in
    run f ~count;
    if print_waves then Waveform.print waves
  ;;

  let frequency_test ~count ~print_waves =
    let tick ~name (local_ handler) _ =
      while true do
        print_endline [%string {|  tick %{name}|}];
        let _ : Step.O_data.t = Step.cycle handler Step.input_hold in
        ()
      done
    in
    run_test ~count ~print_waves (fun handler _ ->
      let _ : finished_event =
        Step.spawn handler ~period:3 (fun handler _ ->
          ignore (Step.spawn handler (tick ~name:"% 3") : finished_event);
          ignore (Step.spawn handler (tick ~name:"% 2") ~period:2 : finished_event);
          ignore (Step.spawn handler (tick ~name:"% 4") ~period:4 : finished_event);
          Step.delay ~num_cycles:1 handler Step.input_hold;
          Step.spawn handler (tick ~name:"% 5") ~period:5 |> Step.wait_for handler)
      in
      Step.delay handler ~num_cycles:21 Step.input_hold;
      Bits.zero 4)
  ;;
end

module Imperative = struct
  module Step = Hardcaml_step_testbench_effectful.Imperative.Cyclesim

  type finished_event = (unit, Step.I_data.t) Step.finished_event

  let run_test testbench ~count ~print_waves =
    let simulator = sim () in
    let waves, simulator = Waveform.create simulator in
    let inputs = Cyclesim.inputs simulator in
    let outputs = Cyclesim.outputs ~clock_edge:Before simulator in
    let f () =
      Step.run_until_finished () ~show_steps:true ~simulator ~testbench:(fun handler ->
        testbench handler ~inputs ~outputs)
    in
    run f ~count;
    if print_waves then Waveform.print waves
  ;;

  let frequency_test ~count ~print_waves =
    let rec tick handler ~name () =
      print_endline [%string {|  tick %{name}|}];
      Step.cycle handler ();
      tick handler ~name ()
    in
    run_test ~count ~print_waves (fun handler ~inputs:_ ~outputs:_ ->
      let _ : finished_event =
        Step.spawn handler ~period:3 (fun handler ->
          ignore (Step.spawn handler (tick ~name:"% 3") : finished_event);
          ignore (Step.spawn handler (tick ~name:"% 2") ~period:2 : finished_event);
          ignore (Step.spawn handler (tick ~name:"% 4") ~period:4 : finished_event);
          Step.cycle handler ();
          Step.spawn handler (tick ~name:"% 5") ~period:5 |> Step.wait_for handler)
      in
      Step.cycle handler ~num_cycles:21 ();
      Bits.zero 4)
  ;;
end

module%test [@tags "runtime5-only"] _ = struct
  let%expect_test "frequency test" =
    List.iter [ Functional.frequency_test; Imperative.frequency_test ] ~f:(fun f ->
      f ~print_waves:true ~count:2;
      [%expect
        {|
        (step_number 0)
          tick % 4
          tick % 2
          tick % 3
        (step_number 1)
        (step_number 2)
          tick % 2
        (step_number 3)
          tick % 3
        (step_number 4)
          tick % 4
          tick % 2
        (step_number 5)
          tick % 5
        (step_number 6)
          tick % 2
          tick % 3
        (step_number 7)
        (step_number 8)
          tick % 4
          tick % 2
        (step_number 9)
          tick % 3
        (step_number 10)
          tick % 5
          tick % 2
        (step_number 11)
        (step_number 12)
          tick % 4
          tick % 2
          tick % 3
        (step_number 13)
        (step_number 14)
          tick % 2
        (step_number 15)
          tick % 5
          tick % 3
        (step_number 16)
          tick % 4
          tick % 2
        (step_number 17)
        (step_number 18)
          tick % 2
          tick % 3
        (step_number 19)
        (step_number 20)
          tick % 5
          tick % 4
          tick % 2
        (step_number 21)
          tick % 3
         ---> 0000

        (step_number 0)
          tick % 4
          tick % 2
          tick % 3
        (step_number 1)
        (step_number 2)
          tick % 2
        (step_number 3)
          tick % 3
        (step_number 4)
          tick % 4
          tick % 2
        (step_number 5)
          tick % 5
        (step_number 6)
          tick % 2
          tick % 3
        (step_number 7)
        (step_number 8)
          tick % 4
          tick % 2
        (step_number 9)
          tick % 3
        (step_number 10)
          tick % 5
          tick % 2
        (step_number 11)
        (step_number 12)
          tick % 4
          tick % 2
          tick % 3
        (step_number 13)
        (step_number 14)
          tick % 2
        (step_number 15)
          tick % 5
          tick % 3
        (step_number 16)
          tick % 4
          tick % 2
        (step_number 17)
        (step_number 18)
          tick % 2
          tick % 3
        (step_number 19)
        (step_number 20)
          tick % 5
          tick % 4
          tick % 2
        (step_number 21)
          tick % 3
         ---> 0000

        ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
        │clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
        │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
        │i0_x           ││                                                   │
        │               ││───────────────────────────────────────────────────│
        │o              ││                                                   │
        │               ││───────────────────────────────────────────────────│
        └───────────────┘└───────────────────────────────────────────────────┘
        |}])
  ;;
end
