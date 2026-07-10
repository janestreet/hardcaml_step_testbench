open Core
open Hardcaml
open Hardcaml_waveterm

module%test _ = struct
  module Bit = struct
    type 'a t = { x : 'a } [@@deriving hardcaml]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; i0 : 'a Bit.t
      ; i1 : 'a Bit.t
      ; i2 : 'a Bit.t
      }
    [@@deriving hardcaml ~rtlmangle:"_"]
  end

  module O = struct
    type 'a t = { o : 'a } [@@deriving hardcaml]
  end

  let create (i : _ I.t) =
    { O.o =
        Signal.reg
          (Signal.Reg_spec.create ~clock:i.clock ())
          Signal.(i.i0.x |: i.i1.x |: i.i2.x)
    }
  ;;

  module Sim = Cyclesim.With_interface (I) (O)
  module Step = Hardcaml_step_testbench.Functional.Cyclesim.Make (I) (O)

  let setx2 handler _ =
    Step.delay handler { Step.input_hold with i2 = { x = Bits.vdd } } ~num_cycles:1
  ;;

  let setx1 handler _ =
    ignore (Step.spawn handler (fun h o -> setx2 h o) : _ Step.finished_event);
    Step.delay handler { Step.input_hold with i1 = { x = Bits.vdd } } ~num_cycles:1
  ;;

  let setx0 handler _ =
    let event = Step.spawn handler (fun h o -> setx1 h o) in
    (Step.wait_for handler event : unit);
    Step.delay handler { Step.input_hold with i0 = { x = Bits.vdd } } ~num_cycles:1
  ;;

  let testbench handler _ =
    Step.delay handler Step.input_hold ~num_cycles:1;
    setx0 handler ();
    Step.delay handler Step.input_hold ~num_cycles:5
  ;;

  let%expect_test "" =
    let simulator = Sim.create create in
    let waves, simulator = Cyclesim.Waveform.create simulator in
    Step.run_until_finished () ~simulator ~testbench;
    Waveform.print waves;
    [%expect
      {|
      ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
      │clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
      │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
      │i0_x           ││                        ┌──────────────────────────│
      │               ││────────────────────────┘                          │
      │i1_x           ││        ┌──────────────────────────────────────────│
      │               ││────────┘                                          │
      │i2_x           ││        ┌──────────────────────────────────────────│
      │               ││────────┘                                          │
      │o              ││                ┌──────────────────────────────────│
      │               ││────────────────┘                                  │
      └───────────────┘└───────────────────────────────────────────────────┘
      |}]
  ;;
end
