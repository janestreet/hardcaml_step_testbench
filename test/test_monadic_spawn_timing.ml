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
open Step.Let_syntax

let setx2 _ =
  let%bind () = Step.delay { Step.input_hold with i2 = { x = Bits.vdd } } ~num_cycles:1 in
  return ()
;;

let setx1 _ =
  let%bind _ = Step.spawn setx2 in
  let%bind () = Step.delay { Step.input_hold with i1 = { x = Bits.vdd } } ~num_cycles:1 in
  return ()
;;

let setx0 _ =
  let%bind _ = Step.spawn setx1 >>= Step.wait_for in
  let%bind () = Step.delay { Step.input_hold with i0 = { x = Bits.vdd } } ~num_cycles:1 in
  return ()
;;

let testbench _ =
  let%bind () = Step.delay Step.input_hold ~num_cycles:1 in
  let%bind () = setx0 () in
  let%bind () = Step.delay Step.input_hold ~num_cycles:5 in
  return ()
;;

let%expect_test "" =
  let simulator = Sim.create create in
  let waves, simulator = Waveform.create simulator in
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
