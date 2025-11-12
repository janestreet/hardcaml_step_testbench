open! Core
open Hardcaml
open Hardcaml_waveterm
open Bits

module%test [@tags "runtime5-only"] _ = struct
  module Dut = struct
    open Signal

    module I = struct
      type 'a t =
        { clock : 'a
        ; a : 'a [@bits 8]
        ; b : 'a [@bits 8]
        ; step : 'a [@bits 8]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { c : 'a [@bits 8]
        ; c_reg : 'a [@bits 8]
        ; t : 'a [@bits 8]
        }
      [@@deriving hardcaml]
    end

    let create ({ I.clock; a; b; step } : _ I.t) =
      let spec = Reg_spec.create ~clock () in
      let t = reg_fb ~width:8 ~f:(fun d -> d +: step) spec in
      let c = a +: b in
      let c_reg = reg spec c in
      { O.c; c_reg; t }
    ;;
  end

  module Sim = Cyclesim.With_interface (Dut.I) (Dut.O)
  module Step = Hardcaml_step_testbench_effectful.Imperative.Cyclesim

  let create_sim () : Sim.t = Sim.create Dut.create

  let test when_to_evaluate_testbenches =
    let sim = create_sim () in
    let inputs = Cyclesim.inputs sim in
    let o_before = Cyclesim.outputs ~clock_edge:Before sim in
    let o_after = Cyclesim.outputs ~clock_edge:After sim in
    let testbench handler : never_returns =
      let a = ref 0 in
      let b = ref 0 in
      let cycle = ref 0 in
      Step.forever handler (fun handler () ->
        a := !a + 1;
        b := !b + 2;
        let o_before = Dut.O.map ~f:(fun a -> to_int_trunc !a) o_before in
        let o_after = Dut.O.map ~f:(fun a -> to_int_trunc !a) o_after in
        inputs.a <--. !a;
        inputs.b <--. !b;
        let step = to_int_trunc !(inputs.step) in
        (* Printing the first two cycles is enough to illustrate how this component works. *)
        if !cycle < 2
        then
          print_string
            (Sexp_pretty.sexp_to_string
               [%message (step : int) (o_before : int Dut.O.t) (o_after : int Dut.O.t)]);
        cycle := !cycle + 1;
        Step.cycle handler ~num_cycles:1 ())
    in
    let sim =
      Step.wrap_never_returns ~when_to_evaluate_testbenches ~testbenches:[ testbench ] sim
    in
    let waves, sim = Waveform.create sim in
    printf "On first call to Cyclesim.cycle:\n";
    inputs.step <--. 1;
    Cyclesim.cycle sim;
    print_endline "";
    printf "On second call to Cyclesim.cycle:\n";
    inputs.step <--. 2;
    Cyclesim.cycle sim;
    print_endline "";
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    printf "Waveform:\n";
    Waveform.print waves
  ;;

  let%expect_test "Wrap testbench to run before cycle" =
    test `Before_cycle;
    [%expect
      {|
      On first call to Cyclesim.cycle:
      ((step 1)
       (o_before (
         (c     0)
         (c_reg 0)
         (t     0)))
       (o_after (
         (c     0)
         (c_reg 0)
         (t     0))))

      On second call to Cyclesim.cycle:
      ((step 2)
       (o_before (
         (c     3)
         (c_reg 0)
         (t     0)))
       (o_after (
         (c     3)
         (c_reg 3)
         (t     1))))

      Waveform:
      ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
      │clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
      │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
      │               ││────────┬───────┬───────┬───────┬───────           │
      │a              ││ 01     │02     │03     │04     │05                │
      │               ││────────┴───────┴───────┴───────┴───────           │
      │               ││────────┬───────┬───────┬───────┬───────           │
      │b              ││ 02     │04     │06     │08     │0A                │
      │               ││────────┴───────┴───────┴───────┴───────           │
      │               ││────────┬───────────────────────────────           │
      │step           ││ 01     │02                                        │
      │               ││────────┴───────────────────────────────           │
      │               ││────────┬───────┬───────┬───────┬───────           │
      │c              ││ 03     │06     │09     │0C     │0F                │
      │               ││────────┴───────┴───────┴───────┴───────           │
      │               ││────────┬───────┬───────┬───────┬───────           │
      │c_reg          ││ 00     │03     │06     │09     │0C                │
      │               ││────────┴───────┴───────┴───────┴───────           │
      │               ││────────┬───────┬───────┬───────┬───────           │
      │t              ││ 00     │01     │03     │05     │07                │
      │               ││────────┴───────┴───────┴───────┴───────           │
      └───────────────┘└───────────────────────────────────────────────────┘
      |}]
  ;;

  let%expect_test "Wrap testbench to run after cycle" =
    test `After_cycle;
    [%expect
      {|
      On first call to Cyclesim.cycle:
      ((step 1)
       (o_before (
         (c     0)
         (c_reg 0)
         (t     0)))
       (o_after (
         (c     0)
         (c_reg 0)
         (t     1))))

      On second call to Cyclesim.cycle:
      ((step 2)
       (o_before (
         (c     3)
         (c_reg 0)
         (t     1)))
       (o_after (
         (c     3)
         (c_reg 3)
         (t     3))))

      Waveform:
      ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
      │clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
      │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
      │               ││────────┬───────┬───────┬───────┬───────           │
      │a              ││ 00     │01     │02     │03     │04                │
      │               ││────────┴───────┴───────┴───────┴───────           │
      │               ││────────┬───────┬───────┬───────┬───────           │
      │b              ││ 00     │02     │04     │06     │08                │
      │               ││────────┴───────┴───────┴───────┴───────           │
      │               ││────────┬───────────────────────────────           │
      │step           ││ 01     │02                                        │
      │               ││────────┴───────────────────────────────           │
      │               ││────────┬───────┬───────┬───────┬───────           │
      │c              ││ 00     │03     │06     │09     │0C                │
      │               ││────────┴───────┴───────┴───────┴───────           │
      │               ││────────────────┬───────┬───────┬───────           │
      │c_reg          ││ 00             │03     │06     │09                │
      │               ││────────────────┴───────┴───────┴───────           │
      │               ││────────┬───────┬───────┬───────┬───────           │
      │t              ││ 00     │01     │03     │05     │07                │
      │               ││────────┴───────┴───────┴───────┴───────           │
      └───────────────┘└───────────────────────────────────────────────────┘
      |}]
  ;;
end
