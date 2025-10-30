open! Import

module%test [@tags "runtime5-only"] _ = struct
  module Event_simulator =
    Hardcaml_step_testbench_effectful.Functional.Event_driven_sim.Simulator

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

    module Step =
      Hardcaml_step_testbench_effectful.Functional.Event_driven_sim.Make (I) (O)

    module Evsim =
      Hardcaml_event_driven_sim.With_interface
        (Hardcaml_event_driven_sim.Two_state_logic)
        (I)
        (O)

    open Hardcaml.Bits

    let testbench handler _ =
      let time () = Event_simulator.Async.current_time () in
      Step.delay handler { Step.input_hold with enable = vdd } ~num_cycles:1;
      print_s [%message "Stepping 1" (time () : int)];
      Step.delay handler { Step.input_hold with enable = gnd } ~num_cycles:1;
      print_s [%message "Stepping 2" (time () : int)];
      Step.delay handler Step.input_hold ~num_cycles:1;
      print_s [%message "Stepping 3" (time () : int)];
      Step.delay handler { Step.input_hold with enable = vdd } ~num_cycles:1;
      print_s [%message "Stepping 4" (time () : int)];
      Step.delay handler Step.input_hold ~num_cycles:1;
      print_s [%message "Stepping 5" (time () : int)];
      Step.delay handler { Step.input_hold with enable = gnd } ~num_cycles:1
    ;;

    let run () =
      let { Evsim.processes; input; output; internal = _; memories = _ } =
        Evsim.create create
      in
      let step_process =
        Step.process
          ()
          ~clock:input.clock.signal
          ~inputs:(I.map input ~f:(fun i -> i.signal))
          ~outputs:(O.map output ~f:(fun o -> o.signal))
          ~testbench:(fun h o -> testbench h o)
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
        ("Stepping 5" ("time ()" 50))
        |}]
    ;;
  end

  (* This test uses multiple different clocks to drive a simple hardcaml fifo, and tried to
   show a bug in the event driven simulator. That issue was seemingly something else
   related to the old implementation of async fifos, but is kept here as a general test
   for the simulator. *)
  module Make_fifo_test (X : sig
      val rd_clk : int
      val wr_clk : int
      val tb_clk : int
    end) =
  struct
    open X

    module Fifo = struct
      module Hierarchy_fifo = struct
        module I = struct
          type 'a t =
            { clock : 'a
            ; clear : 'a
            ; wr : 'a
            ; rd : 'a
            ; d : 'a [@bits 8]
            }
          [@@deriving hardcaml]
        end

        module O = struct
          type 'a t =
            { q : 'a [@bits 8]
            ; empty : 'a
            }
          [@@deriving hardcaml]
        end

        let create scope (i : _ I.t) =
          let fifo =
            Hardcaml.Fifo.create
              ~scope
              ~showahead:true
              ~capacity:16
              ~clock:i.clock
              ~clear:i.clear
              ~wr:i.wr
              ~d:i.d
              ~rd:i.rd
              ()
          in
          { O.q = fifo.q; empty = fifo.empty }
        ;;

        let hierarchical ?(instance = "hierarchy_fifo") scope =
          let module H = Hierarchy.In_scope (I) (O) in
          H.hierarchical ~instance ~scope ~name:"hierarchy_fifo" create
        ;;
      end

      module I = struct
        type 'a t =
          { clocks : 'a array [@length 3]
          ; clear : 'a
          ; wr : 'a
          ; rd : 'a
          }
        [@@deriving hardcaml ~rtlmangle:false]
      end

      module O = struct
        type 'a t =
          { q : 'a [@bits 8]
          ; valid : 'a
          }
        [@@deriving hardcaml]
      end

      let create scope ({ clocks; clear; wr = _; rd } : _ I.t) =
        let open Signal in
        let clock_read = clocks.(rd_clk) in
        let clock_write = clocks.(wr_clk) in
        let spec_write = Reg_spec.create ~clock:clock_write ~clear () in
        let read = wire 1 in
        let wr1 = reg_fb spec_write ~width:1 ~f:(fun v -> ~:v) in
        let wr2 = reg_fb spec_write ~width:1 ~f:(fun v -> ~:v) in
        let wr = wr1 |: wr2 in
        let fifo =
          Hierarchy_fifo.hierarchical
            scope
            { clock = clock_read
            ; clear
            ; wr
            ; d =
                reg_fb spec_write ~clear_to:(one 8) ~enable:wr ~width:8 ~f:(fun d ->
                  d +:. 1)
            ; rd = read
            }
        in
        read <-- (rd &: ~:(fifo.empty));
        { O.q = fifo.q; valid = ~:(fifo.empty) }
      ;;

      let hierarchical ?(instance = "fifo") scope =
        let module H = Hierarchy.In_scope (I) (O) in
        H.hierarchical ~instance ~scope ~name:"fifo" create
      ;;
    end

    include Fifo

    module Step =
      Hardcaml_step_testbench_effectful.Functional.Event_driven_sim.Make (I) (O)

    module Logic = Hardcaml_step_testbench_effectful.Functional.Event_driven_sim.Logic
    module Evsim = Hardcaml_event_driven_sim.With_interface (Logic) (I) (O)
    module Vcd = Hardcaml_event_driven_sim.Vcd.Make (Logic)
    open Hardcaml.Bits

    let testbench handler _ =
      let num_writes = 2 in
      let reads = ref [] in
      let get_read (o : Step.O_data.t) =
        if to_bool o.before_edge.valid
        then reads := to_int_trunc o.before_edge.q :: !reads
        else ()
      in
      (* clear *)
      let cycle_and_capture ?num_cycles handler i =
        let o = Step.cycle handler ?num_cycles i in
        get_read o;
        o
      in
      Step.delay handler { Step.input_hold with clear = vdd } ~num_cycles:2;
      Step.delay handler { Step.input_hold with clear = gnd } ~num_cycles:1;
      (* write *)
      for _ = 0 to num_writes - 1 do
        ignore
          (cycle_and_capture handler { Step.input_hold with wr = vdd; rd = vdd }
           : Step.O_data.t)
      done;
      (* read *)
      for _ = 0 to num_writes - 1 do
        ignore
          (cycle_and_capture handler { Step.input_hold with wr = gnd; rd = vdd }
           : Step.O_data.t)
      done;
      let (_ : Step.O_data.t) =
        cycle_and_capture handler { Step.input_hold with rd = gnd }
      in
      let () =
        let reads = List.rev !reads in
        print_s [%message (reads : int list)];
        [%test_result: int list] reads ~expect:(List.init num_writes ~f:(fun v -> v + 1))
      in
      ()
    ;;

    let run () =
      let { Evsim.processes; input; output; internal; memories = _ } =
        let scope =
          Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
        in
        Evsim.create
          ~config:Hardcaml_event_driven_sim.Config.trace_all
          (hierarchical scope)
      in
      let step_process =
        Step.process
          ()
          ~clock:input.clocks.(tb_clk).signal
          ~inputs:(I.map input ~f:(fun i -> i.signal))
          ~outputs:(O.map output ~f:(fun o -> o.signal))
          ~testbench:(fun h o -> testbench h o)
      in
      let clocks =
        Array.map input.clocks ~f:(fun clock -> Evsim.create_clock clock.signal ~time:5)
        |> Array.to_list
      in
      let traces =
        [ Event_driven_sim.Simulator.Debug.print_signal "out.q" output.q.signal
        ; Event_driven_sim.Simulator.Debug.print_signal "out.valid" output.valid.signal
          (* ; Event_driven_sim.Simulator.Debug.print_signal "clock" input.clocks.(0).signal *)
        ]
      in
      let vcd =
        Option.map (Sys.getenv "EXPECT_TEST_WAVEFORM") ~f:(fun _ ->
          Vcd.create (Stdio.Out_channel.create "test_showahead_fifo.vcd") internal)
      in
      let simulator =
        Event_simulator.create
          (step_process
           :: (clocks
               @ traces
               @ processes
               @ (Option.map vcd ~f:Vcd.processes |> Option.value ~default:[])))
      in
      Option.iter vcd ~f:(fun vcd -> Vcd.attach_to_simulator vcd simulator);
      Event_simulator.run ~time_limit:100 simulator
    ;;

    let%expect_test "" =
      run ();
      [%expect
        {|
        t=0 out.valid=1
        t=0 out.valid=0
        t=35 out.q=00000001
        t=35 out.valid=1
        t=45 out.q=00000000
        t=45 out.valid=0
        t=55 out.q=00000010
        t=55 out.valid=1
        t=65 out.q=00000000
        t=65 out.valid=0
        t=75 out.q=00000011
        t=75 out.valid=1
        (reads (1 2))
        |}]
    ;;
  end

  module%test Different_clocks = struct
    let () =
      for i = 0 to 2 do
        for j = 0 to 2 do
          for k = 0 to 2 do
            let module _ =
              Make_fifo_test (struct
                let rd_clk = i
                let wr_clk = j
                let tb_clk = k
              end)
            in
            ()
          done
        done
      done
    ;;
  end

  module%test Multiple_spawned_things = struct
    let test_multi_spawns () =
      let module Test = Send_and_receive_testbench in
      let module Send_and_receive_testbench =
        Test.Make_effectful
          (Hardcaml_step_testbench_effectful.Functional.Event_driven_sim.Step_modules)
      in
      let module Evstep =
        Hardcaml_step_testbench_effectful.Functional.Event_driven_sim.Make
          (Test.I)
          (Test.O)
      in
      let module Evsim =
        Hardcaml_event_driven_sim.With_interface
          (Hardcaml_event_driven_sim.Two_state_logic)
          (Test.I)
          (Test.O)
      in
      let { Evsim.processes; input; output; internal = _; memories = _ } =
        Evsim.create Test.make_circuit
      in
      let testbench =
        Evstep.deferred
          ()
          ~clock:input.clk.signal
          ~inputs:(Test.I.map input ~f:(fun i -> i.signal))
          ~outputs:(Test.O.map output ~f:(fun o -> o.signal))
          ~testbench:(fun h o -> Send_and_receive_testbench.testbench o h)
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
          00000000000001010000000000000001)))
        |}]
    ;;
  end

  module%test Test_different_ways_outputs_are_affected = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; d : 'a
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { input_passed_thru : 'a
        ; comb_logic_of_input_only : 'a
        ; registered : 'a
        ; comb_logic_after_register : 'a
        }
      [@@deriving hardcaml]
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

    module Testbench (Step_modules : Hardcaml_step_testbench_effectful.Step_modules.S) =
    struct
      module Step =
        Hardcaml_step_testbench_effectful.Functional.Make (Step_modules) (I) (O)

      let testbench (o : Step.O_data.t) (h : Step.Handler.t) =
        print_s [%message (o : Step.O_data.t)];
        let o = Step.cycle h { Step.input_hold with d = Bits.vdd } in
        print_s [%message (o : Step.O_data.t)];
        let o = Step.cycle h { Step.input_hold with d = Bits.gnd } in
        print_s [%message (o : Step.O_data.t)];
        let o = Step.cycle h { Step.input_hold with d = Bits.vdd } in
        print_s [%message (o : Step.O_data.t)];
        let o = Step.cycle h { Step.input_hold with d = Bits.gnd } in
        print_s [%message (o : Step.O_data.t)];
        ()
      ;;
    end

    let test_evsim () =
      let module Testbench =
        Testbench
          (Hardcaml_step_testbench_effectful.Functional.Event_driven_sim.Step_modules)
      in
      let module Evstep =
        Hardcaml_step_testbench_effectful.Functional.Event_driven_sim.Make (I) (O)
      in
      let module Evsim =
        Hardcaml_event_driven_sim.With_interface
          (Hardcaml_event_driven_sim.Two_state_logic)
          (I)
          (O)
      in
      let { Evsim.processes; input; output; internal = _; memories = _ } =
        Evsim.create create_fn
      in
      let testbench =
        Evstep.deferred
          ()
          ~clock:input.clock.signal
          ~inputs:(I.map input ~f:(fun i -> i.signal))
          ~outputs:(O.map output ~f:(fun o -> o.signal))
          ~testbench:(fun h o -> Testbench.testbench o h)
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
      let module Testbench =
        Testbench (Hardcaml_step_testbench_effectful.Functional.Cyclesim.Step_modules)
      in
      let module Step = Hardcaml_step_testbench_effectful.Functional.Cyclesim.Make (I) (O)
      in
      let module Sim = Cyclesim.With_interface (I) (O) in
      let simulator = Sim.create create_fn in
      Step.run_until_finished () ~simulator ~testbench:(fun h o ->
        Testbench.testbench o h)
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
              (comb_logic_after_register 1)))))
          |}]
      ;;
    end

    (* Run both backends.  This will fail if they dont generate the same output values. *)
    module%test Cyclesim = Compare (struct
        let test = test_cyclesim
      end)

    module%test Evsim = Compare (struct
        let test = test_evsim
      end)
  end
end
