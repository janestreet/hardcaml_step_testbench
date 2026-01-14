open! Import
open Hardcaml_waveterm

module%test [@tags "runtime5-only"] _ = struct
  (* Run an example with 2 tasks. One task generates a packet framed by a randomly
     generated [valid] signal. The second task receives the packet. The data is modified
     by the hardware model by inserting a word count within the upper 16 bits. The
     received packet is shown. *)
  let%expect_test "testbench" =
    let module Effectful = Send_and_receive_testbench.Effectful in
    let module Monadic = Send_and_receive_testbench.Monadic in
    let module Tb =
      Hardcaml_step_testbench_effectful.Functional.Cyclesim.Make
        (Send_and_receive_testbench.I)
        (Send_and_receive_testbench.O)
    in
    let module Simulator =
      Cyclesim.With_interface
        (Send_and_receive_testbench.I)
        (Send_and_receive_testbench.O)
    in
    let testbenches =
      [ (fun h o -> Effectful.testbench o h)
      ; (fun h o -> Tb.run_monadic_computation h (Monadic.testbench o))
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

        let port_names_and_widths =
          map port_names_and_widths ~f:(fun (n, b) -> n ^ "_o", b)
        ;;
      end

      include (T : Hardcaml.Interface.S with type 'a t = 'a T.t)
      include Hardcaml.Interface.Make (T)
    end
    in
    let module Tb =
      Hardcaml_step_testbench_effectful.Functional.Cyclesim.Make (Data) (Data_o)
    in
    let module Simulator = Cyclesim.With_interface (Data) (Data_o) in
    let simulator = Simulator.create Fn.id in
    let rec get_outputs h count data =
      if count = 0
      then List.rev data
      else (
        let o = Tb.cycle h Tb.input_hold in
        let o_after_edge = Tb.O_data.after_edge o in
        get_outputs h (count - 1) (Data.map o_after_edge ~f:Bits.to_int_trunc :: data))
    in
    (* All combinations of setting ports at 4 levels. Deepest child should always win. The
       [a] and [b] ports are set identically, but treated differently by the
       [input_default] argument to [run]. *)
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
    let rec set_level data h _output =
      match data with
      | [] -> ()
      | hd :: tl ->
        ignore (Tb.spawn h (fun h output -> set_level tl h output) : _ Tb.finished_event);
        for i = 0 to Array.length hd - 1 do
          Tb.delay h { a = hd.(i); b = hd.(i) } ~num_cycles:1
        done
    in
    let data =
      Tb.run_until_finished
        ()
        ~input_default:{ a = Bits.empty; b = Bits.of_int_trunc ~width (-1) }
        ~simulator
        ~testbench:(fun h _ ->
          let get_outputs_finished =
            Tb.spawn h (fun h _ -> get_outputs h ((1 lsl levels) + 1) [])
          in
          ignore
            (Tb.spawn h (fun h output -> set_level data h output) : _ Tb.finished_event);
          let get_outputs_finished = Tb.wait_for h get_outputs_finished in
          get_outputs_finished)
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
      Hardcaml_step_testbench_effectful.Functional.Cyclesim.Make
        (Interface.Empty)
        (Interface.Empty)
    in
    let module Simulator = Cyclesim.With_interface (Interface.Empty) (Interface.Empty) in
    let simulator = Simulator.create Fn.id in
    let testbench _ h =
      print_s [%message "testbench started"];
      Tb.delay h Tb.input_hold ~num_cycles:3;
      print_s [%message "testbench finished"];
      "Finished"
    in
    let test num_cycles =
      let result =
        Tb.run_with_timeout
          ()
          ~show_steps:true
          ~simulator
          ~timeout:num_cycles
          ~testbench:(fun h o -> testbench o h)
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
    let module Tb = Hardcaml_step_testbench_effectful.Functional.Cyclesim.Make (I) (O) in
    let module Simulator = Cyclesim.With_interface (I) (O) in
    let test normal_spawn =
      let simulator = Simulator.create (fun (x : _ I.t) -> { O.q = x.d }) in
      let waves, simulator = Waveform.create simulator in
      let set_d _ h =
        Tb.delay h { d = Bits.ones 8 } ~num_cycles:1;
        Tb.cycle h { d = Bits.zero 8 }
      in
      let setters _ h =
        let spawn task =
          if normal_spawn
          then Tb.spawn h task
          else Tb.spawn_io h task ~inputs:Tb.merge_inputs ~outputs:Fn.id
        in
        let ev = spawn (fun h o -> set_d o h) in
        ignore (Tb.wait_for h ev : Tb.O_data.t);
        Tb.delay h { d = Bits.one 8 } ~num_cycles:1;
        let ev = spawn (fun h o -> set_d o h) in
        ignore (Tb.wait_for h ev : Tb.O_data.t);
        ()
      in
      let testbench _ h =
        ignore (Tb.spawn h (fun h o -> setters o h) : _ Tb.finished_event);
        Tb.delay h { d = Bits.empty } ~num_cycles:10
      in
      Tb.run_until_finished () ~simulator ~testbench:(fun h o -> testbench o h);
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
      Hardcaml_step_testbench_effectful.Functional.Cyclesim.Make
        (Send_and_receive_testbench.I)
        (Send_and_receive_testbench.O)
    in
    let module Simulator =
      Cyclesim.With_interface
        (Send_and_receive_testbench.I)
        (Send_and_receive_testbench.O)
    in
    let simulator = Simulator.create Send_and_receive_testbench.make_circuit in
    let testbench ~timeout_in_cycles _ h =
      let send_finished =
        Tb.spawn h (fun h _ ->
          Tb.delay h Tb.input_hold ~num_cycles:1;
          `Finished)
      in
      let result = Tb.wait_for_with_timeout h ~timeout_in_cycles send_finished in
      print_s ([%sexp_of: [ `Finished ] option] result);
      ()
    in
    let test ~timeout_in_cycles =
      let testbench h o = testbench ~timeout_in_cycles o h in
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
end
