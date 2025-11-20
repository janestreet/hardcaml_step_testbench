open! Core
open Hardcaml
module M = Functional_event_driven_sim_intf.M
include Hardcaml_event_driven_sim.Two_state_simulator
module Step_modules = Functional_event_driven_sim_intf.Step_modules

module type S =
  Functional_event_driven_sim_intf.S with module Step_modules := Step_modules

module Make (I : Interface.S) (O : Interface.S) = struct
  open Step_modules
  module Deferred = Event_driven_sim.Mini_async.Deferred
  include Functional.Make (Step_modules) (I) (O)
  module Step_modules = Step_modules
  module Step_monad = Step_monad

  let simulator_output outputs = O.map outputs ~f:Simulator.Signal.read

  module Simulation_step = struct
    type t =
      Bits.t Simulator.Signal.t
      -> Bits.t Simulator.Signal.t O.t
      -> Bits.t O.t Before_and_after_edge.t Deferred.t

    let rec wait_for_rising_edge signal =
      let%bind.Deferred () =
        Simulator.Async.wait_for_change (Simulator.Signal.id signal)
      in
      if (not (Logic.to_bool (Simulator.Signal.read_last signal)))
         && Logic.to_bool (Simulator.Signal.read signal)
      then Deferred.return ()
      else wait_for_rising_edge signal
    ;;

    let rec wait_for_falling_edge signal =
      let%bind.Deferred () =
        Simulator.Async.wait_for_change (Simulator.Signal.id signal)
      in
      if Logic.to_bool (Simulator.Signal.read_last signal)
         && not (Logic.to_bool (Simulator.Signal.read signal))
      then Deferred.return ()
      else wait_for_falling_edge signal
    ;;

    let cyclesim_compatible clock outputs =
      let%bind.Deferred () = wait_for_rising_edge clock in
      let before_edge = simulator_output outputs in
      let%bind.Deferred () = wait_for_falling_edge clock in
      let after_edge = simulator_output outputs in
      Deferred.return (Before_and_after_edge.create ~before_edge ~after_edge)
    ;;

    let rising_edge clock outputs =
      let%bind.Deferred () = wait_for_rising_edge clock in
      let at_edge = simulator_output outputs in
      Deferred.return
        (Before_and_after_edge.create ~before_edge:at_edge ~after_edge:at_edge)
    ;;
  end

  let next_input
    ~simulation_step
    ~timeout
    ~clock
    ~inputs
    ~outputs
    ~result_event
    ~input_default
    =
    let timedout =
      let count = ref 0 in
      fun () ->
        Int.incr count;
        match timeout with
        | None -> false
        | Some timeout -> !count >= timeout
    in
    let inport_and_default = I.map2 inputs input_default ~f:(fun i d -> i, d) in
    fun i ->
      I.iter2 inport_and_default i ~f:(fun (i, d) n ->
        if not (Bits.is_empty n)
        then Simulator.( <-- ) i n (* some task has specified a value *)
        else if not (Bits.is_empty d)
        then Simulator.( <-- ) i d (* use default value *)
        else (* hold previous value *)
          ());
      let%bind.Deferred outputs = simulation_step clock outputs in
      match Step_monad.Event.value result_event with
      | None ->
        if timedout ()
        then Deferred.return Step_monad.Component.Next_input.Finished
        else Deferred.return (Step_monad.Component.Next_input.Input outputs)
      | Some _ -> Deferred.return Step_monad.Component.Next_input.Finished
  ;;

  type ('a, 'r) run =
    ?input_default:Logic.t I.t
    -> ?show_steps:bool
    -> ?timeout:int
    -> ?simulation_step:Simulation_step.t
    -> unit
    -> clock:Logic.t Simulator.Signal.t
    -> inputs:Logic.t Simulator.Signal.t I.t
    -> outputs:Logic.t Simulator.Signal.t O.t
    -> testbench:(O_data.t -> 'a t)
    -> 'r

  let deferred
    ?(input_default = input_hold)
    ?show_steps
    ?timeout
    ?(simulation_step = Simulation_step.cyclesim_compatible)
    ()
    ~(clock : Logic.t Simulator.Signal.t)
    ~(inputs : Logic.t Simulator.Signal.t I.t)
    ~(outputs : Logic.t Simulator.Signal.t O.t)
    ~testbench
    =
    let component, result_event =
      Step_monad.create_component
        ~update_children_after_finish:false
        ~created_at:[%here]
        ~start:(start testbench)
        ~input:(module O_data)
        ~output:(module I_data)
        ()
    in
    fun () ->
      let open Simulator.Async.Deferred.Let_syntax in
      let%map () =
        Step_monad.Component.run_until_finished
          component
          ?show_steps
          ~first_input:
            (let outputs = simulator_output outputs in
             Before_and_after_edge.create ~before_edge:outputs ~after_edge:outputs)
          ~next_input:
            (next_input
               ~simulation_step
               ~timeout
               ~clock
               ~inputs
               ~outputs
               ~result_event
               ~input_default)
      in
      match Step_monad.Event.value result_event with
      | None -> None
      | Some x -> Some x.result
  ;;

  let process
    ?input_default
    ?show_steps
    ?timeout
    ?simulation_step
    ()
    ~clock
    ~inputs
    ~outputs
    ~testbench
    =
    let testbench =
      deferred
        ?input_default
        ?show_steps
        ?timeout
        ?simulation_step
        ()
        ~clock
        ~inputs
        ~outputs
        ~testbench
    in
    Simulator.Async.create_process (fun () ->
      let%bind.Deferred _ = testbench () in
      let%bind.Deferred () = Simulator.Async.wait_forever () in
      Deferred.return ())
  ;;
end
