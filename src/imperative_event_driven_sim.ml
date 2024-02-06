open! Core
open Hardcaml
module Logic = Imperative_event_driven_sim_intf.Logic
module Simulator = Imperative_event_driven_sim_intf.Simulator
module Monads = Imperative_event_driven_sim_intf.Monads

module type S =
  Imperative_event_driven_sim_intf.S with module Step_monad = Monads.Step_monad

module Deferred = Event_driven_sim.Mini_async.Deferred
include Imperative.Make (Monads)
module Step_monad = Step_monad

module Simulation_step = struct
  type t = Bits.t Simulator.Event_simulator.Signal.t -> unit Deferred.t

  let rec wait_for_rising_edge signal =
    let%bind.Deferred () =
      Simulator.Event_simulator.Async.wait_for_change
        (Simulator.Event_simulator.Signal.id signal)
    in
    if Logic.is_gnd (Simulator.Event_simulator.Signal.read_last signal)
       && Logic.is_vdd (Simulator.Event_simulator.Signal.read signal)
    then Deferred.return ()
    else wait_for_rising_edge signal
  ;;

  let rec wait_for_falling_edge signal =
    let%bind.Deferred () =
      Simulator.Event_simulator.Async.wait_for_change
        (Simulator.Event_simulator.Signal.id signal)
    in
    if Logic.is_vdd (Simulator.Event_simulator.Signal.read_last signal)
       && Logic.is_gnd (Simulator.Event_simulator.Signal.read signal)
    then Deferred.return ()
    else wait_for_falling_edge signal
  ;;

  let cyclesim_compatible ?(before_edge = Fn.id) ?(after_edge = Fn.id) clock =
    let%bind.Deferred () = wait_for_rising_edge clock in
    before_edge ();
    let%bind.Deferred () = wait_for_falling_edge clock in
    after_edge ();
    Deferred.return ()
  ;;

  let rising_edge clock =
    let%bind.Deferred () = wait_for_rising_edge clock in
    Deferred.return ()
  ;;
end

let next_input ~simulation_step ~timeout ~clock ~result_event =
  let timedout =
    let count = ref 0 in
    fun () ->
      Int.incr count;
      match timeout with
      | None -> false
      | Some timeout -> !count >= timeout
  in
  fun () ->
    let%bind.Deferred () = simulation_step clock in
    match Step_monad.Event.value result_event with
    | None ->
      if timedout ()
      then Deferred.return Step_monad.Component.Next_input.Finished
      else Deferred.return (Step_monad.Component.Next_input.Input ())
    | Some _ -> Deferred.return Step_monad.Component.Next_input.Finished
;;

type ('a, 'r) run =
  ?timeout:int
  -> ?simulation_step:Simulation_step.t
  -> unit
  -> clock:Logic.t Simulator.Event_simulator.Signal.t
  -> testbench:(unit -> 'a t)
  -> 'r

let deferred
  ?timeout
  ?(simulation_step = Simulation_step.rising_edge)
  ()
  ~(clock : Logic.t Simulator.Event_simulator.Signal.t)
  ~testbench
  =
  let component, result_event =
    Step_monad.create_component
      ~update_children_after_finish:false
      ~created_at:[%here]
      ~start:(start testbench)
      ~input:(module No_data)
      ~output:(module No_data)
  in
  fun () ->
    let open Simulator.Event_simulator.Async.Deferred.Let_syntax in
    let%map () =
      Step_monad.Component.run_until_finished
        component
        ~first_input:()
        ~next_input:(next_input ~simulation_step ~timeout ~clock ~result_event)
    in
    match Step_monad.Event.value result_event with
    | None -> None
    | Some x -> Some x.result
;;

let process ?timeout ?simulation_step () ~clock ~testbench =
  let testbench = deferred ?timeout ?simulation_step () ~clock ~testbench in
  Simulator.Event_simulator.Async.create_process (fun () ->
    let%bind.Deferred _ = testbench () in
    let%bind.Deferred () = Simulator.Event_simulator.Async.wait_forever () in
    Deferred.return ())
;;
