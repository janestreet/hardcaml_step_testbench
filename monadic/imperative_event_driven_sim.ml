open! Core
open Hardcaml
include Hardcaml_event_driven_sim.Two_state_simulator
module Step_modules = Hardcaml_step_testbench_kernel.Step_modules.Event_driven_sim
module Step_monad = Step_modules.Step_monad

module type S =
  Imperative_event_driven_sim_intf.S with module Step_modules := Step_modules

module Deferred = Event_driven_sim.Mini_async.Deferred
include Imperative.Make (Step_modules)
open Step_modules

module Simulation_step = struct
  type t = Bits.t Simulator.Signal.t -> unit Deferred.t

  let rec wait_for_rising_edge signal =
    let%bind.Deferred () = Simulator.Async.wait_for_change (Simulator.Signal.id signal) in
    if (not (Logic.to_bool (Simulator.Signal.read_last signal)))
       && Logic.to_bool (Simulator.Signal.read signal)
    then Deferred.return ()
    else wait_for_rising_edge signal
  ;;

  let rec wait_for_falling_edge signal =
    let%bind.Deferred () = Simulator.Async.wait_for_change (Simulator.Signal.id signal) in
    if Logic.to_bool (Simulator.Signal.read_last signal)
       && not (Logic.to_bool (Simulator.Signal.read signal))
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
      else Deferred.return (Step_monad.Component.Next_input.Input O_data.undefined)
    | Some _ -> Deferred.return Step_monad.Component.Next_input.Finished
;;

type ('a, 'r) run =
  ?timeout:int
  -> ?simulation_step:Simulation_step.t
  -> unit
  -> clock:Logic.t Simulator.Signal.t
  -> testbench:(unit -> 'a t)
  -> 'r

let deferred
  ?timeout
  ?(simulation_step = Simulation_step.rising_edge)
  ()
  ~(clock : Logic.t Simulator.Signal.t)
  ~testbench
  =
  let component, result_event =
    Step_monad.create_component
      ~update_children_after_finish:false
      ~created_at:[%here]
      ~start:(start (fun _ -> testbench ()))
      ~input:(module O_data)
      ~output:(module I_data)
      ()
  in
  fun () ->
    let open Simulator.Async.Deferred.Let_syntax in
    let%map () =
      Step_monad.Component.run_until_finished
        component
        ~first_input:O_data.undefined
        ~next_input:(next_input ~simulation_step ~timeout ~clock ~result_event)
    in
    match Step_monad.Event.value result_event with
    | None -> None
    | Some x -> Some x.result
;;

let process ?timeout ?simulation_step () ~clock ~testbench =
  let testbench = deferred ?timeout ?simulation_step () ~clock ~testbench in
  Simulator.Async.create_process (fun () ->
    let%bind.Deferred _ = testbench () in
    let%bind.Deferred () = Simulator.Async.wait_forever () in
    Deferred.return ())
;;
