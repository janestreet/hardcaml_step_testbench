(** Event_driven_sim based step testbench API. Includes functions to run the resulting
    testbench.
*)

open! Core
open Hardcaml
module Logic = Hardcaml_event_driven_sim.Two_state_logic
module Simulator = Hardcaml_event_driven_sim.Make (Logic)

module Monads = struct
  module Input_monad = Simulator.Event_simulator.Async.Deferred
  module Step_monad = Digital_components.Step_monad.Make (Input_monad)
end

module type S = sig
  include Functional.S

  module Simulation_step : sig
    type t

    (** Set inputs, waiting for falling edge, read outputs (for before edge), wait for
        rising edge, read outputs (for after edge). *)
    val cyclesim_compatible : t

    (** Set inputs, waiting for rising edge and read outputs. Before and after edge
        results are the same. *)
    val rising_edge : t
  end

  type ('a, 'r) run =
    ?input_default:Logic.t I.t
    -> ?show_steps:bool
    -> ?timeout:int
    -> ?simulation_step:Simulation_step.t (** Default is [cyclesim_compatible] *)
    -> unit
    -> clock:Logic.t Simulator.Event_simulator.Signal.t
    -> inputs:Logic.t Simulator.Event_simulator.Signal.t I.t
    -> outputs:Logic.t Simulator.Event_simulator.Signal.t O.t
    -> testbench:(O_data.t -> 'a t)
    -> 'r

  (** Create an event sim deferred that can be run inside a process. The result of the
      step testbench is returned.  None is returned if the simulation times out.

      The clock signal should be driven externally - do NOT set it within the step moand.
  *)
  val deferred : ('a, unit -> 'a option Simulator.Event_simulator.Async.Deferred.t) run

  (** Wrap an event sim in a process and wait forever after it completes. *)
  val process : (unit, Simulator.Event_simulator.Process.t) run
end

module M (I : Interface.S) (O : Interface.S) = struct
  module type S =
    S with module Step_monad = Monads.Step_monad and module I = I and module O = O
end

module type Functional_event_driven_sim = sig
  module Monads = Monads
  module M = M
  module Logic = Logic
  module Simulator = Simulator

  module type S = S with module Step_monad = Monads.Step_monad

  module Make (I : Interface.S) (O : Interface.S) :
    M(I)(O).S with module Step_monad = Monads.Step_monad
end
