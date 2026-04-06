(** Event_driven_sim based step testbench API. Includes functions to run the resulting
    testbench. *)

open! Core
open Hardcaml
open Hardcaml_event_driven_sim.Two_state_simulator

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
    -> clock:Logic.t Simulator.Signal.t
    -> inputs:Logic.t Simulator.Signal.t I.t
    -> outputs:Logic.t Simulator.Signal.t O.t
    -> testbench:(Handler.t @ local -> O_data.t -> 'a)
    -> 'r

  (** Create an event sim deferred that can be run inside a process. The result of the
      step testbench is returned. None is returned if the simulation times out.

      The clock signal should be driven externally - do NOT set it within the step moand. *)
  val deferred : ('a, unit -> 'a option Simulator.Async.Deferred.t) run

  (** Wrap an event sim in a process and wait forever after it completes. *)
  val process : (unit, Simulator.Process.t) run
end

module M (I : Interface.S) (O : Interface.S) = struct
  module type S = S with module I = I and module O = O
end

module type Functional_event_driven_sim = sig
  module M = M

  include
    Hardcaml_event_driven_sim.S
    with type Logic.t = Hardcaml_event_driven_sim.Two_state_logic.t

  module Make (I : Interface.S) (O : Interface.S) : M(I)(O).S
end
