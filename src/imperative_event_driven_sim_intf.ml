open! Core
open Hardcaml_event_driven_sim.Two_state_simulator

module Monads = struct
  module Input_monad = Simulator.Async.Deferred
  module Step_monad = Digital_components.Step_monad.Make (Input_monad)
end

module type S = sig
  include Imperative.S

  module Simulation_step : sig
    type t = Hardcaml.Bits.t Simulator.Signal.t -> unit Simulator.Async.Deferred.t

    val cyclesim_compatible
      :  ?before_edge:(unit -> unit)
      -> ?after_edge:(unit -> unit)
      -> t

    (** Set inputs, waiting for rising edge and read outputs. *)
    val rising_edge : t
  end

  type ('a, 'r) run =
    ?timeout:int
    -> ?simulation_step:Simulation_step.t (** Default is [cyclesim_compatible] *)
    -> unit
    -> clock:Logic.t Simulator.Signal.t
    -> testbench:(unit -> 'a t)
    -> 'r

  (** Create an event sim deferred that can be run inside a process. The result of the
      step testbench is returned. None is returned if the simulation times out.

      The clock signal should be driven externally - do NOT set it within the step moand. *)
  val deferred : ('a, unit -> 'a option Simulator.Async.Deferred.t) run

  (** Wrap an event sim in a process and wait forever after it completes. *)
  val process : (unit, Simulator.Process.t) run
end

module type Imperative_event_driven_sim = sig
  module Monads = Monads

  include
    Hardcaml_event_driven_sim.S
    with type Logic.t = Hardcaml_event_driven_sim.Two_state_logic.t

  module type S = S with module Step_monad = Monads.Step_monad

  include S
end
