open! Core
open! Hardcaml
open Digital_components
open Hardcaml_step_testbench_kernel

module type S = sig
  module I_data : Data.S with type t = unit

  module O_data :
    Data.S with type t = unit Hardcaml_step_testbench_kernel.Before_and_after_edge.t

  module Step_modules : Step_modules.S
  module Step_effect := Step_modules.Step_effect

  module Handler : sig
    type t = (O_data.t, I_data.t) Step_effect.Handler.t
  end

  (** [cycle i_data ~num_cycles] waits for [num_cycles] cycles of the simulator to run.
      [cycle] raises if [num_cycles < 1]. *)
  val cycle : Handler.t -> ?num_cycles:int -> unit -> unit

  val start
    :  Handler.t
    -> (Handler.t -> 'a -> 'b)
    -> 'a
    -> ('b, I_data.t) Step_effect.Component_finished.t

  type ('a, 'i) finished_event =
    ('a, 'i) Step_effect.Component_finished.t Step_effect.Event.t

  (** Launch a new task within the current simulation step. *)
  val spawn : Handler.t -> (Handler.t -> unit -> 'a) -> ('a, unit) finished_event

  (** Wait for the given event to occur, and extract its return value. *)
  val wait_for : Handler.t -> ('a, 'i) finished_event -> 'a

  (** Like [wait_for] except it stops waiting after [timeout_in_cycles] and returns
      [None]. Note that the spawned task continues to execute. *)
  val wait_for_with_timeout
    :  Handler.t
    -> ('a, 'i) finished_event
    -> timeout_in_cycles:int
    -> 'a option

  (** Runs the given step function forever. *)
  val forever : Handler.t -> (Handler.t -> unit -> unit) -> never_returns

  val run_monadic_computation
    :  Handler.t
    -> ('a, O_data.t, I_data.t) Step_modules.Step_monad.t
    -> 'a
end

module M (Step_modules : Step_modules.S) = struct
  module type S = S with module Step_modules := Step_modules
end

module type Imperative = sig
  module type S = S

  module M = M
  module Make (Step_modules : Step_modules.S) : M(Step_modules).S
end
