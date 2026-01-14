open! Core
open! Hardcaml
open Digital_components
open Hardcaml_step_testbench_kernel

module type S = sig
  module I_data : Data.S with type t = unit

  module O_data :
    Data.S with type t = unit Hardcaml_step_testbench_kernel.Before_and_after_edge.t

  module Handler : sig
    type t = (O_data.t, I_data.t) Step_effect.Handler.t
  end

  (** [cycle i_data ~num_cycles] waits for [num_cycles] cycles of the simulator to run.
      [cycle] raises if [num_cycles < 1]. *)
  val cycle : Handler.t @ local -> ?num_cycles:int -> unit -> unit

  val start
    :  Handler.t @ local
    -> (Handler.t @ local -> 'a -> 'b)
    -> 'a
    -> ('b, I_data.t) Step_effect.Component_finished.t

  type ('a, 'i) finished_event =
    ('a, 'i) Step_effect.Component_finished.t Step_effect.Event.t

  (** Launch a new task within the current simulation step. *)
  val spawn
    :  ?period:int (** defaults to the period of the parent at run time *)
    -> Handler.t @ local
    -> (Handler.t @ local -> unit -> 'a)
    -> ('a, unit) finished_event

  (** Wait for the given event to occur, and extract its return value. *)
  val wait_for : Handler.t @ local -> ('a, 'i) finished_event -> 'a

  (** Like [wait_for] except it stops waiting after [timeout_in_cycles] and returns
      [None]. Note that the spawned task continues to execute. *)
  val wait_for_with_timeout
    :  Handler.t @ local
    -> ('a, 'i) finished_event
    -> timeout_in_cycles:int
    -> 'a option

  (** Runs the given step function forever. *)
  val forever : Handler.t @ local -> (Handler.t @ local -> unit -> unit) -> never_returns

  val run_monadic_computation
    :  Handler.t @ local
    -> ('a, O_data.t, I_data.t) Step_monad.t
    -> 'a

  module As_monad : sig
    type 'a t = Handler.t @ local -> 'a

    include Monad.S with type 'a t := 'a t

    val cycle : ?num_cycles:int -> unit -> unit t
  end

  module Expert : sig
    val create_component
      :  created_at:[%call_pos]
      -> period:int
      -> update_children_after_finish:bool
      -> (local_ Handler.t -> 'a)
      -> (O_data.t, unit) Component.t
         * ('a, unit) Step_effect.Component_finished.t Step_effect.Event.t

    (** An API to execute a step testbench step-by-step. Every call to [Evaluator.step]
        will execute the step testbench, and the children it spawns, up to the
        synchronization point where they call [Step.cycle h]. *)
    module Evaluator : sig
      module Result : sig
        type 'a t =
          | Finished of 'a
          | Running
      end

      type 'a t

      val create
        :  period:int
        -> update_children_after_finish:bool
        -> (Handler.t @ local -> 'a)
        -> 'a t

      (** Executes the simulation up to synchronization point.

          Note that calling [step] on a finished testbench is NOT a no-op when
          [update_children_after_finished] is set to true. Doing this will result in the
          spawned children tasks being run even when the testbench finished. *)
      val step : ?show_steps:bool -> 'a t -> 'a Result.t

      (** Returns true if the testbench has already completed. When this returns true,
          [step] is guranteed to return [Finished _]. Note that the converse is not true
          -- even if the evaluator hasn't finished, the next call to [step] could cause is
          to transition to a Finished state. *)
      val is_finished : _ t -> bool
    end
  end
end

module type Imperative = sig
  module type S = S

  include S
end
