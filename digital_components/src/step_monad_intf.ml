(** An [('a, 'i, 'o) Step_monad.t] is a description of computation that produces a value
    of type ['a] via a sequential [Component.t] that on each step takes an ['i] and
    produces a ['o].

    The [next_step] operation finishes the current step and pauses until the next step.
    The [spawn] function allows a computation to start other sequential components -- all
    spawned components run in parallel. *)

open! Base

module type S = sig
  module Input_monad : Monad.S
  module Component : Component.M(Input_monad).S

  type ('a, 'i, 'o) t [@@deriving sexp_of]

  include Monad.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t

  val next_step : Source_code_position.t -> 'o -> ('i, 'i, 'o) t
  val thunk : (unit -> ('a, 'i, 'o) t) -> ('a, 'i, 'o) t
  val output_forever : 'o -> (_, _, 'o) t

  (** [for_ i j f] does [f i], [f (i+1)], ... [f j] in sequence.  If [j < i], then
      [for_ i j] immediately returns unit. *)
  val for_ : int -> int -> (int -> (unit, 'i, 'o) t) -> (unit, 'i, 'o) t

  (** [delay o ~num_steps] outputs [o] for [num_steps] and then returns unit.  [delay]
      raises if [num_steps < 0]. *)
  val delay : 'o -> num_steps:int -> (unit, _, 'o) t

  (** [repeat ~count f] does [f ()] [count] times.  [repeat] raises if [count < 0]. *)
  val repeat : count:int -> (unit -> (unit, 'i, 'o) t) -> (unit, 'i, 'o) t

  val wait : output:'o -> until:('i -> bool) -> (unit, 'i, 'o) t

  (** An event is a value that will at some point in time (possibly the past, possibly the
      future) transition from "undetermined" to "determined", with some value.  One
      can [wait_for] an event in a computation. *)
  module Event : sig
    type 'a t [@@deriving sexp_of]

    val value : 'a t -> 'a option
  end

  (** [wait_for event ~output] outputs [output] until the step at which [event] becomes
      determined, at which point the [wait_for] proceeds. *)
  val wait_for : 'a Event.t -> output:'o -> ('a, _, 'o) t

  module Component_finished : sig
    type ('a, 'o) t =
      { output : 'o
      ; result : 'a
      }
    [@@deriving sexp_of]
  end

  (** [spawn] creates a child computation that runs [start].  [spawn] returns on the
      current step, and the child starts on the next step.  The parent computation uses
      [child_input] to adjust its input into the form that the child computation sees,
      and [include_child_output] to incorporate the child's output into its output.

      When [update_children_after_finish], unfinished tasks spawned from within [start]
      will be executed even after [start] completes.
  *)
  val spawn
    :  ?update_children_after_finish:bool (** default is [false] *)
    -> Source_code_position.t
    -> start:('i_c -> (('a, 'o_c) Component_finished.t, 'i_c, 'o_c) t)
    -> input:'i_c Data.t
    -> output:'o_c Data.t
    -> child_input:(parent:'i -> 'i_c)
    -> include_child_output:(parent:'o -> child:'o_c -> 'o)
    -> (('a, 'o_c) Component_finished.t Event.t, 'i, 'o) t

  (** [create_component] creates a [Component.t] that runs the computation described
      by [start]. When [update_children_after_finish] is set to true, all component's
      children will be updated even after the child terminates. This will result in
      tasks spawned from within the child task to execute even after the child
      terminates.
  *)
  val create_component
    :  created_at:Source_code_position.t
    -> update_children_after_finish:bool
    -> start:('i -> (('a, 'o) Component_finished.t, 'i, 'o) t)
    -> input:'i Data.t
    -> output:'o Data.t
    -> ('i, 'o) Component.t * ('a, 'o) Component_finished.t Event.t
end

module M (Input_monad : Monad.S) = struct
  module type S = S with module Input_monad = Input_monad
end

module type Step_monad = sig
  module type S = S

  module M = M
  module Make (Input_monad : Monad.S) : M(Input_monad).S
end
