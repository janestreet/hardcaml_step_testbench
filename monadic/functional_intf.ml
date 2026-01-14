(** Common testbench API between simulators.

    Testbenches written against this API make may be used with either cyclesim or
    event_driven_sim. *)

open Core
open Hardcaml
open Digital_components

include struct
  open Hardcaml_step_testbench_kernel
  module Component = Component
  module Step_monad = Step_monad
end

module type S = sig
  module Io_ports_for_imperative = Hardcaml_step_testbench_kernel.Io_ports_for_imperative
  module Before_and_after_edge = Before_and_after_edge
  module I : Hardcaml.Interface.S
  module O : Hardcaml.Interface.S
  module I_data : Data.S with type t = Bits.t I.t

  module O_data : sig
    type t = Bits.t O.t Before_and_after_edge.t [@@deriving sexp_of]

    val before_edge : t -> Bits.t O.t
    val after_edge : t -> Bits.t O.t

    include Data.S with type t := t
  end

  (** A testbench takes the circuit's output as its input and produces its output as input
      for the circuit. An ['a t] describes a testbench computation that takes zero or more
      steps and produces a value of type ['a]. *)
  type 'a t = ('a, O_data.t, I_data.t) Step_monad.t

  include Monad.S with type 'a t := 'a t

  val start : ('a -> 'b t) -> 'a -> ('b, I_data.t) Step_monad.Component_finished.t t

  (** [cycle i_data ~num_cycles] waits for [num_cycles] cycles of the simulator to run,
      applying [i_data] to the simulator input ports, and returns the output computed in
      the final cycle. [cycle] raises if [num_cycles < 1]. *)
  val cycle : ?num_cycles:int (** default is 1 *) -> I_data.t -> O_data.t t

  (** [for_ i j f] does [f i], [f (i+1)], ... [f j] in sequence. If [j < i], then
      [for_ i j] immediately returns unit. *)
  val for_ : int -> int -> (int -> unit t) -> unit t

  (** [delay inputs ~num_cycles] applies [inputs] for [num_cycles] clock cycles and then
      returns unit. [delay] raises if [num_cycles < 0]. *)
  val delay : ?num_cycles:int -> I_data.t -> unit t

  type ('a, 'b) finished_event =
    ('a, 'b) Step_monad.Component_finished.t Step_monad.Event.t

  (** Launch a new task within the current simulation step. *)
  val spawn
    :  ?update_children_after_finish:bool
         (** When [update_children_after_finish] is set to true. children tasks that have
             finished will still be updated. This will notably trigger an update on nested
             spawns. *)
    -> ?period:int (** defaults to the period of the parent at run time *)
    -> (O_data.t -> 'a t)
    -> ('a, I_data.t) finished_event t

  (** [merge_inputs ~parent ~child] merges the child inputs into the parent. If a child
      input is [empty], the parent's value is used. *)
  val merge_inputs : parent:I_data.t -> child:I_data.t -> I_data.t

  (** Launch a task from a testbench with a [cycle] function taking ['i] to ['o]. The
      [inputs] and [outputs] arguments should construct [I_data.t] and [O_data.t] from the
      types of the child testbench.

      See documentation of [spawn] for an explaination of [update_children_after_finish]. *)
  val spawn_io
    :  ?update_children_after_finish:bool
    -> ?period:int (** defaults to the period of the parent at run time *)
    -> inputs:(parent:'i -> child:I_data.t -> 'i)
    -> outputs:('o -> Bits.t O.t)
    -> (O_data.t -> 'a t)
    -> (('a, I_data.t) finished_event, 'o Before_and_after_edge.t, 'i) Step_monad.t

  (** Create io ports to be used with [spawn_from_imperative] from a simulator. *)
  val create_io_ports_for_imperative
    :  ('i, 'o) Cyclesim.t
    -> inputs:('i -> Bits.t ref I.t)
    -> outputs:('o -> Bits.t ref O.t)
    -> (Bits.t ref I.t, Bits.t ref O.t) Io_ports_for_imperative.t

  (** Similar to [spawn_io], but for a task (which uses the functional step testbench)
      from a task that's using the imperative step testbench. *)
  val spawn_from_imperative
    :  ?update_children_after_finish:bool
    -> ?period:int (** defaults to the period of the parent at run time *)
    -> (Bits.t ref I.t, Bits.t ref O.t) Io_ports_for_imperative.t
    -> (O_data.t -> 'a t)
    -> (('a, I_data.t) finished_event, unit Before_and_after_edge.t, unit) Step_monad.t

  (** Spawns a functional step testbench infinite loop from a imperative testbench, and
      block forever.

      This semantically similar to [spawn_from_imperative >>= wait_for] under the hood but
      with nicer types. *)
  val exec_never_returns_from_imperative
    :  ?update_children_after_finish:bool
    -> ?period:int (** defaults to the period of the parent at run time *)
    -> (Bits.t ref I.t, Bits.t ref O.t) Io_ports_for_imperative.t
    -> (O_data.t -> never_returns t)
    -> (never_returns, unit Before_and_after_edge.t, unit) Step_monad.t

  (** Wait for the given event to occur, and extract its return value. *)
  val wait_for : ('a, 'b) finished_event -> 'a t

  (** Like [wait_for] except it stops waiting after [timeout_in_cycles] and returns
      [None]. Note that the spawned task continues to execute. *)
  val wait_for_with_timeout
    :  ('a, 'b) finished_event
    -> timeout_in_cycles:int
    -> 'a option t

  (** Call [run ~input_default:input_hold] to hold inputs their previous value if they are
      unset by tasks in the testbench. *)
  val input_hold : Bits.t I.t

  (** Call [run ~input_default:input_zero] to set inputs to zero if unset by tasks in the
      testbench. *)
  val input_zero : Bits.t I.t

  (** Call [forever f] to run [f] forever. [forever] never returns.

      To prevent starving other tasks, it's the caller's responsibility to ensure that [f]
      calls [cycle] or [delay] under the hood! *)
  val forever : (unit -> unit t) -> never_returns t

  (** Similar to [forever], but returns unit, for convenience. *)
  val forever_unit : (unit -> unit t) -> unit t

  val never : never_returns t

  val run_effectful_computation
    :  ((O_data.t, I_data.t) Step_effect.Handler.t @ local -> 'a)
    -> 'a t

  module List : sig
    (** Construct a list of step monad results. The binds occurs from [0, 1, ...] which is
        the same as [Deferred.List.init] but opposite to [Base.List.init]. *)
    val init : int -> f:(int -> 'a t) -> 'a list t

    val iter : 'a list -> f:('a -> unit t) -> unit t
    val iteri : 'a list -> f:(int -> 'a -> unit t) -> unit t
    val iter2_exn : 'a list -> 'b list -> f:('a -> 'b -> unit t) -> unit t
    val map : 'a list -> f:('a -> 'b t) -> 'b list t
    val mapi : 'a list -> f:(int -> 'a -> 'b t) -> 'b list t
  end

  module Array : sig
    val init : int -> f:(int -> 'a t) -> 'a array t
    val iter : 'a array -> f:('a -> unit t) -> unit t
    val iteri : 'a array -> f:(int -> 'a -> unit t) -> unit t
    val map : 'a array -> f:('a -> 'b t) -> 'b array t
  end
end

module M (I : Interface.S) (O : Interface.S) = struct
  module type S = S with module I = I and module O = O
end

module type Functional = sig
  module type S = S

  module M = M
  module Make (I : Interface.S) (O : Interface.S) : M(I)(O).S
end
