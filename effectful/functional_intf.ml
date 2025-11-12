(** Common testbench API between simulators.

    Testbenches written against this API make may be used with either cyclesim or
    event_driven_sim. *)

open Core
open Hardcaml
open Digital_components
open Hardcaml_step_testbench_kernel

module type S = sig
  module Io_ports_for_imperative = Hardcaml_step_testbench_kernel.Io_ports_for_imperative
  module Before_and_after_edge = Hardcaml_step_testbench_kernel.Before_and_after_edge
  module Step_modules : Step_modules.S
  module Step_effect := Step_modules.Step_effect
  module I : Hardcaml.Interface.S
  module O : Hardcaml.Interface.S
  module I_data : Data.S with type t = Bits.t I.t

  module O_data : sig
    type t = Bits.t O.t Before_and_after_edge.t [@@deriving sexp_of]

    val before_edge : t -> Bits.t O.t
    val after_edge : t -> Bits.t O.t

    include Data.S with type t := t
  end

  module Handler : sig
    type t = (O_data.t, I_data.t) Step_effect.Handler.t
  end

  val start
    :  Handler.t @ local
    -> (Handler.t @ local -> 'a -> 'b)
    -> 'a
    -> ('b, I_data.t) Step_effect.Component_finished.t

  (** [cycle i_data ~num_cycles] waits for [num_cycles] cycles of the simulator to run,
      applying [i_data] to the simulator input ports, and returns the output computed in
      the final cycle. [cycle] raises if [num_cycles < 1]. *)
  val cycle
    :  Handler.t @ local
    -> ?num_cycles:int (** default is 1 *)
    -> I_data.t
    -> O_data.t

  (** [delay inputs ~num_cycles] applies [inputs] for [num_cycles] clock cycles and then
      returns unit. [delay] raises if [num_cycles < 0]. *)
  val delay : Handler.t @ local -> I_data.t -> num_cycles:int -> unit

  type ('a, 'b) finished_event =
    ('a, 'b) Step_effect.Component_finished.t Step_effect.Event.t

  (** Launch a new task within the current simulation step. *)
  val spawn
    :  ?update_children_after_finish:bool
         (** When [update_children_after_finish] is set to true. children tasks that have
             finished will still be updated. This will notably trigger an update on nested
             spawns. *)
    -> Handler.t @ local
    -> (Handler.t @ local -> O_data.t -> 'a)
    -> ('a, I_data.t) finished_event

  (** [merge_inputs ~parent ~child] merges the child inputs into the parent. If a child
      input is [empty], the parent's value is used. *)
  val merge_inputs : parent:I_data.t -> child:I_data.t -> I_data.t

  (** Launch a task from a testbench with a [cycle] function taking ['i] to ['o]. The
      [inputs] and [outputs] arguments should construct [I_data.t] and [O_data.t] from the
      types of the child testbench.

      See documentation of [spawn] for an explaination of [update_children_after_finish]. *)
  val spawn_io
    :  ?update_children_after_finish:bool
    -> inputs:(parent:'i -> child:I_data.t -> 'i)
    -> outputs:('o -> Bits.t O.t)
    -> ('o Before_and_after_edge.t, 'i) Step_effect.Handler.t @ local
    -> (Handler.t @ local -> O_data.t -> 'a)
    -> ('a, I_data.t) finished_event

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
    -> (Bits.t ref I.t, Bits.t ref O.t) Io_ports_for_imperative.t
    -> (unit Before_and_after_edge.t, unit) Step_effect.Handler.t @ local
    -> (Handler.t @ local -> O_data.t -> 'a)
    -> ('a, I_data.t) finished_event

  (** Spawns a functional step testbench infinite loop from a imperative testbench, and
      block forever.

      This semantically similar to [spawn_from_imperative >>= wait_for] under the hood but
      with nicer types. *)
  val exec_never_returns_from_imperative
    :  ?update_children_after_finish:bool
    -> (Bits.t ref I.t, Bits.t ref O.t) Io_ports_for_imperative.t
    -> (unit Before_and_after_edge.t, unit) Step_effect.Handler.t @ local
    -> (Handler.t @ local -> O_data.t -> never_returns)
    -> never_returns

  (** Wait for the given event to occur, and extract its return value. *)
  val wait_for : Handler.t @ local -> ('a, 'b) finished_event -> 'a

  (** Like [wait_for] except it stops waiting after [timeout_in_cycles] and returns
      [None]. Note that the spawned task continues to execute. *)
  val wait_for_with_timeout
    :  Handler.t @ local
    -> ('a, 'b) finished_event
    -> timeout_in_cycles:int
    -> 'a option

  (** Call [run ~input_default:input_hold] to hold inputs their previous value if they are
      unset by tasks in the testbench. *)
  val input_hold : Bits.t I.t

  (** Call [run ~input_default:input_zero] to set inputs to zero if unset by tasks in the
      testbench. *)
  val input_zero : Bits.t I.t

  (** Call [forever f] to run [f] forever. [forever] never returns.

      To prevent starving other tasks, it's the caller's responsibility to ensure that [f]
      calls [cycle] or [delay] under the hood! *)
  val forever : Handler.t @ local -> (Handler.t @ local -> unit -> unit) -> never_returns

  (** Similar to [forever], but returns unit, for convenience. *)
  val forever_unit : Handler.t @ local -> (Handler.t @ local -> unit -> unit) -> unit

  val never : Handler.t @ local -> never_returns

  val run_monadic_computation
    :  Handler.t @ local
    -> ('a, O_data.t, I_data.t) Step_modules.Step_monad.t
    -> 'a
end

module M (Step_modules : Step_modules.S) (I : Interface.S) (O : Interface.S) = struct
  module type S =
    S with module Step_modules := Step_modules and module I = I and module O = O
end

module type Functional = sig
  module type S = S

  module M = M

  module Make (Step_modules : Step_modules.S) (I : Interface.S) (O : Interface.S) :
    M(Step_modules)(I)(O).S
end
