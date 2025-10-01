open! Core
open Hardcaml

module Monads = struct
  module Input_monad = Monad.Ident
  module Step_monad = Digital_components.Step_monad.Make (Input_monad)
end

module type S = sig
  include Imperative.S

  (** Run the testbench until the main task finishes.

      The optional timeout argument stops the simulation after the given number of steps
      and returns None. Otherwise it will continue until the testbech completes. *)
  val run_with_timeout
    :  ?update_children_after_finish:bool (** default is [false] *)
    -> ?timeout:int (** default is [None] *)
    -> unit
    -> simulator:(_, _) Cyclesim.t
    -> testbench:(unit -> 'a t)
    -> 'a option

  (** Identical to [run_with_timeout], but the [testbench] function takes the simulator
      object. There is no semantical difference at all -- this is just a convenience
      function. *)
  val run_with_timeout'
    :  ?update_children_after_finish:bool (** default is [false] *)
    -> ?timeout:int (** default is [None] *)
    -> unit
    -> simulator:('i, 'o) Cyclesim.t
    -> testbench:(('i, 'o) Cyclesim.t -> 'a t)
    -> 'a option

  (** Run the testbench until completion. *)
  val run_until_finished
    :  ?update_children_after_finish:bool (** default is [false] *)
    -> unit
    -> simulator:(_, _) Cyclesim.t
    -> testbench:(unit -> 'a t)
    -> 'a

  (** [wrap] constructs a [Sim.t] instances such that calling [Cyclesim.cycle sim] will
      step the testbenches by one a cycle. The testbench will stop executing if it's
      completed, but the user will not receive any notification if this happens. The
      provided testbenches are executed sequentially.

      Note that there is no safety gurantees on the main simulation loop and the
      testbenches accessing the ports. It's the programmer's responsibility to think about
      how the port access. For most use cases, it's expected that the testbenches and the
      main simulation loop should be modifying different input ports. *)
  val wrap
    :  ?show_steps:bool
    -> when_to_evaluate_testbenches:[ `Before_cycle | `After_cycle ]
    -> testbenches:(unit -> unit t) list
    -> ('i, 'o) Cyclesim.t
    -> ('i, 'o) Cyclesim.t

  (** Exactly the same as [wrap], but accepts a [never_returns] testbench in the
      signature. *)
  val wrap_never_returns
    :  ?show_steps:bool
    -> when_to_evaluate_testbenches:[ `Before_cycle | `After_cycle ]
    -> testbenches:(unit -> never_returns t) list
    -> ('i, 'o) Cyclesim.t
    -> ('i, 'o) Cyclesim.t
end

module type Imperative_cyclesim = sig
  module Monads = Monads

  module type S = S with module Step_monad = Monads.Step_monad

  include S
end
