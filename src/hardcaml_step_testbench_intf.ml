open Base
open Hardcaml
open Digital_components

(** Core API of step testbenches, decoupled from the functions that run them. **)
module type S_api = sig
  module Step_monad : Digital_components.Step_monad.S
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
      for the circuit.  An ['a t] describes a testbench computation that takes zero or
      more steps and produces a value of type ['a]. *)
  type 'a t = ('a, O_data.t, I_data.t) Step_monad.t

  include Monad.S with type 'a t := 'a t

  (** [cycle i_data ~num_cycles] waits for [num_cycles] cycles of the simulator to run,
      applying [i_data] to the simulator input ports, and returns the output computed in
      the final cycle.  [cycle] raises if [num_cycles < 1]. *)
  val cycle : ?num_cycles:int (** default is 1 *) -> I_data.t -> O_data.t t

  (** [for_ i j f] does [f i], [f (i+1)], ... [f j] in sequence.  If [j < i], then [for_ i
      j] immediately returns unit. *)
  val for_ : int -> int -> (int -> unit t) -> unit t

  (** [delay inputs ~num_cycles] applies [inputs] for [num_cycles] clock cycles and then
      returns unit.  [delay] raises if [num_cycles < 0]. *)
  val delay : I_data.t -> num_cycles:int -> unit t

  type ('a, 'b) finished_event =
    ('a, 'b) Step_monad.Component_finished.t Step_monad.Event.t

  (** Launch a new task within the current simulation step. *)
  val spawn : (O_data.t -> 'a t) -> ('a, I_data.t) finished_event t

  (** [merge_inputs ~parent ~child] merges the child inputs into the parent.  If a child
      input is [empty], the parent's value is used. *)
  val merge_inputs : parent:I_data.t -> child:I_data.t -> I_data.t

  (** Launch a task from a testbench with a [cycle] function taking ['i] to ['o].  The
      [inputs] and [outputs] arguments should construct [I_data.t] and [O_data.t] from the
      types of the child testbench. *)
  val spawn_io
    :  inputs:(parent:'i -> child:I_data.t -> 'i)
    -> outputs:('o -> Bits.t O.t)
    -> (O_data.t -> 'a t)
    -> (('a, I_data.t) finished_event, 'o Before_and_after_edge.t, 'i) Step_monad.t

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

module type Monads = sig
  module Input_monad : Monad.S
  module Step_monad : Digital_components.Step_monad.M(Input_monad).S
end

module M_api (Monads : Monads) (I : Interface.S) (O : Interface.S) = struct
  module type S =
    S_api with module Step_monad = Monads.Step_monad and module I = I and module O = O
end

module Cyclesim_input_monad = Monad.Ident
module Cyclesim_step_monad = Digital_components.Step_monad.Make (Cyclesim_input_monad)

module type S_cyclesim = sig
  include S_api with module Step_monad = Cyclesim_step_monad

  (** A simulator for the design being tested. *)
  module Simulator : sig
    type t = Cyclesim.With_interface(I)(O).t [@@deriving sexp_of]
  end

  (** Run the testbench until the main task finishes. The [input_default] argument
      controls what should happen if an input is unset by tasks in the testbench on any
      particular cycle. If a field is set to [Bits.empty] then the previous value should
      be held. Otherwise, the value provided is used as the default value for that field.

      The optional timeout argument stops the simulation after the given number of steps
      and returns None. Otherwise it will continue until the testbech completes. *)
  val run_with_timeout
    :  ?input_default:Bits.t I.t (** default is [input_hold] *)
    -> ?show_steps:bool (** default is [false] *)
    -> ?timeout:int (** default is [None] *)
    -> unit
    -> simulator:Simulator.t
    -> testbench:(O_data.t -> 'a t)
    -> 'a option

  (** Run the testbench until completion. *)
  val run_until_finished
    :  ?input_default:Bits.t I.t (** default is [input_hold] *)
    -> ?show_steps:bool (** default is [false] *)
    -> unit
    -> simulator:Simulator.t
    -> testbench:(O_data.t -> 'a t)
    -> 'a
end

module Evsim_logic = Hardcaml_event_driven_sim.Two_state_logic
module Evsim = Hardcaml_event_driven_sim.Make (Evsim_logic)
module Evsim_input_monad = Evsim.Event_simulator.Async.Deferred
module Evsim_step_monad = Digital_components.Step_monad.Make (Evsim_input_monad)

(** [Event_driven_sim] implementation of step testbenches.

    Each step testbench is run by a single process that is sensitive to a single clock.
    Multiple stepbenches can be run in different processes.
*)
module type S_evsim = sig
  include S_api with module Step_monad = Evsim_step_monad

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
    ?input_default:Evsim_logic.t I.t
    -> ?show_steps:bool
    -> ?timeout:int
    -> ?simulation_step:Simulation_step.t (** Default is [cyclesim_compatible] *)
    -> unit
    -> clock:Evsim_logic.t Evsim.Event_simulator.Signal.t
    -> inputs:Evsim_logic.t Evsim.Event_simulator.Signal.t I.t
    -> outputs:Evsim_logic.t Evsim.Event_simulator.Signal.t O.t
    -> testbench:(O_data.t -> 'a t)
    -> 'r

  (** Create an event sim deferred that can be run inside a process. The result of the
      step testbench is returned.  None is returned if the simulation times out.

      The clock signal should be driven externally - do NOT set it within the step moand.
  *)
  val deferred : ('a, unit -> 'a option Evsim.Event_simulator.Async.Deferred.t) run

  (** Wrap an event sim in a process and wait forever after it completes. *)
  val process : (unit, Evsim.Event_simulator.Process.t) run
end

module type Hardcaml_step_testbench = sig
  module Before_and_after_edge = Before_and_after_edge

  (** Stepbench API compatible with both [Cyclesim] and [Event_driven_sim]. Testbenches
      can be functorised over [Monads] to be generic. *)
  module Api : sig
    (** Both the input and step monads together for constructing the simulator API. The
        [Cyclesim] and [Event_driven_sim] implementations below can be used to satisfy
        this interface. *)
    module type Monads = Monads

    module type S = S_api

    module M = M_api
    module Make (Monads : Monads) (I : Interface.S) (O : Interface.S) : M(Monads)(I)(O).S
  end

  (** [Cyclesim] implementation of step testbenches. *)
  module Cyclesim : sig
    module Input_monad = Cyclesim_input_monad
    module Step_monad = Cyclesim_step_monad

    module type S = S_cyclesim

    module Make (I : Interface.S) (O : Interface.S) :
      S with module I := I and module O := O
  end

  (** [Event_driven_sim] implementation of step testbenches. *)
  module Event_driven_sim : sig
    module Input_monad = Evsim_input_monad
    module Step_monad = Evsim_step_monad
    module Logic = Evsim_logic
    module Simulator = Evsim

    module type S = S_evsim

    module Make (I : Interface.S) (O : Interface.S) :
      S with module I := I and module O := O
  end

  include module type of Cyclesim
end
