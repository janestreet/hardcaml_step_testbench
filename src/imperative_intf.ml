open! Core
open! Hardcaml
open Digital_components

module type S = sig
  module No_data : Data.S with type t = unit
  module Step_monad : Digital_components.Step_monad.S

  type 'a t = ('a, No_data.t, No_data.t) Step_monad.t

  include Monad.S with type 'a t := 'a t

  (** [cycle i_data ~num_cycles] waits for [num_cycles] cycles of the simulator to run.
      [cycle] raises if [num_cycles < 1]. *)
  val cycle : ?num_cycles:int -> unit -> unit t

  (** [for_ i j f] does [f i], [f (i+1)], ... [f j] in sequence.  If [j < i], then [for_ i
      j] immediately returns unit. *)
  val for_ : int -> int -> (int -> unit t) -> unit t

  val start : ('a -> 'b t) -> 'a -> ('b, No_data.t) Step_monad.Component_finished.t t

  type 'a finished_event = ('a, unit) Step_monad.Component_finished.t Step_monad.Event.t

  (** Launch a new task within the current simulation step. *)
  val spawn : (unit -> 'a t) -> 'a finished_event t

  (** Wait for the given event to occur, and extract its return value. *)
  val wait_for : 'a finished_event -> 'a t

  (** Like [wait_for] except it stops waiting after [timeout_in_cycles] and returns
      [None]. Note that the spawned task continues to execute. *)
  val wait_for_with_timeout : 'a finished_event -> timeout_in_cycles:int -> 'a option t

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

module M (Monads : Step_monads.S) = struct
  module type S = S with module Step_monad = Monads.Step_monad
end

module type Imperative = sig
  module type S = S

  module M = M
  module Make (Monads : Step_monads.S) : M(Monads).S
end
