(** This module is the heart of the step testbench, the [Computation.t] type and the
    [Runner.t] to is used to represent the execution of the computation. *)

open Core

module M (Input_monad : Monad.S) (Component : Component.M(Input_monad).S) = struct
  module type S = sig
    module Computation : sig
      module rec Monadic : sig
        type ('a, 'i, 'o) t =
          | Bind : ('a, 'i, 'o) t * ('a -> ('b, 'i, 'o) t) -> ('b, 'i, 'o) t
          | Current_input : ('i, 'i, 'o) t
          | Next_step : Source_code_position.t * 'o -> ('i, 'i, 'o) t
          | Return : 'a -> ('a, _, _) t
          | Thunk : (unit -> ('a, 'i, 'o) t) -> ('a, 'i, 'o) t
          | Spawn :
              { child : ('i_c, 'o_c) Component.t
              ; child_finished : (_, 'o_c) Component_finished.t Event.t
              ; child_input : parent:'i -> 'i_c
              ; include_child_output : parent:'o -> child:'o_c -> 'o
              }
              -> (unit, 'i, 'o) t
        [@@deriving sexp_of]

        include Monad.S3 with type ('a, 'i, 'o) t := ('a, 'i, 'o) t
      end

      and Effect_ops : sig
        type ('a, 'i, 'o, 'effect) t =
          | Next_step : Source_code_position.t * 'o -> ('i aliased, 'i, 'o, 'effect) t
          | Exec_monadic : ('a, 'i, 'o) Monadic.t -> ('a aliased, 'i, 'o, 'effect) t
      end

      module Eff :
        Effect.S2 with type ('a, 'i, 'o, 'eff) ops := ('a, 'i, 'o, 'eff) Effect_ops.t

      type ('a, 'i, 'o) t =
        | Monadic of ('a, 'i, 'o) Monadic.t
        | Effectful of (('i, 'o) Eff.Handler.t -> 'a)
    end

    (* A [Runner.t] is a stateful value that can run a [Computation.t] one step at a time, and has
     an interface like [Component.S]. *)
    module Runner : sig
      type ('i, 'o) t [@@deriving sexp_of]

      (** Traverse through the tree of children to prune the ones that are finished

          Each component tracks all of their children even if they are "finished". To
          improve performance we periodically . *)
      val prune_children : ('i, 'o) t -> unit

      val output : ('i, 'o) t -> 'o

      (** Returns true if the list of children is empty. Note that calls to prune_children
          can change the result of this function call. *)
      val has_children : ('i, 'o) t -> bool

      val create : start:('i -> ('o, 'i, 'o) Computation.t) -> output:'o -> ('i, 'o) t

      val update_state
        :  ?prune:bool
        -> update_children_after_finish:bool
        -> ('i, 'o) t
        -> 'i
        -> unit
    end
  end
end

module type Step_core = sig
  module M = M

  module Make (Input_monad : Monad.S) (Component : Component.M(Input_monad).S) :
    M(Input_monad)(Component).S
end
