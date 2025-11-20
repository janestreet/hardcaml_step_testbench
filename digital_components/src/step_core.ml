[@@@alert "-experimental_runtime5"]

open Core
include Step_core_intf

module Make (Input_monad : Monad.S) (Component : Component.M(Input_monad).S) = struct
  module Computation = struct
    type 'a aliased_many = 'a Modes.Aliased_many.t

    type ('a, 'i, 'o) monadic =
      | Bind : ('a, 'i, 'o) monadic * ('a -> ('b, 'i, 'o) monadic) -> ('b, 'i, 'o) monadic
      | Current_input : ('i, 'i, 'o) monadic
      | Next_period : Source_code_position.t * 'o -> ('i, 'i, 'o) monadic
      | Return : 'a -> ('a, _, _) monadic
      | Thunk : (unit -> ('a, 'i, 'o) monadic) -> ('a, 'i, 'o) monadic
      | Spawn :
          { child : ('i_c, 'o_c) Component.t
          ; child_finished : (_, 'o_c) Component_finished.t Event.t
          ; child_input : parent:'i -> 'i_c
          ; include_child_output : parent:'o -> child:'o_c -> 'o
          }
          -> (unit, 'i, 'o) monadic

    and ('a, 'i, 'o, 'eff) effect_ops =
      | Next_period :
          Source_code_position.t * 'o
          -> ('i aliased_many, 'i, 'o, 'eff) effect_ops
      | Exec_monadic :
          (('a, 'i, 'o) monadic[@sexp.opaque])
          -> ('a aliased_many, 'i, 'o, 'eff) effect_ops
    [@@deriving sexp_of]

    module Monadic = struct
      module T = struct
        type ('a, 'i, 'o) t = ('a, 'i, 'o) monadic =
          | Bind : ('a, 'i, 'o) t * ('a -> ('b, 'i, 'o) t) -> ('b, 'i, 'o) t
          | Current_input : ('i, 'i, 'o) t
          | Next_period : Source_code_position.t * 'o -> ('i, 'i, 'o) t
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

        let return x = Return x
        let bind t ~f = Bind (t, f)
        let map = `Define_using_bind
      end

      include T
      include Monad.Make3 (T)
    end

    module Effect_ops = struct
      type ('a, 'i, 'o, 'effect) t = ('a, 'i, 'o, 'effect) effect_ops =
        | Next_period :
            Source_code_position.t * 'o
            -> ('i aliased_many, 'i, 'o, 'effect) t
        | Exec_monadic : ('a, 'i, 'o) monadic -> ('a aliased_many, 'i, 'o, 'effect) t
    end

    module Eff = Effect.Make2_rec (struct
        type ('a, 'i, 'o, 'eff) t = ('a, 'i, 'o, 'eff) effect_ops
      end)

    type ('a, 'i, 'o) t =
      | Monadic of ('a, 'i, 'o) monadic
      | Effectful of (('i, 'o) Eff.Handler.t @ local -> 'a)
  end

  module Runner = struct
    (* An [('a, 'i, 'o) Continuation.t] is a computation awaiting a value of type ['a]. It
       is analogous to a call stack. *)

    module Continuation = struct
      type ('a, 'i, 'o) t =
        | Monad_bind :
            ('a -> ('b, 'i, 'o) Computation.Monadic.t) * ('b, 'i, 'o) t
            -> ('a, 'i, 'o) t
        | Effect_continuation :
            ((('a Modes.Aliased_many.t, 'b, 'i, 'o, unit) Computation.Eff.Continuation.t
                Unique.Once.t
             [@sexp.opaque])
            * ('b, 'i, 'o) t)
            -> ('a, 'i, 'o) t
        | Empty : ('o, 'i, 'o) t
      [@@deriving sexp_of]
    end

    (* An [('i, 'o) State.t] is the current state of a running computation, analogous to a
       program counter. *)
    module State = struct
      type ('i, 'o) t =
        | Finished of 'o
        | Running of
            { num_steps_to_stall : int
            ; continuation : ('i, 'i, 'o) Continuation.t
            }
        | Initial_alignment of
            { num_steps_to_stall : int
            ; start : 'i -> ('o, 'i, 'o) Computation.t
            }
        | Unstarted of ('i -> ('o, 'i, 'o) Computation.t)
      [@@deriving sexp_of]
    end

    (* An [('i, 'o) Child.t] is a child component of a parent computation, along with
       information for translating between the parent's ['i] and ['o] and the child's
       ['i_c] and ['o_c]. *)
    module Child = struct
      type ('i, 'o) t =
        | T :
            { component : ('i_c, 'o_c) Component.t
            ; child_finished : (_, 'o_c) Component_finished.t Event.t
            ; child_input : parent:'i -> 'i_c
            ; include_child_output : parent:'o -> child:'o_c -> 'o
            }
            -> ('i, 'o) t

      let sexp_of_t _ _ (T t) = [%sexp (t.component : (_, _) Component.t)]

      let create ~child_finished ~child_input ~component ~include_child_output =
        T { component; child_finished; child_input; include_child_output }
      ;;
    end

    type ('i, 'o) t =
      { mutable state : ('i, 'o) State.t
      ; mutable children : ('i, 'o) Child.t list
      ; mutable output : 'o
      }
    [@@deriving sexp_of]

    let output t = t.output
    let create ~start ~output = { state = Unstarted start; children = []; output }

    (* Each component tracks all of their children even if they are "finished". To improve
       performance we periodically traverse through the tree of children to prune the ones
       that are finished. *)
    let prune_children (type i o) (t : (i, o) t) =
      t.children
      <- List.filter t.children ~f:(fun (Child.T child) ->
           Component.prune_children child.component;
           Component.has_children child.component
           || Option.is_none (Event.value child.child_finished))
    ;;

    let has_children (t : _ t) = not (List.is_empty t.children)

    type ('i, 'o) output_and_next_state =
      { output : 'o
      ; next_state : ('i, 'o) State.t
      }

    let update_state
      (type i o)
      ?(prune = false)
      ~update_children_after_finish
      ~period
      ~step_number
      (t : (i, o) t)
      (current_input : i)
      =
      (* Wait until the next [step_number] is at least [period] away and is also a
         multiple of [period] *)

      (* [Initial_alignment] on startup ensures that we are aligned with the period, so it
         is always safe to step the full requested period on [Next_period] *)
      let next_period_steps_to_stall = period - 1 in
      let rec step
        : type a.
          (a, i, o) Computation.t
          -> (a, i, o) Continuation.t
          -> (i, o) output_and_next_state
        =
        fun computation continuation ->
        match computation with
        | Monadic computation -> handle_monad computation continuation
        | Effectful computation ->
          handle_eff (Computation.Eff.run computation) continuation
      and handle_eff
        : type a.
          (a, i, o, unit) Computation.Eff.result @ once unique
          -> (a, i, o) Continuation.t
          -> (i, o) output_and_next_state
        =
        fun result continuation ->
        match result with
        | Value a -> continue continuation { aliased_many = a }
        | Exception exn -> raise exn
        | Operation (effect, k) ->
          (match effect with
           | Next_period (_, o) ->
             { output = o
             ; next_state =
                 Running
                   { num_steps_to_stall = next_period_steps_to_stall
                   ; continuation =
                       Continuation.Effect_continuation (Unique.Once.make k, continuation)
                   }
             }
           | Exec_monadic monad ->
             handle_monad
               monad
               (Continuation.Effect_continuation (Unique.Once.make k, continuation)))
      and handle_monad
        : type a.
          (a, i, o) Computation.Monadic.t
          -> (a, i, o) Continuation.t
          -> (i, o) output_and_next_state
        =
        fun computation continuation ->
        match computation with
        | Bind (computation, f) -> handle_monad computation (Monad_bind (f, continuation))
        | Current_input -> continue continuation { aliased_many = current_input }
        | Next_period (_, output) ->
          { output
          ; next_state =
              Running { num_steps_to_stall = next_period_steps_to_stall; continuation }
          }
        | Return a -> continue continuation { aliased_many = a }
        | Thunk f -> handle_monad (f ()) continuation
        | Spawn { child; child_finished; child_input; include_child_output } ->
          t.children
          <- Child.create
               ~child_finished
               ~child_input
               ~component:child
               ~include_child_output
             :: t.children;
          continue continuation { aliased_many = () }
      and continue
        : type a.
          (a, i, o) Continuation.t
          -> a Modes.Aliased_many.t @ once unique
          -> (i, o) output_and_next_state
        =
        fun continuation a ->
        let module C = Continuation in
        match continuation with
        | C.Empty -> { output = a.aliased_many; next_state = Finished a.aliased_many }
        | C.Monad_bind (f, c) -> step (Monadic (f a.aliased_many)) c
        | C.Effect_continuation (computation_to_resume, continuation) ->
          handle_eff
            (Effect.continue (Unique.Once.get_exn computation_to_resume) a [])
            continuation
      in
      let[@inline always] maybe_stall ~num_steps_to_stall ~f ~stall =
        assert (num_steps_to_stall >= 0);
        match num_steps_to_stall with
        | 0 -> f ()
        | _ -> stall ~num_steps_to_stall:(num_steps_to_stall - 1)
      in
      let[@inline always] start_updates ~start = step (start current_input) Empty in
      let%tydi { output; next_state } =
        match t.state with
        | Finished output -> { output; next_state = Finished output }
        | Running { num_steps_to_stall; continuation } ->
          maybe_stall
            ~num_steps_to_stall
            ~f:(fun () -> continue continuation { aliased_many = current_input })
            ~stall:(fun ~num_steps_to_stall ->
              { output = t.output
              ; next_state = Running { num_steps_to_stall; continuation }
              })
        | Initial_alignment { num_steps_to_stall; start } ->
          maybe_stall
            ~num_steps_to_stall
            ~f:(fun () -> start_updates ~start)
            ~stall:(fun ~num_steps_to_stall ->
              { output = t.output
              ; next_state = Initial_alignment { num_steps_to_stall; start }
              })
        | Unstarted start ->
          let offset = step_number % period in
          if offset = 0
          then start_updates ~start
          else (
            (* Stall until we are aligned with the period *)
            let num_steps_to_stall = period - offset - 1 in
            { output = t.output
            ; next_state = Initial_alignment { num_steps_to_stall; start }
            })
      in
      t.state <- next_state;
      if prune then prune_children t;
      t.output
      <- List.fold t.children ~init:output ~f:(fun output (Child.T child) ->
           if Option.is_some (Event.value child.child_finished)
              && not update_children_after_finish
           then output
           else (
             let child_input = child.child_input ~parent:current_input in
             Component.update_state
               child.component
               child_input
               ~parent_period:period
               ~step_number;
             let child_output = Component.output child.component child_input in
             child.include_child_output ~parent:output ~child:child_output))
    ;;
  end
end
