open! Base
include Step_monad_intf
module M = M

module Make (Input_monad : Monad.S) = struct
  module Input_monad = Input_monad
  module Component = Component.Make (Input_monad)

  module Component_finished = struct
    type ('a, 'o) t =
      { output : 'o
      ; result : 'a
      }
    [@@deriving sexp_of]
  end

  module Event : sig
    type 'a t [@@deriving sexp_of]

    val create : unit -> 'a t
    val set_value : 'a t -> 'a -> unit
    val value : 'a t -> 'a option
  end = struct
    type 'a t = { mutable value : 'a option } [@@deriving fields ~getters, sexp_of]

    let create () = { value = None }

    let set_value t a =
      if Option.is_some t.value
      then
        raise_s [%message "[Event.set_value] of event whose value has already been set"];
      t.value <- Some a
    ;;
  end

  module Computation = struct
    type ('a, 'i, 'o) t =
      | Bind : ('a, 'i, 'o) t * ('a -> ('b, 'i, 'o) t) -> ('b, 'i, 'o) t
      | Current_input : ('i, 'i, 'o) t
      | Next_step : Source_code_position.t * 'o -> ('i, 'i, 'o) t
      | Return : 'a -> ('a, _, _) t
      | Thunk : (unit -> ('a, 'i, 'o) t) -> ('a, 'i, 'o) t
      | Spawn :
          { child : (('i_c, 'o_c) Component.t[@sexp.opaque])
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

  include Computation
  include Monad.Make3 (Computation)
  open! Let_syntax

  let current_input = Current_input
  let thunk f = Thunk f
  let next_step here o = Next_step (here, o)

  let output_forever output =
    let rec loop () =
      let%bind _ = next_step [%here] output in
      loop ()
    in
    loop ()
  ;;

  let wait_for (event : _ Event.t) ~output =
    let rec loop () =
      (* We use [thunk] to delay checking [Event.value] until the last possible moment,
         when the computation is being evaluated. This can avoid an unnecessary
         [next_step]. *)
      thunk (fun () ->
        match Event.value event with
        | Some a -> return a
        | None ->
          let%bind _ = next_step [%here] output in
          loop ())
    in
    loop ()
  ;;

  let wait ~output ~until =
    let rec loop input =
      if until input
      then return ()
      else (
        let%bind input = next_step [%here] output in
        loop input)
    in
    let%bind input = current_input in
    loop input
  ;;

  let for_ from_ to_ f =
    let rec loop i =
      if i > to_
      then return ()
      else (
        let%bind () = f i in
        loop (i + 1))
    in
    loop from_
  ;;

  let delay output ~num_steps =
    if num_steps < 0
    then
      raise_s
        [%message "[Step_monad.delay] got negative [num_steps]" ~_:(num_steps : int)];
    let rec loop num_steps =
      if num_steps = 0
      then return ()
      else (
        let%bind _ = next_step [%here] output in
        loop (num_steps - 1))
    in
    loop num_steps
  ;;

  let repeat ~count f =
    if count < 0
    then raise_s [%message "[Step_monad.repeat] got negative [count]" ~_:(count : int)];
    let rec loop count =
      if count = 0
      then return ()
      else (
        let%bind () = f () in
        loop (count - 1))
    in
    loop count
  ;;

  (* A [Runner.t] is a stateful value that can run a [t] one step at a time, and has
     an interface like [Component.S]. *)
  module Runner = struct
    (* An [('a, 'i, 'o) Continuation.t] is a computation awaiting a value of type ['a].
       It is analogous to a call stack. *)
    module Continuation = struct
      type ('a, 'i, 'o) t =
        | Bind : ('a -> ('b, 'i, 'o) Computation.t) * ('b, 'i, 'o) t -> ('a, 'i, 'o) t
        | Empty : ('o, 'i, 'o) t
      [@@deriving sexp_of]
    end

    (* An [('i, 'o) State.t] is the current state of a running computation, analogous to a
       program counter. *)
    module State = struct
      type ('i, 'o) t =
        | Finished of 'o
        | Running of ('i, 'i, 'o) Continuation.t
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

    let update_state
      (type i o)
      ?(prune = false)
      ~update_children_after_finish
      (t : (i, o) t)
      (current_input : i)
      =
      let rec step
        : type a.
          (a, i, o) Computation.t -> (a, i, o) Continuation.t -> o * (i, o) State.t
        =
        fun computation continuation ->
        match computation with
        | Bind (computation, f) -> step computation (Bind (f, continuation))
        | Current_input -> continue continuation current_input
        | Next_step (_, output) -> output, Running continuation
        | Return a -> continue continuation a
        | Thunk f -> step (f ()) continuation
        | Spawn { child; child_finished; child_input; include_child_output } ->
          t.children
            <- Child.create
                 ~child_finished
                 ~child_input
                 ~component:child
                 ~include_child_output
               :: t.children;
          continue continuation ()
      and continue : type a. (a, i, o) Continuation.t -> a -> o * (i, o) State.t =
        fun continuation a ->
        let module C = Continuation in
        match continuation with
        | C.Empty -> a, Finished a
        | C.Bind (f, c) -> step (f a) c
      in
      let output, state =
        match t.state with
        | Finished output -> output, t.state
        | Running continuation -> continue continuation current_input
        | Unstarted start -> step (start current_input) Empty
      in
      if prune then prune_children t;
      t.state <- state;
      t.output
        <- List.fold t.children ~init:output ~f:(fun output (Child.T child) ->
             if Option.is_some (Event.value child.child_finished)
                && not update_children_after_finish
             then output
             else (
               let child_input = child.child_input ~parent:current_input in
               Component.update_state child.component child_input;
               let child_output = Component.output child.component child_input in
               child.include_child_output ~parent:output ~child:child_output))
    ;;
  end

  let create_component
    (type a i o)
    ~created_at
    ~update_children_after_finish
    ~(start : i -> ((a, o) Component_finished.t, i, o) t)
    ~(input : i Data.t)
    ~(output : o Data.t)
    : (i, o) Component.t * (a, o) Component_finished.t Event.t
    =
    let component_finished = Event.create () in
    let component =
      Component.create
        (module struct
          module Input = (val input)
          module Output = (val output)

          let created_at = created_at

          type t = (Input.t, Output.t) Runner.t [@@deriving sexp_of]

          let t =
            Runner.create ~output:Output.undefined ~start:(fun i ->
              let%bind x = start i in
              Event.set_value component_finished x;
              return x.output)
          ;;

          let output (t : t) _ = t.output

          let update_state ?prune =
            Runner.update_state ?prune ~update_children_after_finish
          ;;

          let prune_children = Runner.prune_children
          let has_children (t : t) = not (List.is_empty t.children)
        end)
    in
    component, component_finished
  ;;

  let spawn
    ?(update_children_after_finish = false)
    created_at
    ~start
    ~input
    ~output
    ~child_input
    ~include_child_output
    =
    thunk (fun () ->
      let child, child_finished =
        create_component ~update_children_after_finish ~created_at ~start ~input ~output
      in
      let%bind () = Spawn { child; child_finished; child_input; include_child_output } in
      return child_finished)
  ;;
end
