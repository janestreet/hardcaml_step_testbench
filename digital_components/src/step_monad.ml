open! Base
include Step_monad_intf
module M = M

module Make
    (Input_monad : Monad.S)
    (Component : Component.M(Input_monad).S)
    (Step_core : Step_core.M(Input_monad)(Component).S) =
struct
  module Input_monad = Input_monad
  module Component = Component
  module Component_finished = Component_finished
  module Event = Event
  module Runner = Step_core.Runner
  include Step_core.Computation.Monadic
  open! Let_syntax

  let current_input = Current_input
  let thunk f = Thunk f
  let next_step here o = Next_period (here, o)

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

  let create_component
    (type a i o)
    ?period
    ~created_at
    ~update_children_after_finish
    ~(start : i -> ((a, o) Component_finished.t, i, o) t)
    ~(input : i Data.t)
    ~(output : o Data.t)
    ()
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
              Step_core.Computation.Monadic
                (let%bind x = start i in
                 Event.set_value component_finished x;
                 return x.output))
          ;;

          let output (t : t) _ = Runner.output t

          let update_state ?prune ~parent_period ~step_number t =
            let period = Option.value ~default:parent_period period in
            Runner.update_state
              ?prune
              ~update_children_after_finish
              ~period
              ~step_number
              t
          ;;

          let prune_children = Runner.prune_children
          let has_children (t : t) = Runner.has_children t
        end)
    in
    component, component_finished
  ;;

  let spawn
    ?(update_children_after_finish = false)
    ?period
    created_at
    ~start
    ~input
    ~output
    ~child_input
    ~include_child_output
    =
    thunk (fun () ->
      let child, child_finished =
        create_component
          ?period
          ~update_children_after_finish
          ~created_at
          ~start
          ~input
          ~output
          ()
      in
      let%bind () = Spawn { child; child_finished; child_input; include_child_output } in
      return child_finished)
  ;;
end
