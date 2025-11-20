open! Base
include Step_effect_intf
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

  module Handler = struct
    type ('i, 'o) t = ('i, 'o) Step_core.Computation.Eff.Handler.t
  end

  let current_input (handler : ('i, 'o) Handler.t @ local) =
    let%tydi { aliased_many = result } =
      Step_core.Computation.Eff.perform
        handler
        (Step_core.Computation.Effect_ops.Exec_monadic
           Step_core.Computation.Monadic.Current_input)
    in
    result
  ;;

  let next_step here o (handler : _ Handler.t @ local) =
    let%tydi { aliased_many = result } =
      Step_core.Computation.Eff.perform
        handler
        (Step_core.Computation.Effect_ops.Next_period (here, o))
    in
    result
  ;;

  let thunk f (handler : _ Handler.t @ local) = f () handler

  let output_forever (type i o) output (handler : (i, o) Handler.t @ local) =
    while true do
      ignore (next_step [%here] output handler : i)
    done
  ;;

  let wait_for (event : _ Event.t) ~output (handler : _ Handler.t @ local) =
    let result = ref None in
    while Option.is_none !result do
      match Event.value event with
      | Some a -> result := Some a
      | None -> ignore (next_step [%here] output handler : 'i)
    done;
    Option.value_exn !result
  ;;

  let wait ~output ~until (handler : ('i, 'o) Handler.t @ local) =
    let input = ref (current_input handler) in
    while not (until !input) do
      input := next_step [%here] output handler
    done
  ;;

  let delay output ~num_steps (handler : ('i, 'o) Handler.t @ local) =
    if num_steps < 0
    then
      raise_s
        [%message "[Step_effects.delay] got negative [num_steps]" ~_:(num_steps : int)];
    for _ = 1 to num_steps do
      ignore (next_step [%here] output handler : 'i)
    done
  ;;

  let create_component
    (type a i o)
    ?period
    ~created_at
    ~update_children_after_finish
    ~(start : i -> (i, o) Handler.t @ local -> (a, o) Component_finished.t)
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
              Step_core.Computation.Effectful
                (fun handler ->
                  let x = start i handler in
                  Event.set_value component_finished x;
                  x.output))
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
    (handler : ('i, 'o) Handler.t @ local)
    =
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
    let monadic_spawn =
      Step_core.Computation.Monadic.Spawn
        { child; child_finished; child_input; include_child_output }
    in
    let%tydi { aliased_many = () } =
      Step_core.Computation.Eff.perform
        handler
        (Step_core.Computation.Effect_ops.Exec_monadic monadic_spawn)
    in
    child_finished
  ;;

  let run_monadic_computation (handler : ('i, 'o) Handler.t @ local) computation =
    let%tydi { aliased_many } =
      Step_core.Computation.Eff.perform
        handler
        (Step_core.Computation.Effect_ops.Exec_monadic computation)
    in
    aliased_many
  ;;
end
