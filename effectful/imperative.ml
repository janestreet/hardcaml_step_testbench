open! Core
open! Hardcaml
open! Digital_components
open! Hardcaml_step_testbench_kernel

module type S = Imperative_intf.S

module M = Imperative_intf.M

module Make (Step_modules : Step_modules.S) = struct
  module I_data = struct
    type t = unit [@@deriving sexp_of]

    let equal _ _ = true
    let undefined = ()
  end

  module O_data = struct
    type t = unit Hardcaml_step_testbench_kernel.Before_and_after_edge.t
    [@@deriving sexp_of]

    let equal _ _ = true
    let undefined = Hardcaml_step_testbench_kernel.Before_and_after_edge.const ()
  end

  module Handler = struct
    type t = (O_data.t, I_data.t) Step_modules.Step_effect.Handler.t
  end

  let rec cycle (handler : Handler.t @ local) ?(num_cycles = 1) () =
    if num_cycles < 1
    then raise_s [%message "cycle must take 1 or more num_cycles" (num_cycles : int)]
    else if num_cycles = 1
    then (
      let (_ : O_data.t) = Step_modules.Step_effect.next_step [%here] () handler in
      ())
    else (
      let (_ : O_data.t) = Step_modules.Step_effect.next_step [%here] () handler in
      cycle handler ~num_cycles:(num_cycles - 1) ())
  ;;

  let start (handler : Handler.t @ local) testbench output =
    let result = testbench handler output in
    { Step_modules.Step_effect.Component_finished.output = (); result }
  ;;

  type ('a, 'i) finished_event =
    ('a, 'i) Step_modules.Step_effect.Component_finished.t
      Step_modules.Step_effect.Event.t

  let spawn
    (type a)
    ?period
    (handler : Handler.t @ local)
    (task : Handler.t @ local -> unit -> a)
    : (a, unit) finished_event
    =
    Step_modules.Step_effect.spawn
      [%here]
      ?period
      ~start:(fun (_ : O_data.t) handler ->
        start handler (fun handler _ -> task handler ()) ())
      ~input:(module O_data)
      ~output:(module I_data)
      ~child_input:(fun ~parent:_ ->
        Hardcaml_step_testbench_kernel.Before_and_after_edge.const ())
      ~include_child_output:(fun ~parent:_ ~child:_ -> ())
      handler
  ;;

  let wait_for (handler : Handler.t @ local) (event : _ finished_event) =
    let x = Step_modules.Step_effect.wait_for event ~output:() handler in
    x.result
  ;;

  let rec wait_for_with_timeout
    (handler : Handler.t @ local)
    (event : _ finished_event)
    ~timeout_in_cycles
    =
    if timeout_in_cycles < 0
    then raise_s [%message "timeout_in_cycles < 0" (timeout_in_cycles : int)];
    match Step_modules.Step_effect.Event.value event with
    | Some x -> Some x.result
    | None ->
      if timeout_in_cycles = 0
      then None
      else (
        cycle handler ();
        wait_for_with_timeout handler event ~timeout_in_cycles:(timeout_in_cycles - 1))
  ;;

  let forever (handler : Handler.t @ local) f : never_returns =
    while true do
      f handler ()
    done;
    assert false
  ;;

  let run_monadic_computation
    (type a)
    (h @ local)
    (computation : (a, O_data.t, I_data.t) Step_modules.Step_monad.t)
    : a
    =
    Step_modules.Step_effect.run_monadic_computation h computation
  ;;
end
