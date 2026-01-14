open! Core
open Hardcaml
open! Digital_components
include Imperative
include Component.Run_component_until_finished (Monad.Ident)

let next_input timeout simulator result_event =
  let timedout =
    let count = ref 0 in
    fun () ->
      Int.incr count;
      match timeout with
      | None -> false
      | Some timeout -> !count >= timeout
  in
  Staged.stage (fun () ->
    Cyclesim.cycle simulator;
    match Step_effect.Event.value result_event with
    | None ->
      if timedout () then Component.Next_input.Finished else Input O_data.undefined
    | Some _ -> Finished)
;;

let run_with_timeout
  ?(update_children_after_finish = false)
  ?show_steps
  ?timeout
  ()
  ~simulator
  ~testbench
  =
  let component, result_event =
    Expert.create_component ~period:1 ~update_children_after_finish testbench
  in
  Cyclesim.cycle_until_clocks_aligned simulator;
  run_component_until_finished
    ?show_steps
    component
    ~first_input:O_data.undefined
    ~next_input:(Staged.unstage (next_input timeout simulator result_event));
  match Step_effect.Event.value result_event with
  | None -> None
  | Some x -> Some x.result
;;

let run_with_timeout'
  ?update_children_after_finish
  ?show_steps
  ?timeout
  ()
  ~simulator
  ~testbench
  =
  run_with_timeout
    ?update_children_after_finish
    ?show_steps
    ?timeout
    ()
    ~simulator
    ~testbench:(fun handler -> testbench handler simulator)
;;

let run_until_finished ?update_children_after_finish ?show_steps () ~simulator ~testbench =
  match
    run_with_timeout ?update_children_after_finish ?show_steps () ~simulator ~testbench
  with
  | Some result -> result
  | None -> raise_s [%message "Step testbench did not complete with a result."]
;;

let wrap ?(show_steps = false) ~when_to_evaluate_testbenches ~testbenches simulator =
  let evaluators =
    Core.List.map testbenches ~f:(fun testbench ->
      Expert.Evaluator.create ~period:1 ~update_children_after_finish:false testbench)
  in
  let (side, step) : Side.t * Cyclesim.Private.Step.t =
    match when_to_evaluate_testbenches with
    | `Before_cycle -> Before, Before_clock_edge
    | `After_cycle -> After, After_clock_edge
  in
  Cyclesim.Private.modify
    simulator
    [ ( side
      , step
      , fun () ->
          Core.List.iter evaluators ~f:(fun evaluator ->
            if not (Expert.Evaluator.is_finished evaluator)
            then
              ignore
                (Expert.Evaluator.step ~show_steps evaluator
                 : _ Expert.Evaluator.Result.t)) )
    ]
;;

let wrap_never_returns = wrap
