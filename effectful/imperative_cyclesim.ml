open! Core
open Hardcaml
open! Digital_components
module Step_modules = Imperative_cyclesim_intf.Step_modules
include Imperative.Make (Step_modules)
module Step_effect = Step_modules.Step_effect

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
      if timedout ()
      then Step_effect.Component.Next_input.Finished
      else Input O_data.undefined
    | Some _ -> Finished)
;;

let create_component ~update_children_after_finish testbench =
  Step_effect.create_component
    ~update_children_after_finish
    ~created_at:[%here]
    ~start:(fun testbench_arg handler -> start handler testbench testbench_arg)
    ~input:(module O_data)
    ~output:(module I_data)
;;

let run_with_timeout
  ?(update_children_after_finish = false)
  ?timeout
  ()
  ~simulator
  ~testbench
  =
  let component, result_event =
    create_component ~update_children_after_finish (fun handler (_ : O_data.t) ->
      testbench handler)
  in
  Step_effect.Component.run_until_finished
    component
    ~first_input:O_data.undefined
    ~next_input:(Staged.unstage (next_input timeout simulator result_event));
  match Step_effect.Event.value result_event with
  | None -> None
  | Some x -> Some x.result
;;

let run_with_timeout' ?update_children_after_finish ?timeout () ~simulator ~testbench =
  run_with_timeout
    ?update_children_after_finish
    ?timeout
    ()
    ~simulator
    ~testbench:(fun handler -> testbench handler simulator)
;;

let run_until_finished ?update_children_after_finish () ~simulator ~testbench =
  match run_with_timeout ?update_children_after_finish () ~simulator ~testbench with
  | Some result -> result
  | None -> raise_s [%message "Step testbench did not complete with a result."]
;;

type wrapped_testbench =
  | Wrapped_testbench :
      { step_function : unit -> unit
      ; result_event : (_, _) finished_event
      }
      -> wrapped_testbench

let wrap ?(show_steps = false) ~when_to_evaluate_testbenches ~testbenches simulator =
  let wrapped_testbenches =
    Core.List.map testbenches ~f:(fun testbench ->
      let component, result_event =
        create_component
          ~update_children_after_finish:false
          (fun handler (_ : O_data.t) -> testbench handler)
      in
      let step_function =
        Staged.unstage (Step_effect.Component.create_step_function ~show_steps component)
      in
      let step_function () = step_function O_data.undefined in
      ref (Some (Wrapped_testbench { step_function; result_event })))
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
          Core.List.iter wrapped_testbenches ~f:(fun wrapped_tb ->
            match !wrapped_tb with
            | None -> ()
            | Some (Wrapped_testbench { step_function; result_event }) ->
              step_function ();
              (match Step_effect.Event.value result_event with
               | Some _ ->
                 (* The testbench has completed, so we wet this ref to None. *)
                 wrapped_tb := None
               | None -> ())) )
    ]
;;

let wrap_never_returns = wrap
