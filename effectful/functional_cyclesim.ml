open! Core
open Hardcaml
open! Digital_components
module M = Functional_cyclesim_intf.M
module Step_modules = Functional_cyclesim_intf.Step_modules

module Make (I : Interface.S) (O : Interface.S) = struct
  module Step_effect = Step_modules.Step_effect
  include Functional.Make (Step_modules) (I) (O)

  module Simulator = struct
    type t = (Cyclesim.With_interface(I)(O).t[@sexp.opaque]) [@@deriving sexp_of]
  end

  let simulator_output simulator =
    let output clock_edge = O.map (Cyclesim.outputs ~clock_edge simulator) ~f:( ! ) in
    Before_and_after_edge.create ~before_edge:(output Before) ~after_edge:(output After)
  ;;

  let next_input timeout simulator result_event input_default =
    let timedout =
      let count = ref 0 in
      fun () ->
        Int.incr count;
        match timeout with
        | None -> false
        | Some timeout -> !count >= timeout
    in
    let inport_and_default =
      I.map2 (Cyclesim.inputs simulator) input_default ~f:(fun i d -> i, d)
    in
    fun i ->
      I.iter2 inport_and_default i ~f:(fun (i, d) n ->
        if not (Bits.is_empty n)
        then i := n (* some task has specified a value *)
        else if not (Bits.is_empty d)
        then i := d (* use default value *)
        else (* hold previous value *)
          ());
      Cyclesim.cycle simulator;
      match Step_effect.Event.value result_event with
      | None ->
        if timedout ()
        then Step_effect.Component.Next_input.Finished
        else Input (simulator_output simulator)
      | Some _ -> Finished
  ;;

  let run_with_timeout
    ?(input_default = input_hold)
    ?(update_children_after_finish = false)
    ?show_steps
    ?timeout
    ()
    ~(simulator : Simulator.t)
    ~testbench
    =
    let component, result_event =
      Step_effect.create_component
        ~update_children_after_finish
        ~created_at:[%here]
        ~start:(fun output handler -> start handler testbench output)
        ~input:(module O_data)
        ~output:(module I_data)
    in
    Step_effect.Component.run_until_finished
      component
      ?show_steps
      ~first_input:(simulator_output simulator)
      ~next_input:(next_input timeout simulator result_event input_default);
    match Step_effect.Event.value result_event with
    | None -> None
    | Some x -> Some x.result
  ;;

  let run_until_finished
    ?input_default
    ?show_steps
    ?update_children_after_finish
    ()
    ~simulator
    ~testbench
    =
    match
      run_with_timeout
        ?input_default
        ?show_steps
        ?update_children_after_finish
        ()
        ~simulator
        ~testbench
    with
    | Some result -> result
    | None -> raise_s [%message "Step testbench did not complete with a result."]
  ;;
end
