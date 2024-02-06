open! Core
open Hardcaml
open! Digital_components
module Monads = Imperative_cyclesim_intf.Monads

module type S = Imperative_cyclesim_intf.S with module Step_monad = Monads.Step_monad

include Imperative.Make (Monads)

let next_input timeout simulator result_event _ =
  let timedout =
    let count = ref 0 in
    fun () ->
      Int.incr count;
      match timeout with
      | None -> false
      | Some timeout -> !count >= timeout
  in
  Cyclesim.cycle simulator;
  match Step_monad.Event.value result_event with
  | None -> if timedout () then Step_monad.Component.Next_input.Finished else Input ()
  | Some _ -> Finished
;;

let run_with_timeout
  ?(update_children_after_finish = false)
  ?timeout
  ()
  ~simulator
  ~testbench
  =
  let component, result_event =
    Step_monad.create_component
      ~update_children_after_finish
      ~created_at:[%here]
      ~start:(start testbench)
      ~input:(module No_data)
      ~output:(module No_data)
  in
  Step_monad.Component.run_until_finished
    component
    ~first_input:()
    ~next_input:(next_input timeout simulator result_event);
  match Step_monad.Event.value result_event with
  | None -> None
  | Some x -> Some x.result
;;

let run_until_finished ?update_children_after_finish () ~simulator ~testbench =
  match run_with_timeout ?update_children_after_finish () ~simulator ~testbench with
  | Some result -> result
  | None -> raise_s [%message "Step testbench did not complete with a result."]
;;
