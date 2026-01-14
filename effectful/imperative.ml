open! Core
open! Hardcaml
open! Digital_components
open! Hardcaml_step_testbench_kernel

module type S = Imperative_intf.S

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
  type t = (O_data.t, I_data.t) Step_effect.Handler.t
end

let rec cycle (handler : Handler.t @ local) ?(num_cycles = 1) () =
  if num_cycles < 0
  then raise_s [%message "cycle must take 0 or more num_cycles" (num_cycles : int)]
  else if num_cycles = 0
  then ()
  else (
    let (_ : O_data.t) = Step_effect.next_step [%here] () handler in
    cycle handler ~num_cycles:(num_cycles - 1) ())
;;

let start (handler : Handler.t @ local) testbench output =
  let result = testbench handler output in
  { Step_effect.Component_finished.output = (); result }
;;

type ('a, 'i) finished_event =
  ('a, 'i) Step_effect.Component_finished.t Step_effect.Event.t

let spawn
  (type a)
  ?period
  (handler : Handler.t @ local)
  (task : Handler.t @ local -> unit -> a)
  : (a, unit) finished_event
  =
  Step_effect.spawn
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
  let x = Step_effect.wait_for event ~output:() handler in
  x.result
;;

let rec wait_for_with_timeout
  (handler : Handler.t @ local)
  (event : _ finished_event)
  ~timeout_in_cycles
  =
  if timeout_in_cycles < 0
  then raise_s [%message "timeout_in_cycles < 0" (timeout_in_cycles : int)];
  match Step_effect.Event.value event with
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
  (computation : (a, O_data.t, I_data.t) Step_monad.t)
  : a
  =
  Step_effect.run_monadic_computation h computation
;;

let create_component
  ~(created_at : [%call_pos])
  ~period
  ~update_children_after_finish
  testbench
  =
  Step_effect.create_component
    ~period
    ~update_children_after_finish
    ~created_at
    ~start:(fun testbench_arg handler ->
      start handler (fun h (_ : O_data.t) -> testbench h) testbench_arg)
    ~input:(module O_data)
    ~output:(module I_data)
    ()
;;

module Evaluator = struct
  module Result = struct
    type 'a t =
      | Finished of 'a
      | Running
  end

  type 'a t =
    { component : (unit Before_and_after_edge.t, unit) Component.t
    ; result_event : ('a, unit) Step_effect.Component_finished.t Step_effect.Event.t
    ; mutable step_number : int
    }

  let create
    (type a)
    ~period
    ~update_children_after_finish
    (testbench : Handler.t @ local -> a)
    =
    let component, result_event =
      create_component ~period ~created_at:[%here] ~update_children_after_finish testbench
    in
    { component; result_event; step_number = 0 }
  ;;

  let is_finished (t : _ t) = Option.is_some (Step_effect.Event.value t.result_event)

  let step ?(show_steps = false) (type a) (t : a t) : a Result.t =
    if show_steps then print_s [%message "" ~step_number:(t.step_number : int)];
    Component.update_state
      ~prune:(t.step_number % 1000 = 0)
      ~parent_period:1
      ~step_number:t.step_number
      t.component
      (Before_and_after_edge.const ());
    t.step_number <- t.step_number + 1;
    match Step_effect.Event.value t.result_event with
    | None -> Running
    | Some x -> Finished x.result
  ;;
end

module As_monad = struct
  type 'a t = Handler.t @ local -> 'a

  include Monad.Make (struct
      type nonrec 'a t = 'a t

      let return x (_ @ local) = x
      let bind (a : _ t) ~f : _ t = fun (h @ local) -> f (a h) h
      let map = `Define_using_bind
    end)

  let cycle ?num_cycles () (h @ local) = cycle ?num_cycles h ()
end

module Expert = struct
  let create_component = create_component

  module Evaluator = Evaluator
end
