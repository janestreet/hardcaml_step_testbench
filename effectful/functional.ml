open Core
open Hardcaml
open Hardcaml_step_testbench_kernel

module type S = Functional_intf.S

module M = Functional_intf.M

module Make (I : Interface.S) (O : Interface.S) = struct
  module I = I
  module O = O
  module Io_ports_for_imperative = Hardcaml_step_testbench_kernel.Io_ports_for_imperative
  module Before_and_after_edge = Hardcaml_step_testbench_kernel.Before_and_after_edge

  module Interface_as_data (I : Interface.S) :
    Digital_components.Data.S with type t = Bits.t I.t = struct
    type t = Bits.t I.t [@@deriving sexp_of]

    let equal a b =
      With_return.with_return (fun r ->
        I.iter2 a b ~f:(fun a b -> if not (Bits.equal a b) then r.return false);
        true)
    ;;

    let undefined = I.const Bits.empty
  end

  module I_data = Interface_as_data (I)

  module O_data = struct
    module O_data = Interface_as_data (O)

    type t = O_data.t Before_and_after_edge.t [@@deriving sexp_of]

    let equal (a : _ Before_and_after_edge.t) (b : _ Before_and_after_edge.t) =
      let open Before_and_after_edge in
      O_data.equal (before_edge a) (before_edge b)
      && O_data.equal (after_edge a) (after_edge b)
    ;;

    let undefined =
      Before_and_after_edge.create
        ~before_edge:O_data.undefined
        ~after_edge:O_data.undefined
    ;;

    let before_edge t = Before_and_after_edge.before_edge t
    let after_edge t = Before_and_after_edge.after_edge t
  end

  module Handler = struct
    type t = (O_data.t, I_data.t) Step_effect.Handler.t
  end

  let rec cycle (handler : Handler.t) ?(num_cycles = 1) (i : I_data.t) =
    if num_cycles < 1
    then raise_s [%message "cycle must take 1 or more num_cycles" (num_cycles : int)]
    else if num_cycles = 1
    then Step_effect.next_step [%here] i handler
    else (
      Step_effect.delay i ~num_steps:1 handler;
      cycle handler ~num_cycles:(num_cycles - 1) i)
  ;;

  let delay ?(num_cycles = 1) (handler : Handler.t) i =
    Step_effect.delay i ~num_steps:num_cycles handler
  ;;

  let merge_inputs ~parent ~child =
    I.map2 parent child ~f:(fun p c -> if Bits.is_empty c then p else c)
  ;;

  type ('a, 'b) finished_event =
    ('a, 'b) Step_effect.Component_finished.t Step_effect.Event.t

  let start (handler : Handler.t) testbench output =
    let result = testbench handler output in
    { Step_effect.Component_finished.output = I_data.undefined; result }
  ;;

  let spawn_io_different_outputs
    ?update_children_after_finish
    ?period
    ~inputs
    ~outputs
    task
    (parent_handler : ('o_p Before_and_after_edge.t, 'i_p) Step_effect.Handler.t)
    =
    Step_effect.spawn
      ?update_children_after_finish
      ?period
      [%here]
      ~start:(fun output handler -> start handler task output)
      ~input:(module O_data)
      ~output:(module I_data)
      ~child_input:(fun ~parent ->
        Before_and_after_edge.map2 ~f:(fun f x -> f x) outputs parent)
      ~include_child_output:inputs
      parent_handler
  ;;

  let spawn_io ?update_children_after_finish ?period ~inputs ~outputs parent_handler task =
    let outputs = Before_and_after_edge.const outputs in
    spawn_io_different_outputs
      ?update_children_after_finish
      ?period
      ~inputs
      ~outputs
      task
      parent_handler
  ;;

  let spawn_io' ?update_children_after_finish ?period ~inputs ~outputs parent_handler task
    =
    ignore
      (spawn_io ?update_children_after_finish ?period ~inputs ~outputs parent_handler task
       : _ Step_effect.Event.t)
  ;;

  let spawn ?update_children_after_finish ?period (handler : Handler.t) task =
    spawn_io
      ?update_children_after_finish
      ?period
      handler
      task
      ~outputs:Fn.id
      ~inputs:merge_inputs
  ;;

  let spawn' ?update_children_after_finish ?period (handler : Handler.t) task =
    ignore
      (spawn ?update_children_after_finish ?period (handler : Handler.t) task
       : _ Step_effect.Event.t)
  ;;

  let create_io_ports_for_imperative simulator ~inputs ~outputs =
    let inputs = inputs (Cyclesim.inputs simulator) in
    let outputs =
      { Before_and_after_edge.before_edge =
          outputs (Cyclesim.outputs ~clock_edge:Before simulator)
      ; after_edge = outputs (Cyclesim.outputs ~clock_edge:After simulator)
      }
    in
    { Io_ports_for_imperative.inputs; outputs }
  ;;

  let spawn_from_imperative
    ?update_children_after_finish
    ?period
    (io_ports : _ Io_ports_for_imperative.t)
    (parent_handler : (unit Before_and_after_edge.t, unit) Step_effect.Handler.t)
    task
    =
    let { Io_ports_for_imperative.inputs = inputs_ref; outputs = outputs_ref } =
      io_ports
    in
    let inputs ~parent:() ~child:src =
      I.iter2 inputs_ref src ~f:(fun dst src ->
        if not (Bits.is_empty src) then dst := src)
    in
    let outputs =
      Before_and_after_edge.map outputs_ref ~f:(fun dst () -> O.map ~f:( ! ) dst)
    in
    spawn_io_different_outputs
      ?update_children_after_finish
      ?period
      ~inputs
      ~outputs
      task
      parent_handler
  ;;

  let wait_for (handler : Handler.t) (event : _ finished_event) =
    let x = Step_effect.wait_for event ~output:I_data.undefined handler in
    x.result
  ;;

  let exec_never_returns_from_imperative
    ?update_children_after_finish
    ?period
    io_ports
    parent_handler
    (task : Handler.t -> O_data.t -> never_returns)
    : never_returns
    =
    let ev_never_returns =
      spawn_from_imperative
        ?update_children_after_finish
        ?period
        io_ports
        parent_handler
        task
    in
    let finished = Step_effect.wait_for ev_never_returns ~output:() parent_handler in
    finished.result
  ;;

  let input_hold = I.const Bits.empty
  let input_zero = I.map I.port_widths ~f:(fun b -> Bits.zero b)

  let rec wait_for_with_timeout
    (handler : Handler.t)
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
        delay handler input_hold ~num_cycles:1;
        wait_for_with_timeout handler event ~timeout_in_cycles:(timeout_in_cycles - 1))
  ;;

  let forever (handler : Handler.t) f : never_returns =
    while true do
      f handler ()
    done;
    assert false
  ;;

  let forever_unit (handler : Handler.t) f = ignore (forever handler f : never_returns)

  let never (handler : Handler.t) : never_returns =
    forever handler (fun handler () ->
      delay handler input_hold ~num_cycles:1;
      ())
  ;;

  let run_monadic_computation
    (type a)
    h
    (computation : (a, O_data.t, I_data.t) Step_monad.t)
    : a
    =
    Step_effect.run_monadic_computation h computation
  ;;

  module As_monad = struct
    type 'a t = Handler.t -> 'a

    include Monad.Make (struct
        type nonrec 'a t = 'a t

        let return x _ = x
        let bind (a : _ t) ~f : _ t = fun h -> f (a h) h
        let map = `Define_using_bind
      end)
  end
end
