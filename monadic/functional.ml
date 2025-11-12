open Core
open Hardcaml
open Hardcaml_step_testbench_kernel

module type S = Functional_intf.S

module M = Functional_intf.M

module Make (Step_modules : Step_modules.S) (I : Interface.S) (O : Interface.S) = struct
  open Step_modules
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

  type 'a t = ('a, O_data.t, I_data.t) Step_monad.t

  include Monad.Make (struct
      type nonrec 'a t = 'a t

      let return x = Step_monad.return x
      let map = `Custom Step_monad.map
      let bind = Step_monad.bind
    end)

  open Let_syntax

  let rec cycle ?(num_cycles = 1) (i : I_data.t) =
    if num_cycles < 1
    then raise_s [%message "cycle must take 1 or more num_cycles" (num_cycles : int)]
    else if num_cycles = 1
    then Step_monad.next_step [%here] i
    else Step_monad.next_step [%here] i >>= fun _ -> cycle ~num_cycles:(num_cycles - 1) i
  ;;

  let for_ lo hi f = Step_monad.for_ lo hi f
  let delay i ~num_cycles = Step_monad.delay i ~num_steps:num_cycles

  let merge_inputs ~parent ~child =
    I.map2 parent child ~f:(fun p c -> if Bits.is_empty c then p else c)
  ;;

  type ('a, 'b) finished_event =
    ('a, 'b) Step_monad.Component_finished.t Step_monad.Event.t

  let start testbench output =
    let%bind result = testbench output in
    return { Step_monad.Component_finished.output = I_data.undefined; result }
  ;;

  let spawn_io_different_outputs ?update_children_after_finish ~inputs ~outputs task =
    Step_monad.spawn
      ?update_children_after_finish
      [%here]
      ~input:(module O_data)
      ~output:(module I_data)
      ~child_input:(fun ~parent ->
        Before_and_after_edge.map2 ~f:(fun f x -> f x) outputs parent)
      ~include_child_output:inputs
      ~start:(start task)
  ;;

  let spawn_io ?update_children_after_finish ~inputs ~outputs task =
    let outputs = Before_and_after_edge.const outputs in
    spawn_io_different_outputs ?update_children_after_finish ~inputs ~outputs task
  ;;

  let spawn ?update_children_after_finish task =
    spawn_io ?update_children_after_finish task ~outputs:Fn.id ~inputs:merge_inputs
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
    (io_ports : _ Io_ports_for_imperative.t)
    task
    =
    let%tydi { inputs = inputs_ref; outputs = outputs_ref } = io_ports in
    let inputs ~parent:() ~child:src =
      I.iter2 inputs_ref src ~f:(fun dst src ->
        if not (Bits.is_empty src) then dst := src)
    in
    let outputs =
      Before_and_after_edge.map outputs_ref ~f:(fun dst () -> O.map ~f:( ! ) dst)
    in
    spawn_io_different_outputs ?update_children_after_finish ~inputs ~outputs task
  ;;

  let wait_for (event : _ finished_event) =
    let%bind x = Step_monad.wait_for event ~output:I_data.undefined in
    return x.result
  ;;

  let exec_never_returns_from_imperative
    ?update_children_after_finish
    io_ports
    (task : O_data.t -> never_returns t)
    : (never_returns, unit Before_and_after_edge.t, unit) Step_monad.t
    =
    let%bind.Step_monad ev_never_returns =
      spawn_from_imperative ?update_children_after_finish io_ports task
    in
    let%bind.Step_monad _ = Step_monad.wait_for ev_never_returns ~output:() in
    raise_s
      [%message
        "[exec_never_returns_from_imperative] should never return. This is a bug!"]
  ;;

  let input_hold = I.const Bits.empty
  let input_zero = I.map I.port_widths ~f:(fun b -> Bits.zero b)

  let rec wait_for_with_timeout (event : _ finished_event) ~timeout_in_cycles =
    if timeout_in_cycles < 0
    then raise_s [%message "timeout_in_cycles < 0" (timeout_in_cycles : int)];
    match Step_monad.Event.value event with
    | Some x -> return (Some x.result)
    | None ->
      if timeout_in_cycles = 0
      then return None
      else (
        let%bind _ = cycle input_hold in
        wait_for_with_timeout event ~timeout_in_cycles:(timeout_in_cycles - 1))
  ;;

  let forever f : never_returns t =
    let rec loop () = f () >>= loop in
    loop ()
  ;;

  let forever_unit f =
    let%bind _ = forever f in
    return ()
  ;;

  let never : never_returns t =
    forever (fun () ->
      let%bind _ = cycle input_hold in
      return ())
  ;;

  module List = struct
    let init len ~f =
      let rec init i =
        if i = len
        then return []
        else (
          let%bind elt = f i in
          let%bind rst = init (i + 1) in
          return (elt :: rst))
      in
      init 0
    ;;

    let rec iter t ~f =
      match t with
      | [] -> return ()
      | hd :: tl ->
        let%bind () = f hd in
        iter tl ~f
    ;;

    let iter2_exn a b ~f = iter (List.zip_exn a b) ~f:(fun (a, b) -> f a b)

    let iteri t ~f =
      let rec iteri i t ~f =
        match t with
        | [] -> return ()
        | hd :: tl ->
          let%bind () = f i hd in
          iteri (i + 1) tl ~f
      in
      iteri 0 t ~f
    ;;

    let rec map t ~f =
      match t with
      | [] -> return []
      | hd :: tl ->
        let%bind hd = f hd in
        let%bind tl = map tl ~f in
        return (hd :: tl)
    ;;

    let mapi t ~f =
      let rec mapi i t ~f =
        match t with
        | [] -> return []
        | hd :: tl ->
          let%bind hd = f i hd in
          let%bind tl = mapi (i + 1) tl ~f in
          return (hd :: tl)
      in
      mapi 0 t ~f
    ;;
  end

  module Array = struct
    let init len ~f =
      let%bind l = List.init len ~f in
      return (Array.of_list l)
    ;;

    let iter t ~f = List.iter (Array.to_list t) ~f
    let iteri t ~f = List.iteri (Array.to_list t) ~f

    let map t ~f =
      let%bind l = List.map (Array.to_list t) ~f in
      return (Array.of_list l)
    ;;
  end
end
