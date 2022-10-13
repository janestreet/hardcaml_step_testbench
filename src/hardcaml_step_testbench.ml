open! Import
module Before_and_after_edge = Before_and_after_edge

module type S = Hardcaml_step_testbench_intf.S

module Make (I : Interface.S) (O : Interface.S) = struct
  module Simulator = struct
    type t = (Cyclesim.With_interface(I)(O).t[@sexp.opaque]) [@@deriving sexp_of]
  end

  module Interface_as_data (I : Interface.S) :
    Digital_components.Data.S with type t = Bits.t I.t = struct
    type t = Bits.t I.t [@@deriving sexp_of]

    let equal a b =
      With_return.with_return (fun r ->
        I.iter2 a b ~f:(fun a b -> if not (Bits.equal a b) then r.return false);
        true)
    ;;

    let undefined = I.map I.t ~f:(fun _ -> Bits.empty)
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

  let spawn_io ~inputs ~outputs task =
    Step_monad.spawn
      [%here]
      ~input:(module O_data)
      ~output:(module I_data)
      ~child_input:(fun ~parent -> Before_and_after_edge.map ~f:outputs parent)
      ~include_child_output:inputs
      ~start:(start task)
  ;;

  let spawn task = spawn_io task ~outputs:Fn.id ~inputs:merge_inputs

  let wait_for (event : _ finished_event) =
    let%bind x = Step_monad.wait_for event ~output:I_data.undefined in
    return x.result
  ;;

  let input_hold = I.map I.t ~f:(fun _ -> Bits.empty)
  let input_zero = I.map I.t ~f:(fun (_, b) -> Bits.zero b)

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
        else ());
      (* hold previous value *)
      Cyclesim.cycle simulator;
      match Step_monad.Event.value result_event with
      | None ->
        if timedout ()
        then Component.Next_input.Finished
        else Input (simulator_output simulator)
      | Some _ -> Finished
  ;;

  let run_with_timeout
        ?(input_default = input_hold)
        ?show_steps
        ?timeout
        ()
        ~(simulator : Simulator.t)
        ~testbench
    =
    let component, result_event =
      Step_monad.create_component
        ~update_children_after_finish:false
        ~created_at:[%here]
        ~start:(start testbench)
        ~input:(module O_data)
        ~output:(module I_data)
    in
    Component.run_until_finished
      component
      ?show_steps
      ~first_input:(simulator_output simulator)
      ~next_input:(next_input timeout simulator result_event input_default);
    match Step_monad.Event.value result_event with
    | None -> None
    | Some x -> Some x.result
  ;;

  let run_until_finished ?input_default ?show_steps () ~simulator ~testbench =
    match run_with_timeout ?input_default ?show_steps () ~simulator ~testbench with
    | Some result -> result
    | None -> raise_s [%message "Step testbench did not complete with a result."]
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
