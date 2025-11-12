open! Core
open! Hardcaml
open Hardcaml_step_testbench_kernel

module type S = Imperative_intf.S

module M = Imperative_intf.M

module Make (Step_modules : Step_modules.S) = struct
  open Step_modules

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

  type 'a t = ('a, O_data.t, I_data.t) Step_monad.t

  include Monad.Make (struct
      type nonrec 'a t = 'a t

      let return x = Step_monad.return x
      let map = `Custom Step_monad.map
      let bind = Step_monad.bind
    end)

  open Let_syntax

  let rec cycle ?(num_cycles = 1) () =
    if num_cycles < 1
    then raise_s [%message "cycle must take 1 or more num_cycles" (num_cycles : int)]
    else if num_cycles = 1
    then Step_monad.next_step [%here] () >>= fun _ -> return ()
    else
      Step_monad.next_step [%here] () >>= fun _ -> cycle ~num_cycles:(num_cycles - 1) ()
  ;;

  let for_ lo hi f = Step_monad.for_ lo hi f

  let start testbench output =
    let%bind result = testbench output in
    return { Step_monad.Component_finished.output = (); result }
  ;;

  type ('a, 'i) finished_event =
    ('a, 'i) Step_monad.Component_finished.t Step_monad.Event.t

  let spawn (type a) (task : unit -> a t) : (a, unit) finished_event t =
    Step_monad.spawn
      [%here]
      ~input:(module O_data)
      ~output:(module I_data)
      ~child_input:(fun ~parent:_ ->
        Hardcaml_step_testbench_kernel.Before_and_after_edge.const ())
      ~include_child_output:(fun ~parent:_ ~child:_ -> ())
      ~start:(start (fun _ -> task ()))
  ;;

  let wait_for (event : _ finished_event) =
    let%bind x = Step_monad.wait_for event ~output:() in
    return x.result
  ;;

  let rec wait_for_with_timeout (event : _ finished_event) ~timeout_in_cycles =
    if timeout_in_cycles < 0
    then raise_s [%message "timeout_in_cycles < 0" (timeout_in_cycles : int)];
    match Step_monad.Event.value event with
    | Some x -> return (Some x.result)
    | None ->
      if timeout_in_cycles = 0
      then return None
      else (
        let%bind _ = cycle () in
        wait_for_with_timeout event ~timeout_in_cycles:(timeout_in_cycles - 1))
  ;;

  let forever f : never_returns t =
    let rec loop () =
      let%bind () = f () in
      loop ()
    in
    loop ()
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
