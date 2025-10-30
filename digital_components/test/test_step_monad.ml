open Base
open Expect_test_helpers_core
module Data = Digital_components.Data

include struct
  module Component = Digital_components.Component.Make (Monad.Ident)
  module Step_core = Digital_components.Step_core.Make (Monad.Ident) (Component)

  module Step_monad =
    Digital_components.Step_monad.Make (Monad.Ident) (Component) (Step_core)
end

open Step_monad
open! Step_monad.Let_syntax

let run_with_inputs (t : _ Component.t) inputs =
  let sexp_of_input = Component.sexp_of_input t in
  let sexp_of_output = Component.sexp_of_output t in
  let sexp_of_io (input, output) = [%message (input : input) (output : output)] in
  print_s [%sexp (Component.run_with_inputs t inputs : io list)]
;;

let create_component ?(update_children_after_finish = false) created_at start =
  create_component
    ~update_children_after_finish
    ~created_at
    ~start:(fun i ->
      let%bind () = start i in
      return { Component_finished.output = (); result = () })
    ~input:(module Data.Unit)
    ~output:(module Data.Unit)
;;

let test start =
  let component, component_finished = create_component [%here] start in
  Component.run_until_finished
    component
    ~show_steps:true
    ~first_input:()
    ~next_input:(fun () ->
      if Option.is_some (Event.value component_finished) then Finished else Input ());
  print_s [%sexp (component : (_, _) Component.t)] ~hide_positions:true
;;

let spawn start =
  let%bind child_finished =
    spawn
      [%here]
      ~start:(fun () ->
        let%bind result = start () in
        return { Component_finished.output = (); result })
      ~input:(module Data.Unit)
      ~output:(module Data.Unit)
      ~child_input:(fun ~parent:_ -> ())
      ~include_child_output:(fun ~parent:_ ~child:_ -> ())
  in
  return child_finished
;;

let wait_for event = wait_for event ~output:()

let%expect_test "[return]" =
  test (fun () -> return ());
  [%expect
    {|
    (step_number 0)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children ())
      (output   ())))
    |}]
;;

let%expect_test "[bind]" =
  test (fun () ->
    let%bind () = return () in
    return ());
  [%expect
    {|
    (step_number 0)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children ())
      (output   ())))
    |}]
;;

let%expect_test "[next_step]" =
  test (fun () -> next_step [%here] ());
  [%expect
    {|
    (step_number 0)
    (step_number 1)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children ())
      (output   ())))
    |}]
;;

let%expect_test "[spawn] + [wait]" =
  test (fun () ->
    let%bind child_finished = spawn (fun () -> return ()) in
    let%bind _ = wait_for child_finished in
    return ());
  [%expect
    {|
    (step_number 0)
    (step_number 1)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children ((
        lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
        ((state (Finished ()))
         (children ())
         (output   ())))))
      (output ())))
    |}]
;;

let%expect_test "[spawn] + [wait] with child taking a step" =
  test (fun () ->
    let%bind child_finished = spawn (fun () -> next_step [%here] ()) in
    let%bind _ = wait_for child_finished in
    return ());
  [%expect
    {|
    (step_number 0)
    (step_number 1)
    (step_number 2)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children ((
        lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
        ((state (Finished ()))
         (children ())
         (output   ())))))
      (output ())))
    |}]
;;

let%expect_test "[for_]" =
  let test lo hi =
    test (fun () ->
      for_ lo hi (fun i ->
        print_s [%message (i : int)];
        next_step [%here] ()))
  in
  test 0 (-1);
  [%expect
    {|
    (step_number 0)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children ())
      (output   ())))
    |}];
  test 0 0;
  [%expect
    {|
    (step_number 0)
    (i 0)
    (step_number 1)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children ())
      (output   ())))
    |}];
  test 0 1;
  [%expect
    {|
    (step_number 0)
    (i 0)
    (step_number 1)
    (i 1)
    (step_number 2)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children ())
      (output   ())))
    |}];
  test 0 2;
  [%expect
    {|
    (step_number 0)
    (i 0)
    (step_number 1)
    (i 1)
    (step_number 2)
    (i 2)
    (step_number 3)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children ())
      (output   ())))
    |}]
;;

let%expect_test "parallel components" =
  for num_tasks = 1 to 4 do
    print_s [%message (num_tasks : int)];
    test (fun () ->
      let%bind children =
        all
          (List.init num_tasks ~f:(fun task_index ->
             spawn (fun () ->
               for_ 0 2 (fun step_index ->
                 Stdio.printf "%d %d\n" step_index task_index;
                 next_step [%here] ()))))
      in
      let%bind _ = all (List.map children ~f:wait_for) in
      return ())
  done;
  [%expect
    {|
    (num_tasks 1)
    (step_number 0)
    0 0
    (step_number 1)
    1 0
    (step_number 2)
    2 0
    (step_number 3)
    (step_number 4)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children ((
        lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
        ((state (Finished ()))
         (children ())
         (output   ())))))
      (output ())))
    (num_tasks 2)
    (step_number 0)
    0 1
    0 0
    (step_number 1)
    1 1
    1 0
    (step_number 2)
    2 1
    2 0
    (step_number 3)
    (step_number 4)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children (
        (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
         ((state (Finished ()))
          (children ())
          (output   ())))
        (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
         ((state (Finished ()))
          (children ())
          (output   ())))))
      (output ())))
    (num_tasks 3)
    (step_number 0)
    0 2
    0 1
    0 0
    (step_number 1)
    1 2
    1 1
    1 0
    (step_number 2)
    2 2
    2 1
    2 0
    (step_number 3)
    (step_number 4)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children (
        (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
         ((state (Finished ()))
          (children ())
          (output   ())))
        (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
         ((state (Finished ()))
          (children ())
          (output   ())))
        (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
         ((state (Finished ()))
          (children ())
          (output   ())))))
      (output ())))
    (num_tasks 4)
    (step_number 0)
    0 3
    0 2
    0 1
    0 0
    (step_number 1)
    1 3
    1 2
    1 1
    1 0
    (step_number 2)
    2 3
    2 2
    2 1
    2 0
    (step_number 3)
    (step_number 4)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children (
        (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
         ((state (Finished ()))
          (children ())
          (output   ())))
        (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
         ((state (Finished ()))
          (children ())
          (output   ())))
        (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
         ((state (Finished ()))
          (children ())
          (output   ())))
        (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
         ((state (Finished ()))
          (children ())
          (output   ())))))
      (output ())))
    |}]
;;

let%expect_test "[delay]" =
  test (fun () -> delay () ~num_steps:5);
  [%expect
    {|
    (step_number 0)
    (step_number 1)
    (step_number 2)
    (step_number 3)
    (step_number 4)
    (step_number 5)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children ())
      (output   ())))
    |}]
;;

let%expect_test "[repeat]" =
  for count = 0 to 3 do
    test (fun () -> repeat ~count (fun () -> next_step [%here] ()))
  done;
  [%expect
    {|
    (step_number 0)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children ())
      (output   ())))
    (step_number 0)
    (step_number 1)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children ())
      (output   ())))
    (step_number 0)
    (step_number 1)
    (step_number 2)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children ())
      (output   ())))
    (step_number 0)
    (step_number 1)
    (step_number 2)
    (step_number 3)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children ())
      (output   ())))
    |}]
;;

let%expect_test "[spawn] + [for_]" =
  test (fun () ->
    let%bind child_finished =
      spawn (fun () ->
        for_ 0 3 (fun i ->
          print_s [%message (i : int)];
          next_step [%here] ()))
    in
    let%bind _ = wait_for child_finished in
    return ());
  [%expect
    {|
    (step_number 0)
    (i 0)
    (step_number 1)
    (i 1)
    (step_number 2)
    (i 2)
    (step_number 3)
    (i 3)
    (step_number 4)
    (step_number 5)
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children ((
        lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
        ((state (Finished ()))
         (children ())
         (output   ())))))
      (output ())))
    |}]
;;

let%expect_test "output counter" =
  let component, _ =
    Step_monad.create_component
      ~update_children_after_finish:false
      ~created_at:[%here]
      ~input:(module Data.Unit)
      ~output:(module Data.Int)
      ~start:(fun () ->
        let rec loop i =
          let%bind () = next_step [%here] i in
          loop (i + 1)
        in
        loop 0)
  in
  run_with_inputs component (List.init 5 ~f:(fun _ -> ()));
  [%expect
    {|
    (((input ()) (output 0))
     ((input ()) (output 1))
     ((input ()) (output 2))
     ((input ()) (output 3))
     ((input ()) (output 4)))
    |}]
;;

let%expect_test "add1" =
  let component, _ =
    Step_monad.create_component
      ~update_children_after_finish:false
      ~created_at:[%here]
      ~input:(module Data.Int)
      ~output:(module Data.Int)
      ~start:
        (let rec loop i =
           let%bind i = next_step [%here] (i + 1) in
           loop i
         in
         loop)
  in
  run_with_inputs component (List.init 5 ~f:Fn.id);
  [%expect
    {|
    (((input 0) (output 1))
     ((input 1) (output 2))
     ((input 2) (output 3))
     ((input 3) (output 4))
     ((input 4) (output 5)))
    |}]
;;

let%expect_test "child returning a value" =
  test (fun () ->
    let%bind child_finished =
      Step_monad.spawn
        [%here]
        ~input:(module Data.Unit)
        ~output:(module Data.Int)
        ~child_input:(fun ~parent:_ -> ())
        ~include_child_output:(fun ~parent:_ ~child:_ -> ())
        ~start:(fun () ->
          let%bind () = delay 13 ~num_steps:3 in
          return { Component_finished.output = 17; result = "foo" })
    in
    let%bind child_finished = wait_for child_finished in
    print_s [%message (child_finished : (string, int) Component_finished.t)];
    return ());
  [%expect
    {|
    (step_number 0)
    (step_number 1)
    (step_number 2)
    (step_number 3)
    (step_number 4)
    (child_finished (
      (output 17)
      (result foo)))
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children ((
        lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
        ((state (Finished 17)) (children ()) (output 17)))))
      (output ())))
    |}]
;;

let%expect_test "parent runs before child" =
  test (fun () ->
    let%bind _ =
      spawn (fun () ->
        let rec loop () =
          print_s [%message "child"];
          let%bind () = next_step [%here] () in
          loop ()
        in
        loop ())
    in
    for_ 1 3 (fun _ ->
      print_s [%message "parent"];
      next_step [%here] ()));
  [%expect
    {|
    (step_number 0)
    parent
    child
    (step_number 1)
    parent
    child
    (step_number 2)
    parent
    child
    (step_number 3)
    child
    (lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
     ((state (Finished ()))
      (children ((
        lib/hardcaml/digital_components/test/test_step_monad.ml:LINE:COL
        ((state (
           Running (Monad_bind <fun> (Monad_bind <fun> (Monad_bind <fun> Empty)))))
         (children ())
         (output   ())))))
      (output ())))
    |}]
;;

let%expect_test "finished child doesn't contribute to output" =
  let component, component_finished =
    Step_monad.create_component
      ~update_children_after_finish:false
      ~created_at:[%here]
      ~input:(module Data.Unit)
      ~output:(module Data.String)
      ~start:(fun _ ->
        let%bind child_finished =
          Step_monad.spawn
            [%here]
            ~start:(fun () ->
              let%bind () = next_step [%here] "child" in
              return { Component_finished.output = "child_finished"; result = () })
            ~input:(module Data.Unit)
            ~output:(module Data.String)
            ~child_input:(fun ~parent:() -> ())
            ~include_child_output:(fun ~parent ~child ->
              String.concat [ parent; " + "; child ])
        in
        let%bind () = next_step [%here] "before" in
        let%bind _ = Step_monad.wait_for child_finished ~output:"waiting" in
        let%bind () = Step_monad.delay ~num_steps:3 "delay" in
        return { Component_finished.output = "after"; result = () })
  in
  Component.run_until_finished
    component
    ~show_steps:true
    ~first_input:()
    ~next_input:(fun output ->
      print_s [%sexp (output : string)];
      if Option.is_some (Event.value component_finished) then Finished else Input ());
  [%expect
    {|
    (step_number 0)
    "before + child"
    (step_number 1)
    "waiting + child_finished"
    (step_number 2)
    delay
    (step_number 3)
    delay
    (step_number 4)
    delay
    (step_number 5)
    after
    |}]
;;

let%expect_test "grand-child does not run when child terminates" =
  let spawn here f =
    Step_monad.spawn
      here
      ~start:(fun () ->
        let%bind result = f () in
        return { Component_finished.output = (); result })
      ~input:(module Data.Unit)
      ~output:(module Data.Unit)
      ~child_input:(fun ~parent:() -> ())
      ~include_child_output:(fun ~parent:() ~child:() -> ())
  in
  let test ~update_children_after_finish ~number_of_cycles_in_parent =
    let component, _component_finished =
      Step_monad.create_component
        ~update_children_after_finish
        ~created_at:[%here]
        ~input:(module Data.Unit)
        ~output:(module Data.Unit)
        ~start:(fun () ->
          let%bind _child =
            spawn [%here] (fun () ->
              let%bind _grandchild =
                spawn [%here] (fun () ->
                  let rec loop () =
                    Stdio.printf "Printing from grandchild\n";
                    let%bind () = next_step [%here] () in
                    loop ()
                  in
                  loop ())
              in
              let%bind () = next_step [%here] () in
              return ())
          in
          let rec loop i =
            if i = number_of_cycles_in_parent
            then return ()
            else (
              let%bind () = next_step [%here] () in
              loop (i + 1))
          in
          let%bind () = loop 0 in
          return { Component_finished.output = (); result = () })
    in
    ignore
      (Component.run_with_inputs
         component
         (List.init number_of_cycles_in_parent ~f:(Fn.const ()))
       : (_ * _) list)
  in
  (* The tests below demonstrate that despite the grand-child running an infinite loop
     that should print something every cycle, only the first two cycles is printed, since
     the child terminates at after 1 clock cycle.
  *)
  test ~update_children_after_finish:false ~number_of_cycles_in_parent:1;
  [%expect {| Printing from grandchild |}];
  test ~update_children_after_finish:false ~number_of_cycles_in_parent:2;
  [%expect
    {|
    Printing from grandchild
    Printing from grandchild
    |}];
  test ~update_children_after_finish:false ~number_of_cycles_in_parent:3;
  [%expect
    {|
    Printing from grandchild
    Printing from grandchild
    |}];
  test ~update_children_after_finish:false ~number_of_cycles_in_parent:10;
  [%expect
    {|
    Printing from grandchild
    Printing from grandchild
    |}];
  (* The tests belows requires that the grandchild stays alive after the intermediate
     child terminates.
  *)
  test ~update_children_after_finish:true ~number_of_cycles_in_parent:1;
  [%expect {| Printing from grandchild |}];
  test ~update_children_after_finish:true ~number_of_cycles_in_parent:2;
  [%expect
    {|
    Printing from grandchild
    Printing from grandchild
    |}];
  test ~update_children_after_finish:true ~number_of_cycles_in_parent:3;
  [%expect
    {|
    Printing from grandchild
    Printing from grandchild
    Printing from grandchild
    |}];
  test ~update_children_after_finish:true ~number_of_cycles_in_parent:5;
  [%expect
    {|
    Printing from grandchild
    Printing from grandchild
    Printing from grandchild
    Printing from grandchild
    Printing from grandchild
    |}]
;;
