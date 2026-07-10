open Base
open Expect_test_helpers_core
module Data = Digital_components.Data

include struct
  open Digital_components
  module Component = Component
  module Step_effect = Step_effect
end

open Step_effect
include Component.Run_component_until_finished (Monad.Ident)

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
    ~start:(fun h i ->
      start h i;
      { Component_finished.output = (); result = () })
    ~input:(module Data.Unit)
    ~output:(module Data.Unit)
;;

let test start =
  let component, component_finished = create_component [%here] start () in
  run_component_until_finished
    component
    ~show_steps:true
    ~first_input:()
    ~next_input:(fun () ->
      if Option.is_some (Event.value component_finished) then Finished else Input ());
  print_s [%sexp (component : (_, _) Component.t)] ~hide_positions:true
;;

let spawn start h =
  let child_finished =
    spawn
      h
      [%here]
      ~start:(fun h () ->
        let result = start h () in
        { Component_finished.output = (); result })
      ~input:(module Data.Unit)
      ~output:(module Data.Unit)
      ~child_input:(fun ~parent:_ -> ())
      ~include_child_output:(fun ~parent:_ ~child:_ -> ())
  in
  child_finished
;;

let wait_for event h = wait_for h event ~output:()

let%expect_test "[no-op]" =
  test (fun _ () -> ());
  [%expect
    {|
    (step_number 0)
    (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
     ((state (Finished ()))
      (children ())
      (output   ())))
    |}]
;;

let%expect_test "[next_step]" =
  test (fun h () -> next_step h [%here] ());
  [%expect
    {|
    (step_number 0)
    (step_number 1)
    (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
     ((state (Finished ()))
      (children ())
      (output   ())))
    |}]
;;

let%expect_test "[spawn] + [wait]" =
  test (fun h () ->
    let child_finished = spawn (fun _ () -> ()) h in
    ignore (wait_for child_finished h : (unit, unit) Component_finished.t));
  [%expect
    {|
    (step_number 0)
    (step_number 1)
    (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
     ((state (Finished ()))
      (children ((
        lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
        ((state (Finished ()))
         (children ())
         (output   ())))))
      (output ())))
    |}]
;;

let%expect_test "[spawn] + [wait] with child taking a step" =
  test (fun h () ->
    let child_finished = spawn (fun h () -> next_step h [%here] ()) h in
    ignore (wait_for child_finished h : (unit, unit) Component_finished.t));
  [%expect
    {|
    (step_number 0)
    (step_number 1)
    (step_number 2)
    (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
     ((state (Finished ()))
      (children ((
        lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
        ((state (Finished ()))
         (children ())
         (output   ())))))
      (output ())))
    |}]
;;

let%expect_test "[for_]" =
  let test lo hi =
    test (fun h () ->
      for i = lo to hi do
        print_s [%message (i : int)];
        next_step h [%here] ()
      done)
  in
  test 0 (-1);
  [%expect
    {|
    (step_number 0)
    (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
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
    (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
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
    (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
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
    (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
     ((state (Finished ()))
      (children ())
      (output   ())))
    |}]
;;

let%expect_test "parallel components" =
  for num_tasks = 1 to 4 do
    print_s [%message (num_tasks : int)];
    test (fun (h @ local) () ->
      let children =
        List.map (List.range 0 num_tasks) ~f:(fun task_index ->
          spawn
            (fun h () ->
              for step_index = 0 to 2 do
                Stdio.printf "%d %d\n" step_index task_index;
                next_step h [%here] ()
              done)
            h)
      in
      List.iter children ~f:(fun ev ->
        ignore (wait_for ev h : (unit, unit) Component_finished.t))
      [@nontail])
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
    (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
     ((state (Finished ()))
      (children ((
        lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
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
    (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
     ((state (Finished ()))
      (children (
        (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
         ((state (Finished ()))
          (children ())
          (output   ())))
        (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
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
    (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
     ((state (Finished ()))
      (children (
        (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
         ((state (Finished ()))
          (children ())
          (output   ())))
        (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
         ((state (Finished ()))
          (children ())
          (output   ())))
        (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
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
    (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
     ((state (Finished ()))
      (children (
        (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
         ((state (Finished ()))
          (children ())
          (output   ())))
        (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
         ((state (Finished ()))
          (children ())
          (output   ())))
        (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
         ((state (Finished ()))
          (children ())
          (output   ())))
        (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
         ((state (Finished ()))
          (children ())
          (output   ())))))
      (output ())))
    |}]
;;

let%expect_test "[delay]" =
  test (fun h () -> delay h () ~num_steps:5);
  [%expect
    {|
    (step_number 0)
    (step_number 1)
    (step_number 2)
    (step_number 3)
    (step_number 4)
    (step_number 5)
    (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
     ((state (Finished ()))
      (children ())
      (output   ())))
    |}]
;;

let%expect_test "[spawn] + [for_]" =
  test (fun h () ->
    let child_finished =
      spawn
        (fun h () ->
          for i = 0 to 3 do
            print_s [%message (i : int)];
            next_step h [%here] ()
          done)
        h
    in
    ignore (wait_for child_finished h : (unit, unit) Component_finished.t));
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
    (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
     ((state (Finished ()))
      (children ((
        lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
        ((state (Finished ()))
         (children ())
         (output   ())))))
      (output ())))
    |}]
;;

let%expect_test "output counter" =
  let component, _ =
    Step_effect.create_component
      ~update_children_after_finish:false
      ~created_at:[%here]
      ~input:(module Data.Unit)
      ~output:(module Data.Int)
      ~start:(fun h () ->
        let rec loop i =
          next_step h [%here] i;
          loop (i + 1)
        in
        loop 0 [@nontail])
      ()
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
    Step_effect.create_component
      ~update_children_after_finish:false
      ~created_at:[%here]
      ~input:(module Data.Int)
      ~output:(module Data.Int)
      ~start:(fun h (i : int) ->
        let rec loop i =
          let i = next_step h [%here] (i + 1) in
          loop i
        in
        loop i [@nontail])
      ()
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
  test (fun h () ->
    let child_finished =
      Step_effect.spawn
        h
        [%here]
        ~input:(module Data.Unit)
        ~output:(module Data.Int)
        ~child_input:(fun ~parent:_ -> ())
        ~include_child_output:(fun ~parent:_ ~child:_ -> ())
        ~start:(fun h () ->
          delay h 13 ~num_steps:3;
          { Component_finished.output = 17; result = "foo" })
    in
    let child_finished = wait_for child_finished h in
    print_s [%message (child_finished : (string, int) Component_finished.t)]);
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
    (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
     ((state (Finished ()))
      (children ((
        lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
        ((state (Finished 17)) (children ()) (output 17)))))
      (output ())))
    |}]
;;

let%expect_test "parent runs before child" =
  test (fun h () ->
    ignore
      (spawn
         (fun h () ->
           let rec loop () =
             print_s [%message "child"];
             next_step h [%here] ();
             loop ()
           in
           loop () [@nontail])
         h
       : _ Component_finished.t Event.t);
    for _ = 1 to 3 do
      print_s [%message "parent"];
      next_step h [%here] ()
    done);
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
    (lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
     ((state (Finished ()))
      (children ((
        lib/hardcaml/digital_components/test/test_step_effect.ml:LINE:COL
        ((state (
           Running
           (num_steps_to_stall 0)
           (continuation (Effect_continuation (<opaque> Empty)))))
         (children ())
         (output   ())))))
      (output ())))
    |}]
;;

let%expect_test "finished child doesn't contribute to output" =
  let component, component_finished =
    Step_effect.create_component
      ~update_children_after_finish:false
      ~created_at:[%here]
      ~input:(module Data.Unit)
      ~output:(module Data.String)
      ~start:(fun h _ ->
        let child_finished =
          Step_effect.spawn
            h
            [%here]
            ~start:(fun h () ->
              next_step h [%here] "child";
              { Component_finished.output = "child_finished"; result = () })
            ~input:(module Data.Unit)
            ~output:(module Data.String)
            ~child_input:(fun ~parent:() -> ())
            ~include_child_output:(fun ~parent ~child ->
              String.concat [ parent; " + "; child ])
        in
        next_step h [%here] "before";
        ignore
          (Step_effect.wait_for h child_finished ~output:"waiting"
           : _ Component_finished.t);
        Step_effect.delay h "delay" ~num_steps:3;
        { Component_finished.output = "after"; result = () })
      ()
  in
  run_component_until_finished
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
  let spawn h here f =
    Step_effect.spawn
      h
      here
      ~start:(fun h () ->
        let result = f h in
        { Component_finished.output = (); result })
      ~input:(module Data.Unit)
      ~output:(module Data.Unit)
      ~child_input:(fun ~parent:() -> ())
      ~include_child_output:(fun ~parent:() ~child:() -> ())
  in
  let test ~update_children_after_finish ~number_of_cycles_in_parent =
    let component, _component_finished =
      Step_effect.create_component
        ~update_children_after_finish
        ~created_at:[%here]
        ~input:(module Data.Unit)
        ~output:(module Data.Unit)
        ~start:(fun h () ->
          ignore
            (spawn h [%here] (fun h ->
               ignore
                 (spawn h [%here] (fun h ->
                    let rec loop () =
                      Stdio.printf "Printing from grandchild\n";
                      next_step h [%here] ();
                      loop ()
                    in
                    loop () [@nontail])
                  : _ Component_finished.t Event.t);
               next_step h [%here] ())
             : _ Component_finished.t Event.t);
          let rec loop i =
            if i = number_of_cycles_in_parent
            then ()
            else (
              next_step h [%here] ();
              loop (i + 1))
          in
          loop 0;
          { Component_finished.output = (); result = () })
        ()
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
