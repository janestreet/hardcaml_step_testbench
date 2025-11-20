open Base
open Expect_test_helpers_base
open Digital_components
module Component = Component.Make (Monad.Ident)
open Component

let run_with_inputs (t : _ Component.t) inputs =
  let sexp_of_input = Component.sexp_of_input t in
  let sexp_of_output = Component.sexp_of_output t in
  let sexp_of_io (input, output) = [%message (input : input) (output : output)] in
  print_s [%sexp (Component.run_with_inputs t inputs : io list)]
;;

let show (t : _ Component.t) =
  print_s [%sexp (t : (_, _) Component.t)] ~hide_positions:true
;;

let%expect_test "[and_], [or_]" =
  let test t =
    List.iter Bool.all ~f:(fun b1 ->
      List.iter Bool.all ~f:(fun b2 ->
        print_s
          [%message
            "" ~_:(b1 : bool) ~_:(b2 : bool) ~_:(Component.output t (b1, b2) : bool)]))
  in
  test and_;
  [%expect
    {|
    (false false false)
    (false true false)
    (true false false)
    (true true true)
    |}];
  test or_;
  [%expect
    {|
    (false false false)
    (false true true)
    (true false true)
    (true true true)
    |}]
;;

let%expect_test "[map_input], [map_output]" =
  let test t =
    List.iter Bool.all ~f:(fun input ->
      print_s [%message "" (input : bool) ~output:(output t input : bool)])
  in
  test (map_input not_ (module Data.Bool) ~f:not);
  [%expect
    {|
    ((input  false)
     (output false))
    ((input  true)
     (output true))
    |}];
  test (map_output not_ (module Data.Bool) ~f:not);
  [%expect
    {|
    ((input  false)
     (output false))
    ((input  true)
     (output true))
    |}]
;;

let%expect_test "[flip_flop]" =
  let t = flip_flop () in
  let show_output t = print_s [%sexp (output t false : bool)] in
  show t;
  [%expect
    {| (lib/hardcaml/digital_components/src/component.ml:LINE:COL (Flip_flop false)) |}];
  show_output t;
  [%expect {| false |}];
  update_state t true ~parent_period:1 ~step_number:1;
  show t;
  [%expect
    {| (lib/hardcaml/digital_components/src/component.ml:LINE:COL (Flip_flop true)) |}];
  show_output t;
  [%expect {| true |}];
  update_state t false ~parent_period:1 ~step_number:2;
  show t;
  [%expect
    {| (lib/hardcaml/digital_components/src/component.ml:LINE:COL (Flip_flop false)) |}];
  show_output t;
  [%expect {| false |}]
;;

let%expect_test "[flip_flop_with_load_enable]" =
  let t = Flip_flop_with_load_enable.create () in
  show t;
  [%expect
    {|
    (lib/hardcaml/digital_components/src/component.ml:LINE:COL
     (Flip_flop_with_load_enable false))
    |}];
  run_with_inputs
    t
    (List.map
       ~f:(fun (load_enable, input) ->
         { Flip_flop_with_load_enable.Input.load_enable; input })
       [ false, true; true, true; false, false; true, false ]);
  [%expect
    {|
    (((input (
        (input       true)
        (load_enable false)))
      (output false))
     ((input (
        (input       true)
        (load_enable true)))
      (output true))
     ((input (
        (input       false)
        (load_enable false)))
      (output true))
     ((input (
        (input       false)
        (load_enable true)))
      (output false)))
    |}]
;;

let%expect_test "[flip_flop_with_load_enable_and_reset]" =
  let t = Flip_flop_with_load_enable_and_reset.create () in
  show t;
  [%expect
    {|
    (lib/hardcaml/digital_components/src/component.ml:LINE:COL
     (Flip_flop_with_load_enable_and_reset false))
    |}];
  run_with_inputs
    t
    (List.map
       ~f:(fun (load_enable, input, reset) ->
         { Flip_flop_with_load_enable_and_reset.Input.load_enable; input; reset })
       [ false, true, false; false, true, true; true, true, false; true, false, true ]);
  [%expect
    {|
    (((input (
        (input       true)
        (load_enable false)
        (reset       false)))
      (output false))
     ((input (
        (input       true)
        (load_enable false)
        (reset       true)))
      (output false))
     ((input (
        (input       true)
        (load_enable true)
        (reset       false)))
      (output true))
     ((input (
        (input       false)
        (load_enable true)
        (reset       true)))
      (output false)))
    |}]
;;

let%expect_test "[sequence]" =
  let t1 = flip_flop () in
  let t2 = flip_flop () in
  let t = sequence t1 t2 in
  run_with_inputs t [ true; true; false; false; false ];
  [%expect
    {|
    (((input true)  (output false))
     ((input true)  (output true))
     ((input false) (output true))
     ((input false) (output false))
     ((input false) (output false)))
    |}]
;;
