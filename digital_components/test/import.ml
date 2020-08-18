include Base
include Expect_test_helpers_core
include Digital_components

let is_some = Option.is_some
let printf = Stdio.printf

let run_with_inputs (t : _ Component.t) inputs =
  let sexp_of_input = Component.sexp_of_input t in
  let sexp_of_output = Component.sexp_of_output t in
  let sexp_of_io (input, output) = [%message (input : input) (output : output)] in
  print_s [%sexp (Component.run_with_inputs t inputs : io list)]
;;
