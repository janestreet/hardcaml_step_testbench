(** Monads which abstract out the mechanism by which a simulator is updated by the step
    testbench framework. *)

open Core

module type S = sig
  module Input_monad : Monad.S
  module Step_monad : Digital_components.Step_monad.M(Input_monad).S
end
