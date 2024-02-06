open! Core
open Hardcaml

module Monads = struct
  module Input_monad = Monad.Ident
  module Step_monad = Digital_components.Step_monad.Make (Input_monad)
end

module type S = sig
  include Imperative.S

  (** Run the testbench until the main task finishes.

      The optional timeout argument stops the simulation after the given number of steps
      and returns None. Otherwise it will continue until the testbech completes. *)
  val run_with_timeout
    :  ?update_children_after_finish:bool (** default is [false] *)
    -> ?timeout:int (** default is [None] *)
    -> unit
    -> simulator:(_, _) Cyclesim.t
    -> testbench:(No_data.t -> 'a t)
    -> 'a option

  (** Run the testbench until completion. *)
  val run_until_finished
    :  ?update_children_after_finish:bool (** default is [false] *)
    -> unit
    -> simulator:(_, _) Cyclesim.t
    -> testbench:(No_data.t -> 'a t)
    -> 'a
end

module type Imperative_cyclesim = sig
  module Monads = Monads

  module type S = S with module Step_monad = Monads.Step_monad

  include S
end
