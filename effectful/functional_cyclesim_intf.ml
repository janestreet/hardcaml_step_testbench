(** Cyclesim based step testbench API. Includes functions to run the resulting testbench. *)

open! Core
open Hardcaml

module type S = sig
  include Functional.S

  (** A simulator for the design being tested. *)
  module Simulator : sig
    type t = Cyclesim.With_interface(I)(O).t [@@deriving sexp_of]
  end

  (** Run the testbench until the main task finishes. The [input_default] argument
      controls what should happen if an input is unset by tasks in the testbench on any
      particular cycle. If a field is set to [Bits.empty] then the previous value should
      be held. Otherwise, the value provided is used as the default value for that field.

      The optional timeout argument stops the simulation after the given number of steps
      and returns None. Otherwise it will continue until the testbech completes. *)
  val run_with_timeout
    :  ?input_default:Bits.t I.t (** default is [input_hold] *)
    -> ?update_children_after_finish:bool (** default is [false] *)
    -> ?show_steps:bool (** default is [false] *)
    -> ?timeout:int (** default is [None] *)
    -> unit
    -> simulator:Simulator.t
    -> testbench:(Handler.t @ local -> O_data.t -> 'a)
    -> 'a option

  (** Run the testbench until completion. *)
  val run_until_finished
    :  ?input_default:Bits.t I.t (** default is [input_hold] *)
    -> ?show_steps:bool (** default is [false] *)
    -> ?update_children_after_finish:bool (** default is [false] *)
    -> unit
    -> simulator:Simulator.t
    -> testbench:(Handler.t @ local -> O_data.t -> 'a)
    -> 'a
end

module M (I : Interface.S) (O : Interface.S) = struct
  module type S = sig
    include S with module I = I and module O = O
  end
end

module type Functional_cyclesim = sig
  module M = M
  module Make (I : Interface.S) (O : Interface.S) : M(I)(O).S
end
