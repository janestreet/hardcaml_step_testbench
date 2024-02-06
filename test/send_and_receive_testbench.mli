open! Base
open Hardcaml

module Source : sig
  type 'a t =
    { valid : 'a
    ; data : 'a
    ; first : 'a
    ; last : 'a
    }
  [@@deriving hardcaml]
end

module I : sig
  type 'a t =
    { clk : 'a
    ; source : 'a Source.t
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t = { source : 'a Source.t } [@@deriving hardcaml]
end

val make_circuit : Signal.t I.t -> Signal.t O.t

(** Testbench which is abstracted over the monads that build a step testbench and can be
    used from [Cyclesim] and [Event_driven_sim]. *)
module Make (Monads : Hardcaml_step_testbench.Step_monads.S) : sig
  module Tb_source : Hardcaml_step_testbench.Functional.M(Monads)(Source)(Source).S
  module Tb : Hardcaml_step_testbench.Functional.M(Monads)(I)(O).S

  val send_data : first:bool -> num_words:int -> Tb_source.O_data.t -> unit Tb_source.t
  val recv_data : Bits.t list -> Tb.O_data.t -> Bits.t list Tb.t
  val wait_for_next_cycle : Bits.t list -> Bits.t list Tb.t
  val testbench : _ -> Bits.t list Tb.t
end
