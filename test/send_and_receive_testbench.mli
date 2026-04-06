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

module Tb_source : Hardcaml_step_testbench.Functional.M(Source)(Source).S
module Tb : Hardcaml_step_testbench.Functional.M(I)(O).S

val send_data
  :  first:bool
  -> num_words:int
  -> Tb_source.O_data.t
  -> Tb_source.Handler.t @ local
  -> unit

val recv_data : Bits.t list -> Tb.O_data.t -> Tb.Handler.t @ local -> Bits.t list
val wait_for_next_cycle : Bits.t list -> Tb.Handler.t @ local -> Bits.t list
val testbench : _ -> Tb.Handler.t @ local -> Bits.t list
