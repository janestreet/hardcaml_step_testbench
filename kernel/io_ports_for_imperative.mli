(** Type to store the IO ports used when spawning an imperative task. *)

open! Core

type ('i, 'o) t =
  { inputs : 'i
  ; outputs : 'o Before_and_after_edge.t
  }
