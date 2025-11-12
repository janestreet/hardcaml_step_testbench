open! Core

type ('i, 'o) t =
  { inputs : 'i
  ; outputs : 'o Before_and_after_edge.t
  }
