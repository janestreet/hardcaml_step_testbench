type 'a t =
  { before_edge : 'a
  ; after_edge : 'a
  }
[@@deriving sexp_of]

val create : before_edge:'a -> after_edge:'a -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val before_edge : 'a t -> 'a
val after_edge : 'a t -> 'a
