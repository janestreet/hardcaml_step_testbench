type ('a, 'o) t =
  { output : 'o
  ; result : 'a
  }
[@@deriving sexp_of]
