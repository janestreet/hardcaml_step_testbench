type 'a t =
  { before_edge : 'a
  ; after_edge : 'a
  }
[@@deriving sexp_of, fields]

let create ~before_edge ~after_edge = { before_edge; after_edge }
let map t ~f = { before_edge = f t.before_edge; after_edge = f t.after_edge }
