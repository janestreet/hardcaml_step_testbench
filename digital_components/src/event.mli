type 'a t [@@deriving sexp_of]

val create : unit -> 'a t
val set_value : 'a t -> 'a -> unit
val value : 'a t -> 'a option
