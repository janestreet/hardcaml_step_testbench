(** [Data.S] is an interface specifying data that is processed by a digital component. *)

open! Base

module type%template [@mode m = (local, global)] S = sig
  type t [@@deriving sexp_of]

  include Equal.S [@mode m] with type t := t

  val undefined : t
end

module type Data = sig
  module type%template [@mode m = (local, global)] S = S [@mode m]

  type 'd t = (module S with type t = 'd)

  module Bool : S [@mode local] with type t = bool
  module Int : S [@mode local] with type t = int
  module String : S [@mode local] with type t = string
  module Unit : S [@mode local] with type t = unit

  module%template [@mode m = (local, global)] Pair (D1 : S [@mode m]) (D2 : S [@mode m]) :
    S [@mode m] with type t = D1.t * D2.t
end
