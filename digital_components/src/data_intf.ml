(** [Data.S] is an interface specifying data that is processed by a digital component. *)

open! Import

module type S = sig
  type t [@@deriving sexp_of]

  include Equal.S with type t := t

  val undefined : t
end

module type Data = sig
  module type S = S

  type 'd t = (module S with type t = 'd)

  module Bool : S with type t = bool
  module Int : S with type t = int
  module String : S with type t = string
  module Unit : S with type t = unit
  module Pair (D1 : S) (D2 : S) : S with type t = D1.t * D2.t
end
