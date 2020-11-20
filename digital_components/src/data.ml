open! Import
include Data_intf

type 'd t = (module S with type t = 'd)

module Bool = struct
  type t = bool [@@deriving compare, sexp_of]

  let equal = [%compare.equal: t]
  let undefined = false
end

module Int = struct
  type t = int [@@deriving compare, sexp_of]

  let equal = [%compare.equal: t]
  let undefined = 0
end

module String = struct
  type t = string [@@deriving compare, sexp_of]

  let equal = [%compare.equal: t]
  let undefined = ""
end

module Unit = struct
  type t = unit [@@deriving compare, sexp_of]

  let equal = [%compare.equal: t]
  let undefined = ()
end

module Pair (D1 : S) (D2 : S) = struct
  type t = D1.t * D2.t [@@deriving sexp_of]

  let equal (a1, a2) (b1, b2) = D1.equal a1 b1 && D2.equal a2 b2
  let undefined = D1.undefined, D2.undefined
end
