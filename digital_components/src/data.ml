open! Base
include Data_intf

type 'd t = (module S with type t = 'd)

module Bool = struct
  type t = bool [@@deriving compare ~localize, equal ~localize, sexp_of]

  let undefined = false
end

module Int = struct
  type t = int [@@deriving compare ~localize, equal ~localize, sexp_of]

  let undefined = 0
end

module String = struct
  type t = string [@@deriving compare ~localize, equal ~localize, sexp_of]

  let undefined = ""
end

module Unit = struct
  type t = unit [@@deriving compare ~localize, equal ~localize, sexp_of]

  let undefined = ()
end

module%template [@mode m = (local, global)] Pair (D1 : S [@mode m]) (D2 : S [@mode m]) =
struct
  type t = D1.t * D2.t [@@deriving sexp_of, (equal [@mode.explicit m])]

  let undefined = D1.undefined, D2.undefined
end
