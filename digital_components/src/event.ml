open Core

type 'a t = { mutable value : 'a option } [@@deriving fields ~getters, sexp_of]

let create () = { value = None }

let set_value t a =
  if Option.is_some t.value
  then raise_s [%message "[Event.set_value] of event whose value has already been set"];
  t.value <- Some a
;;
