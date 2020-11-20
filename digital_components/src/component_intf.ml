(** A [('i, 'o) Component.t] is a value suitable for modeling both combinational and
    sequential digital logic.  A component is stateless if it is purely combinational, and
    stateful if modeling sequential logic.

    A component has two main functions:

    - an [output] function that returns the output of the component based on an input and
      the current state.

    - an [update_state] function that updates the state based on an input and
      the current state.

    A component is a first class module that can be implemented in any number of ways:

    - a hardware description
    - an OCaml module implementing [Component.S]
    - using [Step_monad]
    - [Component] combinators, like: [sequence], ... *)

open! Import

module Combinational = struct
  module type S = sig
    module Input : Data.S
    module Output : Data.S

    type t [@@deriving sexp_of]

    val t : t
    val created_at : Source_code_position.t
    val output : t -> Input.t -> Output.t
  end

  type ('i, 'o) t = (module S with type Input.t = 'i and type Output.t = 'o)
end

module type S = sig
  module Input : Data.S
  module Output : Data.S

  type t [@@deriving sexp_of]

  val t : t
  val created_at : Source_code_position.t
  val output : t -> Input.t -> Output.t
  val update_state : t -> Input.t -> unit
end

module type Component = sig
  module type S = S

  (** [t] is mostly abstract, but we expose is as a constructor so that the type checker
      knows that [t] is injective. *)
  type ('i, 'o) t_

  type ('i, 'o) t = T of ('i, 'o) t_ [@@deriving sexp_of]
  type ('i, 'o) t_module = (module S with type Input.t = 'i and type Output.t = 'o)

  val sexp_of_input : ('i, _) t -> 'i -> Sexp.t
  val sexp_of_output : (_, 'o) t -> 'o -> Sexp.t
  val input_module : ('i, _) t -> 'i Data.t
  val output_module : (_, 'o) t -> 'o Data.t
  val create : ('i, 'o) t_module -> ('i, 'o) t

  (** [output] returns the output based on an input and its current state, but does not
      update the state.  A component is called "combinational" if [output t i] ignores
      [t].  A component is called "sequential" if [output t i] uses [t].  A sequential
      component is called a "moore machine" if it ignores [i] and a "mealy machine" if it
      uses [i]. *)
  val output : ('i, 'o) t -> 'i -> 'o

  (** [update_state] updates [t]'s state based on an input and its current state *)
  val update_state : ('i, _) t -> 'i -> unit

  (** [run_with_inputs t is] runs [length is] steps with [t], on each step calling
      [update_state] and then [output], pairing the input of that step with the output. *)
  val run_with_inputs : ('i, 'o) t -> 'i list -> ('i * 'o) list

  module Next_input : sig
    type 'i t =
      | Finished
      | Input of 'i
    [@@deriving sexp_of]
  end

  val run_until_finished
    :  ?show_steps:bool (** default is [false] *)
    -> ('i, 'o) t
    -> first_input:'i
    -> next_input:('o -> 'i Next_input.t)
    -> unit

  (** {2 Component combinators} *)

  val sequence : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t

  val map_input
    :  ('i2, 'o) t
    -> 'i1 Data.t
    -> f:('i1 -> 'i2) (** a pure function *)
    -> ('i1, 'o) t

  val map_output
    :  ('i, 'o1) t
    -> 'o2 Data.t
    -> f:('o1 -> 'o2) (** a pure function *)
    -> ('i, 'o2) t

  (** {2 Combinational components} *)

  module Combinational = Combinational

  val create_combinational : ('i, 'o) Combinational.t -> ('i, 'o) t
  val and_ : (bool * bool, bool) t
  val or_ : (bool * bool, bool) t
  val not_ : (bool, bool) t

  (** {2 Sequential components} *)

  val flip_flop : unit -> (bool, bool) t

  module Flip_flop_with_load_enable : sig
    module Input : sig
      type t =
        { input : bool
        ; load_enable : bool
        }

      include Data.S with type t := t
    end

    module Output = Data.Bool

    val create : unit -> (Input.t, Output.t) t
  end

  module Flip_flop_with_load_enable_and_reset : sig
    module Input : sig
      type t =
        { input : bool
        ; load_enable : bool
        ; reset : bool
        }

      include Data.S with type t := t
    end

    module Output = Data.Bool

    val create : unit -> (Input.t, Output.t) t
  end
end
