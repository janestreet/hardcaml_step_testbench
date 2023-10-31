open! Base
include Component_intf

module Make (Input_monad : Monad.S) = struct
  module Input_monad = Input_monad

  type ('i, 'o) t_module = (module Module.S with type Input.t = 'i and type Output.t = 'o)
  type ('i, 'o) t_ = ('i, 'o) t_module
  type ('i, 'o) t = T of ('i, 'o) t_

  let create t = T t

  module Combinational = Combinational

  let create_combinational (type i o) ((module T) : (i, o) Combinational.t) =
    create
      (module struct
        include T

        let update_state ?prune:_ _ _ = ()
        let prune_children _ = ()
        let has_children _ = false
      end)
  ;;

  let input_module (type i o) (T (module T) : (i, o) t) : i Data.t = (module T.Input)
  let output_module (type i o) (T (module T) : (i, o) t) : o Data.t = (module T.Output)
  let sexp_of_input (type i o) (T (module T) : (i, o) t) = T.Input.sexp_of_t
  let sexp_of_output (type i o) (T (module T) : (i, o) t) = T.Output.sexp_of_t

  let sexp_of_t (type i o) _ _ (T (module T) : (i, o) t) =
    [%message "" ~_:(T.created_at : Source_code_position.t) ~_:(T.t : T.t)]
  ;;

  let output (type i o) (T (module T) : (i, o) t) input = T.(output t) input

  let update_state ?prune (type i o) (T (module T) : (i, o) t) input =
    T.update_state ?prune T.t input
  ;;

  let prune_children (type i o) (T (module T) : (i, o) t) = T.(prune_children t)
  let has_children (type i o) (T (module T) : (i, o) t) = T.(has_children t)

  let run_with_inputs t is =
    List.fold is ~init:[] ~f:(fun os i ->
      update_state t i;
      (i, output t i) :: os)
    |> List.rev
  ;;

  module Next_input = struct
    type 'i t =
      | Finished
      | Input of 'i
    [@@deriving sexp_of]
  end

  let run_until_finished
    ?(show_steps = false)
    t
    ~first_input
    ~(next_input : _ -> _ Next_input.t Input_monad.t)
    =
    let step_number = ref 0 in
    let rec loop input =
      if show_steps then Stdio.print_s [%message "" ~step_number:(!step_number : int)];
      Int.incr step_number;
      update_state ~prune:(!step_number % 1000 = 0) t input;
      let output = output t input in
      let%bind.Input_monad next_input = next_input output in
      match next_input with
      | Finished -> Input_monad.return ()
      | Input i -> loop i
    in
    loop first_input
  ;;

  let sequence
    (type a b c)
    (T (module T1) as t1 : (a, b) t)
    (T (module T2) as t2 : (b, c) t)
    : (a, c) t
    =
    T
      (module struct
        module Input = T1.Input
        module Output = T2.Output

        type nonrec t = (T1.Input.t, T1.Output.t) t * (T2.Input.t, T2.Output.t) t
        [@@deriving sexp_of]

        let t = t1, t2
        let created_at = [%here]

        let update_state ?prune ((t1, t2) : t) input =
          let b = output t1 input in
          update_state ?prune t1 input;
          update_state ?prune t2 b
        ;;

        let output (t1, t2) input = output t2 (output t1 input)

        let prune_children (t1, t2) =
          prune_children t1;
          prune_children t2
        ;;

        let has_children (t1, t2) = has_children t1 || has_children t2
      end)
  ;;

  let map_input
    (type i1 i2 o)
    (T (module T) : (i2, o) t)
    (module Input : Data.S with type t = i1)
    ~f
    =
    T
      (module struct
        module Input = Input
        module Output = T.Output

        type t = T.t [@@deriving sexp_of]

        let t = T.t
        let created_at = [%here]
        let output t i1 = T.output t (f i1)
        let update_state ?prune t i1 = T.update_state ?prune t (f i1)
        let prune_children t = T.prune_children t
        let has_children t = T.has_children t
      end)
  ;;

  let map_output
    (type i o1 o2)
    (T (module T) : (i, o1) t)
    (module Output : Data.S with type t = o2)
    ~f
    =
    T
      (module struct
        module Input = T.Input
        module Output = Output

        type t = T.t [@@deriving sexp_of]

        let t = T.t
        let created_at = [%here]
        let output t i = f (T.output t i)
        let update_state ?prune t i = T.update_state ?prune t i
        let prune_children t = T.prune_children t
        let has_children t = T.has_children t
      end)
  ;;

  let create_binary_bool sexp f =
    create_combinational
      (module struct
        module Input = Data.Pair (Data.Bool) (Data.Bool)
        module Output = Data.Bool

        type t = unit

        let sexp_of_t () = sexp
        let created_at = [%here]
        let t = ()
        let output () (b1, b2) = f b1 b2
      end)
  ;;

  let and_ = create_binary_bool [%message "and"] (fun b1 b2 -> b1 && b2)
  let or_ = create_binary_bool [%message "or"] (fun b1 b2 -> b1 || b2)

  let create_unary_bool sexp f =
    create_combinational
      (module struct
        module Input = Data.Bool
        module Output = Data.Bool

        type t = unit

        let sexp_of_t () = sexp
        let created_at = [%here]
        let t = ()
        let output () b = f b
      end)
  ;;

  let not_ = create_unary_bool [%message "not"] (fun b -> not b)

  let flip_flop () =
    create
      (module struct
        module Input = Data.Bool
        module Output = Data.Bool

        type t = bool ref [@@deriving sexp_of]

        let sexp_of_t t = [%message "Flip_flop" ~_:(t : t)]
        let t = ref Output.undefined
        let created_at = [%here]
        let output t _ = !t
        let update_state ?prune:_ t b = t := b
        let prune_children _ = ()
        let has_children _ = false
      end)
  ;;

  module Flip_flop_with_load_enable = struct
    module Input = struct
      type t =
        { input : bool
        ; load_enable : bool
        }
      [@@deriving compare, sexp_of]

      let equal = [%compare.equal: t]
      let undefined = { input = Data.Bool.undefined; load_enable = Data.Bool.undefined }
    end

    module Output = Data.Bool

    let create () =
      create
        (module struct
          module Input = Input
          module Output = Output

          type t = bool ref [@@deriving sexp_of]

          let t = ref Output.undefined
          let sexp_of_t t = [%message "Flip_flop_with_load_enable" ~_:(t : t)]
          let created_at = [%here]
          let output t _ = !t

          let update_state ?prune:_ t { Input.input; load_enable } =
            if load_enable then t := input
          ;;

          let prune_children _ = ()
          let has_children _ = false
        end)
    ;;
  end

  module Flip_flop_with_load_enable_and_reset = struct
    module Input = struct
      type t =
        { input : bool
        ; load_enable : bool
        ; reset : bool
        }
      [@@deriving compare, sexp_of]

      let equal = [%compare.equal: t]

      let undefined =
        { input = Data.Bool.undefined
        ; load_enable = Data.Bool.undefined
        ; reset = Data.Bool.undefined
        }
      ;;
    end

    module Output = Data.Bool

    let create () =
      create
        (module struct
          module Input = Input
          module Output = Output

          type t = bool ref [@@deriving sexp_of]

          let t = ref Output.undefined
          let sexp_of_t t = [%message "Flip_flop_with_load_enable_and_reset" ~_:(t : t)]
          let created_at = [%here]
          let output t _ = !t

          let update_state ?prune:_ t { Input.input; load_enable; reset } =
            if reset then t := false else if load_enable then t := input
          ;;

          let prune_children _ = ()
          let has_children _ = false
        end)
    ;;
  end
end
