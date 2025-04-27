open Base
open Hardcaml

module Source = struct
  type 'a t =
    { valid : 'a
    ; data : 'a [@bits 32]
    ; first : 'a
    ; last : 'a
    }
  [@@deriving hardcaml]
end

module I = struct
  type 'a t =
    { clk : 'a
    ; source : 'a Source.t [@rtlprefix "i_"]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { source : 'a Source.t [@rtlprefix "o_"] } [@@deriving hardcaml]
end

let make_circuit (i : Signal.t I.t) =
  let open Signal in
  let reg_spec = Reg_spec.create () ~clock:i.clk in
  let enable = i.source.valid in
  let count = reg_fb reg_spec ~enable ~width:16 ~f:(fun d -> d +:. 1) in
  { O.source = { i.source with data = count @: i.source.data.:[15, 0] } }
;;

module Make (Monads : Hardcaml_step_testbench.Step_monads.S) = struct
  module Tb_source = Hardcaml_step_testbench.Functional.Make (Monads) (Source) (Source)
  module Tb = Hardcaml_step_testbench.Functional.Make (Monads) (I) (O)

  let rec send_data ~first ~num_words _ : unit Tb_source.t =
    let open Tb_source.Let_syntax in
    if num_words = 0
    then return ()
    else if Random.bool ()
    then (
      let%bind source = Tb_source.cycle Tb_source.input_zero in
      send_data ~first ~num_words source)
    else (
      let%bind source =
        Tb_source.cycle
          { valid = Bits.vdd
          ; data = Bits.of_int_trunc ~width:32 num_words
          ; first = (if first then Bits.vdd else Bits.gnd)
          ; last = (if num_words = 1 then Bits.vdd else Bits.gnd)
          }
      in
      send_data ~first:false ~num_words:(num_words - 1) source)
  ;;

  let rec recv_data data (output : Tb.O_data.t) : Bits.t list Tb.t =
    let open Tb.Let_syntax in
    let source = (Tb.O_data.after_edge output).source in
    if Bits.to_int_trunc source.valid <> 1
    then wait_for_next_cycle data
    else if Bits.to_int_trunc source.first = 1
    then wait_for_next_cycle [ source.data ]
    else (
      let data = source.data :: data in
      if Bits.to_int_trunc source.last = 1
      then return (List.rev data)
      else wait_for_next_cycle data)

  and wait_for_next_cycle data =
    let open Tb.Let_syntax in
    let%bind output = Tb.cycle Tb.input_hold in
    recv_data data output
  ;;

  let testbench _ =
    let open Tb.Let_syntax in
    (* Demonstrate refining I/O types. *)
    let%bind send_finished =
      Tb_source.spawn_io
        ~inputs:(fun ~(parent : _ I.t) ~child ->
          { parent with source = Tb_source.merge_inputs ~parent:parent.source ~child })
        ~outputs:(fun parent -> parent.O.source)
        (send_data ~first:true ~num_words:5)
    in
    let%bind recv_finished = Tb.spawn (recv_data []) in
    let%bind () = Tb.wait_for send_finished in
    let%bind recv_packet = Tb.wait_for recv_finished in
    return recv_packet
  ;;
end
