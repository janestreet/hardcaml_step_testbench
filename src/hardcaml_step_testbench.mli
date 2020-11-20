(** Hardcaml testbench infrastructure which allows multiple threads of execution to be
    interleaved and synchronised at each clock cycle while interacting with the ports of
    a simulation.

    Inputs set to [Bits.empty] are assumed to be unset and may be set in parent tasks.  If
    no task sets an input then the port will either keep its previous value, or be set to
    a known default value depending on the [input_default] argument to the [run] function.

    Inputs set in a child task take precendence over inputs sets in parent tasks. *)

include Hardcaml_step_testbench_intf.Hardcaml_step_testbench (** @inline *)
