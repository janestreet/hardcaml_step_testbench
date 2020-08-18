`Digital_components` is a library aimed at specifying and modeling
hardware.  The main idea is to explicitly model a single step of
computation, corresponding to a "clock cycle".  There is a shared
[Component.t] type that abstractly describes the behavior of a
component in terms of state, input, and output.  A [Component.t] can
be implemented in any number of ways, including OCaml code, a hardware
description, component combinators.
