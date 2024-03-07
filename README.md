Hardcaml Step Testbench
=======================

A monad for interacting with `Hardcaml.Cyclesim` based simulations.

Multiple control threads can be `spawn`ed and can `wait_for` child
threads to complete.

Synchronisation between threads is performed at every clock cycle.

There are separate versions for the `cyclesim` and `event_driven_sim` simulators.

Further, two styles of writing testbenches are provided: functional and imperative.

In the functional style new values for simulation input ports are collected, the
simulation updated, and outputs distributed amoung the control threads.

In the imperative style testbench writers just access the simulator ports directly.
