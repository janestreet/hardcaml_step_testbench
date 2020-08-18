Hardcaml Step Testbench
=======================

A monad for interacting with `Hardcaml.Cyclesim` based simulations.

Multiple control threads can be `spawn`ed and can `wait_for` child
threads to complete.

Synchronisation between threads is performed at every clock cycle. New
values for simulation input ports are collected, the simulation
updated, and outputs distributed amoung the control threads.
