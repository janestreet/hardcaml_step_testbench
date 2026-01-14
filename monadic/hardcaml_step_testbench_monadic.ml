include struct
  open Hardcaml_step_testbench_kernel
  module Step_monad = Step_monad
  module Component = Component
  module Before_and_after_edge = Before_and_after_edge
  module Io_ports_for_imperative = Io_ports_for_imperative
end

module Functional = struct
  include Functional
  module Cyclesim = Functional_cyclesim
  module Event_driven_sim = Functional_event_driven_sim
end

module Imperative = struct
  include Imperative
  module Cyclesim = Imperative_cyclesim
  module Event_driven_sim = Imperative_event_driven_sim
end
