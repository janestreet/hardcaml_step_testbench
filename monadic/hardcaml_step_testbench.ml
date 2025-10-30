module Before_and_after_edge = Hardcaml_step_testbench_kernel.Before_and_after_edge
module Io_ports_for_imperative = Hardcaml_step_testbench_kernel.Io_ports_for_imperative
module Step_modules = Hardcaml_step_testbench_kernel.Step_modules

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
