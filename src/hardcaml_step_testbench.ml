module Before_and_after_edge = Before_and_after_edge
module Step_monads = Step_monads

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
