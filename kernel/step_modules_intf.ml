open Core

module type S = sig
  module Input_monad : Monad.S
end

module type Step_modules = sig
  module type S = S

  module Event_driven_sim : sig
    open Hardcaml_event_driven_sim.Two_state_simulator
    module Input_monad = Simulator.Async.Deferred
    include S with module Input_monad := Input_monad
  end

  module Cyclesim : sig
    module Input_monad = Monad.Ident
    include S with module Input_monad := Input_monad
  end
end
