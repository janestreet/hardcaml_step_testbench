open Core
open Digital_components

module type S = sig
  module Input_monad : Monad.S
  module Component : Component.M(Input_monad).S
  module Step_core : Digital_components.Step_core.M(Input_monad)(Component).S
  module Step_monad : Digital_components.Step_monad.M(Input_monad)(Component)(Step_core).S

  module Step_effect :
    Digital_components.Step_effect.M(Input_monad)(Component)(Step_core).S
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
