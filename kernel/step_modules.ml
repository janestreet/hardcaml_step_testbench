open Core
include Step_modules_intf

module Event_driven_sim = struct
  open Hardcaml_event_driven_sim.Two_state_simulator
  module Input_monad = Simulator.Async.Deferred
  module Component = Digital_components.Component.Make (Input_monad)
  module Step_core = Digital_components.Step_core.Make (Input_monad) (Component)

  module Step_monad =
    Digital_components.Step_monad.Make (Input_monad) (Component) (Step_core)

  module Step_effect =
    Digital_components.Step_effect.Make (Input_monad) (Component) (Step_core)
end

module Cyclesim = struct
  module Input_monad = Monad.Ident
  module Component = Digital_components.Component.Make (Input_monad)
  module Step_core = Digital_components.Step_core.Make (Input_monad) (Component)

  module Step_monad =
    Digital_components.Step_monad.Make (Input_monad) (Component) (Step_core)

  module Step_effect =
    Digital_components.Step_effect.Make (Input_monad) (Component) (Step_core)
end
