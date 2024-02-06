open Core

module type S = sig
  module Input_monad : Monad.S
  module Step_monad : Digital_components.Step_monad.M(Input_monad).S
end
