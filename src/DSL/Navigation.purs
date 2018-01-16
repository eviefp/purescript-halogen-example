module Example.DSL.Navigation
  ( class NavigationDSL
  , navigate
  ) where


import Example.Component.Router.Query (Route)
import Halogen (HalogenM, lift)
import Prelude (class Monad, Unit, (<<<))

-- | DSL for navigating to a route.
class Monad m <= NavigationDSL m where
  navigate :: Route -> m Unit

-- | We need a HalogenM instance in order to be able to use this DSL
-- | within our component's `eval`.
instance navigationDSLHalogenM :: NavigationDSL m => NavigationDSL (HalogenM s f g p o m) where
  navigate = lift <<< navigate