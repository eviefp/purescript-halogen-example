module Example.DSL.Navigation
  ( class NavigationDSL
  , navigate
  ) where

import Control.Monad.Free (liftF)
import Example.Component.Router.Query (Route)
import Halogen (HalogenF(..), HalogenM(..))
import Prelude (class Monad, Unit, (<<<))

class Monad m <= NavigationDSL m where
  navigate :: Route -> m Unit

instance navigationDSLHalogenM :: NavigationDSL m => NavigationDSL (HalogenM s f g p o m) where
  navigate = HalogenM <<< liftF <<< Lift <<< navigate