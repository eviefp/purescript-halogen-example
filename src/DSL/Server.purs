module Example.DSL.Server
  ( class ServerDSL
  , getGreeting
  ) where

import Control.Monad.Free (liftF)
import Data.Either (Either)
import Halogen (HalogenF(..), HalogenM(..))
import Prelude (class Monad, ($), (<<<))

class Monad m <= ServerDSL m where
  getGreeting :: m (Either String String)

instance serverDSLHalogenM :: ServerDSL m => ServerDSL (HalogenM s f g p o m) where
  getGreeting = HalogenM <<< liftF <<< Lift $ getGreeting