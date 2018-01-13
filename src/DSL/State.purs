module Example.DSL.State 
  ( class StateDSL
  , getState
  , modifyState
  , setState
  ) where

import Control.Monad.Free (liftF)
import Halogen (HalogenF(..), HalogenM(..))
import Prelude (class Monad, Unit, const, ($), (<<<))

class (Monad m) <= StateDSL s m | m -> s where
  getState :: m s
  modifyState :: (s -> s) -> m Unit

setState :: forall s m 
          . StateDSL s m
         => s 
         -> m Unit
setState = modifyState <<< const

instance stateDSLHalogenM :: StateDSL s' m => StateDSL s' (HalogenM s f g p o m) where
  getState    = HalogenM <<< liftF <<< Lift $ getState
  modifyState = HalogenM <<< liftF <<< Lift <<< modifyState