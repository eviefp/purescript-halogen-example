module Example.DSL.State 
  ( class StateDSL
  , getState
  , modifyState
  , setState
  ) where

import Control.Monad.Free (liftF)
import Halogen (HalogenF(..), HalogenM(..))
import Prelude (class Monad, Unit, const, ($), (<<<))

-- | Naïve state monad representation. For simplicity, we use two operations:
-- | one for reading and one for writing.
class (Monad m) <= StateDSL s m | m -> s where
  getState    :: m s
  modifyState :: (s -> s) -> m Unit

-- | We define a helper function for setting the state when we don't care about the current state.
setState :: forall s m 
          . StateDSL s m
         => s 
         -> m Unit
setState = modifyState <<< const

-- | We need a HalogenM instance in order to be able to use this DSL
-- | within our component's `eval`.
instance stateDSLHalogenM :: StateDSL s' m => StateDSL s' (HalogenM s f g p o m) where
  getState    = HalogenM <<< liftF <<< Lift $ getState
  modifyState = HalogenM <<< liftF <<< Lift <<< modifyState