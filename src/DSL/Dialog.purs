module Example.DSL.Dialog
  ( class DialogDSL
  , showDialog
  , DialogOptions
  , ActionOptions
  ) where

import Control.Monad.Eff (kind Effect)
import Control.Monad.Free (liftF)
import Halogen (HalogenF(..), HalogenM(..))
import Prelude (class Monad, Unit, (<<<))

type ActionOptions m =
  { name   :: String
  , action :: m Unit
  }

type DialogOptions m = 
  { title    :: String
  , message  :: String
  , actions  :: Array (ActionOptions m)
  }

class (Monad m, Monad n) <= DialogDSL n m where
  showDialog :: DialogOptions n -> m Unit

instance dialogDSLHalogenM :: DialogDSL n m => DialogDSL n (HalogenM s f g p o m) where
  showDialog = HalogenM <<< liftF <<< Lift <<< showDialog