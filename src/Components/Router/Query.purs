module Example.Component.Router.Query
  ( Route (..)
  , Query (..)
  ) where

import Control.Monad.Aff (Aff)
import Example.Component.Dialog (DialogResult)
import Example.DSL.Dialog (DialogOptions)
import Example.EffectType (EffectType)
import Halogen.Aff (HalogenEffects)
import Prelude (class Eq, class Ord)

data Route
  = Home
  | Details

derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

data Query a 
  = Goto Route a
  | ShowDialog (DialogOptions (Aff (HalogenEffects EffectType))) a
  | HandleDialogResult DialogResult a