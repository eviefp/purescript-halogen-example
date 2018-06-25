module Example.Component.Router.Query
  ( Route (..)
  , Query (..)
  ) where

import Effect.Aff (Aff)
import Example.Component.Dialog (DialogResult)
import Example.DSL.Dialog (DialogOptions)
import Prelude (class Eq, class Ord)

data Route
  = Home
  | Details

derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

data Query a
  = Goto Route a
  | ShowDialog (DialogOptions Aff) a
  | HandleDialogResult DialogResult a
