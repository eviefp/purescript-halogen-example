module Example.DSL.Dialog
  ( class DialogDSL
  , showDialog
  , DialogOptions
  , ActionOptions
  ) where

import Halogen (HalogenM, lift)
import Prelude (class Monad, Unit, (<<<))

-- | A dialog can have multiple `ActionOption`s.
-- | Each has a `name` which appears on the button and an action encoded as
-- | an `m Unit`. Initially, this will generally be `ExampleM`, which gets
-- | translated to `Aff` inside the `runExampleM`.
type ActionOptions m =
  { name   :: String
  , action :: m Unit
  }

-- | Dialog title, content and actions.
type DialogOptions m =
  { title    :: String
  , message  :: String
  , actions  :: Array (ActionOptions m)
  }

-- | Shows the dialog and return Unit under the current monad.
class Monad m <= DialogDSL n m where
  showDialog :: DialogOptions n -> m Unit

-- | We need a HalogenM instance in order to be able to use this DSL
-- | within our component's `eval`.
instance dialogDSLHalogenM :: DialogDSL n m => DialogDSL n (HalogenM s f g p o m) where
  showDialog = lift <<< showDialog
