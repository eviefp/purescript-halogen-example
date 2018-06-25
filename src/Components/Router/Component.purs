module Example.Component.Router
  ( component
  ) where

import Effect.Aff (Aff)
import Data.Array ((!!))
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Example.Component.Details as Details
import Example.Component.Dialog (DialogOptionsLite)
import Example.Component.Dialog as Dialog
import Example.Component.Home as Home
import Example.Component.Router.Query (Query(..), Route(..))
import Example.Control.Monad (Example)
import Example.DSL.Dialog (DialogOptions, ActionOptions)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prelude (Unit, Void, absurd, bind, const, discard, map, pure, unit)


-- | Query algebra for direct children of the router component, represented as
-- | a Coproduct.
type ItemQuery = Coproduct3 Dialog.Query Home.Query Details.Query

-- | Slot type for router items.
type ItemSlot = Either3 Unit Unit Unit

type State =
  { route :: Route
  , dialogOptions :: Maybe (DialogOptions Aff)
  }

-- | Router component.
component :: H.Component HH.HTML Query Unit Void Example
component
  = H.parentComponent
    { initialState: const { route: Home, dialogOptions: Nothing }
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ParentHTML Query ItemQuery ItemSlot Example
  render { route, dialogOptions } =
    HH.div_
      [ renderRoute route
      , renderDialog dialogOptions
      ]

    where

    renderRoute :: Route -> H.ParentHTML Query ItemQuery ItemSlot Example
    renderRoute = case _ of
      Home    -> HH.slot' CP.cp2 unit Home.component    unit absurd
      Details -> HH.slot' CP.cp3 unit Details.component unit absurd

    renderDialog :: Maybe (DialogOptions Aff) -> H.ParentHTML Query ItemQuery ItemSlot Example
    renderDialog Nothing     = HH.text ""
    renderDialog (Just opts) = HH.slot' CP.cp1 unit Dialog.component (shred opts) (HE.input HandleDialogResult)

    shred :: forall m. DialogOptions m -> DialogOptionsLite
    shred opts = { title: opts.title, message: opts.message, actions: map getAction opts.actions }

    getAction :: forall m. ActionOptions m -> String
    getAction ao = ao.name

  eval :: Query ~> H.ParentDSL State Query ItemQuery ItemSlot Void Example
  eval (ShowDialog opts next) = do
    H.modify_ _ { dialogOptions = Just opts }
    pure next
  eval (HandleDialogResult (Dialog.DialogResult idx) next) = do
    st <- H.get
    case st.dialogOptions of
      Nothing   -> pure unit
      Just opts -> do
        let maybeAction = opts.actions !! idx
        case maybeAction of
          Nothing -> pure unit
          Just action -> do
            H.liftAff action.action
            H.modify_ _ { dialogOptions = Nothing }
            pure unit
        pure unit
    pure next
  eval (Goto route next) = do
    H.modify_ _ { route = route }
    pure next
