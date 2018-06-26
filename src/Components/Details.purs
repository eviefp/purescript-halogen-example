module Example.Component.Details
  ( component
  , Query (..)
  ) where

import Control.Monad.Reader (ask)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.NaturalTransformation (type (~>))
import Example.Component.Router.Query (Route(..))
import Example.Control.Monad (GlobalState)
import Example.DSL.Dialog (class DialogDSL, showDialog)
import Example.DSL.Navigation (class NavigationDSL, navigate)
import Example.DSL.State (class StateDSL, getState, setState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit, Void, bind, const, discard, identity, pure, show, unit, ($), (<>))

data Query a
  = Initialize a
  | ValueChanged String a
  | UpdateValue a
  | GotoHome a

-- | `answer` is in our Environment / MonadAsk
-- | `secret` is the global `StateDSL`
type State =
  { answer :: Int
  , secret :: Int
  }

component :: ∀ env m
  . MonadAsk { answer :: Int | env } m
 => StateDSL GlobalState m
 => DialogDSL m m
 => NavigationDSL m
 => H.Component HH.HTML Query Unit Void m
component =
  H.lifecycleComponent
    { initialState: const { answer: 0, secret: 0 }
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_
      [ HH.h1_ [ HH.text $ "The answer is " <> show st.answer ]
      , HH.div_
        [ HH.text "Change secret number: "
        , HH.input
          [ HP.type_ HP.InputNumber
          , HP.value $ show st.secret
          , HE.onValueInput (HE.input ValueChanged)
          ]
        ]
      , HH.button
          [ HE.onClick (HE.input_ UpdateValue) ]
          [ HH.text "Update" ]
      , HH.button
          [ HE.onClick (HE.input_ GotoHome) ]
          [ HH.text "Go to home" ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval (Initialize next) = do
    env <- ask
    n <- getState
    H.put { answer: env.answer, secret: n }
    pure next
  eval (ValueChanged val next) = do
    let num = fromString val
    H.modify_ \st -> st { secret = maybe st.secret identity num }
    pure next
  eval (UpdateValue next) = do
    localState <- H.get
    showDialog
      { title: "Confirmation"
      , message: "Are you sure you want to update the value?"
      , actions:
        [ { name: "Yes"
          , action: updateValue localState.secret
          }
        , { name: "Nevermind"
          , action: pure unit
          }
        ]
      }
    pure next

    where

    updateValue :: Int -> m Unit
    updateValue = setState

  eval (GotoHome next) = do
    navigate Home
    pure next
