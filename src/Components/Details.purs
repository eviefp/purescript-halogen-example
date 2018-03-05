module Example.Component.Details
  ( component
  , Query (..)
  ) where

import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.NaturalTransformation (type (~>))
import Example.Component.Router.Query (Route(..))
import Example.Control.MonadRun (Example, navigate, showDialog)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit, Void, bind, const, discard, id, pure, show, unit, ($), (<>))
import Run.Reader (ask)
import Run.State (get, put)


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

component :: H.Component HH.HTML Query Unit Void Example
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

  eval :: Query ~> H.ComponentDSL State Query Void Example
  eval (Initialize next) = do
    answer <- H.lift $ ask
    st <- H.lift $ get
    H.put { answer: answer, secret: st }
    pure next
  eval (ValueChanged val next) = do
    let num = fromString val
    H.modify \st -> st { secret = maybe st.secret id num }
    pure next
  eval (UpdateValue next) = do
    localState <- H.get
    H.lift $ showDialog
      { title: "Confirmation"
      , message: "Are you sure you want to update the value?"
      , actions:
        [ { name: "Yes"
          , action: put localState.secret
          }
        , { name: "Nevermind"
          , action: pure unit
          }
        ]
      }
    pure next

  eval (GotoHome next) = do
    H.lift $ navigate Home
    pure next