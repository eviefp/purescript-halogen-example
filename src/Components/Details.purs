module Example.Component.Details
  ( component
  , Query (..)
  ) where

import Control.Monad.Reader (ask)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.NaturalTransformation (type (~>))
import Example.Component.Router.Query (Route(..))
import Example.Control.Monad (Example)
import Example.DSL.Dialog (showDialog)
import Example.DSL.Navigation (navigate)
import Example.DSL.State (getState, setState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit, Void, bind, const, discard, id, pure, show, unit, ($), (<>))

data Query a 
  = Initialize a
  | ValueChanged String a
  | UpdateValue a
  | GotoHome a

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
    answer <- ask
    st <- getState
    H.put { answer: answer, secret: st }
    pure next
  eval (ValueChanged val next) = do
    let num = fromString val
    H.modify \st -> st { secret = maybe st.secret id num }
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
    
    updateValue :: Int -> Example Unit
    updateValue = setState
    
  eval (GotoHome next) = do
    navigate Home
    pure next