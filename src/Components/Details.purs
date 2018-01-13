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
import Example.DSL.Navigation (navigate)
import Example.DSL.State (modifyState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit, Void, bind, const, discard, id, pure, show, ($), (<>))

data Query a 
  = Initialize a
  | UpdateValue String a
  | GotoHome a

type State = Int

component :: H.Component HH.HTML Query Unit Void Example
component =
  H.lifecycleComponent
    { initialState: const 1
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render n =
    HH.div_
      [ HH.h1_ [ HH.text $ "The answer is " <> show n]
      , HH.div_
        [ HH.text "Change secret number: "
        , HH.input 
          [ HP.type_ HP.InputNumber
          , HE.onValueInput (HE.input UpdateValue) 
          ]
        ]
      , HH.button
          [ HE.onClick (HE.input_ GotoHome) ]
          [ HH.text "Go to home" ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void Example
  eval (Initialize next) = do
    answer <- ask
    H.put answer
    pure next
  eval (UpdateValue val next) = do
    let num = fromString val
    modifyState \n -> maybe n id num
    pure next
  eval (GotoHome next) = do
    navigate Home
    pure next