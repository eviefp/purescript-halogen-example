module Example.Component.Home
  ( component
  , Query (..)
  ) where

import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Example.Component.Router.Query (Route(..))
import Example.Control.Monad (Example)
import Example.DSL.Navigation (navigate)
import Example.DSL.State (getState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prelude (Unit, Void, const, discard, pure, show, ($), (<>), (>>=))

data Query a 
  = Initialize a
  | GotoDetails a

type State = Int

component :: H.Component HH.HTML Query Unit Void Example
component =
  H.lifecycleComponent
    { initialState: const 0
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
      [ HH.h1_ [ HH.text "This is home" ]
      , HH.div_ [ HH.text $ "Secret number is: " <> show n ]
      , HH.button
          [ HE.onClick (HE.input_ GotoDetails) ]
          [ HH.text "Go to details" ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void Example
  eval (Initialize next) = do
    getState >>= H.put
    pure next
  eval (GotoDetails next) = do
    navigate Details
    pure next