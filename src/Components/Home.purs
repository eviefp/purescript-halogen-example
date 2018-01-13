module Example.Component.Home
  ( component
  , Query (..)
  ) where

import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Example.Component.Router.Query (Route(..))
import Example.Control.Monad (Example)
import Example.DSL.Navigation (navigate)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prelude (Unit, Void, const, discard, id, pure)

data Query a 
  = GotoDetails a

type State = Unit

component :: H.Component HH.HTML Query Unit Void Example
component =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render _ =
    HH.div_
      [ HH.h1_ [ HH.text "This is home"]
      , HH.button
          [ HE.onClick (HE.input_ GotoDetails) ]
          [ HH.text "Go to details" ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void Example
  eval (GotoDetails next) = do
    navigate Details
    pure next