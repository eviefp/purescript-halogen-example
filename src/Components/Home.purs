module Example.Component.Home
  ( component
  , Query (..)
  ) where

import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Example.Component.Router.Query (Route(..))
import Example.Control.Monad (ExampleM)
import Example.DSL.Navigation (navigate)
import Example.DSL.Server (getGreeting)
import Example.DSL.State (getState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prelude (Unit, Void, bind, const, discard, identity, pure, show, ($))

data Query a
  = Initialize a
  | GotoDetails a

-- | The secret number is stored in our global state.
-- | We can change it in the `Details` page.
-- |
-- | The greeting is obtained through our server "api".
type State =
 { secretNumber :: Maybe Int
 , greeting     :: String
 }

component :: H.Component HH.HTML Query Unit Void ExampleM
component =
  H.lifecycleComponent
    { initialState: const { secretNumber: Nothing, greeting: "" }
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
      [ HH.h1_ [ HH.text st.greeting ]
      , HH.div_ [ HH.text $ showSecretNumber st.secretNumber ]
      , HH.button
          [ HE.onClick (HE.input_ GotoDetails) ]
          [ HH.text "Go to details" ]
      ]

    where

    showSecretNumber :: Maybe Int -> String
    showSecretNumber Nothing  = "Not loaded yet"
    showSecretNumber (Just n) = show n

  -- | We are able to use `getState`, `getGreeting` and `navigate` here
  -- | from our app's DSLs / free monad.
  eval :: Query ~> H.ComponentDSL State Query Void ExampleM
  eval (Initialize next) = do
    number <- getState
    greetingResult <- getGreeting
    let greeting = either (const "error") identity greetingResult
    H.put { secretNumber: Just number, greeting: greeting }
    pure next
  eval (GotoDetails next) = do
    navigate Details
    pure next
