module Example.Server.ServerAPI
  ( getGreetingImpl
  , secretKey
  , APIToken (..)
  ) where

import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Prelude (otherwise, pure, ($), (<<<), (==))

-- | This is supposed to represent an API token that is secret within the application.
-- | It may very well be a token or any sort of resource we get from an authentication API.
data APIToken = APIToken String

-- | We hardcode this here since we're not really using any server API.
secretKey :: String
secretKey = "Secret-ey secret"

-- | As most APIs, it requires some input, which we represent as an `APIToken`. We can test
-- | this by replacing the token we send to `runExample` in `main`..
getGreetingImpl :: forall eff. APIToken -> Aff eff (Either String String)
getGreetingImpl (APIToken token)
  | token == secretKey = pure <<< pure $ "hello world"
  | otherwise          = pure <<< Left $ "error"