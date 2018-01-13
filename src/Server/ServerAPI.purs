module Example.Server.ServerAPI
  ( getGreeting
  , secretKey
  , APIToken (..)
  ) where

import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Prelude (otherwise, pure, ($), (<<<), (==))

data APIToken = APIToken String

secretKey :: String
secretKey = "Secret-ey secret"

getGreeting :: forall eff. APIToken -> Aff eff (Either String String)
getGreeting (APIToken token)
  | token == secretKey = pure <<< pure $ "hello world"
  | otherwise          = pure <<< Left $ "error"