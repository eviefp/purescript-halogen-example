module Example.DSL.Server
  ( class ServerDSL
  , getGreeting
  ) where

import Data.Either (Either)
import Halogen (HalogenM, lift)
import Prelude (class Monad)

-- | Simple Server API DSL. If we have more server calls, we can just add to this
-- | list here. As an alternative, please check `purescript-affjax-algebra`.
-- |
-- | The `getGreeting` implementation looks similar, but requires an `APIToken`.
-- | We will provide this in our `runExample` transform instead of passing it around
-- | everywhere in our components.
class Monad m <= ServerDSL m where
  getGreeting :: m (Either String String)

-- | We need a HalogenM instance in order to be able to use this DSL
-- | within our component's `eval`.
instance serverDSLHalogenM :: ServerDSL m => ServerDSL (HalogenM s f g p o m) where
  getGreeting = lift getGreeting