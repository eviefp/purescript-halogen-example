module Example.Component.Router.Query
  ( Route (..)
  , Query (..)
  ) where

import Prelude (class Eq, class Ord)

data Route
  = Home
  | Details

derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

data Query a 
  = Goto Route a