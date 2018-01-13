module Example.Component.Router 
  ( component
  ) where

import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Example.Component.Details as Details
import Example.Component.Home as Home
import Example.Component.Router.Query (Route(..), Query(..))
import Example.Control.Monad (Example)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Prelude (Unit, Void, absurd, const, discard, pure, unit)


-- | Query algebra for direct children of the router component, represented as
-- | a Coproduct.
type ItemQuery = Coproduct2 Home.Query Details.Query

-- | Slot type for router items.
type ItemSlot = Either2 Unit Unit

type State = Route

-- | Router component.
component :: H.Component HH.HTML Query Unit Void Example
component
  = H.parentComponent
    { initialState: const Home
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ParentHTML Query ItemQuery ItemSlot Example
  render = renderRoute 

    where

    renderRoute :: Route -> H.ParentHTML Query ItemQuery ItemSlot Example
    renderRoute = case _ of
      Home    -> HH.slot' CP.cp1 unit Home.component    unit absurd
      Details -> HH.slot' CP.cp2 unit Details.component unit absurd

  eval :: Query ~> H.ParentDSL State Query ItemQuery ItemSlot Void Example
  eval (Goto route next) = do
    H.put route
    pure next