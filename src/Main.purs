module Main where

import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (newRef)
import Example.Component.Router as R
import Example.Component.Router.Query (Query(..))
import Example.Control.Monad (EffectType, PushType, runExample)
import FRP.Event (create, subscribe)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, Void, bind, pure, unit, ($), (<<<))

main :: Eff (HA.HalogenEffects EffectType) Unit
main = HA.runHalogenAff do
  body  <- HA.awaitBody

  state <- liftEff <<< newRef $ 0
  event <- liftEff create

  let router' = H.hoist (runExample 42 state event.push) R.component
  driver <- runUI router' unit body
  liftEff $ subscribe event.event (handler driver)

  where

  handler :: H.HalogenIO Query Void (Aff (HA.HalogenEffects EffectType))
          -> PushType
          -> Eff (HA.HalogenEffects EffectType) Unit
  handler driver route = do
    _ <- launchAff $ driver.query <<< H.action <<< Goto $ route
    pure unit