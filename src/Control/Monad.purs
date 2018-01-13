module Example.Control.Monad
  ( Example
  , ExampleM (..)
  , ExampleF (..)
  , EffectType
  , PushType
  , Environment
  , runExample
  ) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Reader (class MonadAsk)
import Example.Component.Router.Query (Route)
import Example.DSL.Navigation (class NavigationDSL)
import FRP (FRP)
import Halogen.Aff (HalogenEffects)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, type (~>), Unit, discard, flip, id, pure, unit, ($), (<<<))

type EffectType = 
  ( console :: CONSOLE
  , frp     :: FRP
  )

type Environment = Int

type Example = ExampleM (HalogenEffects EffectType) Environment

data ExampleF (eff :: # Effect) env a
  = Ask (env -> a)
  | Navigate Route a

newtype ExampleM eff env a = ExampleM (Free (ExampleF eff env) a)

unExampleM :: forall eff env. ExampleM eff env ~> Free (ExampleF eff env)
unExampleM (ExampleM e) = e

derive newtype instance functorExampleM :: Functor (ExampleM eff env)
derive newtype instance applyExampleM :: Apply (ExampleM eff env)
derive newtype instance applicativeExampleM :: Applicative (ExampleM eff env)
derive newtype instance bindExampleM :: Bind (ExampleM eff env)
derive newtype instance monadExampleM :: Monad (ExampleM eff env)

instance monadAskAlerterM :: MonadAsk env (ExampleM eff env) where
  ask = ExampleM <<< liftF <<< Ask $ id

instance navigationDSLAlerterM :: NavigationDSL (ExampleM eff env) where
  navigate = ExampleM <<< liftF <<< flip Navigate unit


type PushType = Route

type PushHandler = PushType -> Eff (HalogenEffects EffectType) Unit

runExample :: Environment -> PushHandler -> Example ~> Aff (HalogenEffects EffectType)
runExample env push = foldFree go <<< unExampleM

   where

   go :: ExampleF (HalogenEffects EffectType) Environment ~> Aff (HalogenEffects EffectType)
   go = case _ of
     Navigate route a -> do
       liftEff <<< push $ route
       pure a
     Ask k ->
       pure (k env)