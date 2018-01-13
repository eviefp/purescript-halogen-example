module Example.Control.Monad
  ( Example
  , ExampleM (..)
  , ExampleF (..)
  , EffectType
  , PushType
  , Environment
  , State
  , StateAction
  , runExample
  ) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Reader (class MonadAsk)
import Example.Component.Router.Query (Route)
import Example.DSL.Navigation (class NavigationDSL)
import Example.DSL.State (class StateDSL)
import FRP (FRP)
import Halogen.Aff (HalogenEffects)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, type (~>), Unit, discard, flip, id, pure, unit, ($), (<$>), (<<<))

type EffectType = 
  ( console :: CONSOLE
  , frp     :: FRP
  )

type Environment = Int

type State = Int

type Example = ExampleM (HalogenEffects EffectType) Environment State

data StateAction st a
  = GetState    (st -> a)
  | ModifyState (st -> st) (Unit -> a)

data ExampleF (eff :: # Effect) env st a
  = Ask (env -> a)
  | Navigate Route a
  | State (StateAction st a)

newtype ExampleM eff env st a = ExampleM (Free (ExampleF eff env st) a)

unExampleM :: forall eff env st. ExampleM eff env st ~> Free (ExampleF eff env st)
unExampleM (ExampleM e) = e

derive newtype instance functorExampleM :: Functor (ExampleM eff env st)
derive newtype instance applyExampleM :: Apply (ExampleM eff env st)
derive newtype instance applicativeExampleM :: Applicative (ExampleM eff env st)
derive newtype instance bindExampleM :: Bind (ExampleM eff env st)
derive newtype instance monadExampleM :: Monad (ExampleM eff env st)

instance monadAskAlerterM :: MonadAsk env (ExampleM eff env st) where
  ask = ExampleM <<< liftF <<< Ask $ id

instance navigationDSLAlerterM :: NavigationDSL (ExampleM eff env st) where
  navigate = ExampleM <<< liftF <<< flip Navigate unit

instance stateDSLExampleM :: StateDSL st (ExampleM eff env st) where
  getState    = ExampleM <<< liftF <<< State <<< GetState $ id
  modifyState = ExampleM <<< liftF <<< State <<< flip ModifyState id


type PushType = Route

type PushHandler = PushType -> Eff (HalogenEffects EffectType) Unit

runExample :: Environment -> Ref State -> PushHandler -> Example ~> Aff (HalogenEffects EffectType)
runExample env state push = foldFree go <<< unExampleM

   where

   go :: ExampleF (HalogenEffects EffectType) Environment State ~> Aff (HalogenEffects EffectType)
   go = case _ of
     Navigate route a -> do
       liftEff <<< push $ route
       pure a
     Ask k ->
       pure (k env)
     State action ->
       case action of
         GetState f -> do
           liftEff $ f <$> readRef state
         ModifyState fs f -> do
           liftEff $ f <$> modifyRef state fs