module Example.Control.Monad
  ( Example
  , ExampleM (..)
  , ExampleF (..)
  , PushType (..)
  , Environment
  , State
  , StateAction
  , runExample
  ) where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Reader (class MonadAsk)
import Example.Component.Router.Query (Route)
import Example.DSL.Dialog (class DialogDSL, DialogOptions, ActionOptions)
import Example.DSL.Navigation (class NavigationDSL)
import Example.DSL.Server (class ServerDSL)
import Example.DSL.State (class StateDSL)
import Example.EffectType (EffectType)
import Example.Server.ServerAPI (APIToken, getGreeting)
import Halogen.Aff (HalogenEffects)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, type (~>), Unit, discard, flip, id, map, pure, unit, ($), (<$>), (<<<))

type Environment = Int

type State = Int

type Example = ExampleM (HalogenEffects EffectType) Environment State

data StateAction st a
  = GetState    (st -> a)
  | ModifyState (st -> st) (Unit -> a)

data ExampleF (eff :: # Effect) env st a
  = Aff (Aff eff a)
  | Ask (env -> a)
  | Navigate Route a
  | State (StateAction st a)
  | ServerAPI (APIToken -> Aff eff a)
  | ShowDialog (DialogOptions (ExampleM eff env st)) a

newtype ExampleM eff env st a = ExampleM (Free (ExampleF eff env st) a)

unExampleM :: forall eff env st. ExampleM eff env st ~> Free (ExampleF eff env st)
unExampleM (ExampleM e) = e

derive newtype instance functorExampleM :: Functor (ExampleM eff env st)
derive newtype instance applyExampleM :: Apply (ExampleM eff env st)
derive newtype instance applicativeExampleM :: Applicative (ExampleM eff env st)
derive newtype instance bindExampleM :: Bind (ExampleM eff env st)
derive newtype instance monadExampleM :: Monad (ExampleM eff env st)

instance monadEffAlerterM :: MonadEff eff (ExampleM eff env st) where
  liftEff = ExampleM <<< liftF <<< Aff <<< liftEff

-- | This instance is simpler since we already have an Aff constructor.
instance monadAffAlerterM :: MonadAff eff (ExampleM eff env st) where
  liftAff = ExampleM <<< liftF <<< Aff

instance monadAskAlerterM :: MonadAsk env (ExampleM eff env st) where
  ask = ExampleM <<< liftF <<< Ask $ id

instance navigationDSLAlerterM :: NavigationDSL (ExampleM eff env st) where
  navigate = ExampleM <<< liftF <<< flip Navigate unit

instance stateDSLExampleM :: StateDSL st (ExampleM eff env st) where
  getState    = ExampleM <<< liftF <<< State <<< GetState $ id
  modifyState = ExampleM <<< liftF <<< State <<< flip ModifyState id

instance serverDSLExampleM :: ServerDSL (ExampleM eff env st) where
  getGreeting = ExampleM <<< liftF <<< ServerAPI $ getGreeting

instance dialogDSLAlerterM :: DialogDSL (ExampleM eff env st) (ExampleM eff env st) where
  showDialog = ExampleM <<< liftF <<< flip ShowDialog unit

data PushType 
  = PushRoute Route
  | PushShowDialog (DialogOptions (Aff (HalogenEffects EffectType)))

type PushHandler = PushType -> Eff (HalogenEffects EffectType) Unit

runExample :: Environment -> Ref State -> PushHandler -> APIToken -> Example ~> Aff (HalogenEffects EffectType)
runExample env state push token = foldFree go <<< unExampleM

   where

   go :: ExampleF (HalogenEffects EffectType) Environment State ~> Aff (HalogenEffects EffectType)
   go = case _ of
    Aff aff -> aff
    Ask k ->
      pure (k env)
    Navigate route a -> do
      liftEff <<< push <<< PushRoute $ route
      pure a
    State action ->
      case action of
        GetState f -> do
          liftEff $ f <$> readRef state
        ModifyState fs f -> do
          liftEff $ f <$> modifyRef state fs
    ServerAPI action -> do
      action token
    ShowDialog opts a -> do
      liftEff <<< push <<< PushShowDialog <<< runOptions $ opts
      pure a

      where

      runOptions :: DialogOptions Example -> DialogOptions (Aff (HalogenEffects EffectType))
      runOptions d = d { actions = map runAction d.actions }

      runAction :: ActionOptions Example -> ActionOptions (Aff (HalogenEffects EffectType))
      runAction a = a { action = foldFree go <<< unExampleM $ a.action }