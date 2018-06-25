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

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Reader (class MonadAsk)
import Example.Component.Router.Query (Route)
import Example.DSL.Dialog (class DialogDSL, DialogOptions, ActionOptions)
import Example.DSL.Navigation (class NavigationDSL)
import Example.DSL.Server (class ServerDSL)
import Example.DSL.State (class StateDSL)
import Example.Server.ServerAPI (APIToken, getGreetingImpl)

-- | Our environment for `MonadAsk`.
type Environment = Int

-- | Our state for `StateDSL`.
type State = Int

-- | Our free monad with types applied to it for effects,
-- | environment and state.
type Example = ExampleM Environment State

-- | Helper type for `StateDSL`.
data StateAction st a
  = GetState    (st -> a)
  | ModifyState (st -> st) (Unit -> a)

-- | Our free functor:
-- | `Aff` for `MonadEffect` and `MonadAff`
-- | `Ask` for `MonadAsk` where the encoded function is `id`
-- | `Navigate` for `NavigationDSL` where we encode the route and `unit`
-- |   in order to be able to derive a functor instance.
-- | `State` for `StateDSL` where we encode get and modify using `StateAction`
-- | `ServerAPI` for `ServerDSL` where we encode calls to a function that
-- |   takes a token and returns an `Aff`.
-- | `ShowDialog` for `DialogDSL` where we store `DialogOptions` that run under
-- |   the `ExampleM monad`.
data ExampleF env st a
  = Aff (Aff a)
  | Ask (env -> a)
  | Navigate Route a
  | State (StateAction st a)
  | ServerAPI (APIToken -> Aff a)
  | ShowDialog (DialogOptions (ExampleM env st)) a

-- | Create a Free monad from our Functor.
newtype ExampleM env st a = ExampleM (Free (ExampleF env st) a)

-- | Helper unwrap function.
unExampleM :: forall env st. ExampleM env st ~> Free (ExampleF env st)
unExampleM (ExampleM e) = e

-- | Free instances thanks to `Free`.
derive newtype instance functorExampleM :: Functor (ExampleM env st)
derive newtype instance applyExampleM :: Apply (ExampleM env st)
derive newtype instance applicativeExampleM :: Applicative (ExampleM env st)
derive newtype instance bindExampleM :: Bind (ExampleM env st)
derive newtype instance monadExampleM :: Monad (ExampleM env st)

-- | This instance is trivial since we already have an Aff constructor.
instance monadEffectAlerterM :: MonadEffect (ExampleM env st) where
  liftEffect = ExampleM <<< liftF <<< Aff <<< liftEffect

-- | This instance is also trivial since we already have an Aff constructor.
instance monadAffAlerterM :: MonadAff (ExampleM env st) where
  liftAff = ExampleM <<< liftF <<< Aff

-- | We throw `id` into our `Ask` functor and we apply it to the
-- | actual environment in our `runExample` transform.
instance monadAskAlerterM :: MonadAsk env (ExampleM env st) where
  ask = ExampleM <<< liftF <<< Ask $ identity

-- | The route is stored through the pointfree syntax.
-- | Store `unit` in our `a` so we can return something after we navigate.
instance navigationDSLAlerterM :: NavigationDSL (ExampleM env st) where
  navigate = ExampleM <<< liftF <<< flip Navigate unit

-- | Encode get/set state using `StateAction`. Each store `id` similarly to
-- | the `AskMonad` instance.
instance stateDSLExampleM :: StateDSL st (ExampleM env st) where
  getState    = ExampleM <<< liftF <<< State <<< GetState $ identity
  modifyState = ExampleM <<< liftF <<< State <<< flip ModifyState identity

-- | This is where we map `ServerDSL` to actual service calls.
instance serverDSLExampleM :: ServerDSL (ExampleM env st) where
  getGreeting = ExampleM <<< liftF <<< ServerAPI $ getGreetingImpl

-- | We store the `DialogOptions` and `unit`, similarly to what we do
-- | for `NavigationDSL`.
instance dialogDSLAlerterM :: DialogDSL (ExampleM env st) (ExampleM env st) where
  showDialog = ExampleM <<< liftF <<< flip ShowDialog unit

-- | `PushType` represents what kind of things we can push
-- | to the `main` event handler.
data PushType
  = PushRoute Route
  | PushShowDialog (DialogOptions Aff)

-- | Helper type from `PushType` to an effect (which is, pushing that type
-- | to main).
type PushHandler = PushType -> Effect Unit

-- | Our natural transform which needs a few prerequisites. Please see `main`
-- | for a description of these.
runExample :: Environment -> Ref State -> PushHandler -> APIToken -> Example ~> Aff
runExample env state push token = foldFree go <<< unExampleM

   where

-- | This is where we define how we translate our Functor to an effect.
   go :: ExampleF Environment State ~> Aff
   go = case _ of
    Aff aff -> aff
    Ask k ->
      pure (k env)
    Navigate route a -> do
      liftEffect <<< push <<< PushRoute $ route
      pure a
    State action ->
      case action of
        GetState f -> do
          liftEffect $ f <$> Ref.read state
        ModifyState fs f -> do
          liftEffect $ f <$> Ref.modify_ fs state
    ServerAPI action -> do
      action token
    ShowDialog opts a -> do
      liftEffect <<< push <<< PushShowDialog <<< runOptions $ opts
      pure a

      where

      runOptions :: DialogOptions Example -> DialogOptions Aff
      runOptions d = d { actions = map runAction d.actions }

-- | We need to translate our contained `Example` to an `Aff` here, just like we do for our component(s).
      runAction :: ActionOptions Example -> ActionOptions Aff
      runAction a = a { action = foldFree go <<< unExampleM $ a.action }
