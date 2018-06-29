module Example.Control.Monad where

import Prelude

import Control.Monad.Reader (ask, asks, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Parallel.Class (class Parallel, parallel, sequential)
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Aff (Aff, ParAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Example.Component.Router.Query (Route)
import Example.DSL.Dialog (class DialogDSL, DialogOptions, ActionOptions)
import Example.DSL.Navigation (class NavigationDSL)
import Example.DSL.Server (class ServerDSL)
import Example.DSL.State (class StateDSL)
import Example.Server.ServerAPI (APIToken, getGreetingImpl)
import Type.Equality as TE

-- | Our Environmentironment for `MonadAsk`.
type Environment =
  { token :: APIToken
  , push :: PushType -> Effect Unit
  , answer :: Int
  , state :: Ref GlobalState
  }

-- | Our state for `StateDSL`.
type GlobalState = Int

newtype ExampleM a = ExampleM (ReaderT Environment Aff a)

-- | Helper unwrap function.
runExampleM :: forall a. ExampleM a -> Environment -> Aff a
runExampleM m env = runReaderT (unwrap m) env

-- | Free instances
derive instance newtypeExampleM :: Newtype (ExampleM a) _
derive newtype instance functorExampleM :: Functor ExampleM
derive newtype instance applyExampleM :: Apply ExampleM
derive newtype instance applicativeExampleM :: Applicative ExampleM
derive newtype instance bindExampleM :: Bind ExampleM
derive newtype instance monadExampleM :: Monad ExampleM
derive newtype instance monadEffectExampleM :: MonadEffect ExampleM
derive newtype instance monadAffExampleM :: MonadAff ExampleM

-- | Applicative version of the application monad over ParAff to
-- | support parallel computations
newtype ExampleA a = ExampleA (ReaderT Environment ParAff a)

derive instance newtypeExampleA :: Newtype (ExampleA a) _
derive newtype instance functorExampleA :: Functor ExampleA
derive newtype instance applyExampleA :: Apply ExampleA
derive newtype instance applicativeExampleA :: Applicative ExampleA

-- | Hand-written instances for ExampleM

-- | Allow parallel computations like parTraverse
instance parallelExampleM :: Parallel ExampleA ExampleM where
  parallel (ExampleM m) = ExampleA $ parallel m
  sequential (ExampleA m) = ExampleM $ sequential m

-- | Allow access to Reader
instance monadAskExampleM :: TE.TypeEquals e Environment => MonadAsk e ExampleM where
  ask = ExampleM $ asks TE.from

-- | The route is stored through the pointfree syntax.
-- | Store `unit` in our `a` so we can return something after we navigate.
instance navigationDSLExampleM :: NavigationDSL ExampleM where
  navigate route = ExampleM do
    env <- ask
    liftEffect $ env.push $ PushRoute route

-- | Encode get/set state
instance stateDSLExampleM :: TE.TypeEquals st GlobalState => StateDSL st ExampleM where
  getState = ExampleM do
    env <- ask
    liftEffect $ TE.from <$> Ref.read env.state

  modifyState f = ExampleM do
    env <- ask
    liftEffect $ Ref.modify_ (TE.to <<< f <<< TE.from) env.state

-- | This is where we map `ServerDSL` to actual service calls.
instance serverDSLExampleM :: ServerDSL ExampleM where
  getGreeting = ExampleM do
    env <- ask
    liftAff $ getGreetingImpl env.token

-- | `PushType` represents what kind of things we can push
-- | to the `main` event handler.
data PushType
  = PushRoute Route
  | PushShowDialog (DialogOptions Aff)

instance dialogDSLExampleM :: DialogDSL ExampleM ExampleM where
  showDialog opts = ExampleM do
    env <- ask

    let runAction :: ActionOptions ExampleM -> ActionOptions Aff
        runAction a = a { action = runExampleM a.action env }

        runOptions :: DialogOptions ExampleM -> DialogOptions Aff
        runOptions d = d { actions = map runAction d.actions }

    liftEffect <<< env.push <<< PushShowDialog <<< runOptions $ opts
