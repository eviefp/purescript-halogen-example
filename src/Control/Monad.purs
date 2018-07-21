module Example.Control.Monad where

import Prelude

import Control.Monad.Reader (ask, asks, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT)
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
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

-- | Our environment for `MonadAsk`.
type Environment =
  { token :: APIToken
  , push :: PushType -> Effect Unit
  , answer :: Int
  , state :: Ref GlobalState
  }

-- | Our state for `StateDSL`.
type GlobalState = Int

-- | Our environment will contain everything we need to implement our monad, so we use it
-- | as our environment in a ReaderT monad stack.
newtype ExampleM a = ExampleM (ReaderT Environment Aff a)
derive instance newtypeExampleM :: Newtype (ExampleM a) _

-- | Helper unwrapping function.
runExampleM :: forall a. ExampleM a -> Environment -> Aff a
runExampleM m env = runReaderT (unwrap m) env

-- | Free instances.
derive newtype instance functorExampleM :: Functor ExampleM
derive newtype instance applyExampleM :: Apply ExampleM
derive newtype instance applicativeExampleM :: Applicative ExampleM
derive newtype instance bindExampleM :: Bind ExampleM
derive newtype instance monadExampleM :: Monad ExampleM
derive newtype instance monadEffectExampleM :: MonadEffect ExampleM
derive newtype instance monadAffExampleM :: MonadAff ExampleM

-- | This is, in theory, just a trivial MonadAsk instance.
-- | The only reason it looks a bit more complicated is PureScript does not allow us
-- | to create instances for type synonyms (which includes row types), we have to use
-- | a workaround with TypeEquals, asserting a type `e` is equal to our `Environment`
-- | type synonym.
instance monadAskExampleM :: TE.TypeEquals e Environment => MonadAsk e ExampleM where
  ask = ExampleM $ asks TE.from

-- | Navigate will simply use the `push` part of our environment
-- | to send the new route to the router through our event listener.
instance navigationDSLExampleM :: NavigationDSL ExampleM where
  navigate route = ExampleM do
    env <- ask
    liftEffect $ env.push $ PushRoute route

-- | Encode get/set state. We use the same trick as for `MonadAsk`, using
-- | our environment's `state`.
instance stateDSLExampleM :: TE.TypeEquals st GlobalState => StateDSL st ExampleM where
  getState = ExampleM do
    env <- ask
    liftEffect $ TE.from <$> Ref.read env.state

  modifyState f = ExampleM do
    env <- ask
    liftEffect $ Ref.modify_ (TE.to <<< f <<< TE.from) env.state

-- | This is where we map `ServerDSL` to actual service calls, using the token
-- | stored in our environment.
instance serverDSLExampleM :: ServerDSL ExampleM where
  getGreeting = ExampleM do
    env <- ask
    liftAff $ getGreetingImpl env.token

-- | `PushType` represents what kind of things we can push
-- | to the `main` event handler.
data PushType
  = PushRoute Route
  | PushShowDialog (DialogOptions Aff)

-- | We need to convert our `DialogOptions` and `ActionOptions`
-- | from `ExampleM` to `Aff`, then we push it through the
-- | router using our event handler (similarly to how we do routing).
instance dialogDSLExampleM :: DialogDSL ExampleM ExampleM where
  showDialog opts = ExampleM do
    env <- ask

    let runAction :: ActionOptions ExampleM -> ActionOptions Aff
        runAction a = a { action = runExampleM a.action env }

        runOptions :: DialogOptions ExampleM -> DialogOptions Aff
        runOptions d = d { actions = map runAction d.actions }

    liftEffect <<< env.push <<< PushShowDialog <<< runOptions $ opts
