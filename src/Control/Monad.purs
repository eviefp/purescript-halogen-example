module Example.Control.Monad where

import Prelude

import Control.Monad.Reader (ask, runReaderT)
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

-- | Our Environmentironment for `MonadAsk`.
newtype Environment = Environment
  { token :: APIToken
  , push :: PushType -> Effect Unit
  , answer :: Int
  , state :: Ref GlobalState
  }
derive instance newtypeEnvironment :: Newtype Environment _

-- | Our state for `StateDSL`.
newtype GlobalState = GlobalState Int
derive instance newtypeState :: Newtype GlobalState _

newtype ExampleM a = ExampleM (ReaderT Environment Aff a)
derive instance newtypeExampleM :: Newtype (ExampleM a) _

-- | Helper unwrap function.
runExampleM :: forall a. ExampleM a -> Environment -> Aff a
runExampleM m env = runReaderT (unwrap m) env

-- | Free instances
derive newtype instance functorExampleM :: Functor ExampleM
derive newtype instance applyExampleM :: Apply ExampleM
derive newtype instance applicativeExampleM :: Applicative ExampleM
derive newtype instance bindExampleM :: Bind ExampleM
derive newtype instance monadExampleM :: Monad ExampleM
derive newtype instance monadEffectExampleM :: MonadEffect ExampleM
derive newtype instance monadAffExampleM :: MonadAff ExampleM
derive newtype instance monadAskExampleM :: MonadAsk Environment ExampleM

-- | The route is stored through the pointfree syntax.
-- | Store `unit` in our `a` so we can return something after we navigate.
instance navigationDSLExampleM :: NavigationDSL ExampleM where
  navigate route = ExampleM do
    env <- unwrap <$> ask
    liftEffect $ env.push $ PushRoute route

-- | Encode get/set state
instance stateDSLExampleM :: StateDSL GlobalState ExampleM where
  getState = ExampleM do
    env <- unwrap <$> ask
    liftEffect $ Ref.read env.state

  modifyState f = ExampleM do
    env <- unwrap <$> ask
    liftEffect $ Ref.modify_ f env.state

-- | This is where we map `ServerDSL` to actual service calls.
instance serverDSLExampleM :: ServerDSL ExampleM where
  getGreeting = ExampleM do
    env <- unwrap <$> ask
    liftAff $ getGreetingImpl env.token

-- | `PushType` represents what kind of things we can push
-- | to the `main` event handler.
data PushType
  = PushRoute Route
  | PushShowDialog (DialogOptions Aff)

instance dialogDSLExampleM :: DialogDSL ExampleM ExampleM where
  showDialog opts = ExampleM do
    env <- unwrap <$> ask

    let runAction :: ActionOptions ExampleM -> ActionOptions Aff
        runAction a = a { action = runExampleM a.action (Environment env) }

        runOptions :: DialogOptions ExampleM -> DialogOptions Aff
        runOptions d = d { actions = map runAction d.actions }

    liftEffect <<< env.push <<< PushShowDialog <<< runOptions $ opts
