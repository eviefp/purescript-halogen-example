module Example.Control.MonadRun
  where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef')
import Data.Either (Either)
import Data.Functor.Variant (case_, expand, on)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import Example.Component.Router.Query (Route)
import Example.DSL.Dialog (DialogOptions, ActionOptions)
import Example.EffectType (EffectType)
import Example.Server.ServerAPI (APIToken, getGreetingImpl)
import Halogen.Aff (HalogenEffects)
import Prelude (class Functor, type (~>), Unit, discard, flip, id, map, pure, unit, (#), ($), (<$), (<<<))
import Run (AFF, Run, VariantF, lift, runRec)
import Run.Reader (READER, Reader(..))
import Run.State as RunState


----------------------------------------------------------------------
-- NavigateF
----------------------------------------------------------------------
data NavigateF a = NavigateF Route a

derive instance functorNavigateF ∷ Functor NavigateF

type NAVIGATE = FProxy NavigateF

_navigate = SProxy ∷ SProxy "navigate"

navigate ∷ ∀ a r. Route → Run (navigate ∷ NAVIGATE | r) Unit
navigate = lift _navigate <<< flip NavigateF unit

runNavigate ∷ ∀ a. PushHandler → NavigateF a → Aff (HalogenEffects EffectType) a
runNavigate push (NavigateF route next) = next <$ (liftEff <<< push <<< PushRoute $ route)

----------------------------------------------------------------------
-- ServerF
----------------------------------------------------------------------
data ServerF token eff a = ServerF (token → Aff eff a)

derive instance functorServerF ∷ Functor (ServerF token eff)

type SERVER token eff = FProxy (ServerF token eff)

_server = SProxy ∷ SProxy "server"

getGreeting ∷ ∀ eff a r. Run (server ∷ (SERVER APIToken eff) | r) (Either String String)
getGreeting = lift _server <<< ServerF $ getGreetingImpl

runServer ∷ ∀ token eff a. token → ServerF token eff a → Aff eff a
runServer token (ServerF fn) = fn token

----------------------------------------------------------------------
-- ShowDialogF
----------------------------------------------------------------------
data ShowDialogF m a = ShowDialogF (DialogOptions m) a

derive instance functorShowDialogF ∷ Functor (ShowDialogF m)

type SHOWDIALOG m = FProxy (ShowDialogF m)

_showDialog = SProxy ∷ SProxy "showDialog"

showDialog ∷ ∀ m a r. DialogOptions m → Run (showDialog ∷ (SHOWDIALOG m) | r) Unit
showDialog = lift _showDialog <<< flip ShowDialogF unit

runDialog ∷ ∀ var1 m a. (VariantF var1 ~> Aff (HalogenEffects EffectType)) → PushHandler → ShowDialogF (Run var1) a → Aff (HalogenEffects EffectType) a
runDialog trans push (ShowDialogF opts a) = do
  liftEff <<< push <<< PushShowDialog <<< runOptions $ opts
  pure a

  where

  runOptions ∷ DialogOptions (Run var1) → DialogOptions (Aff (HalogenEffects EffectType))
  runOptions d = d { actions = map runAction d.actions }

  runAction ∷ ActionOptions (Run var1) → ActionOptions (Aff (HalogenEffects EffectType))
  runAction a = a { action = runRec trans $ a.action }

----------------------------------------------------------------------
-- Stuff
----------------------------------------------------------------------

_aff = SProxy :: SProxy "aff"
_reader = SProxy :: SProxy "reader"
_state = SProxy :: SProxy "state"

runReader ∷ ∀ env a eff. env → Reader env a → Aff eff a
runReader env (Reader k) = pure (k env)

runState ∷ ∀ state a eff. Ref state → RunState.State state a → Aff (ref :: REF | eff) a
runState state (RunState.State mod get) = liftEff $ modifyRef' state (fix get mod)
  where
  fix ∷ (state → a) → (state → state) → state → { state ∷ state, value ∷ a}
  fix s2a s2s s = { state: s2s s, value: s2a s }

type Environment = Int
type State = Int

type UnrecursiveRows r =
  ( aff ∷ AFF (HalogenEffects EffectType)
  , reader ∷ READER Environment
  , navigate ∷ NAVIGATE
  , state ∷ RunState.STATE State
  , server ∷ SERVER APIToken (HalogenEffects EffectType)
  | r
  )

type RunRows = UnrecursiveRows (showDialog ∷ SHOWDIALOG (Run (UnrecursiveRows ())))

type Example = Run RunRows

data PushType
  = PushRoute Route
  | PushShowDialog (DialogOptions (Aff (HalogenEffects EffectType)))

-- | Helper type from `PushType` to an effect (which is, pushing that type
-- | to main).
type PushHandler = PushType -> Eff (HalogenEffects EffectType) Unit

runExample ∷ Environment → Ref State → PushHandler → APIToken → Example ~> Aff (HalogenEffects EffectType)
runExample env state push token = runRec go

  where

  go ∷ VariantF RunRows ~> Aff (HalogenEffects EffectType)
  go =
    case_
      # on _aff id
      # on _reader (runReader env)
      # on _navigate (runNavigate push)
      # on _state (runState state)
      # on _server (runServer token)
      # on _showDialog (runDialog go' push)

    where

    go' ∷ VariantF (UnrecursiveRows ()) ~> Aff (HalogenEffects EffectType)
    go' v = go <<< expand $ v
