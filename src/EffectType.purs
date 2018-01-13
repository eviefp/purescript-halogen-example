module Example.EffectType
  ( EffectType
  ) where

import Control.Monad.Eff.Console (CONSOLE)
import FRP (FRP)

type EffectType = 
  ( console :: CONSOLE
  , frp     :: FRP
  )