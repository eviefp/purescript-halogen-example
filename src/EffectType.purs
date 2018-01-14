module Example.EffectType
  ( EffectType
  ) where

import FRP (FRP)

-- | We only use the `purescript-behavior`'s `FRP` on top of the halogen effects
type EffectType = (frp :: FRP)