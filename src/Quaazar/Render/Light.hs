-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Quaazar.Render.Light where

import Control.Lens
import Control.Monad.State ( State, get, put )
import Numeric.Natural ( Natural )
import Quaazar.Core.Light ( Omni(..), ShadowLOD(..) )

-- |Shadow configuration. Holds for each shadow level of detail the available
-- textures number and their resolutions.
data ShadowConf = ShadowConf {
    _lowShadowMaxNb    :: Natural
  , _lowShadowSize     :: Natural
  , _mediumShadowMaxNb :: Natural
  , _mediumShadowSize  :: Natural
  , _highShadowMaxNb   :: Natural
  , _highShadowSize    :: Natural
  }

makeLenses ''ShadowConf

-- |Extract from an omnidirectional light and the current shadowmaps pool the
-- level of detail to use and the shadowmap index. After calling that function,
-- the pool might be modified.
--
-- If a given pool is empty, the next lower pool is tried until the lowest pool
-- is empty, then shadows are disabled for all remaining lights.
addShadowInfo :: Natural -> Natural -> Natural -> Omni -> State (Natural,Natural,Natural) (Omni,Natural)
addShadowInfo lmax mmax hmax omni@(Omni _ _ _ lod) = case lod of
    Nothing -> noShadows
    Just lod' -> do
      (l,m,h) <- get
      case lod' of
        LowShadow -> getLow l m h
        MediumShadow -> getMedium l m h
        HighShadow -> getHigh l m h
  where
    noShadows = return (omni,0)
    getLow l m h
      | l < lmax = put (succ l,m,h) >> return (omni,l)
      | otherwise = noShadows
    getMedium l m h
      | m < mmax = put (l,succ m,h) >> return (omni,m)
      | otherwise = getLow l m h
    getHigh l m h
      | h < hmax = put (l,m,succ h) >> return (omni,h)
      | otherwise = getMedium l m h 
