-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Photon.Render.Semantics (
    -- *
  ) where

lightCastShadowsSem :: Int
lightCastShadowsSem = 0

lightColorSem :: Int
lightColorSem = 1

lightPowerSem :: Int
lightPowerSem = 2

lightRadiusSem :: Int
lightRadiusSem = 3

lightTypeSem :: Int
lightTypeSem = 4
