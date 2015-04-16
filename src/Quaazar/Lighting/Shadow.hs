{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Quaazar.Lighting.Shadow (
    -- * Shadows
    ShadowLOD(..)
  ) where

import Data.Aeson

-- |'ShadowLOD' represents the level of detail of a shadow. There’s currently
-- three levels of detail:
--
--   - 'LowShadow' is a /low-detail/ shadow. You should use that LOD when you
--     don’t need details (distant shadows or very big shadows);
--   - 'MediumShadow' is a /medium-detail/ shadow. Used for common shadowing;
--   - 'HighShadow' is a /high-detail/ shadow. You can use that LOD for close
--     and/or small shadows the user might see pretty well.
data ShadowLOD
  = LowShadow
  | MediumShadow
  | HighShadow
    deriving (Eq,Ord,Read,Show)

instance FromJSON ShadowLOD where
  parseJSON = withText "shadow level of detail" parseText
    where
      parseText t
        | t == "low"    = return LowShadow
        | t == "medium" = return MediumShadow
        | t == "high"   = return HighShadow
        | otherwise     = fail "unknown shadow level of detail"
