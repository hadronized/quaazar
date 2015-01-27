-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Make a better world with a colored-one! This module exports a very
-- straight-forward type, 'Color', which is a 4-float vector.
--
-- > For now, any vector-related function/type uses "linear"’s vectors.
----------------------------------------------------------------------------

module Quaazar.Core.Color (
    -- * Color
    Color(unColor)
  , color
  , colorR
  , colorG
  , colorB
  ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Types ( typeMismatch )
import Linear.V3

-- |A color is a 3-float vector. The four channels are:
--
--   - *red*
--   - *green*
--   - *blue*
newtype Color = Color { unColor :: V3 Float } deriving (Eq,Ord,Show)

instance FromJSON Color where
  parseJSON v = do
    a' <- parseJSON v
    case a' of
      [r,g,b] -> return (color r g b)
      _         -> typeMismatch "color" v

color :: Float -> Float -> Float -> Color
color r g b = Color (V3 r g b)

colorR :: Color -> Float
colorR (Color c) = c^._x

colorG :: Color -> Float
colorG (Color c) = c^._y

colorB :: Color -> Float
colorB (Color c) = c^._z
