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

module Photon.Core.Color (
    -- * Color
    Color(unColor)
  , color
  , colorR
  , colorG
  , colorB
  , colorA
  ) where

import Control.Lens
import Data.Aeson
import Linear.V4

-- |A color is a 4-float vector. The four channels are:
--
--   - *red*
--   - *green*
--   - *blue*
--   - *alpha*
newtype Color = Color { unColor :: V4 Float } deriving (Eq,Ord,Show)

instance FromJSON Color where
  parseJSON v = do
    [r,g,b,a] <- parseJSON v
    return $ color r g b a

color :: Float -> Float -> Float -> Float -> Color
color r g b a = Color (V4 r g b a)

colorR :: Color -> Float
colorR (Color c) = c^._x

colorG :: Color -> Float
colorG (Color c) = c^._y

colorB :: Color -> Float
colorB (Color c) = c^._z

colorA :: Color -> Float
colorA (Color c) = c^._w
