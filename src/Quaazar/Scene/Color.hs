-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
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

module Quaazar.Scene.Color (
    -- * Color
    Color(unColor)
  , color
  , colorR
  , colorG
  , colorB
    -- * Coder-colors
  , white
  , black
  , red
  , green
  , blue
  , yellow
  , magenta
  , cyan
    -- * Bright coder-colors
  , brightRed
  , brightGreen
  , brightBlue
  , brightYellow
  , brightMagenta
  , brightCyan
  ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Types ( typeMismatch )
import Foreign.Storable ( Storable )
import Linear.V3

-- |A color is a 3-float vector. The four channels are:
--
--   - *red*
--   - *green*
--   - *blue*
newtype Color = Color { unColor :: V3 Float } deriving (Eq,Ord,Show,Storable)

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

white :: Color
white = color 1 1 1

black :: Color
black = color 0 0 0

red :: Color
red = color 1 0 0

brightRed :: Color
brightRed = color 1 0.5 0.5

green :: Color
green = color 0 1 0

brightGreen :: Color
brightGreen = color 0.5 1 0.5

blue :: Color
blue = color 0 0 1

brightBlue :: Color
brightBlue = color 0.5 0.5 1

yellow :: Color
yellow = color 1 1 0

brightYellow :: Color
brightYellow = color 1 1 0.5

magenta :: Color
magenta = color 1 0 1

brightMagenta :: Color
brightMagenta = color 1 0.5 1

cyan :: Color
cyan = color 0 1 1

brightCyan :: Color
brightCyan = color 0.5 1 1
