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

module Photon.Core.Texture (
    -- *
  ) where

import Control.Lens
import Numeric.Natural ( Natural )

-- |A texture gathers texels (encoded with a specific format). Textures have
-- a /width/ and a /height/.
data Texture = Texture {
    -- |Width (in texels).
    _texWidth  :: Natural
    -- |Height (in texels).
  , _texHeight :: Natural
    -- |Texture format. See 'TexelFormat' for further information.
  , _texFormat :: TexelFormat
    -- |Texels.
  , _texTexels :: [Float]
  } deriving (Eq,Show)

-- |Possible format for a texel.
data TexelFormat
  = R
  | RG
  | RGB
  | RGBA
    deriving (Eq,Ord,Show)
