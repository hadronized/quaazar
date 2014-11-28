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
    -- * Texture
    Texture(Texture)
  , texWidth
  , texHeight
  , texFormat
  , texTexels
    -- * Texture format
  , TextureFormat(..)
    -- * Reaction
  , TextureSpawned(..)
  , TextureLost(..)
  , TextureEffect(..)
  , changeWidth
  , changeHeight
  , changeFormat
  , changeTexels
  ) where

import Control.Lens
import Numeric.Natural ( Natural )
import Photon.Core.Effect

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

-- TODO: add loader from disk image here

newtype TextureSpawned = TextureSpawned (Managed Texture) deriving (Eq,Show)

newtype TextureLost = TextureLost (Managed Texture) deriving (Eq,Show)

data TextureEffect
  = WidthChanged (Managed Texture) Natural
  | HeightChanged (Managed Texture) Natural
  | FormatChanged (Managed Texture) TexelFormat
  | TexelsChanged (Managed Texture) [Float]
    deriving (Eq,Show)

instance EffectfulManage Texture TextureSpawned TextureLost where
  spawned = TextureSpawned
  lost = TextureLost

changeWidth :: (Effect TextureEffect m)
            => Managed Texture
            -> (Natural -> Natural)
            -> m (Managed Texture)
changeWidth t f = do
    react (WidthChanged t nw)
    return (t & managed . texWidth .~ nw)
  where nw = f (t^.managed.texWidth)

changeHeight :: (Effect TextureEffect m)
             => Managed Texture
             -> (Natural -> Natural)
             -> m (Managed Texture)
changeHeight t f = do
    react (HeightChanged t nh)
    return (t & managed . texHeight .~ nh)
  where nh = f (t^.managed.texHeight)

changeFormat :: (Effect TextureEffect m)
             => Managed Texture
             -> (TextureFormat -> TextureFormat)
             -> m (Managed Texture)
changeFormat t f = do
    react (FormatChanged t nf)
    return (t & managed . texFormat .~ nf)
  where nf = f (t^.managed.texFormat)

changeTexels :: (Effect TextureEffect m)
             => Managed Texture
             -> ([Float] -> [Float])
             -> m (Managed Texture)
changeTexels t f = do
    react (TexelsChanged t nt)
    return (t & managed . texTexels .~ nt)
  where nt = f (t^.managed.texTexels)
