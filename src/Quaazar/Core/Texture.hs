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

module Quaazar.Core.Texture (
    -- * Texture
    Texture(Texture)
  , texWidth
  , texHeight
  , texFormat
  , texTexels
    -- * Texel format
  , TexelFormat(..)
  ) where

import Codec.Picture
import Control.Monad.Error.Class ( MonadError(..) )
import Control.Lens
import Data.Vector ( Vector, fromList )
import Data.Vector.Storable ( toList )
import Numeric.Natural ( Natural )
import Quaazar.Utils.Log

-- |A texture gathers texels (encoded within a specific format). Textures have a
-- /width/ and a /height/.
data Texture = Texture {
    -- |Width (in texels).
    _texWidth  :: Natural
    -- |Height (in texels).
  , _texHeight :: Natural
    -- |Texture format. See 'TexelFormat' for further information.
  , _texFormat :: TexelFormat
    -- |Texels.
  , _texTexels :: Vector Float
  } deriving (Eq,Show)

-- |Possible format for a texel.
data TexelFormat
  = R
  | RG
  | RGB
  | RGBA
    deriving (Eq,Ord,Show)

imageToTexture :: (MonadError Log m) => DynamicImage -> m Texture
imageToTexture dynim = case dynim of
    ImageY8 img -> return $ fromGreyscale imax8 img
    ImageY16 img -> return $ fromGreyscale imax16 img
    {-
    ImageYF img -> fromPixelF img
    ImageYA8 img -> fromPixelYA8 img
    ImageYA16 img -> fromPixelYA16 img
    ImageRGB8 img -> fromPixelRGB8 img
    ImageRGB16 img -> fromPixelRGB16 img
    ImageRGBA8 img -> fromPixelRGBA8 img
    ImgaeRGBA16 img -> fromPixelRGBA16 img
    -}
    _ -> throwError_ "unimplemented image format"
  where
    imax8 = 1 / 255
    imax16 = 1 / realToFrac (maxBound :: Pixel16)

fromGreyscale :: (Pixel a,Real (PixelBaseComponent a)) => Float -> Image a -> Texture
fromGreyscale imax (Image w h pixels) =
    Texture (fromIntegral w) (fromIntegral h) R pixels'
  where
    pixels' = fromList . fmap ((*imax) . realToFrac) $ toList pixels

throwError_ :: (MonadError Log m) => String -> m a
throwError_ = throwError . Log ErrorLog CoreLog

makeLenses ''Texture
