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
import Codec.Picture.Types
import Control.Monad.Error.Class ( MonadError(..) )
import Control.Monad.Trans ( liftIO )
import Control.Lens
import Data.Bifunctor ( first )
import Data.Either.Combinators ( eitherToError )
import Data.Vector ( Vector, fromList )
import Numeric.Natural ( Natural )
import Quaazar.Core.Loader ( Load(..) )
import Quaazar.Utils.Log
import System.FilePath ( (</>) )

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

instance Load Texture where
  loadRoot = const "textures"
  loadExt = const ""
  load rootPath name = do
      info CoreLog $ "loading texture " ++ show name
      img <- liftIO . fmap (first onError) $
        readImage (rootPath </> loadRoot (undefined :: Texture)  </> name)
      eitherToError img >>= imageToTexture
    where
      onError = Log ErrorLog CoreLog

-- |Possible format for a texel.
data TexelFormat
  = R
  | RG
  | RGB
  | RGBA
    deriving (Eq,Ord,Show)

imageToTexture :: (MonadLogger m,MonadError Log m) => DynamicImage -> m Texture
imageToTexture dynimg = do
  info CoreLog $ "texture type is " ++ showImageFormat dynimg
  case dynimg of
    ImageY8 img -> return $ convertImage_ y8Converter img
    ImageY16 img -> return $ convertImage_ y16Converter img
    ImageYF img -> return $ convertImage_ yfConverter img
    ImageYA8 img -> return $ convertImage_ ya8Converter img
    ImageYA16 img -> return $ convertImage_ ya16Converter img
    ImageRGB8 img -> return $ convertImage_ rgb8Converter img
    ImageRGB16 img -> return $ convertImage_ rgb16Converter img
    ImageRGBF img -> return $ convertImage_ rgbfConverter img
    ImageRGBA8 img -> return $ convertImage_ rgba8Converter img
    ImageRGBA16 img -> return $ convertImage_ rgba16Converter img
    ImageYCbCr8 img -> return $ convertImage_ rgb8Converter (convertImage img)
    _ -> throwError_ "unimplemented image format"
 
showImageFormat :: DynamicImage -> String
showImageFormat dynimg = case dynimg of
  ImageY8{} -> "Y8"
  ImageY16{} -> "Y16"
  ImageYF{} -> "F"
  ImageYA8{} -> "YA8"
  ImageYA16{} -> "YA16"
  ImageRGB8{} -> "RGB8"
  ImageRGB16{} -> "RGB16"
  ImageRGBF{} -> "RGBF"
  ImageRGBA8{} -> "RGBA8"
  ImageRGBA16{} -> "RGBA16"
  ImageYCbCr8{} -> "YCbCr8"
  _ -> "unknown format"

imax8, imax16 :: Float
imax8 = 1 / 255
imax16 = 1 / realToFrac (maxBound :: Pixel16)

convertImage_ :: forall a. (Pixel a)
             => (a -> [Float])
             -> Image a
             -> Texture
convertImage_ converter image@(Image w h _) = Texture w' h' texft pixels'
  where
    w' = fromIntegral w
    h' = fromIntegral h
    texft = case componentCount (undefined :: a) of
      1 -> R
      2 -> RG
      3 -> RGB
      4 -> RGBA
      _ -> error "convertImage_: more-than-4-components pixel"
    pixels' = fromList . concat $ [converter $ pixelAt image x y | y <- [0..h-1], x <- [0..w-1]]

y8Converter :: Pixel8 -> [Float]
y8Converter y = [imax8 * realToFrac y]

y16Converter :: Pixel16 -> [Float]
y16Converter y = [imax16 * realToFrac y]

yfConverter :: PixelF -> [Float]
yfConverter y = [realToFrac y]

ya8Converter :: PixelYA8 -> [Float]
ya8Converter (PixelYA8 y a) = map ((*imax8) . realToFrac) [y,a]

ya16Converter :: PixelYA16 -> [Float]
ya16Converter (PixelYA16 y a) = map ((*imax16) . realToFrac) [y,a]

rgb8Converter :: PixelRGB8 -> [Float]
rgb8Converter (PixelRGB8 r g b) = map ((*imax8) . realToFrac ) [r,g,b]

rgb16Converter :: PixelRGB16 -> [Float]
rgb16Converter (PixelRGB16 r g b) = map ((*imax16) . realToFrac ) [r,g,b]

rgbfConverter :: PixelRGBF -> [Float]
rgbfConverter (PixelRGBF r g b) = map realToFrac [r,g,b]

rgba8Converter :: PixelRGBA8 -> [Float]
rgba8Converter (PixelRGBA8 r g b a) = map ((*imax8) . realToFrac) [r,g,b,a]

rgba16Converter :: PixelRGBA16 -> [Float]
rgba16Converter (PixelRGBA16 r g b a) = map ((*imax16) . realToFrac) [r,g,b,a]

throwError_ :: (MonadError Log m) => String -> m a
throwError_ = throwError . Log ErrorLog CoreLog

makeLenses ''Texture
