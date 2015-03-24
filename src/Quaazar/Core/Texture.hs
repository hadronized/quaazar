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
import Control.Monad.Trans ( liftIO )
import Control.Lens
import Data.Bifunctor ( first )
import Data.Either.Combinators ( eitherToError )
import Data.Vector ( Vector, fromList )
import Data.Vector.Storable ( toList )
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

imageToTexture :: (MonadError Log m) => DynamicImage -> m Texture
imageToTexture dynim = case dynim of
  ImageY8 img -> return $ fromGreyscale imax8 img
  ImageY16 img -> return $ fromGreyscale imax16 img
  ImageYF img -> return $ fromPixelF img
  ImageYA8 img -> return $ fromGreyscaleAlpha imax8 img
  ImageYA16 img -> return $ fromGreyscaleAlpha imax16 img
  ImageRGB8 img -> return $ fromRGB imax8 img
  ImageRGB16 img -> return $ fromRGB imax16 img
  ImageRGBF img -> return $ fromRGBF img
  ImageRGBA8 img -> return $ fromRGBA imax8 img
  ImageRGBA16 img -> return $ fromRGBA imax16 img
  _ -> throwError_ "unimplemented image format"
 
imax8, imax16 :: Float
imax8 = 1 / 255
imax16 = 1 / realToFrac (maxBound :: Pixel16)

fromGreyscale :: (Pixel a,Real (PixelBaseComponent a))
              => Float
              -> Image a
              -> Texture
fromGreyscale imax (Image w h pixels) =
    Texture (fromIntegral w) (fromIntegral h) R pixels'
  where
    pixels' = fromList . map ((*imax) . realToFrac) $ toList pixels

fromPixelF :: Image PixelF -> Texture
fromPixelF (Image w h pixels) =
  Texture (fromIntegral w) (fromIntegral h) R . fromList $ toList pixels

fromGreyscaleAlpha :: (Pixel a,Real (PixelBaseComponent a)) 
                   => Float 
                   -> Image a 
                   -> Texture
fromGreyscaleAlpha imax (Image w h pixels) =
    Texture (fromIntegral w) (fromIntegral h) RG pixels'
  where
    pixels' = fromList . map ((*imax) . realToFrac) $ toList pixels

fromRGB :: (Pixel a,Real (PixelBaseComponent a))
        => Float
        -> Image a
        -> Texture
fromRGB imax (Image w h pixels) =
    Texture (fromIntegral w) (fromIntegral h) RGB pixels'
  where
    pixels' = fromList . map ((*imax) . realToFrac) $ toList pixels

fromRGBF :: Image PixelRGBF -> Texture
fromRGBF (Image w h pixels) =
  Texture (fromIntegral w) (fromIntegral h) RGB . fromList $ toList pixels

fromRGBA :: (Pixel a,Real (PixelBaseComponent a))
         => Float
         -> Image a
         -> Texture
fromRGBA imax (Image w h pixels) =
    Texture (fromIntegral w) (fromIntegral h) RGBA pixels'
  where
    pixels' = fromList . map ((*imax) . realToFrac) $ toList pixels

throwError_ :: (MonadError Log m) => String -> m a
throwError_ = throwError . Log ErrorLog CoreLog

makeLenses ''Texture
