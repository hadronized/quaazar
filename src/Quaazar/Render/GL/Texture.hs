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

module Quaazar.Render.GL.Texture where

import Codec.Picture
import Codec.Picture.Types
import Control.Monad.Error.Class ( MonadError(..) )
import Control.Monad.Trans ( MonadIO(..) )
import Foreign.Marshal.Array ( withArray )
import Foreign.Ptr ( nullPtr )
import Foreign.Storable ( Storable )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Core.Loader ( Load(..) )
import Quaazar.Core.Resource ( Manager, Resource )
import Quaazar.Render.GL.GLObject
import Quaazar.Utils.Log

data Wrap
  = ClampToEdge
  | ClampToBorder
  | Repeat
    deriving (Eq,Show)

data Filter
  = Nearest
  | Linear
    deriving (Eq,Show)

data Format
  = R
  | RG
  | RGB
  | RGBA
  | Depth
    deriving (Eq,Show)

data InternalFormat
  = R32F
  | RG32F
  | RGB32F
  | RGBA32F
  | Depth32F
    deriving (Eq,Show)

data CompareFunc
  = Never
  | Less
  | Equal
  | LessOrEqual
  | Greater
  | GreaterOrEqual
  | NotEqual
  | Always
    deriving (Eq,Show)

-- |This typeclass defines what a **texture** is. All textures have to
-- implement that class, which exposes very basic texture features, such as
-- texture ID, binding, parameters setting (wrap, filters, depth comparison
-- function, mipmap base level and max level).
--
-- For storage allocation and texels transfer, please refer to either
-- 'Unlayegred' or 'Layered'.
class IsTexture t where
  -- |OpenGL ID of the texture.
  textureID :: t -> GLuint
  -- |Bind the texture to the current OpenGL context.
  bindTexture :: (MonadIO m) => t -> m ()
  -- |Unbind the texture in the current OpenGL context. Note that this function
  -- doesn’t handle texture units and therefore shouldn’t be used.
  unbindTexture :: (MonadIO m) => t -> m ()
  -- TODO: should be setTextureWraps
  -- |Set the wrapping behavior of a texture. Currently, a single 'Wrap' is
  -- passed as argument and apply against the **S** and **T** coordinates.
  setTextureWrap :: (MonadIO m) => t -> Wrap -> m ()
  -- |Set the minification and magnification filters of a texture. Both the
  -- filters are assigned the same value.
  setTextureFilters :: (MonadIO m) => t -> Filter -> m ()
  -- |Set the texture’s depth comparison function. If given 'Nothing', no
  -- comparison will be performed.
  setTextureCompareFunc :: (MonadIO m) => t -> Maybe CompareFunc -> m ()
  -- |Set the texture’s mipmap base level. That is, the level from which all
  -- other levels are computed if ordered.
  setTextureBaseLevel :: (MonadIO m) => t -> Natural -> m ()
  -- |Set the texture’s mipmap max level.
  setTextureMaxLevel :: (MonadIO m) => t -> Natural -> m ()

-- |Unlayered textures.
--
-- An unlayered texture has no layer. It’s a raw texture. Whatever it’s a
-- **1D**, **2D** or **cubemap**, it has no /layer/.
class (IsTexture t) => Unlayered t where
  -- |Allocate some storage for the given texture.
  setTextureStorage :: (MonadIO m) => t -> InternalFormat -> Natural -> Natural -> m ()
  -- |
  transferTexels :: (MonadIO m,Storable a) => t -> Natural -> Natural -> Format -> [a] -> m ()
  -- |
  unlayeredTexture :: (MonadIO m,MonadScoped IO m,Storable a)
                   => Natural
                   -> Natural
                   -> Format
                   -> InternalFormat
                   -> Wrap
                   -> Filter
                   -> Maybe CompareFunc
                   -> Natural
                   -> Natural
                   -> [a]
                   -> m t

class (IsTexture t) => Layered t where
  -- |
  setTextureStorageLayer :: (MonadIO m) => t -> InternalFormat -> Natural -> Natural -> Natural -> m ()
  -- |
  transferTexelsLayer :: (MonadIO m,Storable a) => t -> Natural -> Natural -> Natural -> Format -> [a] -> m ()

newtype Texture2D = Texture2D { unTexture2D :: GLuint } deriving (Eq,Ord,Show)

instance GLObject Texture2D where
  genObjects n = genericGenObjects n glGenTextures glDeleteTextures Texture2D

instance IsTexture Texture2D where
  textureID = unTexture2D
  bindTexture (Texture2D t) = liftIO $ glBindTexture gl_TEXTURE_2D t
  unbindTexture _ = liftIO $ glBindTexture gl_TEXTURE_2D 0
  setTextureWrap _ = setTextureWrap_ gl_TEXTURE_2D
  setTextureFilters _ = setTextureFilters_ gl_TEXTURE_2D
  setTextureCompareFunc _ = setTextureCompareFunc_ gl_TEXTURE_2D
  setTextureBaseLevel _ = setTextureBaseLevel_ gl_TEXTURE_2D
  setTextureMaxLevel _ = setTextureMaxLevel_ gl_TEXTURE_2D

{-
instance Load Texture2D where
  loadRoot = const "textures"
  loadExt = const ""
  load rootPath name = do
      info CoreLog $ "loading texture " ++ name
      img <- liftIO . fmap (first onError) $
        readImage (rootPath </> loadRoot (undefined :: Texture)  </> name)
      eitherToError img >>= imageToTexture
    where
      onError = Log ErrorLog CoreLog
  load_ = load ""

instance Resource () Texture2D
-}

type Texture2DManager = Manager () Texture2D

instance Unlayered Texture2D where
  setTextureStorage _ ift w h = liftIO $
      glTexStorage2D gl_TEXTURE_2D 1 ift' (fromIntegral w) (fromIntegral h)
    where
      ift' = fromIntegral (fromInternalFormat ift)
  transferTexels _ = transferPixels_ gl_TEXTURE_2D
  unlayeredTexture w h ft ift wrap mmf cmpf baseLvl maxLvl texels = do
    tex <- genObject
    bindTexture tex
    setTextureStorage tex ift w h
    setTextureWrap tex wrap
    setTextureFilters tex mmf
    setTextureCompareFunc tex cmpf
    setTextureBaseLevel tex baseLvl
    setTextureMaxLevel tex maxLvl
    transferTexels tex w h ft texels
    unbindTexture tex
    return tex

newtype Texture2DArray = Texture2DArray { unTexture2DArray :: GLuint } deriving (Eq,Ord,Show)

instance GLObject Texture2DArray where
  genObjects n = genericGenObjects n glGenTextures glDeleteTextures Texture2DArray

instance IsTexture Texture2DArray where
  textureID = unTexture2DArray
  bindTexture (Texture2DArray t) = liftIO $ glBindTexture gl_TEXTURE_2D_ARRAY t
  unbindTexture _ = liftIO $ glBindTexture gl_TEXTURE_2D_ARRAY 0
  setTextureWrap _ = setTextureWrap_ gl_TEXTURE_2D_ARRAY
  setTextureFilters _ = setTextureFilters_ gl_TEXTURE_2D_ARRAY
  setTextureCompareFunc _ = setTextureCompareFunc_ gl_TEXTURE_2D_ARRAY
  setTextureBaseLevel _ = setTextureBaseLevel_ gl_TEXTURE_2D_ARRAY
  setTextureMaxLevel _ = setTextureMaxLevel_ gl_TEXTURE_2D_ARRAY

instance Layered Texture2DArray where
  setTextureStorageLayer _ ift w h n = liftIO $
      glTexStorage3D gl_TEXTURE_2D_ARRAY 1 ift' (fromIntegral w) (fromIntegral h) (fromIntegral n)
    where
        ift' = fromIntegral (fromInternalFormat ift)
  transferTexelsLayer = error "texture 2D array transfer not implemented yet"

newtype Cubemap = Cubemap { unCubemap :: GLuint } deriving (Eq,Ord,Show)

instance GLObject Cubemap where
  genObjects n = genericGenObjects n glGenTextures glDeleteTextures Cubemap

instance IsTexture Cubemap where
  textureID = unCubemap
  bindTexture (Cubemap t) = liftIO $ glBindTexture gl_TEXTURE_CUBE_MAP t
  unbindTexture _ = liftIO $ glBindTexture gl_TEXTURE_CUBE_MAP 0
  setTextureWrap _ = setTextureWrap_ gl_TEXTURE_CUBE_MAP
  setTextureFilters _ = setTextureFilters_ gl_TEXTURE_CUBE_MAP
  setTextureCompareFunc _ = setTextureCompareFunc_ gl_TEXTURE_CUBE_MAP
  setTextureBaseLevel _ = setTextureBaseLevel_ gl_TEXTURE_CUBE_MAP
  setTextureMaxLevel _ = setTextureMaxLevel_ gl_TEXTURE_CUBE_MAP

instance Unlayered Cubemap where
  setTextureStorage _ ift w h = liftIO $
      glTexStorage2D gl_TEXTURE_CUBE_MAP 1 ift' (fromIntegral w) (fromIntegral h)
    where
      ift' = fromIntegral (fromInternalFormat ift)
  transferTexels = error "transferTexels: cubemap support not implemented yet"
  unlayeredTexture = error "uniTexture: cubemap support not implemented yet"

newtype CubemapArray = CubemapArray { unCubemapArray :: GLuint } deriving (Eq,Ord,Show)

instance GLObject CubemapArray where
  genObjects n = genericGenObjects n glGenTextures glDeleteTextures CubemapArray

instance IsTexture CubemapArray where
  textureID = unCubemapArray
  bindTexture (CubemapArray t) = liftIO $ glBindTexture gl_TEXTURE_CUBE_MAP_ARRAY t
  unbindTexture _ = liftIO $ glBindTexture gl_TEXTURE_CUBE_MAP_ARRAY 0
  setTextureWrap _ = setTextureWrap_ gl_TEXTURE_CUBE_MAP_ARRAY
  setTextureFilters _ = setTextureFilters_ gl_TEXTURE_CUBE_MAP_ARRAY
  setTextureCompareFunc _ = setTextureCompareFunc_ gl_TEXTURE_CUBE_MAP_ARRAY
  setTextureBaseLevel _ = setTextureBaseLevel_ gl_TEXTURE_CUBE_MAP_ARRAY
  setTextureMaxLevel _ = setTextureMaxLevel_ gl_TEXTURE_CUBE_MAP_ARRAY

instance Layered CubemapArray where
  setTextureStorageLayer _ ift w h n = liftIO $
      glTexStorage3D gl_TEXTURE_CUBE_MAP_ARRAY 1 ift' (fromIntegral w)
        (fromIntegral h) (fromIntegral $ 6*n)
    where
      ift' = fromIntegral (fromInternalFormat ift)
  transferTexelsLayer = error "transferArrayTexels: cubemap array support not implemented yet"

bindTextureAt :: (MonadIO m,IsTexture t) => t -> Natural -> m ()
bindTextureAt tex unit = do
  liftIO $ glActiveTexture (gl_TEXTURE0 + fromIntegral unit)
  bindTexture tex

setTextureWrap_ :: (MonadIO m) => GLenum -> Wrap -> m ()
setTextureWrap_ t wrap = liftIO $ do
    glTexParameteri t gl_TEXTURE_WRAP_R wrap'
    glTexParameteri t gl_TEXTURE_WRAP_S wrap'
    glTexParameteri t gl_TEXTURE_WRAP_T wrap'
  where
    wrap'  = fromIntegral (fromWrap wrap)

setTextureFilters_ :: (MonadIO m) => GLenum -> Filter -> m ()
setTextureFilters_ t filt = liftIO $ do
    glTexParameteri t gl_TEXTURE_MIN_FILTER filt'
    glTexParameteri t gl_TEXTURE_MAG_FILTER filt'
  where
    filt'  = fromIntegral (fromFilter filt)

transferPixels_ :: (MonadIO m,Storable a) => GLenum -> Natural -> Natural -> Format -> [a] -> m ()
transferPixels_ t w h ft texels = liftIO $ do
    withArray texels (glTexSubImage2D t 0 0 0 (fromIntegral w) (fromIntegral h) ft' gl_FLOAT)
  where
    ft' = fromFormat ft

setTextureCompareFunc_ :: (MonadIO m) => GLenum -> Maybe CompareFunc -> m ()
setTextureCompareFunc_ t = maybe compareNothing compareRefToTexture
  where
    compareNothing = liftIO $
      glTexParameteri t gl_TEXTURE_COMPARE_MODE (fromIntegral gl_NONE)
    compareRefToTexture func = liftIO $ do
      glTexParameteri t gl_TEXTURE_COMPARE_MODE (fromIntegral gl_COMPARE_REF_TO_TEXTURE)
      glTexParameteri t gl_TEXTURE_COMPARE_FUNC (fromIntegral $ fromCompareFunc func)

setTextureBaseLevel_ :: (MonadIO m) => GLenum -> Natural -> m ()
setTextureBaseLevel_ t l = liftIO $
    glTexParameteri t gl_TEXTURE_MAX_LEVEL (fromIntegral l)

setTextureMaxLevel_ :: (MonadIO m) => GLenum -> Natural -> m ()
setTextureMaxLevel_ t l = liftIO $
    glTexParameteri t gl_TEXTURE_MAX_LEVEL (fromIntegral l)

fromWrap :: Wrap -> GLenum
fromWrap wrap = case wrap of
  ClampToEdge -> gl_CLAMP_TO_EDGE
  ClampToBorder -> gl_CLAMP_TO_BORDER
  Repeat -> gl_REPEAT

fromFilter :: Filter -> GLenum
fromFilter filt = case filt of
  Nearest -> gl_NEAREST
  Linear  -> gl_LINEAR

fromInternalFormat :: InternalFormat -> GLenum
fromInternalFormat ift = case ift of
  R32F     -> gl_R32F
  RG32F    -> gl_RG32F
  RGB32F   -> gl_RGB32F
  RGBA32F  -> gl_RGBA32F
  Depth32F -> gl_DEPTH_COMPONENT32F

fromFormat :: Format -> GLenum
fromFormat ft = case ft of
  R     -> gl_RED
  RG    -> gl_RG
  RGB   -> gl_RGB
  RGBA  -> gl_RGBA
  Depth -> gl_DEPTH_COMPONENT

fromCompareFunc :: CompareFunc -> GLenum
fromCompareFunc func = case func of
  Never -> gl_NEVER
  Less -> gl_LESS
  Equal -> gl_EQUAL
  LessOrEqual -> gl_LEQUAL
  Greater -> gl_GREATER
  GreaterOrEqual -> gl_GEQUAL
  NotEqual -> gl_NOTEQUAL
  Always -> gl_ALWAYS

-------------------------------------------------------------------------------
-- Loading from disk

-- |Convert a 'DynamicImage' to a 'Texture2D'.
imageToTexture :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m)
               => DynamicImage
               -> Wrap
               -> Filter
               -> Maybe CompareFunc
               -> Natural
               -> Natural
               -> m Texture2D
imageToTexture dynimg wrap flt cmpf baseLvl maxLvl = do
  info CoreLog $ "texture type is " ++ showImageFormat dynimg
  case dynimg of
    ImageY8 img -> convertImage_ y8Converter img wrap flt cmpf baseLvl maxLvl
    ImageY16 img -> convertImage_ y16Converter img wrap flt cmpf baseLvl maxLvl
    ImageYF img -> convertImage_ yfConverter img wrap flt cmpf baseLvl maxLvl
    ImageYA8 img -> convertImage_ ya8Converter img wrap flt cmpf baseLvl maxLvl
    ImageYA16 img -> convertImage_ ya16Converter img wrap flt cmpf baseLvl maxLvl
    ImageRGB8 img -> convertImage_ rgb8Converter img wrap flt cmpf baseLvl maxLvl
    ImageRGB16 img -> convertImage_ rgb16Converter img wrap flt cmpf baseLvl maxLvl
    ImageRGBF img -> convertImage_ rgbfConverter img wrap flt cmpf baseLvl maxLvl
    ImageRGBA8 img -> convertImage_ rgba8Converter img wrap flt cmpf baseLvl maxLvl
    ImageRGBA16 img -> convertImage_ rgba16Converter img wrap flt cmpf baseLvl maxLvl
    ImageYCbCr8 img -> convertImage_ rgb8Converter (convertImage img) wrap flt cmpf baseLvl maxLvl
    _ -> throwError_ "unimplemented image format"

-- |Show the image format of a 'DynamicImage'.
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

-- |Cached inverse of constants for colorspace conversion.
imax8, imax16 :: Float
imax8 = 1 / 255
imax16 = 1 / realToFrac (maxBound :: Pixel16)

-- |Turn a @(Pixel a) => Image a@ into a 'Texture2D'.
convertImage_ :: forall m a. (MonadIO m,MonadScoped IO m,Pixel a)
             => (a -> [Float])
             -> Image a
             -> Wrap
             -> Filter
             -> Maybe CompareFunc
             -> Natural
             -> Natural
             -> m Texture2D
convertImage_ converter image@(Image w h _) wrap flt cmpf baseLvl maxLvl =
    unlayeredTexture w' h' ft ift wrap flt cmpf baseLvl maxLvl texels
  where
    w' = fromIntegral w
    h' = fromIntegral h
    (ft,ift) = case componentCount (undefined :: a) of
      1 -> (R,R32F)
      2 -> (RG,RG32F)
      3 -> (RGB,RGB32F)
      4 -> (RGBA,RGBA32F)
      _ -> error "convertImage_: more-than-4-components pixel"
    texels = concat $ [converter $ pixelAt image x y | y <- [0..h-1], x <- [0..w-1]]

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
