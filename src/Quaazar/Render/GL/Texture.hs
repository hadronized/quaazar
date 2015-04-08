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

import Control.Monad.Trans ( MonadIO(..) )
import Foreign.Marshal.Array ( withArray )
import Foreign.Ptr ( nullPtr )
import Foreign.Storable ( Storable )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Render.GL.GLObject

data Wrap
  = ClampToEdge
  | ClampToBorder
  | Repeat
    deriving (Eq,Show)

data Filter
  = Nearest
  | Linear
    deriving (Eq,Show)

data InternalFormat
  = R32F
  | RG32F
  | RGB32F
  | RGBA32F
  | Depth32F
    deriving (Eq,Show)

data Format
  = R
  | RG
  | RGB
  | RGBA
  | Depth
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

class IsTexture t where
  -- |
  textureID :: t -> GLuint
  -- |
  bindTexture :: (MonadIO m) => t -> m ()
  -- |
  unbindTexture :: (MonadIO m) => t -> m ()
  -- |
  setTextureWrap :: (MonadIO m) => t -> Wrap -> m ()
  -- |
  setTextureFilters :: (MonadIO m) => t -> Filter -> m ()
  -- |
  setTextureCompareFunc :: (MonadIO m) => t -> Maybe CompareFunc -> m ()
  -- |
  setTextureBaseLevel :: (MonadIO m) => t -> Int -> m ()
  -- |
  setTextureMaxLevel :: (MonadIO m) => t -> Int -> m ()

class Unidimensional t where
  -- |
  setTextureStorage :: (MonadIO m) => t -> InternalFormat -> Natural -> Natural -> m ()
  -- |
  transferPixels :: (MonadIO m,Storable a) => t -> Natural -> Natural -> Format -> [a] -> m ()

class Multidimensional t where
  -- |
  setTextureArrayStorage :: (MonadIO m) => t -> InternalFormat -> Natural -> Natural -> Natural -> m ()
  -- |
  transferArrayPixels :: (MonadIO m,Storable a) => t -> Natural -> Natural -> Natural -> Format -> [a] -> m ()

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

instance Unidimensional Texture2D where
  setTextureStorage _ ift w h = liftIO $
      glTexStorage2D gl_TEXTURE_2D 1 ift' (fromIntegral w) (fromIntegral h)
    where
      ift' = fromIntegral (fromInternalFormat ift)
  transferPixels _ = transferPixels_ gl_TEXTURE_2D

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

instance Multidimensional Texture2DArray where
  setTextureArrayStorage _ ift w h n = liftIO $
      glTexStorage3D gl_TEXTURE_2D_ARRAY 1 ift' (fromIntegral w) (fromIntegral h) (fromIntegral n)
    where
        ift' = fromIntegral (fromInternalFormat ift)

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

instance Unidimensional Cubemap where
  setTextureStorage _ ift w h = liftIO $
      glTexStorage2D gl_TEXTURE_CUBE_MAP 1 ift' (fromIntegral w) (fromIntegral h)
    where
      ift' = fromIntegral (fromInternalFormat ift)
  transferPixels = error "cubemap pixels transfer not implemented yet"

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

instance Multidimensional CubemapArray where
  setTextureArrayStorage _ ift w h n = liftIO $
      glTexStorage3D gl_TEXTURE_CUBE_MAP_ARRAY 1 ift' (fromIntegral w)
        (fromIntegral h) (fromIntegral $ 6*n)
    where
      ift' = fromIntegral (fromInternalFormat ift)
  transferArrayPixels = error "cubemap array pixels transfer not implemented yet"

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

setTextureBaseLevel_ :: (MonadIO m) => GLenum -> Int -> m ()
setTextureBaseLevel_ t l = liftIO $
    glTexParameteri t gl_TEXTURE_MAX_LEVEL (fromIntegral l)

setTextureMaxLevel_ :: (MonadIO m) => GLenum -> Int -> m ()
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
