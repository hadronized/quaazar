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

module Quaazar.Render.GL.Texture where

import Foreign.Marshal ( alloca )
import Foreign.Marshal.Array ( peekArray, withArray, withArrayLen )
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

class TextureLike t where
  -- |
  textureID :: t -> GLuint
  -- |
  bindTexture :: t -> IO ()
  -- |
  unbindTexture :: t -> IO ()
  -- |
  setTextureWrap :: t -> Wrap -> IO ()
  -- |
  setTextureFilters :: t -> Filter -> IO ()
  -- |
  setTextureImage :: (Storable a) => t -> InternalFormat -> Natural -> Natural -> Format -> [a] -> IO ()
  -- |
  setTextureNoImage :: t -> InternalFormat -> Natural -> Natural -> Format -> IO ()
  -- |
  setTextureCompareFunc :: t -> Maybe CompareFunc -> IO ()
  -- |
  setTextureMaxLevel :: t -> Int -> IO ()

newtype Texture2D = Texture2D { unTexture2D :: GLuint } deriving (Eq,Ord,Show)

instance GLObject Texture2D where
  genObjects n = alloca $ \p -> do
    glGenTextures (fromIntegral n) p
    fmap (map Texture2D) $ peekArray n p
  deleteObjects a = withArrayLen (map unTexture2D a) $ \s p ->
    glDeleteTextures (fromIntegral s) p

instance TextureLike Texture2D where
  textureID = unTexture2D
  bindTexture (Texture2D t) = glBindTexture gl_TEXTURE_2D t
  unbindTexture _ = glBindTexture gl_TEXTURE_2D 0
  setTextureWrap _ = setTextureWrap_ gl_TEXTURE_2D
  setTextureFilters _ = setTextureFilters_ gl_TEXTURE_2D
  setTextureImage _ = setTextureImage_ gl_TEXTURE_2D
  setTextureNoImage _ ift w h ft =
      glTexImage2D gl_TEXTURE_2D 0 ift' (fromIntegral w) (fromIntegral h) 0 ft' gl_FLOAT nullPtr
    where
      ift' = fromIntegral (fromInternalFormat ift)
      ft' = fromFormat ft
  setTextureCompareFunc _ = setTextureCompareFunc_ gl_TEXTURE_2D
  setTextureMaxLevel _ = setTextureMaxLevel_ gl_TEXTURE_2D

newtype Cubemap = Cubemap { unCubemap :: GLuint } deriving (Eq,Ord,Show)

instance GLObject Cubemap where
  genObjects n = alloca $ \p -> do
    glGenTextures (fromIntegral n) p
    fmap (map Cubemap) $ peekArray n p
  deleteObjects a = withArrayLen (map unCubemap a) $ \s p ->
    glDeleteTextures (fromIntegral s) p

instance TextureLike Cubemap where
  textureID = unCubemap
  bindTexture (Cubemap t) = glBindTexture gl_TEXTURE_CUBE_MAP t
  unbindTexture _ = glBindTexture gl_TEXTURE_CUBE_MAP 0
  setTextureWrap _ = setTextureWrap_ gl_TEXTURE_CUBE_MAP
  setTextureFilters _ = setTextureFilters_ gl_TEXTURE_CUBE_MAP
  setTextureImage = error "setting image of cubemap not supported yet"
  setTextureNoImage _ ift w h ft = mapM_ texImage2D
      [
        gl_TEXTURE_CUBE_MAP_POSITIVE_X
      , gl_TEXTURE_CUBE_MAP_NEGATIVE_X
      , gl_TEXTURE_CUBE_MAP_POSITIVE_Y
      , gl_TEXTURE_CUBE_MAP_NEGATIVE_Y
      , gl_TEXTURE_CUBE_MAP_POSITIVE_Z
      , gl_TEXTURE_CUBE_MAP_NEGATIVE_Z
      ]
    where
      ift' = fromIntegral (fromInternalFormat ift)
      ft' = fromFormat ft
      texImage2D target =
        glTexImage2D target 0 ift' (fromIntegral w) (fromIntegral h) 0 ft' gl_FLOAT nullPtr
  setTextureCompareFunc _ = setTextureCompareFunc_ gl_TEXTURE_CUBE_MAP
  setTextureMaxLevel _ = setTextureMaxLevel_ gl_TEXTURE_CUBE_MAP

bindTextureAt :: (TextureLike t) => t -> Natural -> IO ()
bindTextureAt tex unit = do
  glActiveTexture (gl_TEXTURE0 + fromIntegral unit)
  bindTexture tex

setTextureWrap_ :: GLenum -> Wrap -> IO ()
setTextureWrap_ t wrap = do
    glTexParameteri t gl_TEXTURE_WRAP_R wrap'
    glTexParameteri t gl_TEXTURE_WRAP_S wrap'
    glTexParameteri t gl_TEXTURE_WRAP_T wrap'
  where
    wrap'  = fromIntegral (fromWrap wrap)

setTextureFilters_ :: GLenum -> Filter -> IO ()
setTextureFilters_ t filt = do
    glTexParameteri t gl_TEXTURE_MIN_FILTER filt'
    glTexParameteri t gl_TEXTURE_MAG_FILTER filt'
  where
    filt'  = fromIntegral (fromFilter filt)

setTextureImage_ :: (Storable a) => GLenum -> InternalFormat -> Natural -> Natural -> Format -> [a] -> IO ()
setTextureImage_ t ift w h ft texels =
    withArray texels (glTexImage2D t 0 ift' (fromIntegral w) (fromIntegral h) 0 ft' gl_FLOAT)
  where
    ift'   = fromIntegral (fromInternalFormat ift)
    ft'    = fromFormat ft

setTextureCompareFunc_ :: GLenum -> Maybe CompareFunc -> IO ()
setTextureCompareFunc_ t = maybe compareNothing compareRefToTexture
  where
    compareNothing =
      glTexParameteri t gl_TEXTURE_COMPARE_MODE (fromIntegral gl_NONE)
    compareRefToTexture func = do
      glTexParameteri t gl_TEXTURE_COMPARE_MODE (fromIntegral gl_COMPARE_REF_TO_TEXTURE)
      glTexParameteri t gl_TEXTURE_COMPARE_FUNC (fromIntegral $ fromCompareFunc func)

setTextureMaxLevel_ :: GLenum -> Int -> IO ()
setTextureMaxLevel_ t l =
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
