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

module Photon.Render.GL.Texture where

import Control.Applicative
import Foreign.Concurrent
import Foreign.Marshal ( malloc, free )
import Foreign.Marshal.Array ( withArray )
import Foreign.Ptr ( nullPtr )
import Foreign.Storable ( Storable )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Photon.Render.GL.GLObject

data Texture
  = Texture2D GLObject
  | Cubemap GLObject
    deriving (Eq,Show)

data Wrap
  = Clamp
  | Fract
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

genTexture2D :: IO Texture
genTexture2D = fmap Texture2D genTexture_

genCubemap :: IO Texture
genCubemap = fmap Cubemap genTexture_

bindTexture :: Texture -> IO ()
bindTexture t = case t of
    Texture2D tex -> withGLObject tex (glBindTexture gl_TEXTURE_2D)
    Cubemap   tex -> withGLObject tex (glBindTexture gl_TEXTURE_CUBE_MAP)

bindTextureAt :: Texture -> Natural -> IO ()
bindTextureAt tex unit = do
  glActiveTexture (gl_TEXTURE0 + fromIntegral unit)
  bindTexture tex

unbindTexture :: Texture -> IO ()
unbindTexture t = glBindTexture target 0
  where
    target = textureTarget_ t

setTextureWrap :: Texture -> Wrap -> IO ()
setTextureWrap t wrap = do
    glTexParameteri target gl_TEXTURE_WRAP_S wrap'
    glTexParameteri target gl_TEXTURE_WRAP_T wrap'
  where
    target = textureTarget_ t
    wrap'  = fromIntegral (fromWrap wrap)

setTextureFilters :: Texture -> Filter -> IO ()
setTextureFilters t filt = do
    glTexParameteri target gl_TEXTURE_MIN_FILTER filt'
    glTexParameteri target gl_TEXTURE_MAG_FILTER filt'
  where
    target = textureTarget_ t
    filt'  = fromIntegral (fromFilter filt)

setTextureImage :: (Storable a) => Texture -> InternalFormat -> Natural -> Natural -> Format -> [a] -> IO ()
setTextureImage t ift w h ft texels =
    withArray texels (glTexImage2D target 0 ift' (fromIntegral w) (fromIntegral h) 0 ft' gl_FLOAT)
  where
    target = textureTarget_ t
    ift'   = fromIntegral (fromInternalFormat ift)
    ft'    = fromFormat ft

setTextureNoImage :: Texture -> InternalFormat -> Natural -> Natural -> Format -> IO ()
setTextureNoImage t ift w h ft =
    case t of
      Texture2D{} -> texImage2D gl_TEXTURE_2D
      Cubemap{}   -> mapM_ texImage2D
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
    texImage2D target = glTexImage2D target 0 ift' (fromIntegral w) (fromIntegral h) 0 ft' gl_FLOAT nullPtr

unTexture :: Texture -> GLObject
unTexture t = case t of
  Texture2D o -> o
  Cubemap   o -> o

genTexture_ :: IO GLObject
genTexture_ = do
  p <- malloc
  glGenTextures 1 p
  GLObject <$> newForeignPtr p (glDeleteTextures 1 p >> free p)

textureTarget_ :: Texture -> GLenum
textureTarget_ t = case t of
  Texture2D{} -> gl_TEXTURE_2D
  Cubemap{}   -> gl_TEXTURE_CUBE_MAP

fromWrap :: Wrap -> GLenum
fromWrap wrap = case wrap of
  Clamp -> gl_CLAMP_TO_EDGE
  Fract -> gl_REPEAT

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
