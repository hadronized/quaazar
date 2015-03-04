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

module Quaazar.Render.Forward.Lighting where

import Control.Applicative
import Control.Monad ( foldM, void )
import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Graphics.Rendering.OpenGL.Raw
import Linear
import Numeric.Natural ( Natural )
import Foreign hiding ( void )
import Quaazar.Core.Color ( Color(..) )
import Quaazar.Core.Entity ( Entity, entityPosition )
import Quaazar.Core.Light ( Omni(..) )
import Quaazar.Render.GL.Buffer hiding ( MapAccess(..) )
import qualified Quaazar.Render.GL.Buffer as B ( MapAccess(..) )
import Quaazar.Render.GL.Framebuffer as FB ( Target(..), bindFramebuffer )
import Quaazar.Render.GL.GLObject
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Shader ( Uniform, Uniformable, (@=), uniform )
import Quaazar.Render.GL.Texture ( Filter(..), Format(..), InternalFormat(..)  )
import Quaazar.Render.GLSL
import Quaazar.Utils.Log

-- |'Lighting' gathers information about lighting in the scene.
data Lighting = Lighting {
    _lightUniforms    :: LightingUniforms
  , _lightOff         :: Offscreen
  , _lightOmniBuffer  :: Buffer
  }

data LightingUniforms = LightingUniforms {
    _lightCamProjViewU :: Uniform (M44 Float)
  , _lightModelU       :: Uniform (M44 Float)
  , _lightEyeU         :: Uniform (V3 Float)
  , _lightLigAmbCol    :: Uniform Color
  , _lightLigAmbPow    :: Uniform Float
  , _lightLigOmniNb    :: Uniform Word32
  }

makeLenses ''Lighting
makeLenses ''LightingUniforms

getLighting :: (Applicative m,MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
            => Natural
            -> Natural
            -> Natural
            -> m Lighting
getLighting w h nbLights = do
  info CoreLog "generating lighting"
  off <- genOffscreen w h Nearest RGB32F RGB
  omniBuffer <- genOmniBuffer nbLights
  return (Lighting uniforms off omniBuffer)
  where
    uniforms = getLightingUniforms

getLightingUniforms :: LightingUniforms
getLightingUniforms =
    LightingUniforms
      (sem camProjViewSem)
      (sem modelSem)
      (sem eyeSem)
      (sem ligAmbColSem)
      (sem ligAmbPowSem)
      (sem ligOmniNbSem)
  where
    sem :: (Uniformable a) => Int -> Uniform a
    sem = uniform . fromIntegral

purgeLightingFramebuffer :: Lighting -> IO ()
purgeLightingFramebuffer lighting = do
  bindFramebuffer (lighting^.lightOff.offscreenFB) FB.ReadWrite
  glClearColor 0 0 0 0
  glClear $ gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT

genOmniBuffer :: (MonadScoped IO m,MonadIO m) => Natural -> m Buffer
genOmniBuffer nbLights = do
    buffer <- genObject
    bindBuffer buffer ShaderStorageBuffer
    initBuffer ShaderStorageBuffer bytes
    unbindBuffer ShaderStorageBuffer
    return buffer
  where
    bytes = nbLights * fromIntegral omniBytes

-- WARNING: padding
omniBytes :: Int
omniBytes =
    sizeOf (undefined :: V3 Float) -- transform
    + sizeOf (undefined :: Float) -- float padding
  + sizeOf (undefined :: Color)
  + sizeOf (undefined :: Float) -- power
  + sizeOf (undefined :: Float) -- radius
    + sizeOf (undefined :: V3 Float) -- vec2 padding

-- Poke omnidirectional lights at a given pointer. That pointer should be gotten
-- from the SSBO.
pokeOmnis :: [(Omni,Entity)] -> Ptr Word8 -> IO Word32
pokeOmnis omnis ptr = do
    (_,nbLights) <- foldM cache (ptr,0) omnis
    return nbLights
  where
    cache (ptr',nbLights) (omni,ent) = do
      writeAt ptr' omni ent
      return (ptr' `advancePtr` omniBytes,succ nbLights)
    writeAt ptr' (Omni col pw rad _) ent = do
      pokeByteOff ptr' 0 (ent^.entityPosition)
      pokeByteOff ptr' 16 (unColor col)
      pokeByteOff ptr' 28 pw
      pokeByteOff ptr' 32 rad

pushOmnis :: [(Omni,Entity)] -> Lighting -> IO ()
pushOmnis omnis lighting = do
    bindBufferAt (lighting^.lightOmniBuffer) ShaderStorageBuffer ligOmniSSBOBP
    void . withMappedBuffer ShaderStorageBuffer B.Write $ \ptr -> do
      nbLights <- pokeOmnis omnis ptr
      lighting^.lightUniforms.lightLigOmniNb @= nbLights
