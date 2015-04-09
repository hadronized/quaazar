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

module Quaazar.Render.Lighting where

import Control.Applicative
import Control.Monad ( foldM, void )
import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Control.Monad.Trans.State ( StateT )
import Graphics.Rendering.OpenGL.Raw
import Data.Traversable ( for )
import Linear
import Numeric.Natural ( Natural )
import Foreign hiding ( void )
import Quaazar.Core.Color ( Color(..) )
import Quaazar.Core.Transform ( Transform, transformPosition )
import Quaazar.Core.Light ( Omni(..) )
import Quaazar.Render.GL.Buffer hiding ( MapAccess(..) )
import qualified Quaazar.Render.GL.Buffer as B ( MapAccess(..) )
import Quaazar.Render.GL.Framebuffer as FB ( AttachmentPoint(..), Target(..)
                                           , bindFramebuffer )
import Quaazar.Render.GL.GLObject
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Shader ( Uniform, Uniformable, (@=), uniform )
import Quaazar.Render.GL.Texture ( Filter(..), Format(..), InternalFormat(..) )
import Quaazar.Render.GLSL
import Quaazar.Utils.Log

-- |'Lighting' gathers information about lighting in the scene.
data Lighting = Lighting {
    _lightOff        :: Offscreen -- FIXME: destroy that
  , _lightOmniBuffer :: Buffer
  , _shadows         :: Maybe (ShadowConf,Shadows)
  }

-- |Shadow configuration. Holds for each shadow level of detail the available
-- textures number and their resolutions.
data ShadowConf = ShadowConf {
    _lowShadowMaxNb    :: Natural
  , _lowShadowSize     :: Natural
  , _mediumShadowMaxNb :: Natural
  , _mediumShadowSize  :: Natural
  , _highShadowMaxNb   :: Natural
  , _highShadowSize    :: Natural
  }

data Shadows = Shadows {
    _lowShadows    :: CubeOffscreenArray
  , _mediumShadows :: CubeOffscreenArray
  , _highShadows   :: CubeOffscreenArray
  }

makeLenses ''Lighting
makeLenses ''ShadowConf
makeLenses ''Shadows

-- |@getLighting w h nbMaxLights shadowConf@ creates a 'Lighting' object that
-- can be used later in conjuction with lighting shaders. 'w' and 'h' define
-- the resolution of the render frame. 'nbMaxLights' is a limit used to
getLighting :: (Applicative m,MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
            => Natural
            -> Natural
            -> Natural
            -> Maybe ShadowConf
            -> m Lighting
getLighting w h nbMaxLights shadowConf = do
  info CoreLog "generating lighting"
  off <- genOffscreen w h Nearest RGB32F RGB
  omniBuffer <- genOmniBuffer nbMaxLights
  shadows <- for shadowConf $ \conf -> do
    lowShadows <- getShadows (conf^.lowShadowSize) (conf^.lowShadowMaxNb)
    mediumShadows <- getShadows (conf^.mediumShadowSize) (conf^.mediumShadowMaxNb)
    highShadows <- getShadows (conf^.highShadowSize) (conf^.highShadowMaxNb)
    return (conf,Shadows lowShadows mediumShadows highShadows)
  return (Lighting off omniBuffer shadows)

getShadows :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m)
           => Natural
           -> Natural
           -> m CubeOffscreenArray
getShadows cubeSize d = genCubeOffscreenArray cubeSize d Nearest RGB32F RGB (ColorAttachment 0) Depth32F Depth DepthAttachment

camProjViewUniform :: Uniform (M44 Float)
camProjViewUniform = uniform camProjViewSem

modelUniform :: Uniform (M44 Float)
modelUniform = uniform modelSem

eyeUniform :: Uniform (V3 Float)
eyeUniform = uniform eyeSem

ligAmbColUniform :: Uniform Color
ligAmbColUniform = uniform ligAmbColSem

ligAmbPowUniform :: Uniform Float
ligAmbPowUniform = uniform ligAmbPowSem

ligOmniNbUniform :: Uniform Word32
ligOmniNbUniform = uniform ligOmniNbSem

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
  + sizeOf (undefined :: Color) -- color
  + sizeOf (undefined :: Float) -- power
  + sizeOf (undefined :: Float) -- radius
  + sizeOf (undefined :: Word32) -- shadow lod
  + sizeOf (undefined :: Word32) -- shadowmap index
    + sizeOf (undefined :: Float) -- float padding

-- Poke omnidirectional lights at a given pointer. That pointer should be gotten
-- from the SSBO.
pokeOmnis :: [(Omni,Transform)] -> Ptr Word8 -> StateT (Natural,Natural,Natural) IO Word32
pokeOmnis omnis ptr = do
    (_,nbLights) <- foldM cache (ptr,0) omnis
    return nbLights
  where
    cache (ptr',nbLights) (omni,ent) = do
      (shadowLOD,shadowIndex) <- getShadowInfo omni
      writeAt ptr' (omni,shadowIndex) ent
      return (ptr' `advancePtr` omniBytes,succ nbLights)
    writeAt ptr' (Omni col pw rad shadowLOD,shadowIndex) ent = liftIO $ do
      pokeByteOff ptr' 0 (ent^.transformPosition)
      pokeByteOff ptr' 16 (unColor col)
      pokeByteOff ptr' 28 pw
      pokeByteOff ptr' 32 rad
      pokeByteOff ptr' 36 $ case shadowLOD of
        Nothing -> 0
        Just lod -> case lod of
          LowShadow    -> 1
          MediumShadow -> 2
          HighShadow   -> 3
      pokeByteOff ptr' 40 (fromIntegral shadowIndex :: Word32)

-- TODO: add priority switch.
-- |Extract from an omnidirectional light and the current shadowmaps pool the
-- level of detail to use and the shadowmap index.
getShadowInfo :: Omni -> StateT (Natural,Natural,Natural) m (Natural,Natural)
getShadowInfo (Omni _ _ _ lod) = case lod of
    Nothing -> noShadows
    Just lod' -> do
      (l,m,h) <- get
      case lod' of
        LowShadow
          | l < lmax -> put (succ l,m,h) >> return (1,l)
          | otherwise -> noShadows
        MediumShadow
          | m < mmax -> put (l,succ m,h) >> return (2,m)
          | otherwise -> noShadows
        HighShadow
          | h < hmax -> put (l,m,succ h) >> return (3,h)
          | otherwise -> noShadows
  where
    noShadows = return (0,0)
  
-- |Send omnidirectional lights to the GPU.
pushOmnis :: [(Omni,Transform)] -> Buffer -> IO ()
pushOmnis omnis omniBuffer = do
  bindBufferAt omniBuffer ShaderStorageBuffer ligOmniSSBOBP
  void . withMappedBuffer ShaderStorageBuffer B.Write $ \ptr -> do
    nbLights <- pokeOmnis omnis ptr
    ligOmniNbUniform @= nbLights
