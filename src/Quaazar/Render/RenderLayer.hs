{-# LANGUAGE ExistentialQuantification #-}

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

module Quaazar.Render.RenderLayer where

import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.State ( evalState )
import Control.Monad.Trans ( MonadIO )
import Data.Bits ( (.|.) )
import Data.Foldable ( for_, traverse_ )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Core.Hierarchy ( Instance, instCarried, instTransform )
import Quaazar.Core.Light
import Quaazar.Core.Projection ( Projection )
import Quaazar.Render.Camera ( GPUCamera(..), gpuCamera )
import Quaazar.Render.Compositing
import Quaazar.Render.Lighting
import Quaazar.Render.Viewport ( Viewport(Viewport), setViewport )
import Quaazar.Render.GL.Buffer ( Buffer )
import Quaazar.Render.GL.Framebuffer ( Framebuffer, Target(..)
                                     , bindFramebuffer )
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Shader ( (@=), unused )
import Quaazar.Render.GL.Texture ( Filter(..), Format(..), InternalFormat(..)
                                 , bindTextureAt )
import Quaazar.Render.GL.VertexArray ( bindVertexArray )
import Quaazar.Render.Light
import Quaazar.Render.Lighting
import Quaazar.Render.Mesh ( GPUMesh, renderMesh )
import Quaazar.Render.PostFX ( GPUPostFX(..) )
import Quaazar.Render.Shader ( GPUProgram(..) )
import Quaazar.Render.Texture ( GPUTexture(GPUTexture) ) 
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

newtype RenderLayer = RenderLayer {
    unRenderLayer :: Framebuffer                -- ^ lighting framebuffer
                  -> Buffer                     -- ^ omni light buffer
                  -> Maybe (ShadowConf,Shadows) -- ^ shadows configuration
                  -> IO ()
  }

data GPUModelGroup = forall mat. GPUModelGroup (GPUProgram mat) [Instance (GPUMesh,mat)]

modelGroup :: GPUProgram mat -> [Instance (GPUMesh,mat)] -> GPUModelGroup
modelGroup = GPUModelGroup

renderLayer :: Instance Projection
            -> Ambient
            -> [Instance Omni]
            -> [GPUModelGroup]
            -> RenderLayer
renderLayer cam ambient omnis models =
  RenderLayer $ \fb omniBuffer shadowsConf -> do
    let Ambient ligAmbCol ligAmbPow = ambient
    omnisWithShadows <- case shadowsConf of
      Just (conf,shadows) -> do 
        let
          omnisWithShadows = flip evalState (0,0,0) $ mapM (addShadowInfo_ lmax mmax hmax) omnis
          lmax = conf^.lowShadowMaxNb
          mmax = conf^.mediumShadowMaxNb
          hmax = conf^.highShadowMaxNb
        cleanShadows shadows
        traverse_ (genShadowmap_ shadows meshes) omnisWithShadows
        return omnisWithShadows
      Nothing -> return $ map addNoShadows omnis
    bindFramebuffer fb ReadWrite
    glClearColor 0 0 0 0
    glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
    for_ models $ \group -> renderModelGroup group $ do
      runCamera (gpuCamera cam) camProjViewUniform unused eyeUniform
      ligAmbColUniform @= ligAmbCol
      ligAmbPowUniform @= ligAmbPow
      pushOmnis omnisWithShadows omniBuffer
  where
    addShadowInfo_ lmax mmax hmax inst = do
      let
        omni = instCarried inst
        trsf = instTransform inst
      (omni',i) <- addShadowInfo lmax mmax hmax omni
      return (omni',i,trsf)
    addNoShadows inst =
      let
        omni = instCarried inst
        trsf = instTransform inst
      in (omni,0,trsf)
    meshes = concatMap dropProgram models
    dropProgram (GPUModelGroup _ mshs) = map (fmap fst) mshs
    genShadowmap_ shadows meshes (omni,shadowmapIndex,trsf) =
      genShadowmap omni shadowmapIndex trsf meshes shadows

-- TODO: try to use the texture interface directly
cleanShadows :: Shadows -> IO ()
cleanShadows (Shadows _ low medium high) = do
  -- low shadows
  bindFramebuffer (low^.cubeOffscreenArrayFB) ReadWrite
  glClearColor 0 0 0 0
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  -- medium shadows
  bindFramebuffer (medium^.cubeOffscreenArrayFB) ReadWrite
  glClearColor 0 0 0 0
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  -- high shadows
  bindFramebuffer (high^.cubeOffscreenArrayFB) ReadWrite
  glClearColor 0 0 0 0
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT

renderModelGroup :: GPUModelGroup -> IO () -> IO ()
renderModelGroup (GPUModelGroup prog insts) sendUniforms = do
  useProgram prog
  sendUniforms
  traverse_ (renderMeshInstance $ sendToProgram prog) insts

renderMeshInstance :: (mat -> IO ()) -> Instance (GPUMesh,mat) -> IO ()
renderMeshInstance sinkMat inst = do
    sinkMat mat
    renderMesh gmesh modelUniform trsf
  where
    (gmesh,mat) = instCarried inst
    trsf  = instTransform inst

renderLayerCompositor :: (MonadIO m,MonadScoped IO m,MonadError Log m)
                      => Viewport
                      -> m (Compositor RenderLayer (GPUTexture,GPUTexture))
renderLayerCompositor vp = do
    Offscreen nodeColor nodeDepth nodeFB <- genOffscreen w h Nearest RGBA32F RGBA
    return . Compositor $ \_ omniBuffer shadowsConf rl -> do
      setViewport vp
      unRenderLayer rl nodeFB omniBuffer shadowsConf
      return (GPUTexture $ bindTextureAt nodeColor,GPUTexture $ bindTextureAt nodeDepth)
  where
    Viewport _ _ w h = vp
