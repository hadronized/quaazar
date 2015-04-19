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
import Quaazar.Lighting.Light
import Quaazar.Render.Camera ( GPUCamera(..), gpuCamera )
import Quaazar.Render.Compositing
import Quaazar.Render.Lighting
import Quaazar.Render.Viewport ( Viewport(Viewport), setViewport )
import Quaazar.Render.GL.Buffer ( Buffer )
import Quaazar.Render.GL.Framebuffer ( Framebuffer, Target(..)
                                     , bindFramebuffer )
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Shader ( Program', Semantics(..), (@=), unused
                                , useProgram )
import Quaazar.Render.GL.Texture ( Filter(..), InternalFormat(..), Texture2D
                                 , Unit(..) )
import Quaazar.Render.Light
import Quaazar.Render.Mesh ( GPUMesh, renderMesh )
import Quaazar.Render.Projection ( Projection )
import Quaazar.Scene.Hierarchy ( Instance, instCarried, instTransform )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

newtype RenderLayer = RenderLayer {
    unRenderLayer :: Framebuffer                -- ^ lighting framebuffer
                  -> Buffer                     -- ^ omni light buffer
                  -> Maybe (ShadowConf,Shadows) -- ^ shadows configuration
                  -> IO ()
  }

data GPUModelGroup = forall mat. GPUModelGroup (Program' mat) [Instance (GPUMesh,mat)]

modelGroup :: Program' mat -> [Instance (GPUMesh,mat)] -> GPUModelGroup
modelGroup = GPUModelGroup

renderLayer :: Instance Projection
            -> Viewport
            -> Ambient
            -> [Instance Omni]
            -> [GPUModelGroup]
            -> RenderLayer
renderLayer cam vp ambient omnis models =
  RenderLayer $ \fb omniBuffer shadowsConf -> do
    let Ambient ligAmbCol ligAmbPow = ambient
    (omnisWithShadows,bindShadowmaps_) <- case shadowsConf of
      Just (conf,shdws) -> do 
        let
          omnisWithShadows = flip evalState (0,0,0) $ mapM (addShadowInfo_ lmax mmax hmax) omnis
          lmax = conf^.lowShadowMaxNb
          mmax = conf^.mediumShadowMaxNb
          hmax = conf^.highShadowMaxNb
        cleanShadows shdws
        traverse_ (genShadowmap_ shdws) omnisWithShadows
        return (omnisWithShadows,bindShadowmaps shdws)
      Nothing -> return (map addNoShadows omnis,return ())
    bindFramebuffer fb ReadWrite
    setViewport vp
    glClearColor 0 0 0 0
    glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
    for_ models $ \group -> renderModelGroup group $ do
      runCamera (gpuCamera cam) camProjViewUniform unused eyeUniform
      ligAmbColUniform @= ligAmbCol
      ligAmbPowUniform @= ligAmbPow
      pushOmnis omnisWithShadows omniBuffer
      bindShadowmaps_
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
    genShadowmap_ shdws (omni,shadowmapIndex,trsf) =
      genShadowmap omni shadowmapIndex trsf meshes shdws

cleanShadows :: Shadows -> IO ()
cleanShadows (Shadows _ low medium high) = do
  -- low shadows
  bindFramebuffer (fst low ^. cubeOffscreenArrayFB) ReadWrite
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  -- medium shadows
  bindFramebuffer (fst medium ^. cubeOffscreenArrayFB) ReadWrite
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  -- high shadows
  bindFramebuffer (fst high ^. cubeOffscreenArrayFB) ReadWrite
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT

-- TODO
bindShadowmaps :: Shadows -> IO ()
bindShadowmaps (Shadows _ low medium high) = do
  lowShadowmapsUniform @= (fst low ^. cubeOffscreenArrayColormaps,Unit 3)
  mediumShadowmapsUniform @= (fst medium ^.cubeOffscreenArrayColormaps,Unit 4)
  highShadowmapsUniform @= (fst high ^.cubeOffscreenArrayColormaps,Unit 5)

renderModelGroup :: GPUModelGroup -> IO () -> IO ()
renderModelGroup (GPUModelGroup (prog,semantics) insts) sendUniforms = do
  useProgram prog
  sendUniforms
  traverse_ (renderMeshInstance semantics) insts

renderMeshInstance :: (mat -> Semantics ()) -> Instance (GPUMesh,mat) -> IO ()
renderMeshInstance semantics inst = do
    runSemantics $ semantics mat
    renderMesh gmesh modelUniform trsf
  where
    (gmesh,mat) = instCarried inst
    trsf  = instTransform inst

renderLayerCompositor :: (MonadIO m,MonadScoped IO m,MonadError Log m)
                      => Viewport
                      -> m (Compositor RenderLayer (Texture2D,Texture2D))
renderLayerCompositor vp = do
    Offscreen nodeColor nodeDepth nodeFB <- genOffscreen w h Nearest RGBA32F
    return . Compositor $ \_ omniBuffer shadowsConf rl -> do
      setViewport vp
      unRenderLayer rl nodeFB omniBuffer shadowsConf
      return (nodeColor,nodeDepth)
  where
    Viewport _ _ w h = vp
