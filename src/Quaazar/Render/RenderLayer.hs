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
import Control.Monad.State ( evalState )
import Data.Bits ( (.|.) )
import Data.Foldable ( for_, traverse_ )
import Graphics.Rendering.OpenGL.Raw
import Quaazar.Lighting.Light
import Quaazar.Render.Camera ( GPUCamera(..), gpuCamera )
import Quaazar.Render.Lighting
import Quaazar.Render.Viewport ( Viewport, setViewport )
import Quaazar.Render.GL.Buffer ( Buffer )
import Quaazar.Render.GL.Framebuffer
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Shader
import Quaazar.Render.GL.Texture ( Unit(..) )
import Quaazar.Render.Light
import Quaazar.Render.Mesh ( GPUMesh, renderMesh )
import Quaazar.Render.Projection ( Projection )
import Quaazar.Render.Semantics
import Quaazar.Scene.Hierarchy ( Instance, instCarried, instTransform )
import Quaazar.Scene.Model

newtype RenderLayer = RenderLayer {
    unRenderLayer :: Framebuffer                -- output framebuffer
                  -> Buffer                     -- omni light buffer
                  -> Maybe (ShadowConf,Shadows) -- shadows configuration
                  -> IO ()
  }

-- |'GroupModel' is used to gather models that uses the same shader program.
-- A 'GroupModel' owns two kind 'ShaderSemantics':
--
--   - one gets executed only once for a whole 'GroupModel': that’s very
--     convenient for shared and relatively constant values, like time over a
--     frame, the resolution of the screen, and so on and so forth;
--   - the other one runs for each model and is considered to be a thin analogy
--     to /materials/.
data GroupModel =
  forall mat.
    GroupModel
      Program
      (ShaderSemantics ())
      (mat -> ShaderSemantics ())
      [Instance (Model mat)]

renderLayer :: Instance Projection
            -> Viewport
            -> Ambient
            -> [Instance Omni]
            -> [GroupModel]
            -> RenderLayer
renderLayer cam vp ambient omnis groups =
  RenderLayer $ \fb omniBuffer shadowsConf -> do
    let Ambient ligAmbCol ligAmbPow = ambient
    (omnisWithShadows,maybeBindShadowmaps_) <- case shadowsConf of
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
    glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
    setViewport vp
    for_ groups $ \group -> renderGroupModel group $ do
      runCamera (gpuCamera cam) camProjViewUniform unused eyeUniform
      ligAmbColUniform @= ligAmbCol
      ligAmbPowUniform @= ligAmbPow
      pushOmnis omnisWithShadows omniBuffer
      maybeBindShadowmaps_
  where
    addShadowInfo_ lmax mmax hmax inst = do
      let
        omni = instCarried inst
        trsf = instTransform inst
      (omni',i) <- addShadowInfo lmax mmax hmax omni
      return (omni',i,trsf)
    addNoShadows inst =
      let omni = instCarried inst
          trsf = instTransform inst
      in (omni,0,trsf)
    genShadowmap_ shdws (omni,shadowmapIndex,trsf) =
      genShadowmap omni shadowmapIndex trsf (concatMap dropProgram groups) shdws

dropProgram :: GroupModel -> [Instance GPUMesh]
dropProgram (GroupModel _ _ _ i) = map (fmap $ fst . unModel) i

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

renderGroupModel :: GroupModel -> IO () -> IO ()
renderGroupModel (GroupModel prog gmSemantics matSemantics insts) beforeRender = do
  useProgram prog
  runShaderSemantics gmSemantics
  beforeRender 
  traverse_ (renderMeshInstance matSemantics) insts

renderMeshInstance :: (mat -> ShaderSemantics ()) -> Instance (Model mat) -> IO ()
renderMeshInstance semantics inst = do
    runShaderSemantics $ semantics mat
    renderMesh gmesh modelUniform trsf
  where
    (gmesh,mat) = unModel $ instCarried inst
    trsf  = instTransform inst
