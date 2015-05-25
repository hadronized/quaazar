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
import Data.Foldable ( traverse_ )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Lighting.Light
import Quaazar.Render.Camera ( GPUCamera(..), gpuCamera )
import Quaazar.Render.Lighting
import Quaazar.Render.Viewport ( Viewport, setViewport )
import Quaazar.Render.GL.Buffer ( Buffer )
import Quaazar.Render.GL.Framebuffer ( Framebuffer, Target(..)
                                     , bindFramebuffer )
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Shader ( Uniform, Uniformable(..), (@=), unused
                                , useProgram )
import Quaazar.Render.GL.Texture ( Unit(..) )
import Quaazar.Render.Light
import Quaazar.Render.Mesh ( GPUMesh, renderMesh )
import Quaazar.Render.Projection ( Projection )
import Quaazar.Render.Semantics
import Quaazar.Scene.Hierarchy ( Instance, instCarried, instTransform )

newtype Layer = Layer { layerID :: Natural } deriving (Eq,Ord,Show)

instance Uniformable Layer where
  sendUniform l (Layer i) = sendUniform l i

newtype RenderLayer = RenderLayer {
    unRenderLayer :: Framebuffer                -- output framebuffer
                  -> Buffer                     -- omni light buffer
                  -> Maybe (ShadowConf,Shadows) -- shadows configuration
                  -> Layer
                  -> IO ()
  }

renderLayer :: Instance Projection
            -> Viewport
            -> Ambient
            -> [Instance Omni]
            -> Program' mat
            -> [Instance (GPUMesh,mat)]
            -> RenderLayer
renderLayer cam vp ambient omnis shader models =
  RenderLayer $ \fb omniBuffer shadowsConf layer -> do
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
    setViewport vp
    glClearColor 0 0 0 0
    glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
    renderModels shader models $ do
      runCamera (gpuCamera cam) camProjViewUniform unused eyeUniform
      ligAmbColUniform @= ligAmbCol
      ligAmbPowUniform @= ligAmbPow
      pushOmnis omnisWithShadows omniBuffer
      layerUniform @= layer
      maybeBindShadowmaps_
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
    genShadowmap_ shdws (omni,shadowmapIndex,trsf) =
      genShadowmap omni shadowmapIndex trsf (map (fmap fst) models) shdws

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

renderModels :: Program' mat -> [Instance (GPUMesh,mat)] -> IO () -> IO ()
renderModels (prog,semantics) insts beforeRender = do
  useProgram prog
  beforeRender 
  traverse_ (renderMeshInstance semantics) insts

renderMeshInstance :: (mat -> ShaderSemantics ()) -> Instance (GPUMesh,mat) -> IO ()
renderMeshInstance semantics inst = do
    runShaderSemantics $ semantics mat
    renderMesh gmesh modelUniform trsf
  where
    (gmesh,mat) = instCarried inst
    trsf  = instTransform inst

layerUniform :: Uniform Layer 
layerUniform = toUniform LayerSem
