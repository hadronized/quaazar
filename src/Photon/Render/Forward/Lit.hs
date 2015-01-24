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

module Photon.Render.Forward.Lit where

import Control.Lens
import Data.Bits ( (.|.) )
import Data.Monoid
import Graphics.Rendering.OpenGL.Raw
import Photon.Core.Entity
import Photon.Render.Camera ( GPUCamera(..) )
import Photon.Render.Forward.Accumulation
import Photon.Render.Forward.Lighting
import Photon.Render.Forward.Shaded ( Shaded(..) )
import Photon.Render.Forward.Shadowing
import Photon.Render.Forward.Viewport
import Photon.Render.GL.Framebuffer ( Target(..), bindFramebuffer )
import Photon.Render.GL.Offscreen
import Photon.Render.GL.Shader ( unused, useProgram )
import Photon.Render.GL.Texture ( bindTextureAt )
import Photon.Render.GL.VertexArray ( bindVertexArray )
import Photon.Render.Light ( GPULight(..) )

newtype Lit = Lit { unLit :: Viewport -> Lighting -> Shadowing -> Accumulation -> GPUCamera -> IO () }

instance Monoid Lit where
  mempty =  Lit $ \_ _ _ _ _ -> return ()
  Lit f `mappend` Lit g = Lit $ \v l s a c -> f v l s a c >> g v l s a c

lighten :: GPULight -> Entity -> Shaded -> Lit
lighten gpulig ent shd = Lit lighten_
  where
    lighten_ screenViewport lighting shadowing accumulation gpucam = do
      purgeShadowingFramebuffer shadowing
      onlyIfCastShadows gpulig $ generateLightDepthmap screenViewport shadowing
        shd gpulig ent -- FIXME: per-light
      purgeLightingFramebuffer lighting
      applyLighting lighting shd gpulig ent
      generateShadowmap lighting shadowing accumulation gpulig ent gpucam
      purgeAccumulationFramebuffer2 accumulation
      combineShadows lighting shadowing accumulation
      accumulate lighting shadowing accumulation

applyLighting :: Lighting -> Shaded -> GPULight -> Entity -> IO ()
applyLighting lighting shd gpulig ent = do
    useProgram (lighting^.omniLightProgram)
    glDisable gl_BLEND
    glEnable gl_DEPTH_TEST
    runLight gpulig (lunis^.omniLightColU) (lunis^.omniLightPowU) (lunis^.omniLightRadU)
      (lunis^.omniLightPosU) unused unused ent
    unShaded shd lighting
  where
    lunis = lighting^.omniLightUniforms

generateLightDepthmap :: Viewport
                      -> Shadowing
                      -> Shaded
                      -> GPULight
                      -> Entity
                      -> IO ()
generateLightDepthmap screenViewport shadowing shd gpulig ent = do
    useProgram (shadowing^.shadowCubeDepthmapProgram)
    runLight gpulig unused unused unused ligPosU ligProjViewsU ligIRadU ent
    glDisable gl_BLEND
    glEnable gl_DEPTH_TEST
    setViewport shdwViewport
    unShadedNoMaterial shd shadowing
    setViewport screenViewport
  where
    sunis = shadowing^.shadowUniforms
    ligProjViewsU = sunis^.shadowDepthLigProjViewsU
    ligPosU = sunis^.shadowDepthLigPosU
    ligIRadU = sunis^.shadowDepthLigIRadU
    shdwViewport = shadowing^.shadowViewport

generateShadowmap :: Lighting
                  -> Shadowing
                  -> Accumulation
                  -> GPULight
                  -> Entity
                  -> GPUCamera
                  -> IO ()
generateShadowmap lighting shadowing accumulation gpulig lent gpucam = do
    useProgram (shadowing^.shadowShadowProgram)
    bindFramebuffer (shadowing^.shadowShadowOff.offscreenFB) ReadWrite
    bindTextureAt (lighting^.lightOff.offscreenDepthmap) 0
    bindTextureAt (shadowing^.shadowDepthCubeOff.cubeOffscreenColorTex) 1
    bindVertexArray (accumulation^.accumVA)
    runCamera gpucam unused iProjViewU unused
    runLight gpulig unused unused ligRadU ligPosU unused unused lent
    glDrawArrays gl_TRIANGLE_STRIP 0 4
  where
    sunis = shadowing^.shadowUniforms
    ligRadU = sunis^.shadowShadowLigRadU
    ligPosU = sunis^.shadowShadowLigPosU
    iProjViewU = sunis^.shadowShadowIProjViewU

-- The idea is to copy the lighting render into the second accum buffer. We
-- then copy the shadowmap and blend the two images with a smart blending
-- function.
combineShadows :: Lighting -> Shadowing -> Accumulation -> IO ()
combineShadows lighting shadowing accumulation = do
  useProgram (accumulation^.accumProgram)
  bindFramebuffer (accumulation^.accumOff2.offscreenFB) ReadWrite
  glDisable gl_DEPTH_TEST
  glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
  glDisable gl_BLEND
  -- copy the lighting render
  bindTextureAt (lighting^.lightOff.offscreenRender) 0
  bindVertexArray (accumulation^.accumVA)
  glDrawArrays gl_TRIANGLE_STRIP 0 4
  -- copy the shadowmap
  glEnable gl_BLEND
  glBlendFunc gl_DST_COLOR gl_ZERO
  bindTextureAt (shadowing^.shadowShadowOff.offscreenRender) 0
  glDrawArrays gl_TRIANGLE_STRIP 0 4

accumulate :: Lighting -> Shadowing -> Accumulation -> IO ()
accumulate lighting shadowing accumulation = do
  useProgram (accumulation^.accumProgram)
  bindFramebuffer (accumulation^.accumOff.offscreenFB) ReadWrite
  glDisable gl_DEPTH_TEST
  glEnable gl_BLEND
  glBlendFunc gl_ONE gl_ONE
  bindTextureAt (accumulation^.accumOff2.offscreenRender) 0
  bindVertexArray (accumulation^.accumVA)
  glDrawArrays gl_TRIANGLE_STRIP 0 4
