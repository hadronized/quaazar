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
import Data.Monoid
import Graphics.Rendering.OpenGL.Raw
import Photon.Core.Entity
import Photon.Render.Camera ( GPUCamera(..) )
import Photon.Render.Forward.Accumulation
import Photon.Render.Forward.Lighting
import Photon.Render.Forward.Shaded ( Shaded(..) )
import Photon.Render.Forward.Shadowing
import Photon.Render.GL.Framebuffer ( Target(..), bindFramebuffer )
import Photon.Render.GL.Offscreen
import Photon.Render.GL.Shader ( unused, useProgram )
import Photon.Render.GL.Texture ( bindTextureAt )
import Photon.Render.GL.VertexArray ( bindVertexArray )
import Photon.Render.Light ( GPULight(..) )

newtype Lit = Lit { unLit :: Lighting -> Shadowing -> Accumulation -> GPUCamera -> IO () }

instance Monoid Lit where
  mempty =  Lit $ \_ _ _ _ -> return ()
  Lit f `mappend` Lit g = Lit $ \l s a c -> f l s a c >> g l s a c

lighten :: GPULight -> Entity -> Shaded -> Lit
lighten gpulig ent shd = Lit lighten_
  where
    lighten_ lighting shadowing accumulation gpucam = do
      purgeShadowingFramebuffer shadowing
      generateLightDepthmap shadowing shd gpulig ent -- FIXME: per-light
      purgeLightingFramebuffer lighting
      applyLighting lighting shd gpulig ent
      generateShadowmap lighting shadowing accumulation gpulig ent gpucam
      accumulate lighting shadowing accumulation

applyLighting :: Lighting -> Shaded -> GPULight -> Entity -> IO ()
applyLighting lighting shd gpulig ent = do
    useProgram (lighting^.omniLightProgram)
    glDisable gl_BLEND
    glEnable gl_DEPTH_TEST
    runLight gpulig (lunis^.lightColU) (lunis^.lightPowU) (lunis^.lightRadU)
      (lunis^.lightPosU) unused unused ent
    unShaded shd lighting
  where
    lunis = lighting^.lightUniforms

generateLightDepthmap :: Shadowing
                      -> Shaded
                      -> GPULight
                      -> Entity
                      -> IO ()
generateLightDepthmap shadowing shd gpulig ent = do
    useProgram (shadowing^.shadowCubeDepthmapProgram)
    runLight gpulig unused unused unused ligPosU ligProjViewsU ligIRadU ent
    glDisable gl_BLEND
    glEnable gl_DEPTH_TEST
    unShadedNoMaterial shd shadowing
  where
    sunis = shadowing^.shadowUniforms
    ligProjViewsU = sunis^.shadowDepthLigProjViewsU
    ligPosU = sunis^.shadowDepthLigPosU
    ligIRadU = sunis^.shadowDepthLigIRadU

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
    runCamera gpucam iProjViewU unused unused
    runLight gpulig unused unused ligRadU ligPosU unused unused lent
    glDrawArrays gl_TRIANGLE_STRIP 0 4
  where
    sunis = shadowing^.shadowUniforms
    ligRadU = sunis^.shadowShadowLigRadU
    ligPosU = sunis^.shadowShadowLigPosU
    iProjViewU = sunis^.shadowShadowIProjViewU

accumulate :: Lighting -> Shadowing -> Accumulation -> IO ()
accumulate lighting shadowing accumulation = do
  useProgram (accumulation^.accumProgram)
  bindFramebuffer (accumulation^.accumOff.offscreenFB) ReadWrite
  glClear gl_DEPTH_BUFFER_BIT -- FIXME: glDisable gl_DEPTH_TEST ?
  glEnable gl_BLEND
  glBlendFunc gl_ONE gl_ONE
  bindTextureAt (shadowing^.shadowShadowOff.offscreenRender) 0
  bindVertexArray (accumulation^.accumVA)
  glDrawArrays gl_TRIANGLE_STRIP 0 4
