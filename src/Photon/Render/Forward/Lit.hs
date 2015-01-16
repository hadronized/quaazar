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
import Photon.Render.Forward.Accumulation
import Photon.Render.Forward.Lighting
import Photon.Render.Forward.Shaded ( Shaded(..) )
import Photon.Render.Forward.Shadowing
import Photon.Render.GL.Framebuffer ( Target(..), bindFramebuffer )
import Photon.Render.GL.Offscreen
import Photon.Render.GL.Shader ( useProgram )
import Photon.Render.GL.Texture ( bindTextureAt )
import Photon.Render.GL.VertexArray ( bindVertexArray )
import Photon.Render.Light ( GPULight(..) )

newtype Lit = Lit { unLit :: Lighting -> Shadowing -> Accumulation -> IO () }

instance Monoid Lit where
  mempty =  Lit $ \_ _ _ -> return ()
  Lit f `mappend` Lit g = Lit $ \l s a -> f l s a >> g l s a

lighten :: GPULight -> Entity -> Shaded -> Lit
lighten gpulig ent shd = Lit lighten_
  where
    lighten_ lighting shadowing accumulation = do
      purgeShadowingFramebuffer shadowing
      generateLightDepthmap shadowing shd gpulig ent 0.1 -- FIXME: per-light
      purgeLightingFramebuffer lighting
      withLight lighting shadowing shd gpulig ent
      accumulate lighting accumulation

withLight :: Lighting -> Shadowing -> Shaded -> GPULight -> Entity -> IO ()
withLight lighting shadowing shd gpulig ent = do
    useProgram (lighting^.omniLightProgram)
    glDisable gl_BLEND
    glEnable gl_DEPTH_TEST
    shadeWithLight gpulig (lunis^.lightColU) (lunis^.lightPowU) (lunis^.lightRadU)
      (lunis^.lightPosU) ent
    bindTextureAt (shadowing^.shadowDepthCubeOff.cubeOffscreenColorTex) 0
    unShaded shd lighting
  where
    lunis = lighting^.lightUniforms

generateLightDepthmap :: Shadowing
                      -> Shaded
                      -> GPULight
                      -> Entity
                      -> Float
                      -> IO ()
generateLightDepthmap shadowing shd lig ent znear = do
    useProgram (shadowing^.shadowCubeDepthmapProgram)
    genDepthmap lig ligProjViewsU ligPosU ligIRadU ent znear
    glDisable gl_BLEND
    glEnable gl_DEPTH_TEST
    unShadedNoMaterial shd shadowing
  where
    sunis = shadowing^.shadowUniforms
    ligProjViewsU = sunis^.shadowDepthLigProjViewsU
    ligPosU = sunis^.shadowDepthLigPosU
    ligIRadU = sunis^.shadowDepthLigIRadU

accumulate :: Lighting -> Accumulation -> IO ()
accumulate lighting accumulation = do
  useProgram (accumulation^.accumProgram)
  bindFramebuffer (accumulation^.accumOff.offscreenFB) ReadWrite
  glClear gl_DEPTH_BUFFER_BIT -- FIXME: glDisable gl_DEPTH_TEST ?
  glEnable gl_BLEND
  glBlendFunc gl_ONE gl_ONE
  bindTextureAt (lighting^.lightOff.offscreenTex) 0
  bindVertexArray (accumulation^.accumVA)
  glDrawArrays gl_TRIANGLE_STRIP 0 4
