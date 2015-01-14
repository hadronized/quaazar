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
import Linear
import Photon.Core.Entity
import Photon.Render.Forward.Accumulation
import Photon.Render.Forward.Lighting
import Photon.Render.Forward.Shaded ( Shaded(..) )
import Photon.Render.Forward.Shadowing
import Photon.Render.GL.Framebuffer ( Target(..), bindFramebuffer )
import Photon.Render.GL.Offscreen
import Photon.Render.GL.Shader ( (@=), unused, useProgram )
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
      generateLightDepthmap lighting shadowing shd gpulig ent
      purgeLightingFramebuffer lighting
      withLight lighting shadowing shd gpulig ent
      accumulate lighting accumulation

withLight :: Lighting -> Shadowing -> Shaded -> GPULight -> Entity -> IO ()
withLight lighting shadowing shd gpulig ent = do
    useProgram (lighting^.omniLightProgram)
    glDisable gl_BLEND
    glEnable gl_DEPTH_TEST
    shadeWithLight gpulig (lunis^.lightColU) (lunis^.lightPowU) (lunis^.lightRadU)
      (lunis^.lightPosU) unused ent
    bindTextureAt (shadowing^.shadowCubeRender) 0
    unShaded shd lighting
  where
    lunis = lighting^.lightUniforms

generateLightDepthmap :: Lighting
                      -> Shadowing
                      -> Shaded
                      -> GPULight
                      -> Entity
                      -> IO ()
generateLightDepthmap lighting shadowing shd lig lent = do
    genDepthmap lig $ do
      useProgram (shadowing^.shadowCubeDepthmapProgram)
      ligProjViewsU @= lightProjViews
      ligPosU @= ligPos
      ligIRadU @= 1 / lightRadius lig
      glDisable gl_BLEND
      glEnable gl_DEPTH_TEST
      unShadedNoMaterial shd lighting
  where
    proj = shadowing^.shadowProjection
    sunis = shadowing^.shadowUniforms
    ligProjViewsU = sunis^.shadowLigProjViewsU
    ligPosU = sunis^.shadowLigPosU
    ligIRadU = sunis^.shadowLigIRadU
    modelU = sunis^.shadowModelU
    lightProjViews = map ((proj !*!) . completeM33RotMat . fromQuaternion)
      [
        axisAngle yAxis (-pi/2) * axisAngle zAxis pi -- positive x
      , axisAngle yAxis (pi/2) * axisAngle zAxis pi -- negative x
      , axisAngle xAxis (-pi/2) -- positive y
      , axisAngle xAxis (pi/2) -- negative y
      , axisAngle yAxis pi * axisAngle zAxis pi -- positive z
      , axisAngle zAxis (pi) -- negative z
      ]
    ligPos = lent^.entityPosition

completeM33RotMat :: M33 Float -> M44 Float
completeM33RotMat (V3 (V3 a b c) (V3 d e f) (V3 g h i)) =
  V4
    (V4 a b c 0)
    (V4 d e f 0)
    (V4 g h i 0)
    (V4 0 0 0 1)

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
