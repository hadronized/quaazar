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
import Linear
import Photon.Core.Color
import Photon.Core.Entity
import Photon.Core.Light
import Photon.Core.Projection ( Projection(..), projectionMatrix )
import Photon.Render.Camera ( GPUCamera(..) )
import Photon.Render.Forward.Accumulation
import Photon.Render.Forward.Lighting
import Photon.Render.Forward.Shaded ( Shaded(..) )
import Photon.Render.Forward.Shadowing
import Photon.Render.Forward.Viewport
import Photon.Render.GL.Framebuffer ( Target(..), bindFramebuffer )
import Photon.Render.GL.Offscreen
import Photon.Render.GL.Shader ( (@=), unused, useProgram )
import Photon.Render.GL.Texture ( bindTextureAt )
import Photon.Render.GL.VertexArray ( bindVertexArray )

newtype Lit = Lit { unLit :: Viewport -> Lighting -> Shadowing -> Accumulation -> GPUCamera -> IO () }

instance Monoid Lit where
  mempty =  Lit $ \_ _ _ _ _ -> return ()
  Lit f `mappend` Lit g = Lit $ \v l s a c -> f v l s a c >> g v l s a c

lighten :: Light -> Entity -> Shaded -> Lit
lighten lig ent shd = case lig of
    Omni ligCol ligPower ligRad castShadows
      | castShadows -> Lit $ omniWithShadows ligCol ligPower ligRad
      | otherwise -> Lit $ omniWithoutShadows ligCol ligPower ligRad
  where
    omniWithShadows ligCol ligPower ligRad screenViewport lighting shadowing accumulation gpucam = do
      purgeShadowingFramebuffer shadowing
      generateOmniLightDepthmap screenViewport shadowing shd ligRad ent
      purgeLightingFramebuffer lighting
      applyOmniLighting lighting shd ligCol ligPower ligRad ent
      generateOmniShadowmap lighting shadowing accumulation ligRad ent gpucam
      purgeAccumulationFramebuffer2 accumulation
      combineOmniShadows lighting shadowing accumulation
      accumulate lighting accumulation
    omniWithoutShadows ligCol ligPower ligRad _ lighting _ accumulation _ = do
      purgeAccumulationFramebuffer2 accumulation
      applyOmniLighting lighting shd ligCol ligPower ligRad ent
      accumulate lighting accumulation

applyOmniLighting :: Lighting -> Shaded -> Color -> Float -> Float -> Entity -> IO ()
applyOmniLighting lighting shd ligCol ligPower ligRad ent = do
    useProgram (lighting^.omniLightProgram)
    glDisable gl_BLEND
    glEnable gl_DEPTH_TEST
    lunis^.omniLightColU @= ligCol
    lunis^.omniLightPowU @= ligPower
    lunis^.omniLightRadU @= ligRad
    lunis^.omniLightPosU @= ent^.entityPosition
    unShaded shd lighting
  where
    lunis = lighting^.omniLightUniforms

generateOmniLightDepthmap :: Viewport
                          -> Shadowing
                          -> Shaded
                          -> Float
                          -> Entity
                          -> IO ()
generateOmniLightDepthmap screenViewport shadowing shd ligRad ent = do
    useProgram (shadowing^.shadowCubeDepthmapProgram)
    ligProjViewsU @= omniProjViews 0.1 ligRad -- TODO: per-light znear
    ligPosU @= ent^.entityPosition
    ligIRadU @= 1 / ligRad
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

omniProjViews :: Float -> Float -> [M44 Float]
omniProjViews znear radius =
    map ((proj znear !*!) . completeM33RotMat . fromQuaternion)
      [
        axisAngle yAxis (-pi/2) * axisAngle zAxis pi -- positive x
      , axisAngle yAxis (pi/2) * axisAngle zAxis pi -- negative x
      , axisAngle xAxis (-pi/2) -- positive y
      , axisAngle xAxis (pi/2) -- negative y
      , axisAngle yAxis pi * axisAngle zAxis pi -- positive z
      , axisAngle zAxis (pi) -- negative z
      ]
  where
    proj znear = projectionMatrix $ Perspective (pi/2) 1 znear radius

completeM33RotMat :: M33 Float -> M44 Float
completeM33RotMat (V3 (V3 a b c) (V3 d e f) (V3 g h i)) =
  V4
    (V4 a b c 0)
    (V4 d e f 0)
    (V4 g h i 0)
    (V4 0 0 0 1)

generateOmniShadowmap :: Lighting
                      -> Shadowing
                      -> Accumulation
                      -> Float
                      -> Entity
                      -> GPUCamera
                      -> IO ()
generateOmniShadowmap lighting shadowing accumulation ligRad lent gpucam = do
    useProgram (shadowing^.shadowShadowProgram)
    bindFramebuffer (shadowing^.shadowShadowOff.offscreenFB) ReadWrite
    bindTextureAt (lighting^.lightOff.offscreenDepthmap) 0
    bindTextureAt (shadowing^.shadowDepthCubeOff.cubeOffscreenColorTex) 1
    bindVertexArray (accumulation^.accumVA)
    runCamera gpucam unused iProjViewU unused
    ligRadU @= ligRad
    ligPosU @= lent^.entityPosition
    glDrawArrays gl_TRIANGLE_STRIP 0 4
  where
    sunis = shadowing^.shadowUniforms
    ligRadU = sunis^.shadowShadowLigRadU
    ligPosU = sunis^.shadowShadowLigPosU
    iProjViewU = sunis^.shadowShadowIProjViewU

-- The idea is to copy the lighting render into the second accum buffer. We
-- then copy the shadowmap and blend the two images with a smart blending
-- function.
combineOmniShadows :: Lighting -> Shadowing -> Accumulation -> IO ()
combineOmniShadows lighting shadowing accumulation = do
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

accumulate :: Lighting -> Accumulation -> IO ()
accumulate lighting accumulation = do
  useProgram (accumulation^.accumProgram)
  bindFramebuffer (accumulation^.accumOff.offscreenFB) ReadWrite
  glDisable gl_DEPTH_TEST
  glEnable gl_BLEND
  glBlendFunc gl_ONE gl_ONE
  bindTextureAt (accumulation^.accumOff2.offscreenRender) 0
  bindVertexArray (accumulation^.accumVA)
  glDrawArrays gl_TRIANGLE_STRIP 0 4
