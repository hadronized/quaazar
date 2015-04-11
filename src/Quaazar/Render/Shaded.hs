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

module Quaazar.Render.Shaded where

import Control.Lens
import Control.Monad.State ( evalState )
import Data.Bits ( (.|.) )
import Graphics.Rendering.OpenGL.Raw
import Quaazar.Core.Light
import Quaazar.Core.Transform
import Quaazar.Render.Camera ( GPUCamera(..) )
import Quaazar.Render.GL.Buffer ( Buffer )
import Quaazar.Render.GL.Framebuffer ( Framebuffer, Target(..)
                                     , bindFramebuffer )
import Quaazar.Render.GL.Shader ( (@=), unused )
import Quaazar.Render.Light
import Quaazar.Render.Lighting
import Quaazar.Render.Rendered ( Rendered(..) )
import Quaazar.Render.Shader ( GPUProgram(..) )

newtype Shaded = Shaded {
    unShaded :: Framebuffer                -- ^ lighting framebuffer
             -> Buffer                     -- ^ omni light buffer
             -> Maybe (ShadowConf,Shadows) -- ^ shadows configuration
             -> GPUCamera
             -> Ambient
             -> [(Omni,Transform)]
             -> IO ()
  }

shade :: GPUProgram mat -> Rendered mat -> Shaded
shade gprog rdrd = Shaded $ \fb omniBuffer shadowsConf gcam ambient omnis -> do
    let Ambient ligAmbCol ligAmbPow = ambient
    omnisWithShadows <- case shadowsConf of
      Just (conf,_) -> do 
        let
          omnisWithShadows = flip evalState (0,0,0) $ mapM (addShadowInfo_ lmax mmax hmax) omnis
          lmax = conf^.lowShadowMaxNb
          mmax = conf^.mediumShadowMaxNb
          hmax = conf^.highShadowMaxNb
        -- TODO: create shadowmaps
        return omnisWithShadows
      Nothing -> return $ map addNoShadows omnis
    useProgram gprog
    runCamera gcam camProjViewUniform unused eyeUniform
    bindFramebuffer fb ReadWrite
    purgeFramebuffer fb
    ligAmbColUniform @= ligAmbCol
    ligAmbPowUniform @= ligAmbPow
    pushOmnis omnisWithShadows omniBuffer
    unRendered rdrd modelUniform (sendToProgram gprog)
  where
    addShadowInfo_ lmax mmax hmax (omni,transform) = do
      (omni',lod,index) <- addShadowInfo lmax mmax hmax omni
      return (omni',lod,index,transform)
    addNoShadows (omni,transform) = (omni,0,0,transform)

purgeFramebuffer :: Framebuffer -> IO ()
purgeFramebuffer fb = do
  glClearColor 0 0 0 0
  glClear $ gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT
