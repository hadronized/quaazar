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

module Quaazar.Render.Lit where

import Control.Lens
import Control.Monad.State ( evalState )
import Data.Bits ( (.|.) )
import Data.Monoid ( Monoid(..) )
import Data.Foldable ( for_ )
import Graphics.Rendering.OpenGL.Raw
import Quaazar.Core.Light
import Quaazar.Core.Transform
import Quaazar.Render.Lighting
import Quaazar.Render.Rendered ( Rendered(..) )
import Quaazar.Render.GL.Buffer ( Buffer )
import Quaazar.Render.GL.Framebuffer ( Framebuffer, Target(..)
                                     , bindFramebuffer )
import Quaazar.Render.GL.Shader ( (@=) )
import Quaazar.Render.Light

newtype Lit mat = Lit {
    unLit :: Framebuffer                -- ^ lighting framebuffer
          -> Buffer                     -- ^ omni light buffer
          -> Maybe (ShadowConf,Shadows) -- ^ shadows configuration
          -> (mat -> IO ())             -- ^ material sink
          -> IO ()
  }

lighten :: Ambient -> [(Omni,Transform)] -> Rendered mat -> Lit mat
lighten (Ambient ligAmbCol ligAmbPow) omnis shd = Lit lighten_
  where
    lighten_ lightingFB omniBuffer shadowsConf sinkMat = do
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
      purgeLightingFramebuffer lightingFB
      ligAmbColUniform @= ligAmbCol
      ligAmbPowUniform @= ligAmbPow
      pushOmnis omnisWithShadows omniBuffer
      unRendered shd modelUniform sinkMat
    addShadowInfo_ lmax mmax hmax (omni,transform) = do
      (omni',lod,index) <- addShadowInfo lmax mmax hmax omni
      return (omni',lod,index,transform)
    addNoShadows (omni,transform) = (omni,0,0,transform)

purgeLightingFramebuffer :: Framebuffer -> IO ()
purgeLightingFramebuffer fb = do
  bindFramebuffer fb ReadWrite
  glClearColor 0 0 0 0
  glClear $ gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT
