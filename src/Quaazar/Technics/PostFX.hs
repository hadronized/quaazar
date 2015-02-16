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

module Quaazar.Technics.PostFX (
    -- * Gamma correction
    gammaCorrection
  , getGammaCorrection
  ) where

import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO )
import Quaazar.Core.PostFX ( PostFX(PostFX) )
import Quaazar.Render.PostFX ( GPUPostFX, gpuPostFXFree )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

gammaCorrection :: PostFX
gammaCorrection = PostFX $ unlines
  [
    "#version 430 core"
  , "uniform sampler2D sourceTex;"
  , "out vec4 frag;"
  , "void main() {"
  , "  frag = pow(texelFetch(sourceTex, ivec2(gl_FragCoord.xy), 0), vec4(1./2.2));"
  , "}"
  ]

getGammaCorrection :: (MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m) => m (GPUPostFX ())
getGammaCorrection = gpuPostFXFree gammaCorrection
