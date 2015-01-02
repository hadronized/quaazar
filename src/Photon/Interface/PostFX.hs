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

module Photon.Interface.PostFX (
    -- * Gamma correction
    gammaCorrection
  ) where

import Photon.Core.PostFX ( PostFX(PostFX) )

gammaCorrection :: PostFX
gammaCorrection = PostFX $ unlines
  [
    "#version 330 core"
  , "uniform sampler2D sourceTex;"
  , "out vec4 frag;"
  , "void main() {"
  , "  frag = pow(texelFetch(sourceTex, ivec2(gl_FragCoord.xy), 0), vec4(1./2.2));"
  , "}"
  ]
