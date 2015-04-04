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

module Quaazar.Technics.Compositing (
    -- * End of pipeline
    gammaCorrection
  ) where

import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO )
import Numeric.Natural ( Natural )
import Quaazar.Render.Compositing ( Compositor, renderNode )
import Quaazar.Render.Forward.Viewport ( Viewport )
import Quaazar.Render.Shader ( Uniform, ($=), uniform )
import Quaazar.Render.Texture ( GPUTexture )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

gammaCorrectionSrc :: String
gammaCorrectionSrc = unlines
  [
    "#version 430 core"
  , "layout (location = 0) uniform sampler2D sourceTex;"
  , "out vec4 frag;"
  , "void main() {"
  , "  frag = pow(texelFetch(sourceTex, ivec2(gl_FragCoord.xy), 0), vec4(1./2.2));"
  , "}"
  ]

gammaCorrection :: (MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
                => Viewport
                -> m (Compositor GPUTexture GPUTexture)
gammaCorrection vp = do
  compt <- renderNode vp gammaCorrectionSrc $ \source ->
    gammaCorrectionSourceUniform $= (source,0)
  return (fmap fst compt)

gammaCorrectionSourceUniform :: Uniform (GPUTexture,Natural)
gammaCorrectionSourceUniform = uniform 0