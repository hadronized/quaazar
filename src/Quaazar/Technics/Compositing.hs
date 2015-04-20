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
import Quaazar.Render.Compositing ( Compositor, renderNode )
import Quaazar.Render.GL.Shader ( Uniform, ($=), uniform )
import Quaazar.Render.GL.Texture ( Texture2D, Unit )
import Quaazar.Render.Viewport ( Viewport )
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
                -> m (Compositor Texture2D Texture2D)
gammaCorrection vp = do
  compt <- renderNode vp gammaCorrectionSrc $ \source ->
    gammaCorrectionSourceUniform $= (source,0)
  return (fmap fst compt)

gammaCorrectionSourceUniform :: Uniform (Texture2D,Unit)
gammaCorrectionSourceUniform = uniform 0
