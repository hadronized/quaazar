-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- This module exports 'PostFX', a useful type used to alter a frame after
-- it’s been fulfilled with a render. It enables enhancing the final
-- aspect of a render, or alter it in fancy ways.
--
-- You can’t directly build a 'PostFX' since this type is backend’s
-- renderer-dependent. In order to abstract that away, a new type is
-- introduced: 'FrameShader'. A 'FrameShader' can be seen as a function
-- from a /pixel/ to its updated pixel version. A few extra stuff is
-- available, like time, nearby pixels lookup functions and so on.
--
-- In order to turn a 'FrameShader' into a 'PostFX', use the 'Renderer'’s
-- 'compileFrameShader' function.
----------------------------------------------------------------------------

module Photon.Render.PostFX (
    -- * Post effects
    PostFX(..)
  ) where

import Data.Word ( Word32 )
import Numeric.Natural ( Natural )

-- |A post-process  effect is an endomorphism between two frames.
newtype PostFX = PostFX String deriving (Eq,Show)
