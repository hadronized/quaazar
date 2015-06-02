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

module Quaazar.Render.Glyph (
    -- *
  ) where

import Control.Lens
import Control.Monad.Trans ( MonadIO )
import Numeric.Natural
import Quaazar.Render.GL.Buffer
import Quaazar.Render.GL.GLObject
import Quaazar.Utils.Scoped

data Glyph = Glyph {
    _glyphWidth :: Natural
  , _glyphHeight :: Natural
  , _glyphX :: Int
  , _glyphY :: Int
  , _glyphXBase :: Int
  , _glyphYBase :: Int
  } deriving (Eq,Show)

makeLenses ''Glyph

type GlyphMap = Char -> Maybe Glyph

getLineBuffer :: (MonadIO m,MonadScoped IO m)
              => Natural
              -> m Buffer
getLineBuffer maxChar = do
  buf <- genObject
  bindBuffer buf ShaderStorageBuffer
  initBuffer ShaderStorageBuffer _
  unbindBuffer ShaderStorageBuffer
  pure buf
