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

module Quaazar.Render.GL.Primitive where

import Graphics.Rendering.OpenGL.Raw

data Primitive
  = Point
  | Line
  | SLine
  | Triangle
  | STriangle
    deriving (Eq,Show)

fromPrimitive :: Primitive -> GLenum
fromPrimitive primitive = case primitive of
  Point -> gl_POINTS
  Line  -> gl_LINES
  SLine -> gl_LINE_STRIP
  Triangle -> gl_TRIANGLES
  STriangle -> gl_TRIANGLE_STRIP