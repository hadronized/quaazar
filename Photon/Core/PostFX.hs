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

module Photon.Core.PostFX (
    -- * Post effects
    PostFX(..)
    -- * Frame shader
  , FrameShader
  , int
  , unsigned
  , float
  , relative
  , absolute
  , pixel
  , time
  , (*@)
  , (@*)
  , (@/)
  ) where

import Data.Word ( Word32 )
import Numeric.Natural ( Natural )

-- |A post-process  effect is an endomorphism between two frames.
newtype PostFX frame = PostFX { runPostFX :: frame -> frame }

-- |Frame shader DSL.
data E
  = Add E E
  | Sub E E
  | Mul E E
  | Div E E
  | Recip E
  | Abs E
  | LMul E E
  | RMul E E 
  | Pi
  | Exp E
  | Sqrt E
  | Log E
  | Pow E E
  | Sin E
  | Tan E
  | Cos E
  | ASin E
  | ATan E
  | ACos E
  | SinH E
  | TanH E
  | CosH E
  | ASinH E
  | ATanH E
  | ACosH E
  | LitI Int
  | LitU Word32
  | LitF Float
  | RelLk Int Int
  | AbsLk Natural Natural
  | Time
  | Vec2 E E
  | Vec3 E E E
  | Vec4 E E E E
    deriving (Eq,Show)

type FrameShader = E

instance Num E where
  (+)         = Add
  (-)         = Sub
  (*)         = Mul
  abs         = Abs
  signum      = undefined
  fromInteger = LitI . fromInteger

instance Fractional E where
  (/)          = Div
  recip        = Recip
  fromRational = float . fromRational

instance Floating E where
  pi      = Pi
  exp     = Exp
  sqrt    = Sqrt
  log     = Log
  (**)    = Pow
  logBase = undefined
  sin     = Sin
  tan     = Tan
  cos     = Cos
  asin    = ASin
  atan    = ATan
  acos    = ACos
  sinh    = SinH
  tanh    = TanH
  cosh    = CosH
  asinh   = ASinH
  atanh   = ATanH
  acosh   = ACosH

-- |Int literal.
int :: Int -> E
int = LitI

-- |Unsigned int literal.
unsigned :: Word32 -> E
unsigned = LitU

-- |Float literal.
float :: Float -> E
float = LitF

-- |Relative lookup. @relative row col@ performs a lookup at @row@ rows
-- and @col@ columns from the currently computing pixel and retrieve the
-- corresponding pixel. Both values in pixels.
relative :: Int -> Int -> E
relative = RelLk

-- |Absolute lookup. @absolute row col@ performs a lookup at absolute
-- position from upper-left corner of the frame. Both values in pixels.
absolute :: Natural -> Natural -> E
absolute = AbsLk

-- |Currently computing pixel.
pixel :: E
pixel = relative 0 0

-- |Time.
time :: E
time = Time

-- |Build a 2D-vector.
vec2 :: E -> E -> E
vec2 = Vec2

-- |Build a 3D-vector.
vec3 :: E -> E -> E -> E
vec3 = Vec3

-- |Build a 4D-vector.
vec4 :: E -> E -> E -> E -> E
vec4 = Vec4

-- |Outer left multiplication.
(*@) :: E -> E -> E
(*@) = LMul

-- |Outer right multiplication.
(@*) :: E -> E -> E
(@*) = RMul

-- |Outer right division.
(@/) :: E -> E -> E
a @/ k = a @* recip k
