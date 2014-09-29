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
-- it’s been fulfilled with a render. It enables to enhance the final
-- aspect of a render, or alter it in fancy ways.
--
-- You can’t directly build 'PostFX' since this type is backend’s
-- renderer-dependent. In order to abstract that away, a new type is
-- introduced: 'FrameShader'. A 'FrameShader' can be seen as a function
-- from a /pixel/ to its updated pixel version. A few extra stuff is
-- available, like time, nearby pixels and so on.
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
  ) where

import Data.Word ( Word32 )
import Numeric.Natural ( Natural )

-- |A post-process  effect is an endomorphism between two frames.
newtype PostFX frame = PostFX { runPostFX :: frame -> frame }

-- |Frame shader DSL.
data FrameShader
  = Add FrameShader FrameShader
  | Sub FrameShader FrameShader
  | Mul FrameShader FrameShader
  | Div FrameShader FrameShader
  | Abs FrameShader
  | LMul FrameShader FrameShader
  | RMul FrameShader FrameShader 
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
    deriving (Eq,Show)

type E = FrameShader

instance Num E where
  (+)         = Add
  (-)         = Sub
  (*)         = Mul
  abs         = Abs
  signum      = undefined
  fromInteger = LitI . fromInteger

instance Fractional E where
  (/) = Div
  fromRational = float . fromRational

instance Floating E where
  pi      = float pi
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

int :: Int -> E
int = LitI

unsigned :: Word32 -> E
unsigned = LitU

float :: Float -> E
float = LitF

relative :: Int -> Int -> E
relative = RelLk

absolute :: Natural -> Natural -> E
absolute = AbsLk

pixel :: E
pixel = relative 0 0

time :: E
time = Time

(*@) :: E -> E -> E
(*@) = LMul

(@*) :: E -> E -> E
(@*) = RMul
