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

-- FrameShader -> PostFX frame

-- |A post-process effect is an endomorphism between two frames.
newtype PostFX frame = PostFX { runPostFX :: frame -> frame }

data FrameShader
  = Add FrameShader FrameShader
  | Sub FrameShader FrameShader
  | Mul FrameShader FrameShader
  | Div FrameShader FrameShader
  | Abs FrameShader
  | LMul FrameShader FrameShader
  | RMul FrameShader FrameShader 
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
