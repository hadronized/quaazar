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

module Photon.Render.OpenGL.Forward.PostFX where

import Photon.Render.OpenGL.Forward.Frame
import Photon.Render.PostFX ( PostFX(..), FrameShader(..) )

stringCompileFrameShader :: FrameShader -> String
stringCompileFrameShader fs = unlines
    [
      "#version 330 core"
    , "uniform sampler2D src;"
    , "uniform float time;"
    , "void main() {"
    , stringCompile fs
    , "}"
    ]

stringCompile :: FrameShader -> String
stringCompile fs = case fs of
  Add l r -      > stringCompile l ++ " + " ++ stringCompile r
  Sub l r       -> stringCompile l ++ " - " ++ stringCompile r
  Mul l r       -> stringCompile l ++ " * " ++ stringCompile r
  Div l r       -> stringCompile l ++ " / " ++ stringCompile r
  Abs x         -> "abs(" ++ stringCompile x ++ ")"
  LMul l r      -> show l ++ " * " ++ stringCompile r
  RMul l r      -> stringCompile l ++ " * " ++ show r
  Pi            -> show (pi :: Float)
  Exp x         -> "exp(" ++ stringCompile x ++ ")"
  Sqrt x        -> "sqrt(" ++ stringCompile x ++ ")"
  Log x         -> "log(" ++ stringCompile x ++ ")"
  Pow x p       -> "pow(" ++ stringCompile x ++ "," ++ stringCompile p ++ ")"
  Sin x         -> "sin(" ++ stringCompile x ++ ")"
  Tan x         -> "tan(" ++ stringCompile x ++ ")"
  Cos x         -> "cos(" ++ stringCompile x ++ ")"
  ASin x        -> "asin(" ++ stringCompile x ++ ")"
  ATan x        -> "atan(" ++ stringCompile x ++ ")"
  ACos x        -> "acos(" ++ stringCompile x ++ ")"
  SinH x        -> "sinh(" ++ stringCompile x ++ ")"
  TanH x        -> "tanh(" ++ stringCompile x ++ ")"
  CosH x        -> "cosh(" ++ stringCompile x ++ ")"
  ASinH x       -> "asinh(" ++ stringCompile x ++ ")"
  ATanH x       -> "atanh(" ++ stringCompile x ++ ")"
  ACosH x       -> "acosh(" ++ stringCompile x ++ ")"
  LitI x        -> show x
  LitU x        -> show x
  LitF x        -> show x
  RelLk c r     -> "texelFetch(source,ivec2(gl_FragCoord.xy) + ivec2(" ++ show c ++ "," ++ show r ++ "),0)"
  AbsLk c r     -> "texelFetch(source,ivec2(" ++ show c ++ "," ++ show r ++ "),0)"
  Time          -> "time"
  -- FIXME: this is wrong since we can’t build an ivec*, or uvec*
  Vec2 x y      -> "vec2(" ++ stringCompile x ++ "," ++ stringCompile y ++ ")"
  Vec3 x y z    -> "vec3(" ++ stringCompile x ++ "," ++ stringCompile y ++ "," ++ stringCompile z ++ ")"
  Vec4 x y z w  -> "vec4(" ++ stringCompile x ++ "," ++ stringCompile y ++ "," ++ stringCompile z ++ "," ++ stringCompile w ++ ")"
