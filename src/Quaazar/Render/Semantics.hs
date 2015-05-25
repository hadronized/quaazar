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

module Quaazar.Render.Semantics where

import Numeric.Natural ( Natural )
import Quaazar.Render.GL.Shader

--------------------------------------------------------------------------------
-- Shader Semantics

-- |Shader semantics are used by the user to customize shaders. It basically
-- exposes all 'Uniformable' instances, but constraint them into pure code.
--
-- See '($=)' for building 'ShaderSemantics'.
newtype ShaderSemantics a = ShaderSemantics {
    -- |Update all semantics into the currently bound shader program.ã
    runShaderSemantics :: IO a
  } deriving (Applicative,Functor,Monad)

-- |Map a semantic to its value.
($=) :: (Uniformable a) => Uniform a -> a -> ShaderSemantics ()
s $= a = ShaderSemantics $ s @= a

-- FIXME: needs a better name
-- |'Program' with its 'Semantic's.
type Program' a = (Program,a -> ShaderSemantics ())

--------------------------------------------------------------------------------
-- GLSL inputs
data InputSem
  = CoInput
  | NoInput
  | UVInput
    deriving (Enum,Eq,Ord,Show)

declInput :: InputSem -> String -> String
declInput i n = "layout (location = " ++ show (fromInputSem i) ++ ") in " ++ n ++ ";"

fromInputSem :: InputSem -> Natural
fromInputSem = fromIntegral . fromEnum

--------------------------------------------------------------------------------
-- GLSL uniform semantics
data UniformSem
  = CamProjViewSem
  | ModelSem
  | EyeSem
  | LigAmbColSem
  | LigAmbPowSem
  | LigOmniNbSem
  | LigProjViewsSem
  | LigPosSem
  | LigIRadSem
  | ShadowmapIndexSem
  | LowShadowmapsSem
  | MediumShadowmapsSem
  | HighShadowmapsSem
  | LayerSem
  | ExtendSem Int 

instance Enum UniformSem where
  fromEnum u = case u of
    CamProjViewSem -> 0
    ModelSem -> 1
    EyeSem -> 2
    LigAmbColSem -> 3
    LigAmbPowSem -> 4
    LigOmniNbSem -> 5
    LigProjViewsSem -> 6
    LigPosSem -> 7
    LigIRadSem -> 8
    ShadowmapIndexSem -> 14
    LowShadowmapsSem -> 15
    MediumShadowmapsSem -> 16
    HighShadowmapsSem -> 17
    LayerSem -> 18
    ExtendSem sem -> 19 + fromIntegral sem
  toEnum i
    | i == 0 = CamProjViewSem
    | i == 1 = ModelSem
    | i == 2 = EyeSem
    | i == 3 = LigAmbColSem
    | i == 4 = LigAmbPowSem
    | i == 5 = LigOmniNbSem
    | i == 6 = LigProjViewsSem
    | i == 7 = LigPosSem
    | i == 8 = LigIRadSem
    | i == 14 = ShadowmapIndexSem
    | i == 15 = LowShadowmapsSem
    | i == 16 = MediumShadowmapsSem
    | i == 17 = HighShadowmapsSem
    | i == 18 = LayerSem
    | otherwise = ExtendSem $ i - 19

declUniform :: UniformSem -> String -> String
declUniform s n = "layout (location = " ++ show (fromUniformSem s) ++ ") uniform " ++ n ++ ";"

fromUniformSem :: UniformSem -> Natural
fromUniformSem = fromIntegral . fromEnum

toUniform :: (Uniformable a) => UniformSem -> Uniform a
toUniform = uniform . fromUniformSem

extendUniformSem :: (Enum s) => s -> UniformSem
extendUniformSem = ExtendSem . fromEnum

--------------------------------------------------------------------------------
-- GLSL uniform block binding points
declUniformBlock :: Natural -> String -> String
declUniformBlock bp block = "layout (std430,binding = " ++ show bp ++ ") buffer " ++ block ++ ";"

ligOmniSSBOBP :: Natural
ligOmniSSBOBP = 0
