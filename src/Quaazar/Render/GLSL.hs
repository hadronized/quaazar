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

module Quaazar.Render.GLSL where

import Numeric.Natural ( Natural )

--------------------------------------------------------------------------------
-- GLSL SEMANTICS
declUniform :: Int -> String -> String
declUniform s n = "layout (location = " ++ show s ++ ") uniform " ++ n ++ ";"

camProjViewSem :: Int
camProjViewSem = 0

modelSem :: Int
modelSem = 1

eyeSem :: Int
eyeSem = 2

ligAmbColSem :: Int
ligAmbColSem = 3

ligAmbPowSem :: Int
ligAmbPowSem = 4

ligOmniNbSem :: Int
ligOmniNbSem = 5

--------------------------------------------------------------------------------
-- GLSL BINDING POINTS
declUniformBlock :: Natural -> String -> String
declUniformBlock bp block = "layout (std430,binding = " ++ show bp ++ ") buffer " ++ block ++ ";"

ligOmniSSBOBP :: Natural
ligOmniSSBOBP = 0
