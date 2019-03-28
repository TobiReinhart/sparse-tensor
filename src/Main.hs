{-# LANGUAGE DataKinds #-}

module Main (
 main
) where

import TensorTreeNumeric4 (getTensorRank,
                           getTensorRank2,
                           getTensorRank3,
                           getTensorRank4,
                           getTensorRank5,
                           shiftVarLabels,
                           genericAreaM,
                           toListShowVar)
import ScalarEquations (genericInt)
import Data.List (sort, intersperse)

import Data.Ratio ((%), denominator, numerator)

prettyVar :: (Int, Rational) -> String
prettyVar (i, v) = case denominator v of 
                    1 -> "x" ++ show i ++ " * (" ++ show (numerator v) ++ ")"

prettyVars :: [(Int, Rational)] -> String
prettyVars = concat . intersperse " + " . map prettyVar

prettySparse :: ((Int, Int), [(Int, Rational)]) -> String
prettySparse ((i, j), v) = "(" ++ show i ++ ", " ++ show j ++ ") = " ++ prettyVars v ++ ","

main :: IO ()
main = putStr $ unlines $ map prettySparse $ sort $ map (\([a,m,n], x) -> ((4*m+n+1, a+1), x)) $ toListShowVar genericInt
