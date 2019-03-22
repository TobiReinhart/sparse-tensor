{-# LANGUAGE DataKinds #-}

module ScalarEquations (Ansatz0, Ansatz1, Ansatz2, Ansatz3,
                        Eqn0, Eqn1, Eqn2, Eqn3, eqn0, eqn1, eqn2)
where

import TensorTreeNumeric4 (Tensor8, interArea, trianMapAreaI, trianMapAreaJ,
                           tensorContr3, tensorProd8, invEta, tensorAdd8,
                           toListShow8, tensorTransU9, tensorTransL20, tensorTransL9,
                           tensorTransU20, delta20, delta9, delta3, interEqn3,
                           interEqn2, trianMapI2, trianMapJ2, tensorTransL3, tensorTransU3,
                           tensorSub8, flatArea, tensorContr20, VarMap, tensorProdWith8, multVarsMap, tensorContrWith20, addVarsMap, tensorAddWith8, tensorTransWithU20)

import PerturbationTree2_2 (triangleMap2P)

import Data.Ratio (denominator, numerator)
import qualified Data.Map.Strict as M ((!))

showMatLab' :: Int -> [((Int, Int), Rational)] -> String
showMatLab' offset = unlines . map showMatLabSingle . filter ((/=0) . snd)
    where
        showMatLabSingle ((i, j), v) = if denominator v /= 1
                                       then undefined
                                       else show (i+offset) ++ " " ++ show j ++ " " ++ show (numerator v)


showMatLab :: [((Int, Int), Rational)] -> String
showMatLab = showMatLab' 0

int :: Tensor8 1 1 0 0 0 0 1 1 Rational
int = interArea trianMapAreaI trianMapAreaJ

type Ansatz0 = Tensor8 1 0 0 0 0 0 0 0 VarMap
type Ansatz1 = Tensor8 2 0 0 0 0 0 0 0 VarMap
type Ansatz2 = Tensor8 3 0 0 0 0 0 0 0 VarMap
type Ansatz3 = Tensor8 4 0 0 0 0 0 0 0 VarMap

type Eqn0 = Tensor8 0 0 0 0 0 0 1 1 VarMap
type Eqn1 = Tensor8 1 0 0 0 0 0 1 1 VarMap
type Eqn2 = Tensor8 2 0 0 0 0 0 1 1 VarMap
type Eqn3 = Tensor8 3 0 0 0 0 0 1 1 VarMap

flatInt :: Tensor8 0 1 0 0 0 0 1 1 Rational
flatInt = tensorContr20 (0,1) $ tensorProd8 int flatArea

mult :: VarMap -> Rational -> VarMap
mult = flip multVarsMap

eqn0 :: Ansatz0 -> Eqn0
eqn0 ansatz0 = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 mult ansatz0 flatInt

eqn1 :: Ansatz0 -> Ansatz1 -> Eqn1
eqn1 ansatz0 ansatz1 = result
    where
        block1 = tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 mult ansatz1 flatInt
        block2 = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 mult ansatz0 int
        result = tensorAddWith8 addVarsMap block1 block2

eqn2 :: Ansatz1 -> Ansatz2 -> Eqn2
eqn2 ansatz1 ansatz2 = result
    where
        block1 = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 mult ansatz2 flatInt
        block2 = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 mult ansatz1 int
        block3 = tensorTransWithU20 (0,1) addVarsMap block2
        result = tensorAddWith8 addVarsMap block1 $ tensorAddWith8 addVarsMap block2 block3
