{-# LANGUAGE DataKinds #-}

module DiffeoEquations (diffeo0, diffeo1)
where

import TensorTreeNumeric4 (Tensor8, VarMap,
                           shiftVarLabels,
                           interArea, trianMapAreaI, trianMapAreaJ,
                           flatAreaNoEps,
                           tensorContr20,
                           tensorContr3, tensorProd8, invEta, tensorAdd8,
                           tensorProdWith8, tensorAddWith8, tensorContrWith20,
                           tensorTransWithU20, addVarsMap, multVarsMap,
                           toListShow8, tensorTransU9, tensorTransL20, tensorTransL9,
                           tensorTransU20, delta20, delta9, delta3, interEqn3,
                           interEqn2, trianMapI2, trianMapJ2, tensorTransL3, tensorTransU3,
                           tensorSub8)

mult :: VarMap -> Rational -> VarMap
mult = flip multVarsMap

int :: Tensor8 1 1 0 0 0 0 1 1 Rational
int = interArea trianMapAreaI trianMapAreaJ

intFlat :: Tensor8 0 1 0 0 0 0 1 1 Rational
intFlat = tensorContr20 (0,0) $ tensorProd8 flatAreaNoEps int

diffeo0 :: Tensor8 1 0 0 0 0 0 0 0 VarMap -> Tensor8 0 0 0 0 0 0 1 1 VarMap
diffeo0 ansatz4 = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 mult ansatz4 intFlat

diffeo1 :: Tensor8 1 0 0 0 0 0 0 0 VarMap ->
           Tensor8 2 0 0 0 0 0 0 0 VarMap ->
           Tensor8 1 0 0 0 0 0 1 1 VarMap
diffeo1 ansatz4 ansatz8 = result
    where
        block1 = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 mult ansatz4 int
        block2 = tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 mult ansatz8 intFlat
        result = tensorAddWith8 addVarsMap block1 block2
