{-# LANGUAGE DataKinds #-}

module AnsatzEquations (ansatzA, ansatzAB)
where

import TensorTreeNumeric4 (Tensor8, VarMap,
                           shiftVarLabels,
                           interArea, trianMapAreaI, trianMapAreaJ,
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

intASym :: Tensor8 1 1 0 0 0 0 2 0 Rational
intASym = result
    where
        intUp  = tensorContr3 (1,0) $ tensorProd8 int invEta
        intUpT = tensorTransU3 (0,1) intUp
        result = tensorSub8 intUp intUpT

ansatzA :: Tensor8 1 0 0 0 0 0 0 0 VarMap ->
           Tensor8 1 0 0 0 0 0 2 0 VarMap
ansatzA ansatz4 = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 mult ansatz4 intASym

ansatzAB :: Tensor8 2 0 0 0 0 0 0 0 VarMap ->
            Tensor8 2 0 0 0 0 0 2 0 VarMap
ansatzAB ansatz8 = result
    where
        product     = tensorProdWith8 mult ansatz8 intASym
        contraction = tensorContrWith20 (1,0) addVarsMap product
        trans       = tensorTransWithU20 (0,1) addVarsMap contraction
        result      = tensorAddWith8 addVarsMap contraction trans
