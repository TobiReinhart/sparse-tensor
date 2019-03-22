{-# LANGUAGE DataKinds #-}

module VariousMatrices (condMat, flatAreaSDown', newInt)
where

import TensorTreeNumeric4 (Tensor8, interArea, trianMapAreaI, trianMapAreaJ,
                           tensorContr3, tensorProd8, invEta, tensorAdd8,
                           toListShow8, tensorTransU9, tensorTransL20, tensorTransL9,
                           tensorTransU20, delta20, delta9, delta3, interEqn3,
                           interEqn2, trianMapI2, trianMapJ2, tensorTransL3, tensorTransU3,
                           tensorSub8, flatArea, tensorContr20, interJ2,
                           tensorSMult, interIArea, flatAreaSDown, flatAreaSUp)
import PerturbationTree2_2 (triangleMap2P)

import AnsatzMatrices (int, inta, intI)
import qualified Data.Map.Strict as M ((!))
import Data.Ratio

trian = triangleMap2P

interI :: Tensor8 1 0 0 0 0 0 0 4 Rational
interI = interIArea trianMapAreaI

flatAreaSDown' :: Tensor8 0 0 0 0 0 0 0 4 Rational
flatAreaSDown' = result
    where
        prod = tensorProd8 interI flatArea
        result = tensorContr20 (0,0) prod

intASym :: Tensor8 1 1 0 0 0 0 2 0 Rational
intASym = result
    where
        intUp  = tensorContr3 (1,0) $ tensorProd8 int invEta
        intUpT = tensorTransU3 (0,1) intUp
        result = tensorSub8 intUp intUpT

cond :: Tensor8 1 1 0 0 0 0 2 0 Rational
cond = intASym

newInt :: Tensor8 0 1 0 0 0 0 4 0 Rational
newInt = result
    where
        prod = tensorProd8 int $ tensorProd8 flatArea flatAreaSUp
        block1 = tensorContr20 (0,1) $ tensorContr3 (1,0) prod
        block2 = tensorSMult (-1) $ tensorTransU3 (0,1) $ block1
        total  = tensorAdd8 block1 block2
        result = tensorSMult (1%2) total

evalCond :: ([Int], Rational) -> ((Int, Int), Rational)
evalCond ([a,b,m,n], x) = ((row, column), x)
    where
        row = 16*b + 4*m + n + 1
        column = a + 1

condMat :: [((Int, Int), Rational)]
condMat = map evalCond $ toListShow8 cond
