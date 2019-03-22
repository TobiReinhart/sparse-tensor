{-# LANGUAGE DataKinds #-}

module ScalarMatrices (pde0Mat, pde1Mat, intcondMat)
where

import TensorTreeNumeric4 (Tensor8, interArea, trianMapAreaI, trianMapAreaJ,
                           tensorContr3, tensorProd8, invEta, tensorAdd8,
                           toListShow8, tensorTransU9, tensorTransL20, tensorTransL9,
                           tensorTransU20, delta20, delta9, delta3, interEqn3,
                           interEqn2, trianMapI2, trianMapJ2, tensorTransL3, tensorTransU3,
                           tensorSub8, flatArea, tensorContr20, interJ2,
                           tensorSMult)
import PerturbationTree2_2 (triangleMap2P)

import AnsatzMatrices (int, inta, intI)
import qualified Data.Map.Strict as M ((!))

trian = triangleMap2P

intFlat :: Tensor8 0 1 0 0 0 0 1 1 Rational
intFlat = tensorContr20 (0,1) $ tensorProd8 int flatArea

pde0 :: Tensor8 0 1 0 0 0 0 1 1 Rational
pde0 = intFlat

evalpde0 :: ([Int], Rational) -> ((Int, Int), Rational)
evalpde0 ([a,m,n], x) = ((row, column), x)
    where
        row = 4*m + n + 1
        column = a + 1

pde1_0 :: Tensor8 1 1 0 0 0 0 1 1 Rational
pde1_0 = tensorSMult 2 int

evalpde1_0 :: ([Int], Rational) -> ((Int, Int), Rational)
evalpde1_0 ([b,a,m,n], x) = ((row, column), x)
    where
        row = 16*b + 4*m + n + 1
        column = a + 1

pde1_1 :: Tensor8 1 2 0 0 0 0 1 1 Rational
pde1_1 = result
    where
        block1 = tensorProd8 delta20 $ intFlat
        block2 = tensorTransL20 (0,1) block1
        result = tensorAdd8 block1 block2

evalpde1_1 :: ([Int], Rational) -> ((Int, Int), Rational)
evalpde1_1 ([a2,a1,b1,m,n], x) = ((row, column), x)
    where
        row = 16*a2 + 4*m + n + 1
        a' = min a1 b1 + 1
        b' = max a1 b1 + 1
        column = trian M.! [a', b'] + 315

intcond :: Tensor8 0 1 0 0 0 0 2 2 Rational
intcond = result
    where
        block1 = tensorContr20 (0,1) $ tensorProd8 int intFlat
        block2 = tensorSMult (-1) $ tensorTransU3 (0,1) $ tensorTransL3 (0,1) block1
        result = tensorAdd8 block1 block2

evalintcond :: ([Int], Rational) -> ((Int, Int), Rational)
evalintcond ([a,m,r,n,s], x) = ((row, column), x)
    where
        row = 64*s + 16*r + 4*n + m + 1
        column = a + 1

pde0Mat :: [((Int, Int), Rational)]
pde0Mat = map evalpde0 $ toListShow8 pde0

intcondMat :: [((Int, Int), Rational)]
intcondMat = map evalintcond $ toListShow8 intcond

pde1Mat :: [((Int, Int), Rational)]
pde1Mat = eval1 ++ eval2
    where
        eval1 = map evalpde1_0 $ toListShow8 pde1_0
        eval2 = map evalpde1_1 $ toListShow8 pde1_1
