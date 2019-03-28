{-# LANGUAGE DataKinds #-}

module IntMatrices (intABJMat)
where

import TensorTreeNumeric4 (Tensor8, interArea, trianMapAreaI, trianMapAreaJ,
                           tensorContr3, tensorProd8, invEta, tensorAdd8,
                           toListShow8, tensorTransU9, tensorTransL20, tensorTransL9,
                           tensorTransU20, delta20, delta9, delta3, interEqn3,
                           interEqn2, trianMapI2, trianMapJ2, tensorTransL3, tensorTransU3,
                           tensorSub8, flatAreaNoEps, tensorContr20)
import PerturbationTree2_2 (triangleMap2P)

import AnsatzMatrices (int, inta, intI)
import qualified Data.Map.Strict as M ((!))

trian = triangleMap2P

intFlat :: Tensor8 0 1 0 0 0 0 1 1 Rational
intFlat = tensorContr20 (0,1) $ tensorProd8 int flatAreaNoEps

intABJ :: Tensor8 1 2 0 0 1 1 2 2 Rational
intABJ = result
    where
        block1          = tensorProd8 intFlat intI
        block2          = tensorContr20 (0,1) $ tensorProd8 int $ tensorProd8 intFlat $ tensorProd8 delta20 delta9
        totalBlock      = tensorSub8 block1 block2
        totalBlockTrans = tensorTransU3 (0,1) $ tensorTransL3 (0,1) totalBlock
        result          = tensorSub8 totalBlock totalBlockTrans

evalIntABJMat :: ([Int], Rational) -> ((Int, Int), Rational)
evalIntABJMat ([a2,a1,b1,i2,j1,r,m,s,n], x) = ((row, column), x)
    where
        row = 10*256*a2 + 256*i2 + 64*r + 16*s + 4*m + n + 1
        a' = a1 + 1
        bj = 10*b1 + j1 + 84 + 21 + 1
        column = trian M.! [a', bj] + 315 + 1

intABJMat :: [((Int, Int), Rational)]
intABJMat = map evalIntABJMat $ toListShow8 intABJ
