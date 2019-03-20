{-# LANGUAGE DataKinds #-}

module AnsatzMatrices (showMatLab, int, inta, intI,
                       ansatzAMat, ansatzAaMat, ansatzAIMat,
                       ansatzABMat, ansatzABbMat, ansatzABJMat,
                       ansatzAaBbMat, ansatzAaBJMat, ansatzAIBJMat)
where

import TensorTreeNumeric4 (Tensor8, interArea, trianMapAreaI, trianMapAreaJ,
                           tensorContr3, tensorProd8, invEta, tensorAdd8,
                           toListShow8, tensorTransU9, tensorTransL20, tensorTransL9,
                           tensorTransU20, delta20, delta9, delta3, interEqn3,
                           interEqn2, trianMapI2, trianMapJ2, tensorTransL3, tensorTransU3,
                           tensorSub8)

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

inta :: Tensor8 1 1 0 0 0 0 2 2 Rational
inta = interEqn2 trianMapAreaI trianMapAreaJ

intI :: Tensor8 1 1 0 0 1 1 1 1 Rational
intI = interEqn3 trianMapI2 trianMapJ2 trianMapAreaI trianMapAreaJ

intASym :: Tensor8 1 1 0 0 0 0 2 0 Rational
intASym = result
    where
        intUp  = tensorContr3 (1,0) $ tensorProd8 int invEta
        intUpT = tensorTransU3 (0,1) intUp
        result = tensorSub8 intUp intUpT

intaASym :: Tensor8 1 1 0 0 0 0 3 1 Rational
intaASym = result
    where
        intUp  = tensorTransU3 (1,2) $ tensorContr3 (2,0) $ tensorProd8 inta invEta
        intUpT = tensorTransU3 (0,1) intUp
        result = tensorSub8 intUp intUpT

intIASym :: Tensor8 1 1 0 0 1 1 2 0 Rational
intIASym = result
    where
        intUp  = tensorContr3 (1,0) $ tensorProd8 intI invEta
        intUpT = tensorTransU3 (0,1) intUp
        result = tensorSub8 intUp intUpT

ansatzA :: Tensor8 1 1 0 0 0 0 2 0 Rational
ansatzA = intASym

ansatzAa :: Tensor8 1 1 0 0 0 0 3 1 Rational
ansatzAa = intaASym

ansatzAI :: Tensor8 1 1 0 0 1 1 2 0 Rational
ansatzAI = intIASym

ansatzAB :: Tensor8 2 2 0 0 0 0 2 0 Rational
ansatzAB = result
    where
        block1     = tensorProd8 delta20 intASym
        block2     = tensorTransU20 (0,1) block1
        total      = tensorAdd8 block1 block2
        totalTrans = tensorTransL20 (0,1) total
        result     = tensorAdd8 total totalTrans

ansatzABb :: Tensor8 2 2 0 0 0 0 3 1 Rational
ansatzABb = result
    where
        block1     = tensorProd8 delta20 intaASym
        block2     = tensorProd8 intASym $ tensorProd8 delta20 delta3
        result     = tensorAdd8 block1 block2

ansatzABJ :: Tensor8 2 2 0 0 1 1 2 0 Rational
ansatzABJ = result
    where
        block1     = tensorProd8 delta20 intIASym
        block2     = tensorTransL20 (0,1) $ tensorTransU20 (0,1) $ tensorProd8 delta20 $ tensorProd8 delta9 intASym
        result     = tensorAdd8 block1 block2

ansatzAaBb :: Tensor8 2 2 0 0 0 0 4 2 Rational
ansatzAaBb = result
    where
        block1     = tensorProd8 delta20 $ tensorProd8 delta3 intaASym
        block2     = tensorTransU20 (0,1) $ tensorTransU3 (0,3) block1
        total      = tensorAdd8 block1 block2
        totalTrans = tensorTransL20 (0,1) $ tensorTransL3 (0,1) total
        result     = tensorAdd8 total totalTrans

ansatzAaBJ :: Tensor8 2 2 0 0 1 1 3 1 Rational
ansatzAaBJ = result
    where
        block1     = tensorProd8 delta20 $ tensorProd8 delta3 intIASym
        block2     = tensorProd8 intaASym $ tensorProd8 delta20 delta9
        result     = tensorAdd8 block1 block2

ansatzAIBJ :: Tensor8 2 2 0 0 2 2 2 0 Rational
ansatzAIBJ = result
    where
        block1     = tensorProd8 delta20 $ tensorProd8 delta9 intIASym
        block2     = tensorTransU20 (0,1) $ tensorTransU9 (0,1) block1
        total      = tensorAdd8 block1 block2
        totalTrans = tensorTransL20 (0,1) $ tensorTransL9 (0,1) total
        result     = tensorAdd8 total totalTrans

evalAnsatzAMat :: ([Int], Rational) -> ((Int, Int), Rational)
evalAnsatzAMat ([b,a,m,n], x) = ((row, column), x)
    where
        row = 16*b + 4*m + n + 1
        column = a + 1 + 1

evalAnsatzAaMat :: ([Int], Rational) -> ((Int, Int), Rational)
evalAnsatzAaMat ([a2,a1,m,n,p2,p1], x) = ((row, column), x)
    where
        row = 16*4*a2 + 16*p2 + 4*m + n + 1
        column = 4*a1 + p1 + 1 + 1 + 21

evalAnsatzAIMat :: ([Int], Rational) -> ((Int, Int), Rational)
evalAnsatzAIMat ([b,a,j,i,m,n], x) = ((row, column), x)
    where
        row = 160*b + 16*j + 4*m + n + 1
        column = 10*a + i + 1 + 1 + 21 + 84

evalAnsatzABMat :: ([Int], Rational) -> ((Int, Int), Rational)
evalAnsatzABMat ([a2,b2,a1,b1,m,n], x) = ((row, column), x)
    where
        trian = triangleMap2P
        row = 16*21*a2 + 16*b2 + 4*m + n + 1
        column = trian M.! [ min a1 b1 + 1, max a1 b1 + 1] + 315 + 1

evalAnsatzABbMat :: ([Int], Rational) -> ((Int, Int), Rational)
evalAnsatzABbMat ([a2,b2,a1,b1,m,n,p2,p1], x) = ((row, column), x)
    where
        trian = triangleMap2P
        row = 16*4*21*a2 + 16*4*b2 + 16*p2 + 4*m + n + 1
        column = trian M.! [ a1 + 1, 4*b1 + p1 + 21 + 1] + 315 + 1

evalAnsatzABJMat :: ([Int], Rational) -> ((Int, Int), Rational)
evalAnsatzABJMat ([a2,b2,a1,b1,i2,i1,m,n], x) = ((row, column), x)
    where
        trian = triangleMap2P
        row = 16*10*21*a2 + 16*10*b2 + 16*i2 + 4*m + n + 1
        column = trian M.! [ a1 + 1, 10*b1 + i1 + 21 + 84 + 1] + 315 + 1

evalAnsatzAaBbMat :: ([Int], Rational) -> ((Int, Int), Rational)
evalAnsatzAaBbMat ([a2,b2,a1,b1,p2,m,n,q2,p1,q1], x) = ((row, column), x)
    where
        trian = triangleMap2P
        row = 16*10*4*21*4*a2 + 16*4*21*p2 + 16*4*b2 + 16*q2 + 4*m + n + 1
        ap = 4*a1 + p1 + 21 + 1
        bq = 4*b1 + q1 + 21 + 1
        column = trian M.! [min ap bq, max ap bq] + 315 + 1

evalAnsatzAaBJMat :: ([Int], Rational) -> ((Int, Int), Rational)
evalAnsatzAaBJMat ([a2,b2,a1,b1,i2,i1,p2,m,n,p1], x) = ((row, column), x)
    where
        trian = triangleMap2P
        row = 16*10*21*4*a2 + 16*10*21*p2 + 16*10*b2 + 16*i2 + 4*m + n + 1
        column = trian M.! [ 4*a1 + p1 + 21 + 1, 10*b1 + i1 + 21 + 84 + 1] + 315 + 1

evalAnsatzAIBJMat :: ([Int], Rational) -> ((Int, Int), Rational)
evalAnsatzAIBJMat ([a2,b2,a1,b1,i2,j2,i1,j1,m,n], x) = ((row, column), x)
    where
        trian = triangleMap2P
        row = 16*10*21*10*a2 + 16*10*21*i2 + 16*10*b2 + 16*j2 + 4*m + n + 1
        ai = 10*a1 + i1 + 84 + 21 + 1
        bj = 10*b1 + j1 + 84 + 21 + 1
        column = trian M.! [min ai bj, max ai bj] + 315 + 1

ansatzAMat :: [((Int, Int), Rational)]
ansatzAMat = map evalAnsatzAMat $ toListShow8 ansatzA

ansatzAaMat :: [((Int, Int), Rational)]
ansatzAaMat = map evalAnsatzAaMat $ toListShow8 ansatzAa

ansatzAIMat :: [((Int, Int), Rational)]
ansatzAIMat = map evalAnsatzAIMat $ toListShow8 ansatzAI

ansatzABMat :: [((Int, Int), Rational)]
ansatzABMat = map evalAnsatzABMat $ toListShow8 ansatzAB

ansatzABbMat :: [((Int, Int), Rational)]
ansatzABbMat = map evalAnsatzABbMat $ toListShow8 ansatzABb

ansatzABJMat :: [((Int, Int), Rational)]
ansatzABJMat = map evalAnsatzABJMat $ toListShow8 ansatzABJ

ansatzAaBbMat :: [((Int, Int), Rational)]
ansatzAaBbMat = map evalAnsatzAaBbMat $ toListShow8 ansatzAaBb

ansatzAaBJMat :: [((Int, Int), Rational)]
ansatzAaBJMat = map evalAnsatzAaBJMat $ toListShow8 ansatzAaBJ

ansatzAIBJMat :: [((Int, Int), Rational)]
ansatzAIBJMat = map evalAnsatzAIBJMat $ toListShow8 ansatzAIBJ
