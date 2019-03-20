{-# LANGUAGE DataKinds #-}

module DiffeoMatrices (diffeo_0_0Mat, diffeo_0_1Mat, diffeo_0_2Mat,
                       diffeo_1_0_0Mat, diffeo_1_0_2Mat,
                       diffeo_1_1_1Mat,
                       diffeo_1_2_0Mat, diffeo_1_2_2Mat)
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

intJ :: Tensor8 0 0 0 0 0 1 2 0 Rational
intJ = interJ2 trianMapJ2

intaI :: Tensor8 1 1 0 0 0 1 3 1 Rational
intaI = result
    where
        block1' = tensorProd8 intJ int
        block1  = tensorAdd8 block1' $ tensorTransU3 (1,2) block1'
        block2  = tensorProd8 delta20 $ tensorProd8 delta3 intJ
        result  = tensorSub8 block1 block2

intFlat :: Tensor8 0 1 0 0 0 0 1 1 Rational
intFlat = tensorContr20 (0,1) $ tensorProd8 int flatArea

diffeo_0_0 :: Tensor8 0 1 0 0 0 0 1 1 Rational
diffeo_0_0 = intFlat

evalDiffeo_0_0 :: ([Int], Rational) -> ((Int, Int), Rational)
evalDiffeo_0_0 ([a,m,n], x) = ((row, column), x)
    where
        row = 4*m + n + 1
        column = a + 1 + 1

diffeo_0_1 :: Tensor8 0 1 0 0 0 0 2 2 Rational
diffeo_0_1 = result
    where
        block1 = tensorProd8 delta3 intFlat
        block2 = tensorTransU3 (0,1) block1
        result = tensorAdd8 block1 block2

evalDiffeo_0_1 :: ([Int], Rational) -> ((Int, Int), Rational)
evalDiffeo_0_1 ([a,p2,m,p1,n], x) = ((row, column), x)
    where
        row = 16*p2 + 4*m + n + 1
        column = 4*a + p1 + 21 + 1 + 1

diffeo_0_2 :: Tensor8 0 1 0 0 0 1 3 1 Rational
diffeo_0_2 = result
    where
        block1 = tensorProd8 intJ intFlat
        block2 = tensorTransU3 (0,1) block1
        block3 = tensorTransU3 (1,2) block1
        block4 = tensorTransU3 (0,2) block1
        block5 = tensorTransU3 (0,1) $ tensorTransU3 (1,2) block1
        block6 = tensorTransU3 (0,1) $ tensorTransU3 (0,2) block1
        result = tensorAdd8 block1 $ tensorAdd8 block2 $
                 tensorAdd8 block3 $ tensorAdd8 block4 $
                 tensorAdd8 block5 block6

evalDiffeo_0_2 :: ([Int], Rational) -> ((Int, Int), Rational)
evalDiffeo_0_2 ([a,i,p,q,m,n], x) = ((row, column), x)
    where
        row = 64*p + 16*q + 4*m + n + 1
        column = 10*a + i + 21 + 84 + 1 + 1

diffeo_1_0_0_A :: Tensor8 1 1 0 0 0 0 1 1 Rational
diffeo_1_0_0_A = tensorSMult 2 int

evalDiffeo_1_0_0_A :: ([Int], Rational) -> ((Int, Int), Rational)
evalDiffeo_1_0_0_A ([b,a,m,n], x) = ((row, column), x)
    where
        row = 16*b + 4*m + n + 1
        column = a + 21 + 1 + 1

diffeo_1_0_0_AB :: Tensor8 1 2 0 0 0 0 1 1 Rational
diffeo_1_0_0_AB = result
    where
        block1 = tensorProd8 delta20 $ intFlat
        block2 = tensorTransL20 (0,1) block1
        result = tensorAdd8 block1 block2

evalDiffeo_1_0_0_AB :: ([Int], Rational) -> ((Int, Int), Rational)
evalDiffeo_1_0_0_AB ([a2,a1,b1,m,n], x) = ((row, column), x)
    where
        row = 16*b1 + 4*m + n + 1
        a' = min a1 b1 + 1
        b' = max a1 b1 + 1
        column = trian M.! [a', b'] + 315 + 1

diffeo_1_0_2_AI :: Tensor8 1 1 0 0 1 1 1 1 Rational
diffeo_1_0_2_AI = intI

evalDiffeo_1_0_2_AI :: ([Int], Rational) -> ((Int, Int), Rational)
evalDiffeo_1_0_2_AI ([b,a,j,i,m,n], x) = ((row, column), x)
    where
        row = 16*10*b + 16*j + 4*m + n + 1
        column = 10*a + i + 84 + 21 + 1 + 1

diffeo_1_0_2_ABJ :: Tensor8 1 2 0 0 1 1 1 1 Rational
diffeo_1_0_2_ABJ = tensorProd8 intFlat $ tensorProd8 delta20 delta9

evalDiffeo_1_0_2_ABJ :: ([Int], Rational) -> ((Int, Int), Rational)
evalDiffeo_1_0_2_ABJ ([b2,j2,a,b1,j1,m,n], x) = ((row, column), x)
    where
        row = 16*10*b2 + 16*j2 + 4*m + n + 1
        a' = a + 1
        bj = 10*b1 + j1 + 84 + 21 + 1
        column = trian M.! [a', bj] + 315 + 1

diffeo_1_1_1_AI :: Tensor8 1 1 0 0 0 1 3 1 Rational
diffeo_1_1_1_AI = tensorSMult 4 intaI

evalDiffeo_1_1_1_AI :: ([Int], Rational) -> ((Int, Int), Rational)
evalDiffeo_1_1_1_AI ([a2,a1,i,a,m,b,n], x) = ((row, column), x)
    where
        row = 256*a2 + 64*a + 16*b + 4*m + n + 1
        column = 10*a1 + i + 84 + 21 + 1 + 1

diffeo_1_1_1_AaBb :: Tensor8 1 2 0 0 0 0 3 3 Rational
diffeo_1_1_1_AaBb = result
    where
        block1 = tensorProd8 delta20 $ tensorProd8 delta3 $ tensorProd8 delta3 intFlat
        block2 = tensorTransU3 (1,2) block1
        block3 = tensorTransL20 (0,1) $ tensorTransL3 (0,1) block1
        block4 = tensorTransL20 (0,1) $ tensorTransL3 (0,1) block2
        result = tensorAdd8 block1 $ tensorAdd8 block2 $ tensorAdd8 block3 block4

evalDiffeo_1_1_1_AaBb :: ([Int], Rational) -> ((Int, Int), Rational)
evalDiffeo_1_1_1_AaBb ([a2,a1,b1,p2,q2,m,p1,q1,n], x) = ((row, column), x)
    where
        row = 256*a2 + 64*p2 + 16*q2 + 4*m + n + 1
        a' = 4*a1 + p1 + 21 + 1
        b' = 4*b1 + q1 + 21 + 1
        column = trian M.! [min a' b', max a' b'] + 315 + 1

diffeo_1_2_0_AI :: Tensor8 1 1 0 0 0 1 3 1 Rational
diffeo_1_2_0_AI = result
    where
        block1 = tensorProd8 intJ int
        block2 = tensorTransU3 (0,1) block1
        block3 = tensorTransU3 (1,2) block1
        block4 = tensorTransU3 (0,2) block1
        block5 = tensorTransU3 (0,1) $ tensorTransU3 (1,2) block1
        block6 = tensorTransU3 (0,1) $ tensorTransU3 (0,2) block1
        result = tensorAdd8 block1 $ tensorAdd8 block2 $
                 tensorAdd8 block3 $ tensorAdd8 block4 $
                 tensorAdd8 block5 block6

evalDiffeo_1_2_0_AI :: ([Int], Rational) -> ((Int, Int), Rational)
evalDiffeo_1_2_0_AI ([a2,a1,i,p,q,m,n], x) = ((row, column), x)
    where
        row = 256*a2 + 64*p + 16*q + 4*m + n + 1
        column = 10*a1 + i + 84 + 21 + 1 + 1

diffeo_1_2_0_ABJ :: Tensor8 1 2 0 0 0 1 3 1 Rational
diffeo_1_2_0_ABJ = result
    where
        block1 = tensorProd8 delta20 $ tensorProd8 intJ intFlat
        block2 = tensorTransU3 (0,1) block1
        block3 = tensorTransU3 (1,2) block1
        block4 = tensorTransU3 (0,2) block1
        block5 = tensorTransU3 (0,1) $ tensorTransU3 (1,2) block1
        block6 = tensorTransU3 (0,1) $ tensorTransU3 (0,2) block1
        result = tensorAdd8 block1 $ tensorAdd8 block2 $
                 tensorAdd8 block3 $ tensorAdd8 block4 $
                 tensorAdd8 block5 block6

evalDiffeo_1_2_0_ABJ :: ([Int], Rational) -> ((Int, Int), Rational)
evalDiffeo_1_2_0_ABJ ([a2,a1,b,j,p,q,m,n], x) = ((row, column), x)
    where
        row = 256*a2 + 64*p + 16*q + 4*m + n + 1
        a' = a1 + 1
        bj = 10*b + j + 84 + 21 + 1
        column = trian M.! [a', bj] + 315 + 1

diffeo_1_2_2 :: Tensor8 1 2 0 0 1 2 3 1 Rational
diffeo_1_2_2 = result
    where
        block1 = tensorProd8 delta20 $ tensorProd8 delta9 $ tensorProd8 intJ intFlat
        block2 = tensorTransU3 (0,1) block1
        block3 = tensorTransU3 (1,2) block1
        block4 = tensorTransU3 (0,2) block1
        block5 = tensorTransU3 (0,1) $ tensorTransU3 (1,2) block1
        block6 = tensorTransU3 (0,1) $ tensorTransU3 (0,2) block1
        total  = tensorAdd8 block1 $ tensorAdd8 block2 $
                 tensorAdd8 block3 $ tensorAdd8 block4 $
                 tensorAdd8 block5 block6
        totalT = tensorTransL20 (0,1) $ tensorTransL9 (0,1) total
        result = tensorAdd8 total totalT

evalDiffeo_1_2_2 :: ([Int], Rational) -> ((Int, Int), Rational)
evalDiffeo_1_2_2 ([a2,a1,b1,i2,i1,j1,p,q,m,n], x) = ((row, column), x)
    where
        row = 10*256*a2 + 256*i2 + 64*p + 16*q + 4*m + n + 1
        ai = 10*a1 + i1 + 84 + 21 + 1
        bj = 10*b1 + j1 + 84 + 21 + 1
        column = trian M.! [min ai bj, max ai bj] + 315 + 1

diffeo_0_0Mat :: [((Int, Int), Rational)]
diffeo_0_0Mat = map evalDiffeo_0_0 $ toListShow8 diffeo_0_0

diffeo_0_1Mat :: [((Int, Int), Rational)]
diffeo_0_1Mat = map evalDiffeo_0_1 $ toListShow8 diffeo_0_1

diffeo_0_2Mat :: [((Int, Int), Rational)]
diffeo_0_2Mat = map evalDiffeo_0_2 $ toListShow8 diffeo_0_2

diffeo_1_0_0Mat :: [((Int, Int), Rational)]
diffeo_1_0_0Mat = eval1 ++ eval2
    where
        eval1 = map evalDiffeo_1_0_0_A $ toListShow8 diffeo_1_0_0_A
        eval2 = map evalDiffeo_1_0_0_AB $ toListShow8 diffeo_1_0_0_AB

diffeo_1_0_2Mat :: [((Int, Int), Rational)]
diffeo_1_0_2Mat = eval1 ++ eval2
    where
        eval1 = map evalDiffeo_1_0_2_AI $ toListShow8 diffeo_1_0_2_AI
        eval2 = map evalDiffeo_1_0_2_ABJ $ toListShow8 diffeo_1_0_2_ABJ

diffeo_1_1_1Mat :: [((Int, Int), Rational)]
diffeo_1_1_1Mat = eval1 ++ eval2
    where
        eval1 = map evalDiffeo_1_1_1_AI $ toListShow8 diffeo_1_1_1_AI
        eval2 = map evalDiffeo_1_1_1_AaBb $ toListShow8 diffeo_1_1_1_AaBb

diffeo_1_2_0Mat :: [((Int, Int), Rational)]
diffeo_1_2_0Mat = eval1 ++ eval2
    where
        eval1 = map evalDiffeo_1_2_0_AI $ toListShow8 diffeo_1_2_0_AI
        eval2 = map evalDiffeo_1_2_0_ABJ $ toListShow8 diffeo_1_2_0_ABJ

diffeo_1_2_2Mat :: [((Int, Int), Rational)]
diffeo_1_2_2Mat = eval1
    where
        eval1 = map evalDiffeo_1_2_2 $ toListShow8 diffeo_1_2_2
