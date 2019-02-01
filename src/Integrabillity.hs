--this module contains the functions that we need to construct the integrabillity conditions

--pushes type stuff to kind stuff (prefixed with ')
{-# LANGUAGE DataKinds #-}
--matching on type constructors
{-# LANGUAGE GADTs #-}
--kind signature
{-# LANGUAGE KindSignatures #-}
--type family definitions
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
--infix type plus and mult
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Integrabillity (
    intCond1, intCond1Zero, intCond1RelFac, intCond1noFactor, index2SparseCond1, mkEqnSparseCond1, int1Zero, index2SparseCond1Zero, mkEqnSparseCond1Zero, index2SparseBlock1Eta,
    mkEqnSparseBlock1Eta, intCond2, index2SparseCond2, mkEqnSparseCond2, intCond3, index2SparseCond3, mkEqnSparseCond3,
    projectorMatrix, projectorMatrixSym, projectorMatrixMixed1, projectorMatrixMixed2, index2SparseProjector, mkEqnSparseProjector,
    projectorMatrix2, index2SparseProjector2, mkEqnSparseProjector2, intCond2_1, index2SparseintCond2_1, mkEqnSparseintCond2_1,
    intCond2_1Symbol, index2SparseintCond2_1Symbol, mkEqnSparseintCond2_1Symbol, densityEqnMetric1, densityEqnMetric2, index2SparseDens1, index2SparseDens2,
    mkEqnSparseDens1, mkEqnSparseDens2, densityEqnArea1, densityEqnArea2, index2SparseDens1Area, index2SparseDens2Area,
    mkEqnSparseDens1Area, mkEqnSparseDens2Area, areaMetricMetric1, index2SparseAreaM, mkEqnSparseAreaM,
    areaMetricMetric1intCond, index2SparseAreaMintCond, mkEqnSparseAreaMintCond, 
    intCond2_1SymbolPure, index2SparseintCond2_1SymbolPure, mkEqnSparseintCond2_1SymbolPure,
    intCond2_1SymbolRed, intCond2_1SymbolRedWrong, index2SparseintCond2_1SymbolRed, mkEqnSparseintCond2_1SymbolRed,
    mkEqnSparseintCond2_1SymbolRedFull,
    int1Test, int1Test2, int1Test3,
    flatMetricInter, mkEqnSparseflatMetricInter,
    flatMetricInterProlong, mkEqnSparseflatMetricInterProlong,
    intRankDef1, mkEqnSparseintRankDef1, intRankDef2, intRankDef3, mkEqnSparseintRankDef3, intRankDef4, mkEqnSparseintRankDef4, intRankDef5,
    intCondComp, mkEqnSparseintCondComp,
    intCondSym, mkEqnSparseintCondSym, intCondCompZero,
    intCondOrd2, mkEqnSparseintCondOrd2,
    mkEqnSparseintCond2_1New, mkEqnSparseintCondCompNew,
    interProTest,
    intCondCompNoSym, mkEqnSparseintCondCompNoSym,
    inter4noFactor, inter4Factor, mkEqnSparseinterMat,
    inter6noFactor, inter6Factor, mkEqnSparseinter6Mat,
    prolongation1AI_AI, prolongation1AI_ACK,
    mkEqnSparseprolongation1AI_AI, mkEqnSparseprolongation1AI_ACK,
    prolongation2AaBb, prolongation2AaBbC,
    mkEqnSparseprolongation2AaBb, mkEqnSparseprolongation2AaBbC
    
) where

    import Index
    import Tensor
    import BasicTensors
    import Ivar
    import Pde
    import EquivarianceEqns
    import qualified Data.Map as M 

    intCond1 ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 1 0 0 2 1 0 0 Rational 
    intCond1 map1Area map2Area map1Metric map2Metric = tens
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        antiSym = aSymI_2 map1Metric
                        part1 = tensorProductWith (*) intArea delta_9
                        part2 = tensorProductWith (*) intMetric delta_20
                        block1 = tensorAdd part1 part2
                        block2 = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta antiSym
                        tens = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,0) (+) $ tensorProductWith (*) block1 block2

    intCond1RelFac ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 1 0 0 2 1 0 0 Rational 
    intCond1RelFac map1Area map2Area map1Metric map2Metric = tens
                    where
                        intArea = interAreanoFactor map1Area map2Area
                        intMetric = interMetricnoFactor map1Metric map2Metric
                        antiSym = aSymI_2 map1Metric
                        part1 = tensorProductWith (*) intArea delta_9
                        part2 = tensorSMult (2) $ tensorProductWith (*) intMetric delta_20
                        block1 = tensorAdd part1 part2
                        block2 = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta antiSym
                        tens = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,0) (+) $ tensorProductWith (*) block1 block2


    --there is some problem with the inetrtwiners !!!
    --
    --
    --

    intCond1noFactor ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 1 0 0 2 1 0 0 Rational 
    intCond1noFactor map1Area map2Area map1Metric map2Metric = tens
                    where
                        intArea = interAreanoFactor map1Area map2Area
                        intMetric = interMetricnoFactor map1Metric map2Metric
                        antiSym = aSymI_2 map1Metric
                        part1 = tensorProductWith (*) intArea delta_9
                        part2 = tensorProductWith (*) intMetric delta_20
                        block1 = tensorAdd part1 part2
                        block2 = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta antiSym
                        tens = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,0) (+) $ tensorProductWith (*) block1 block2


    intCond1Zero ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 1 0 0 2 1 0 0 Rational 
    intCond1Zero map1Area map2Area map1Metric map2Metric = tens
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        antiSym = aSymI_2 map1Metric
                        part1 = tensorProductWith (*) intArea delta_9
                        part2 = tensorProductWith (*) intMetric delta_20
                        block1 = part2
                        block2 = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta antiSym
                        tens = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,0) (+) $ tensorProductWith (*) block1 block2


    index2SparseCond1 :: Index 1 1 0 0 2 1 0 0 -> (Int,Int) 
    index2SparseCond1 (Index x1  x2  _  _  x5  x6  _  _) = ((b-1)*100+(j-1)*10+(k),(a-1)*10+i)
                         where 
                             b = 1 + (fromEnum $ getValInd x1 0)
                             a = 1 + (fromEnum $ getValInd x2 0)
                             j = 1 + (fromEnum $ getValInd x5 0)
                             k = 1 + (fromEnum $ getValInd x5 1)
                             i = 1 + (fromEnum $ getValInd x6 0)

    mkEqnSparseCond1 :: Tensor 1 1 0 0 2 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseCond1 (Tensor map1) = M.mapKeys index2SparseCond1 map1 

    int1Zero :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 -> Tensor 0 1 0 0 1 0 0 0 Rational
    int1Zero map1Area map2Area map1Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorContractWith_3 (1,0) (+) prod 
                where
                    flat = eqn1_1Flat map1Area map2Area
                    antiSym = aSymI_2 map1Metric
                    prod = tensorProductWith (*) flat $ tensorProductWith (*) invEta antiSym

    int1Test :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 -> Tensor 0 1 0 0 1 0 0 0 Rational
    int1Test map1Area map2Area map1Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorContractWith_3 (1,0) (+) prod 
                where
                    flat = eqn1_1Flat map1Area map2Area
                    sym = symI_2 map1Metric
                    prod = tensorProductWith (*) flat $ tensorProductWith (*) invEta sym

    int1Test2 :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 -> Tensor 1 2 0 0 1 0 0 0 Rational
    int1Test2 map1Area map2Area map1Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorContractWith_3 (1,0) (+) prod 
                where
                    flat = eqn1_1Flat map1Area map2Area
                    sym = interI_2 map1Metric
                    prod = tensorProductWith (*) delta_20 $ tensorProductWith (*) flat $ tensorProductWith (*) invEta sym

    int1Test3 :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 -> Tensor 1 2 0 0 1 0 0 0 Rational
    int1Test3 map1Area map2Area map1Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorContractWith_3 (1,0) (+) tens 
                where
                    flat = eqn1_1Flat map1Area map2Area
                    sym = interI_2 map1Metric
                    prod = tensorProductWith (*) delta_20 $ tensorProductWith (*) flat $ tensorProductWith (*) invEta sym       
                    tens = tensorAdd prod $ tensorTranspose 2 (0,1) prod 

    index2SparseCond1Zero :: Index 0 1 0 0 1 0 0 0 -> (Int,Int) 
    index2SparseCond1Zero (Index _  x2  _  _  x5  _  _  _) = (k,a)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             k = 1 + (fromEnum $ getValInd x5 0)
                            
    mkEqnSparseCond1Zero :: Tensor 0 1 0 0 1 0 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseCond1Zero (Tensor map1) = M.mapKeys index2SparseCond1Zero map1 

    index2SparseBlock1Eta :: Index 0 1 0 0 0 0 2 0 -> (Int,Int) 
    index2SparseBlock1Eta (Index _  x2  _  _  _  _  x7  _) = ((r-1)*4+n,a)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             n = 1 + (fromEnum $ getValInd x7 0)
                             r = 1 + (fromEnum $ getValInd x7 1)

    mkEqnSparseBlock1Eta :: Tensor 0 1 0 0 0 0 2 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseBlock1Eta (Tensor map1) = M.mapKeys index2SparseBlock1Eta map1 

    intCond2 ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 -> Tensor 1 1 0 0 1 0 1 1 Rational 
    intCond2 map1Area map2Area map1Metric = tensorContractWith_3 (1,0) (+) $ tensorContractWith_3 (0,2) (+) $ tensorContractWith_3 (3,3) (+) prod
                    where
                        antiSym = aSymI_2 map1Metric
                        int1 = tensorProductWith (*) (interArea map1Area map2Area) delta_3 
                        int2 = tensorProductWith (*) ( tensorTranspose 8 (0,1) $ tensorProductWith (*) delta_3 delta_3 ) delta_20
                        intTotal = tensorSub int1 int2 
                        prod = tensorProductWith (*) intTotal $ tensorProductWith (*) invEta antiSym

    index2SparseCond2 :: Index 1 1 0 0 1 0 1 1 -> (Int,Int) 
    index2SparseCond2 (Index x1  x2  _  _  x5  _  x7  x8) = (347+(b-1)*40+(k-1)*4+(r-1),21+(a-1)*4+s+1)
                         where 
                             b = 1 + (fromEnum $ getValInd x1 0)
                             a = 1 + (fromEnum $ getValInd x2 0)
                             k = 1 + (fromEnum $ getValInd x5 0)
                             r = 1 + (fromEnum $ getValInd x7 0)
                             s = 1 + (fromEnum $ getValInd x8 0)

    mkEqnSparseCond2 :: Tensor 1 1 0 0 1 0 1 1 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseCond2 (Tensor map1) = M.mapKeys index2SparseCond2 map1 

    intCond3 ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 -> Tensor 1 1 0 0 1 0 0 0 Rational 
    intCond3 map1Area map2Area map1Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ tensorContractWith_3 (2,2) (+) prod
                    where
                        antiSym = aSymI_2 map1Metric
                        intTotal = interArea map1Area map2Area 
                        prod = tensorProductWith (*) intTotal $ tensorProductWith (*) invEta antiSym

    index2SparseCond3 :: Index 1 1 0 0 1 0 0 0 -> (Int,Int) 
    index2SparseCond3 (Index x1  x2  _  _  x5  _  _  _) = ((b-1)*10+k,a)
                         where 
                             b = 1 + (fromEnum $ getValInd x1 0)
                             a = 1 + (fromEnum $ getValInd x2 0)
                             k = 1 + (fromEnum $ getValInd x5 0)

    mkEqnSparseCond3 :: Tensor 1 1 0 0 1 0 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseCond3 (Tensor map1) = M.mapKeys index2SparseCond3 map1 
                             

    projectorMatrix :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Linds_3 2) Uind_9 -> Tensor 1 0 0 0 1 0 0 6 Rational 
    projectorMatrix map1Area map1Metric = tensorProductWith (*) (interI_Area map1Area) (interI_2 map1Metric)

    index2SparseProjector :: Index 1 0 0 0 1 0 0 6 -> (Int,Int) 
    index2SparseProjector (Index x1  _  _  _  x5  _  _  x8) = ((a-1)*10+b,i*4^5+j*4^4+k*4^3+l*4^2+p*4+q+1)
                         where 
                             a = 1 + (fromEnum $ getValInd x1 0)
                             b = 1 + (fromEnum $ getValInd x5 0)
                             i =  (fromEnum $ getValInd x8 0)
                             j =  (fromEnum $ getValInd x8 1)
                             k =  (fromEnum $ getValInd x8 2)
                             l =  (fromEnum $ getValInd x8 3)
                             p =  (fromEnum $ getValInd x8 4)
                             q =  (fromEnum $ getValInd x8 5)

    mkEqnSparseProjector :: Tensor 1 0 0 0 1 0 0 6 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseProjector (Tensor map1) = M.mapKeys index2SparseProjector map1 

    projectorMatrixSym :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Linds_3 2) Uind_9 -> Tensor 1 0 0 0 1 0 0 6 Rational 
    projectorMatrixSym map1Area map1Metric = tensorProductWith (*) (symI_Area map1Area) (symI_2 map1Metric)

    projectorMatrixMixed1 :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Linds_3 2) Uind_9 -> Tensor 1 0 0 0 1 0 0 6 Rational 
    projectorMatrixMixed1 map1Area map1Metric = tensorProductWith (*) (interI_Area map1Area) (symI_2 map1Metric)

    projectorMatrixMixed2 :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Linds_3 2) Uind_9 -> Tensor 1 0 0 0 1 0 0 6 Rational 
    projectorMatrixMixed2 map1Area map1Metric = tensorProductWith (*) (symI_Area map1Area) (interI_2 map1Metric)

    projectorMatrix2 :: M.Map (Linds_3 4) Uind_20 ->  Tensor 1 0 0 0 0 0 0 4 Rational 
    projectorMatrix2 map1Area = (interI_Area map1Area) 

    index2SparseProjector2 :: Index 1 0 0 0 0 0 0 4 -> (Int,Int) 
    index2SparseProjector2 (Index x1  _  _  _  _  _  _  x8) = (a,i*4^3+j*4^2+k*4+l+1)
                         where 
                             a = 1 + (fromEnum $ getValInd x1 0)
                             i =  (fromEnum $ getValInd x8 0)
                             j =  (fromEnum $ getValInd x8 1)
                             k =  (fromEnum $ getValInd x8 2)
                             l =  (fromEnum $ getValInd x8 3)

    mkEqnSparseProjector2 :: Tensor 1 0 0 0 0 0 0 4 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseProjector2 (Tensor map1) = M.mapKeys index2SparseProjector2 map1 

    --now the third order Int-conditions

    intCond2_1 :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 2 2 0 0 2 1 0 0 Rational 
    intCond2_1 map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) prod
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        antiSym = aSymI_2 map1Metric
                        block1 = tensorProductWith (*) intArea $ tensorProductWith (*) delta_20 delta_9
                        block2 = tensorProductWith (*) delta_20 $ tensorProductWith (*) intArea delta_9
                        block3 = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_20 intMetric
                        totalBlock1 = tensorAdd block1 $ tensorAdd block2 block3 
                        totalBlock2 = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta antiSym
                        prod = tensorProductWith (*) totalBlock1 totalBlock2

    index2SparseintCond2_1 :: Index 2 2 0 0 2 1 0 0 -> (Int,Int) 
    index2SparseintCond2_1 (Index x1  x2  _  _  x5  x6  _  _) = ((e-1)*2100+(f-1)*100+(j-1)*10+k,(a-1)*210+(b-1)*10+i)
                         where 
                             e = 1 + (fromEnum $ getValInd x1 0)
                             f = 1 + (fromEnum $ getValInd x1 1)
                             a = 1 + (fromEnum $ getValInd x2 0)
                             b = 1 + (fromEnum $ getValInd x2 1)
                             j = 1 +  (fromEnum $ getValInd x5 0)
                             k = 1 +  (fromEnum $ getValInd x5 1)
                             i = 1 +  (fromEnum $ getValInd x6 0)

    index2SparseintCond2_1New :: M.Map [Int] Int -> Index 2 2 0 0 2 1 0 0 -> (Int,Int) 
    index2SparseintCond2_1New trian (Index x1  x2  _  _  x5  x6  _  _) = ((e-1)*2100+(f-1)*100+(j-1)*10+k,316 + (M.!) trian [a,105+(b-1)*10+i])
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      f = 1 + (fromEnum $ getValInd x1 1)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      b = 1 + (fromEnum $ getValInd x2 1)
                                                      j = 1 +  (fromEnum $ getValInd x5 0)
                                                      k = 1 +  (fromEnum $ getValInd x5 1)
                                                      i = 1 +  (fromEnum $ getValInd x6 0)

    --seems to be in the right order

    mkEqnSparseintCond2_1 :: Tensor 2 2 0 0 2 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseintCond2_1 (Tensor map1) = M.mapKeys index2SparseintCond2_1 map1 


    mkEqnSparseintCond2_1New :: M.Map [Int] Int -> Tensor 2 2 0 0 2 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseintCond2_1New trian (Tensor map1) = M.mapKeys (index2SparseintCond2_1New trian) map1

    intCond2_1Symbol :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 2 3 0 0 2 1 0 0 Rational 
    intCond2_1Symbol map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) total
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        sym = interI_2 map1Metric
                        flatInter = tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) intArea (flatArea map2Area)
                        tens = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_9 flatInter
                        tensTranspose = tensorTranspose 2 (0,2) tens 
                        totalTens = tensorAdd tens tensTranspose
                        totalBlockEta = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta sym
                        total = tensorProductWith (*) totalTens totalBlockEta


    index2SparseintCond2_1Symbol :: M.Map (Int,Int) Int -> Index 2 3 0 0 2 1 0 0 -> (Int,Int) 
    index2SparseintCond2_1Symbol map1 (Index x1  x2  _  _  x5  x6  x7  x8) = (10*10*21*(e-1)+10*10*(f-1)+10*(j-1)+k,(x-1)*210+(b-1)*10+i)
                         where 
                             e = 1 + (fromEnum $ getValInd x1 0)
                             f = 1 + (fromEnum $ getValInd x1 1)
                             a = 1 + (fromEnum $ getValInd x2 0)
                             b = 1 + (fromEnum $ getValInd x2 1)
                             c = 1 + (fromEnum $ getValInd x2 2)
                             j = 1 + (fromEnum $ getValInd x5 0)
                             k = 1 + (fromEnum $ getValInd x5 1)
                             i = 1 + (fromEnum $ getValInd x6 0)
                             x = (M.!) map1 (min a c, max a c)
                             

    mkEqnSparseintCond2_1Symbol :: M.Map (Int,Int) Int -> Tensor 2 3 0 0 2 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseintCond2_1Symbol map2 (Tensor map1) = M.mapKeys (index2SparseintCond2_1Symbol map2) map1 

    --now the pure symbol (we need to symmetrize and multiply by 2 to get no fractions)

    intCond2_1SymbolPure :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 2 3 0 0 1 1 1 1 Rational 
    intCond2_1SymbolPure map1Area map2Area map1Metric map2Metric = tensorAdd tens tensTranspose 
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        flatInter = tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) intArea (flatArea map2Area)
                        tens = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_9 flatInter
                        tensTranspose = tensorTranspose 2 (0,2) tens 

    index2SparseintCond2_1SymbolPure :: M.Map (Int,Int) Int -> Index 2 3 0 0 1 1 1 1 -> (Int,Int) 
    index2SparseintCond2_1SymbolPure map1 (Index x1  x2  _  _  x5  x6  x7  x8) = (10*16*21*(e-1)+10*16*(f-1)+16*(j-1)+(m-1)*4+n,(x-1)*210+(b-1)*10+i)
                         where 
                             e = 1 + (fromEnum $ getValInd x1 0)
                             f = 1 + (fromEnum $ getValInd x1 1)
                             a = 1 + (fromEnum $ getValInd x2 0)
                             b = 1 + (fromEnum $ getValInd x2 1)
                             c = 1 + (fromEnum $ getValInd x2 2)
                             j = 1 + (fromEnum $ getValInd x5 0)
                             i = 1 + (fromEnum $ getValInd x6 0)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)
                             x = (M.!) map1 (min a c, max a c)

    mkEqnSparseintCond2_1SymbolPure :: M.Map (Int,Int) Int -> Tensor 2 3 0 0 1 1 1 1 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseintCond2_1SymbolPure map2 (Tensor map1) = M.mapKeys (index2SparseintCond2_1SymbolPure map2) map1 

    intCond2_1SymbolRed :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 2 0 0 1 0 0 0 Rational 
    intCond2_1SymbolRed map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) total
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        sym = interI_2 map1Metric
                        flatInter = tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) intArea (flatArea map2Area)
                        tens = tensorProductWith (*) delta_20 flatInter
                        tensTranspose = tensorTranspose 2 (0,1) tens 
                        totalTens = tensorAdd tens tensTranspose
                        totalBlockEta = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta sym
                        total = tensorProductWith (*) totalTens totalBlockEta

    intCond2_1SymbolRedWrong :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 2 0 0 1 0 0 0 Rational 
    intCond2_1SymbolRedWrong map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) total
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        sym = interI_2 map1Metric
                        flatInter = tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) intArea (flatArea map2Area)
                        tens = tensorProductWith (*) delta_20 flatInter
                        totalTens = tens 
                        totalBlockEta = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta sym
                        total = tensorProductWith (*) totalTens totalBlockEta


    index2SparseintCond2_1SymbolRed :: M.Map (Int,Int) Int -> Index 1 2 0 0 1 0 0 0 -> (Int,Int) 
    index2SparseintCond2_1SymbolRed map1 (Index x1  x2  _  _  x5  x6  _  _) = (10*(e-1)+k,x)
                         where 
                             e = 1 + (fromEnum $ getValInd x1 0)
                             a = 1 + (fromEnum $ getValInd x2 0)
                             c = 1 + (fromEnum $ getValInd x2 1)
                             k = 1 + (fromEnum $ getValInd x5 0)
                             x = (M.!) map1 (min a c, max a c)  
                             
    index2SparseintCond2_1SymbolRedFull :: Index 1 2 0 0 1 0 0 0 -> (Int,Int) 
    index2SparseintCond2_1SymbolRedFull (Index x1  x2  _  _  x5  x6  _  _) = (10*(e-1)+k,21*(a-1)+c)
                         where 
                             e = 1 + (fromEnum $ getValInd x1 0)
                             a = 1 + (fromEnum $ getValInd x2 0)
                             c = 1 + (fromEnum $ getValInd x2 1)
                             k = 1 + (fromEnum $ getValInd x5 0)
                             
    mkEqnSparseintCond2_1SymbolRed :: M.Map (Int,Int) Int -> Tensor 1 2 0 0 1 0 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseintCond2_1SymbolRed map2 (Tensor map1) = M.mapKeys (index2SparseintCond2_1SymbolRed map2) map1 

    mkEqnSparseintCond2_1SymbolRedFull :: Tensor 1 2 0 0 1 0 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseintCond2_1SymbolRedFull (Tensor map1) = M.mapKeys (index2SparseintCond2_1SymbolRedFull ) map1 

    

    densityEqnMetric1 :: M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 0 0 0 0 1 1 1 1 Rational 
    densityEqnMetric1 map1Metric map2Metric = tens 
                where
                    intMetric = interMetric map1Metric map2Metric
                    tens = tensorAdd (tensorProductWith (*) delta_9 delta_3) intMetric    
                    
    densityEqnMetric2 :: M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 0 0 0 0 2 2 1 1 Rational 
    densityEqnMetric2 map1Metric map2Metric = tensorAdd tens tens2
                where
                    intMetric = interMetric map1Metric map2Metric
                    block1 = tensorProductWith (*) delta_9 $ tensorProductWith (*) delta_9 delta_3
                    block2 = tensorProductWith (*) intMetric delta_9
                    block3 = tensorTranspose 6 (0,1) $ tensorProductWith (*) delta_9 intMetric 
                    tens = tensorAdd block1 $ tensorAdd block2 block3 
                    tens2 = tensorTranspose 6 (0,1) tens

    index2SparseDens1 :: Index 0 0 0 0 1 1 1 1 -> (Int,Int) 
    index2SparseDens1 (Index _  _  _  _  x5  x6  x7  x8) = ((b-1)*16+(m-1)*4+n,a)
                         where 
                             a = 1 + (fromEnum $ getValInd x6 0)
                             b = 1 + (fromEnum $ getValInd x5 0)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    index2SparseDens2 :: Index 0 0 0 0 2 2 1 1 -> (Int,Int) 
    index2SparseDens2 (Index _  _  _  _  x5  x6  x7  x8) = ((c-1)*160+(b-1)*16+(m-1)*4+n,(d-1)*10+a)
                         where 
                             a = 1 + (fromEnum $ getValInd x6 0)
                             b = 1 + (fromEnum $ getValInd x5 0)
                             d = 1 + (fromEnum $ getValInd x6 1)
                             c = 1 + (fromEnum $ getValInd x5 1)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    mkEqnSparseDens1 ::  Tensor 0 0 0 0 1 1 1 1 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseDens1 (Tensor map1) = M.mapKeys index2SparseDens1  map1 

    mkEqnSparseDens2 :: Tensor 0 0 0 0 2 2 1 1 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseDens2  (Tensor map1) = M.mapKeys index2SparseDens2 map1

    densityEqnArea1 :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> Tensor 1 1 0 0 0 0 1 1 Rational 
    densityEqnArea1 map1Area map2Area = tens 
                where
                    intArea = interArea map1Area map2Area
                    tens = tensorAdd (tensorProductWith (*) delta_20 delta_3) intArea    
                    
    densityEqnArea2 :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> Tensor 2 2 0 0 0 0 1 1 Rational 
    densityEqnArea2 map1Area map2Area = tensorAdd tens tens2
                where
                    intArea = interArea map1Area map2Area
                    block1 = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_20 delta_3
                    block2 = tensorProductWith (*) intArea delta_20
                    block3 = tensorTranspose 2 (0,1) $ tensorProductWith (*) delta_20 intArea 
                    tens = tensorAdd block1 $ tensorAdd block2 block3 
                    tens2 = tensorTranspose 2 (0,1) tens

    index2SparseDens1Area :: Index 1 1 0 0 0 0 1 1 -> (Int,Int) 
    index2SparseDens1Area (Index x1  x2   _  _  _  _  x7  x8) = ((b-1)*16+(m-1)*4+n,a)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             b = 1 + (fromEnum $ getValInd x1 0)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    index2SparseDens2Area :: Index 2 2 0 0 0 0 1 1 -> (Int,Int) 
    index2SparseDens2Area (Index x1  x2  _  _  _  _  x7  x8) = ((c-1)*16*21+(b-1)*16+(m-1)*4+n,(d-1)*21+a)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             b = 1 + (fromEnum $ getValInd x1 0)
                             d = 1 + (fromEnum $ getValInd x2 1)
                             c = 1 + (fromEnum $ getValInd x1 1)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    mkEqnSparseDens1Area ::  Tensor 1 1 0 0 0 0 1 1 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseDens1Area (Tensor map1) = M.mapKeys index2SparseDens1Area  map1 

    mkEqnSparseDens2Area :: Tensor 2 2 0 0 0 0 1 1 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseDens2Area  (Tensor map1) = M.mapKeys index2SparseDens2Area map1

    areaMetricMetric1 ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> Tensor 0 1 0 0 1 1 1 1 Rational
    areaMetricMetric1 map1Area map2Area = tensorProductWith (*) delta_9 flat 
                where
                    intArea = interArea map1Area map2Area
                    flat = tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) intArea $ flatArea map2Area

    index2SparseAreaM :: Index 0 1 0 0 1 1 1 1 -> (Int,Int) 
    index2SparseAreaM (Index _  x2  _  _  x5  x6  x7  x8) = ((j-1)*16+(m-1)*4+n,(a-1)*10+i)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             i = 1 + (fromEnum $ getValInd x6 0)
                             j = 1 + (fromEnum $ getValInd x5 0)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    mkEqnSparseAreaM :: Tensor 0 1 0 0 1 1 1 1 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAreaM  (Tensor map1) = M.mapKeys index2SparseAreaM map1


    areaMetricMetric1intCond ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 1 0 0 2 1 0 0 Rational
    areaMetricMetric1intCond map1Area map2Area map1Metric map2Metric = tens
                where
                    intArea = interArea map1Area map2Area
                    intMetric = interMetric map1Metric map2Metric
                    antiSym = aSymI_2 map1Metric
                    sum1 = tensorProductWith (*) intArea delta_9
                    sum2 = tensorProductWith (*) delta_20 intMetric
                    diff = tensorAdd sum1 sum2 
                    block2 = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta antiSym
                    tens = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ tensorProductWith (*) diff block2

    index2SparseAreaMintCond :: Index 1 1 0 0 2 1 0 0 -> (Int,Int) 
    index2SparseAreaMintCond (Index x1  x2  _  _  x5  x6  _  _) = ((b-1)*100+(j-1)*10+k,(a-1)*10+i)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             b = 1 + (fromEnum $ getValInd x1 0)
                             i = 1 + (fromEnum $ getValInd x6 0)
                             j = 1 + (fromEnum $ getValInd x5 0)
                             k = 1 + (fromEnum $ getValInd x5 1)

    mkEqnSparseAreaMintCond :: Tensor 1 1 0 0 2 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAreaMintCond  (Tensor map1) = M.mapKeys index2SparseAreaMintCond map1


    flatMetricInter :: M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 0 0 0 0 0 1 1 1 Rational
    flatMetricInter map1Metric map2Metric = tensorContractWith_9 (0,1) (+) tens 
            where
                metInter = interMetric map1Metric map2Metric
                tens = tensorProductWith (*) metInter $ etaAbs map2Metric
    
    index2SparseflatMetricInter :: Index 0 0 0 0 0 1 1 1 -> (Int,Int) 
    index2SparseflatMetricInter (Index _  _  _  _  _  x6  x7  x8) = (i,(n-1)*4+m)
                         where 
                             i = 1 + (fromEnum $ getValInd x6 0)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    mkEqnSparseflatMetricInter :: Tensor 0 0 0 0 0 1 1 1 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseflatMetricInter  (Tensor map1) = M.mapKeys index2SparseflatMetricInter map1

    flatMetricInterProlong :: M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 0 0 0 0 1 2 1 1 Rational
    flatMetricInterProlong map1Metric map2Metric = tensorAdd tens tensTrans
                where
                    flatInter = flatMetricInter map1Metric map2Metric 
                    tens = tensorProductWith (*) delta_9 flatInter 
                    tensTrans = tensorTranspose 6 (0,1) tens

    index2SparseflatMetricInterProlong :: Index 0 0 0 0 1 2 1 1 -> (Int,Int) 
    index2SparseflatMetricInterProlong (Index _  _  _  _  x5  x6  x7  x8) = ((l-1)*10+j,(k-1)*16+(n-1)*4+m)
                         where 
                             k = 1 + (fromEnum $ getValInd x5 0) 
                             l = 1 + (fromEnum $ getValInd x6 0)
                             j = 1 + (fromEnum $ getValInd x6 1)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    mkEqnSparseflatMetricInterProlong :: Tensor 0 0 0 0 1 2 1 1 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseflatMetricInterProlong  (Tensor map1) = M.mapKeys index2SparseflatMetricInterProlong map1

    intRankDef1 ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 0 2 0 0 3 0 0 0 Rational 
    intRankDef1 map1Area map2Area map1Metric map2Metric = tensorSub tensTrans $ tensorTranspose 5 (1,2) $ tensorAdd tens tensTrans
                where
                    intArea = interArea map1Area map2Area
                    flatInt = tensorContractWith_20 (0,1) (+) $  tensorProductWith (*) intArea (flatArea map2Area) 
                    symEta = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta $ interI_2 map1Metric 
                    block2 = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+)  $ tensorProductWith (*) flatInt symEta 
                    totalBlock = tensorProductWith (*) delta_20 block2
                    newInter = tensorTranspose 8 (1,2) $ tensorProductWith  (*)  (interJ_Area map2Area) $ tensorProductWith (*) (interI_2 map1Metric) (interI_2 map1Metric)
                    pro = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorContractWith_3 (3,3) (+)  newInter
                    tens = tensorContractWith_20 (0,2) (+) $ tensorProductWith (*) totalBlock pro
                    tensTrans = tensorTranspose 2 (0,1) tens

    index2SparseintRankDef1 :: Index 0 2 0 0 3 0 0 0 -> (Int,Int) 
    index2SparseintRankDef1 (Index _  x2  _  _  x5  _  _  _) = ((k-1)*100+(j-1)*10+i,(a-1)*21+c)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0) 
                             c = 1 + (fromEnum $ getValInd x2 1)
                             i = 1 + (fromEnum $ getValInd x5 0)
                             j = 1 + (fromEnum $ getValInd x5 1)
                             k = 1 + (fromEnum $ getValInd x5 2)

    mkEqnSparseintRankDef1 :: Tensor 0 2 0 0 3 0 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseintRankDef1  (Tensor map1) = M.mapKeys index2SparseintRankDef1 map1

      
    intRankDef2 ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 2 0 0 1 0 0 0 Rational 
    intRankDef2 map1Area map2Area map1Metric map2Metric = pro 
                where
                    intArea = interArea map1Area map2Area
                    flatInt = tensorContractWith_20 (0,1) (+) $  tensorProductWith (*) intArea (flatArea map2Area) 
                    symEta = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta $ interI_2 map1Metric 
                    block2 = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+)  $ tensorProductWith (*) flatInt symEta 
                    totalBlock = tensorProductWith (*) delta_20 block2
                    newInter1 = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorContractWith_3 (3,3) (+) $ tensorTranspose 8 (1,2) $ tensorProductWith  (*)  (interJ_Area map2Area) $ tensorProductWith (*) (interI_2 map1Metric) (interI_2 map1Metric)
                    newInter2 = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorContractWith_3 (3,3) (+) $ tensorTranspose 7 (1,2) $ tensorProductWith  (*)  (interI_Area map1Area) $ tensorProductWith (*) (interJ_2 map2Metric) (interJ_2 map2Metric) 
                    interPro = tensorContractWith_9 (1,0) (+) $ tensorProductWith (*) newInter1 newInter2
                    tensTrans = tensorTranspose 2 (0,1) totalBlock
                    tens = tensorAdd totalBlock tensTrans
                    pro = tensorContractWith_9 (0,0) (+) $ tensorContractWith_20 (0,2) (+) $ tensorProductWith (*) tens interPro

    intRankDef3 ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 0 2 0 0 2 0 0 0 Rational 
    intRankDef3 map1Area map2Area map1Metric map2Metric = tensorSub pro proTrans 
                where
                    intArea = interArea map1Area map2Area
                    flatInt = tensorContractWith_20 (0,1) (+) $  tensorProductWith (*) intArea (flatArea map2Area) 
                    symEta = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta $ interI_2 map1Metric 
                    block2 = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+)  $ tensorProductWith (*) flatInt symEta 
                    totalBlock = tensorProductWith (*) delta_20 block2
                    mixedDelta1 = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorContractWith_3 (3,3) (+) $ tensorTranspose 7 (1,2) $ tensorProductWith (*) (interI_Area map1Area) $ tensorProductWith (*) (interJ_2 map2Metric) invEta
                    mixedDelta2 = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorContractWith_3 (3,3) (+) $ tensorTranspose 8 (1,2) $ tensorProductWith (*) (interJ_Area map2Area) $ tensorProductWith (*) (interI_2 map1Metric) eta
                    tensTrans = tensorTranspose 2 (0,1) totalBlock
                    tens = tensorAdd totalBlock tensTrans
                    pro = tensorContractWith_20 (0,2) (+) $ tensorProductWith (*) tens mixedDelta2 
                    proTrans = tensorTranspose 5 (0,1) pro 

    --this should yield zero !!!

    index2SparseintRankDef3 :: Index 0 2 0 0 2 0 0 0 -> (Int,Int) 
    index2SparseintRankDef3 (Index _  x2  _  _  x5  _  _  _) = ((i-1)*10+j,(a-1)*21+c)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0) 
                             c = 1 + (fromEnum $ getValInd x2 1)
                             i = 1 + (fromEnum $ getValInd x5 0)
                             j = 1 + (fromEnum $ getValInd x5 1)

    mkEqnSparseintRankDef3 :: Tensor 0 2 0 0 2 0 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseintRankDef3  (Tensor map1) = M.mapKeys index2SparseintRankDef3 map1


    intRankDef4 ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 0 0 0 0 2 2 0 0 Rational 
    intRankDef4 map1Area map2Area map1Metric map2Metric = tensorSub pro proTrans 
                where
                    intArea = interArea map1Area map2Area
                    flatInt = tensorContractWith_20 (0,1) (+) $  tensorProductWith (*) intArea (flatArea map2Area) 
                    symEta = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta $ interI_2 map1Metric 
                    block2 = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+)  $ tensorProductWith (*) flatInt symEta 
                    totalBlock = tensorProductWith (*) delta_20 block2
                    mixedDelta1 = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorContractWith_3 (3,3) (+) $ tensorTranspose 7 (1,2) $ tensorProductWith (*) (interI_Area map1Area) $ tensorProductWith (*) (interJ_2 map2Metric) invEta
                    mixedDelta2 = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorContractWith_3 (3,3) (+) $ tensorTranspose 8 (1,2) $ tensorProductWith (*) (interJ_Area map2Area) $ tensorProductWith (*) (interI_2 map1Metric) eta
                    tensTrans = tensorTranspose 2 (0,1) totalBlock
                    tens = tensorAdd totalBlock tensTrans
                    pro = tensorContractWith_20 (0,0) (+) $ tensorContractWith_20 (0,0) (+) $ tensorContractWith_20 (0,2) (+) $ tensorProductWith (*) tens $ tensorProductWith (*) mixedDelta1 $ tensorProductWith (*) mixedDelta1 mixedDelta2 
                    proTrans = tensorTranspose 5 (0,1) pro 

    index2SparseintRankDef4 :: Index 0 0 0 0 2 2 0 0 -> (Int,Int) 
    index2SparseintRankDef4 (Index _  _  _  _  x5  x6  _  _) = ((i-1)*10+j,(a-1)*10+c)
                         where 
                             a = 1 + (fromEnum $ getValInd x6 0) 
                             c = 1 + (fromEnum $ getValInd x6 1)
                             i = 1 + (fromEnum $ getValInd x5 0)
                             j = 1 + (fromEnum $ getValInd x5 1)

    mkEqnSparseintRankDef4 :: Tensor 0 0 0 0 2 2 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseintRankDef4  (Tensor map1) = M.mapKeys index2SparseintRankDef4 map1

    intRankDef5 ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 0 2 0 0 2 0 0 0 Rational 
    intRankDef5 map1Area map2Area map1Metric map2Metric = tensorSub pro proTrans 
                where
                    intArea = interArea map1Area map2Area
                    flatInt = tensorContractWith_20 (0,1) (+) $  tensorProductWith (*) intArea (flatArea map2Area) 
                    symEta = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta $ interI_2 map1Metric 
                    block2 = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+)  $ tensorProductWith (*) flatInt symEta 
                    totalBlock = tensorProductWith (*) delta_20 block2
                    tensTrans = tensorTranspose 2 (0,1) totalBlock
                    tens = tensorAdd totalBlock tensTrans
                    pro = tensorContractWith_20 (0,2) (+) $ tensorProductWith (*) tens block2 
                    proTrans = tensorTranspose 2 (0,1) pro 

    --now the complicated integrabillity condition for L:A:BI

    intCondComp ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 2 0 0 3 1 0 0 Rational 
    intCondComp map1Area map2Area map1Metric map2Metric = aSym 
            where
                intArea = interArea map1Area map2Area
                intMetric = interMetric map1Metric map2Metric
                flatA = flatArea map2Area
                sym = interI_2 map1Metric
                newInter = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorProductWith (*) intArea $ tensorProductWith (*) invEta sym 
                newInterMetric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorProductWith (*) intMetric $ tensorProductWith (*) invEta sym 
                block1 = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_9 $ 
                        tensorContractWith_20 (0,1) (+) $ tensorContractWith_20 (1,2) (+) $ tensorProductWith (*) newInter $ tensorProductWith (*) newInter flatA 
                block2 = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_9 $ tensorProductWith (*) 
                        ( tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta sym)
                        ( tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) newInter flatA)
                block3 = tensorProductWith (*) delta_9 $ tensorProductWith (*) newInter ( tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) newInter flatA) 
                block4 = tensorProductWith (*) delta_20 $ tensorProductWith (*) newInterMetric ( tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) newInter flatA)
                totalTens = tensorAdd block1 $ tensorAdd block2 $ tensorAdd block3 block4 
                tensTranspose = tensorTranspose 5 (1,2) totalTens 
                aSym = tensorSub totalTens tensTranspose 


    index2SparseintCondComp :: Index 1 2 0 0 3 1 0 0 -> (Int,Int) 
    index2SparseintCondComp (Index x1  x2  _  _  x5  x6  _  _) = ((c-1)*1000+(j-1)*100+(k-1)*10+l,(b-1)*21*10+(a-1)*10+i)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0) 
                             b = 1 + (fromEnum $ getValInd x2 1) 
                             c = 1 + (fromEnum $ getValInd x1 0)
                             i = 1 + (fromEnum $ getValInd x6 0)
                             j = 1 + (fromEnum $ getValInd x5 0)
                             k = 1 + (fromEnum $ getValInd x5 1)
                             l = 1 + (fromEnum $ getValInd x5 2)

    index2SparseintCondCompNew :: M.Map [Int] Int -> Index 1 2 0 0 3 1 0 0 -> (Int,Int) 
    index2SparseintCondCompNew trian (Index x1  x2  _  _  x5  x6  _  _) = ((c-1)*1000+(j-1)*100+(k-1)*10+l,(M.!) trian [b,105+(a-1)*10+i])
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0) 
                             b = 1 + (fromEnum $ getValInd x2 1) 
                             c = 1 + (fromEnum $ getValInd x1 0)
                             i = 1 + (fromEnum $ getValInd x6 0)
                             j = 1 + (fromEnum $ getValInd x5 0)
                             k = 1 + (fromEnum $ getValInd x5 1)
                             l = 1 + (fromEnum $ getValInd x5 2)


    --check if this is the same way of eval as in intCond2_1 -> seems to be the right order

    --maybe check this with making ansätze 
                    
    mkEqnSparseintCondComp :: Tensor 1 2 0 0 3 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseintCondComp  (Tensor map1) = M.mapKeys index2SparseintCondComp map1

    mkEqnSparseintCondCompNew :: M.Map [Int] Int -> Tensor 1 2 0 0 3 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseintCondCompNew trian  (Tensor map1) = M.mapKeys (index2SparseintCondCompNew trian) map1


    intCondSym :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 0 1 0 0 2 0 0 0 Rational 
    intCondSym map1Area map2Area map1Metric map2Metric = tensorSub total totalTrans 
            where
                intArea = interArea map1Area map2Area
                flatA = flatArea map2Area
                sym = interI_2 map1Metric
                newInter = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorProductWith (*) intArea $ tensorProductWith (*) invEta sym 
                block1 = tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) newInter $ tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) newInter flatA 
                block2 = tensorProductWith (*) (tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta sym) (tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) newInter flatA  )
                total = tensorAdd block1 block2 
                totalTrans = tensorTranspose 5 (0,1) total 

    index2SparseintCondSym :: Index 0 1 0 0 2 0 0 0 -> (Int,Int) 
    index2SparseintCondSym (Index _  x2  _  _  x5  _  _  _) = ((j-1)*10+k,a+1)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0) 
                             k = 1 + (fromEnum $ getValInd x5 0)
                             j = 1 + (fromEnum $ getValInd x5 1)

    mkEqnSparseintCondSym :: Tensor 0 1 0 0 2 0 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseintCondSym  (Tensor map1) = M.mapKeys index2SparseintCondSym map1



    intCondCompZero ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 2 0 0 3 1 0 0 Rational 
    intCondCompZero map1Area map2Area map1Metric map2Metric = aSym 
            where
                intArea = interArea map1Area map2Area
                intMetric = interMetric map1Metric map2Metric
                flatA = flatArea map2Area
                sym = interI_2 map1Metric
                newInter = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorProductWith (*) intArea $ tensorProductWith (*) invEta sym 
                newInterMetric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorProductWith (*) intMetric $ tensorProductWith (*) invEta sym 
                block1 = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_9 $ 
                        tensorContractWith_20 (0,1) (+) $ tensorContractWith_20 (1,2) (+) $ tensorProductWith (*) newInter $ tensorProductWith (*) newInter flatA 
                block2 = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_9 $ tensorProductWith (*) 
                        ( tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta sym)
                        ( tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) newInter flatA)
                block3 = tensorProductWith (*) delta_9 $ tensorProductWith (*) newInter ( tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) newInter flatA) 
                block4 = tensorProductWith (*) delta_20 $ tensorProductWith (*) newInterMetric ( tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) newInter flatA)
                totalTens = block1
                tensTranspose = tensorTranspose 5 (1,2) totalTens 
                aSym = tensorSub totalTens tensTranspose 


    intCondOrd2 :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 1 0 0 3 1 0 0 Rational 
    intCondOrd2 map1Area map2Area map1Metric map2Metric = tensorSub totalTens totalTrans
            where
                intArea = interArea map1Area map2Area
                intMetric = interMetric map1Metric map2Metric
                flatA = flatArea map2Area
                sym = interI_2 map1Metric
                newInter = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorProductWith (*) intArea $ tensorProductWith (*) invEta sym 
                newInterMetric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorProductWith (*) intMetric $ tensorProductWith (*) invEta sym 
                block1 = tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) newInter $ tensorProductWith (*) delta_9 newInter 
                block2 = tensorContractWith_9 (1,1) (+) $  tensorProductWith (*) delta_20 $ tensorProductWith (*) newInterMetric newInterMetric
                totalTens = tensorAdd block1 block2 
                totalTrans = tensorTranspose 5 (0,2) totalTens

    index2SparseintCondOrd2 :: Index 1 1 0 0 3 1 0 0 -> (Int,Int) 
    index2SparseintCondOrd2 (Index x1  x2  _  _  x5  x6  _  _) = ((d-1)*1000+(l-1)*100+(j-1)*10+k,(a-1)*10+i)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0) 
                             d = 1 + (fromEnum $ getValInd x1 0)
                             i = 1 + (fromEnum $ getValInd x6 0)
                             l = 1 + (fromEnum $ getValInd x5 0)
                             j = 1 + (fromEnum $ getValInd x5 1)
                             k = 1 + (fromEnum $ getValInd x5 2)



    mkEqnSparseintCondOrd2 :: Tensor 1 1 0 0 3 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseintCondOrd2  (Tensor map1) = M.mapKeys index2SparseintCondOrd2 map1

    interProTest :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 0 1 0 0 2 0 0 0 Rational 
    interProTest map1Area map2Area map1Metric map2Metric = tensorSub pro proTrans
            where
                intArea = interArea map1Area map2Area
                sym = interI_2 map1Metric
                newInter = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorProductWith (*) intArea $ tensorProductWith (*) invEta sym 
                flatA = flatArea map2Area
                pro = tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) newInter $ tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) newInter flatA 
                proTrans = tensorTranspose 5 (0,1)  pro 

    intCondCompNoSym :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 2 0 0 1 1 2 2 Rational 
    intCondCompNoSym map1Area map2Area map1Metric map2Metric = aSym 
            where
                intArea = interArea map1Area map2Area
                intMetric = interMetric map1Metric map2Metric
                flatA = flatArea map2Area
                sym = interI_2 map1Metric
                block1 = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_9 $ 
                        tensorContractWith_20 (0,1) (+) $ tensorContractWith_20 (1,2) (+) $ tensorProductWith (*) intArea $ tensorProductWith (*) intArea flatA 
                block2 = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_9 $ tensorProductWith (*) delta_3
                        ( tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) intArea flatA)
                block3 = tensorProductWith (*) delta_9 $ tensorProductWith (*) intArea ( tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) intArea flatA) 
                block4 = tensorProductWith (*) delta_20 $ tensorProductWith (*) intMetric ( tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) intArea flatA)
                totalTens = tensorAdd block1 $ tensorAdd block2 $ tensorAdd block3 block4 
                tensTranspose = tensorTranspose 7 (0,1) $ tensorTranspose 8 (0,1) totalTens 
                aSym = tensorSub totalTens tensTranspose 

    index2SparseintCondCompNoSym :: Index 1 2 0 0 1 1 2 2 -> (Int,Int) 
    index2SparseintCondCompNoSym (Index x1  x2  _  _  x5  x6  x7  x8) = ((c-1)*10*4^4+(j-1)*4^4+(m-1)*4^3+(n-1)*4^2+(r-1)*4+s,(b-1)*21*10+(a-1)*10+i)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0) 
                             b = 1 + (fromEnum $ getValInd x2 1) 
                             c = 1 + (fromEnum $ getValInd x1 0)
                             i = 1 + (fromEnum $ getValInd x6 0)
                             j = 1 + (fromEnum $ getValInd x5 0)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)
                             r = 1 + (fromEnum $ getValInd x7 1)
                             s = 1 + (fromEnum $ getValInd x8 1)

    mkEqnSparseintCondCompNoSym :: Tensor 1 2 0 0 1 1 2 2 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseintCondCompNoSym  (Tensor map1) = M.mapKeys index2SparseintCondCompNoSym map1

    --some intertwiner matrices

    
    inter4noFactor :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> Tensor 1 1 0 0 0 0 0 0 Rational
    inter4noFactor map1Area map2Area = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorContractWith_3 (3,3) (+)  prod
                where
                    intI = interI_Area map1Area 
                    intJnoF = interJ_AreanoFactor  map2Area
                    prod = tensorProductWith (*) intI intJnoF

    inter4Factor :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> Tensor 1 1 0 0 0 0 0 0 Rational
    inter4Factor map1Area map2Area = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorContractWith_3 (3,3) (+)  prod
                where
                    intI = symI_Area map1Area 
                    intJnoF = interJ_Area  map2Area
                    prod = tensorProductWith (*) intI intJnoF

    index2SparseinterMat:: Index 1 1 0 0 0 0 0 0 -> (Int,Int) 
    index2SparseinterMat (Index x1  x2  _  _  _  _  _  _) = (b,a)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0) 
                             b = 1 + (fromEnum $ getValInd x1 0) 
                             
                
    mkEqnSparseinterMat :: Tensor 1 1 0 0 0 0 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseinterMat  (Tensor map1) = M.mapKeys index2SparseinterMat map1


    inter6noFactor :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 -> M.Map (Uinds_3 2) Lind_9 ->Tensor 1 1 0 0 1 1 0 0 Rational
    inter6noFactor map1Area map2Area map1Metric map2Metric = tensorProductWith (*) contr1 contr2 
                where
                    intI = interI_Area map1Area 
                    intJnoF = interJ_AreanoFactor  map2Area
                    intI2 = interI_2 map1Metric
                    intJ2noFac = interJ_2noFactor map2Metric 
                    prod1 = tensorProductWith (*) intI intJnoF
                    contr1 = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorContractWith_3 (3,3) (+)  prod1
                    prod2 = tensorProductWith (*) intI2 intJ2noFac 
                    contr2 = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) prod2 

    inter6Factor :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 -> M.Map (Uinds_3 2) Lind_9 ->Tensor 1 1 0 0 1 1 0 0 Rational
    inter6Factor map1Area map2Area map1Metric map2Metric = tensorProductWith (*) contr1 contr2 
                where
                    symI = symI_Area map1Area 
                    intJ = interJ_Area map2Area
                    symI2 = symI_2 map1Metric
                    intJ2 = interJ_2 map2Metric 
                    prod1 = tensorProductWith (*) symI intJ
                    contr1 = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorContractWith_3 (3,3) (+)  prod1
                    prod2 = tensorProductWith (*) symI2 intJ2 
                    contr2 = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) prod2 


    index2Sparseinter6Mat:: Index 1 1 0 0 1 1 0 0 -> (Int,Int) 
    index2Sparseinter6Mat (Index x1  x2  _  _  x5  x6 _  _) = ((b-1)*10+j,(a-1)*10+i)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0) 
                             b = 1 + (fromEnum $ getValInd x1 0)
                             i = 1 + (fromEnum $ getValInd x6 0) 
                             j = 1 + (fromEnum $ getValInd x5 0) 

    mkEqnSparseinter6Mat :: Tensor 1 1 0 0 1 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseinter6Mat  (Tensor map1) = M.mapKeys index2Sparseinter6Mat map1

    prolongation1AI_AI :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 -> M.Map (Uinds_3 2) Lind_9 ->Tensor 1 1 0 0 1 1 1 1 Rational
    prolongation1AI_AI map1Area map2Area map1Metric map2Metric = tens 
                where
                    int3 = interEqn1_3 map1Area map2Area map1Metric map2Metric 
                    block1 = tensorProductNumeric delta_20 $ tensorProductNumeric delta_9 delta_3
                    tens = tensorAdd block1 int3 

    prolongation1AI_ACK :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 -> M.Map (Uinds_3 2) Lind_9 ->Tensor 1 2 0 0 1 1 1 1 Rational
    prolongation1AI_ACK map1Area map2Area map1Metric map2Metric = tens 
                where
                    intArea = interArea map1Area map2Area
                    flatA = flatArea map2Area
                    flatInter = tensorContractWith_20 (0,1) (+) $ tensorProductNumeric intArea flatA
                    tens = tensorProductNumeric delta_20 $ tensorProductNumeric delta_9 flatInter

    
    index2Sparseprolongation1AI_AI:: Index 1 1 0 0 1 1 1 1 -> (Int,Int) 
    index2Sparseprolongation1AI_AI (Index x1  x2  _  _  x5  x6  x7  x8) = ((b-1)*10*16+(k-1)*16+(m-1)*4+n,(a-1)*10+i)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0) 
                             b = 1 + (fromEnum $ getValInd x1 0)
                             i = 1 + (fromEnum $ getValInd x6 0) 
                             k = 1 + (fromEnum $ getValInd x5 0) 
                             m = 1 + (fromEnum $ getValInd x7 0) 
                             n = 1 + (fromEnum $ getValInd x8 0) 

    index2Sparseprolongation1AI_ACK:: Index 1 2 0 0 1 1 1 1 -> (Int,Int) 
    index2Sparseprolongation1AI_ACK (Index x1  x2  _  _  x5  x6  x7  x8) = ((b-1)*10*16+(k-1)*16+(m-1)*4+n,(c-1)*210+(a-1)*10+i+210)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0) 
                             c = 1 + (fromEnum $ getValInd x2 1) 
                             b = 1 + (fromEnum $ getValInd x1 0)
                             i = 1 + (fromEnum $ getValInd x6 0) 
                             k = 1 + (fromEnum $ getValInd x5 0) 
                             m = 1 + (fromEnum $ getValInd x7 0) 
                             n = 1 + (fromEnum $ getValInd x8 0)

    mkEqnSparseprolongation1AI_AI :: Tensor 1 1 0 0 1 1 1 1 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseprolongation1AI_AI  (Tensor map1) = M.mapKeys index2Sparseprolongation1AI_AI map1


    mkEqnSparseprolongation1AI_ACK :: Tensor 1 2 0 0 1 1 1 1 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseprolongation1AI_ACK  (Tensor map1) = M.mapKeys index2Sparseprolongation1AI_ACK map1

    --the second order prolongation of the first eqn

    prolongation2AaBbC :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 -> M.Map (Uinds_3 2) Lind_9 ->Tensor 2 3 0 0 0 0 3 3 Rational
    prolongation2AaBbC map1Area map2Area map1Metric map2Metric = tensorAdd tens tensTrans
                        where
                            intArea = interArea map1Area map2Area
                            flatA = flatArea map2Area
                            flatInter = tensorContractWith_20 (0,1) (+) $ tensorProductNumeric intArea flatA
                            tens = tensorProductNumeric delta_20 $ tensorProductNumeric delta_20 $ tensorProductNumeric delta_3 $ tensorProductNumeric delta_3 flatInter
                            tensTrans = tensorTranspose 2 (0,1) $ tensorTranspose 8 (1,2) tens

    index2Sparseprolongation2AaBbC :: Index 2 3 0 0 0 0 3 3 -> (Int,Int) 
    index2Sparseprolongation2AaBbC  (Index x1  x2  _  _  _ _  x7  x8) = ((e-1)*21*4^6+(f-1)*4^4+(r-1)*4^3+(s-1)*4^2+(m-1)*4+u,(c-1)*21^2*4*4+(b-1)*21*4*4+(q-1)*21*4+(a-1)*4+p+21^2*16)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      f = 1 + (fromEnum $ getValInd x1 1)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      b = 1 + (fromEnum $ getValInd x2 1)
                                                      c = 1 + (fromEnum $ getValInd x2 2)
                                                      p = 1 +  (fromEnum $ getValInd x8 0)
                                                      q = 1 +  (fromEnum $ getValInd x8 1)
                                                      r = 1 +  (fromEnum $ getValInd x7 0)
                                                      s = 1 +  (fromEnum $ getValInd x7 1)
                                                      m = 1 +  (fromEnum $ getValInd x7 2)
                                                      u = 1 +  (fromEnum $ getValInd x8 2)

    mkEqnSparseprolongation2AaBbC :: Tensor 2 3 0 0 0 0 3 3 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseprolongation2AaBbC  (Tensor map1) = M.mapKeys index2Sparseprolongation2AaBbC map1

    prolongation2AaBb :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 -> M.Map (Uinds_3 2) Lind_9 ->Tensor 2 2 0 0 0 0 3 3 Rational
    prolongation2AaBb map1Area map2Area map1Metric map2Metric = tensorAdd tens tensTrans
                        where
                            int2 = interEqn1_2 map1Area map2Area
                            block1 = tensorProductNumeric delta_20 $ tensorProductNumeric delta_20 $ tensorProductNumeric delta_3 $ tensorProductNumeric delta_3 delta_3
                            block2 = tensorProductNumeric int2 $ tensorProductNumeric delta_20 delta_3
                            block3 = tensorTranspose 1 (0,1) $ tensorTranspose 7 (1,2) block2 
                            tens = tensorAdd block1 $ tensorAdd block2 block3
                            tensTrans = tensorTranspose 2 (0,1) $ tensorTranspose 8 (1,2) tens

    index2Sparseprolongation2AaBb :: Index 2 2 0 0 0 0 3 3 -> (Int,Int) 
    index2Sparseprolongation2AaBb  (Index x1  x2  _  _  _  _  x7  x8) = ((e-1)*21*4^6+(f-1)*4^4+(r-1)*4^3+(s-1)*4^2+(m-1)*4+u,(b-1)*21*4*4+(q-1)*21*4+(a-1)*4+p)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      f = 1 + (fromEnum $ getValInd x1 1)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      b = 1 + (fromEnum $ getValInd x2 1)
                                                      p = 1 +  (fromEnum $ getValInd x8 0)
                                                      q = 1 +  (fromEnum $ getValInd x8 1)
                                                      r = 1 +  (fromEnum $ getValInd x7 0)
                                                      s = 1 +  (fromEnum $ getValInd x7 1)
                                                      m = 1 +  (fromEnum $ getValInd x7 2)
                                                      u = 1 +  (fromEnum $ getValInd x8 2)

    mkEqnSparseprolongation2AaBb :: Tensor 2 2 0 0 0 0 3 3 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseprolongation2AaBb  (Tensor map1) = M.mapKeys index2Sparseprolongation2AaBb map1
