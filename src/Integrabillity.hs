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
    areaMetricMetric1intCond, index2SparseAreaMintCond, mkEqnSparseAreaMintCond
    
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
    index2SparseCond1 (x1, x2, _, _, x5, x6, _, _) = (1187+(b-1)*100+(j-1)*10+(k-1),21*5+(a-1)*10+i+1)
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

    index2SparseCond1Zero :: Index 0 1 0 0 1 0 0 0 -> (Int,Int) 
    index2SparseCond1Zero (_, x2, _, _, x5, _, _, _) = (k,a)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             k = 1 + (fromEnum $ getValInd x5 0)
                            
    mkEqnSparseCond1Zero :: Tensor 0 1 0 0 1 0 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseCond1Zero (Tensor map1) = M.mapKeys index2SparseCond1Zero map1 

    index2SparseBlock1Eta :: Index 0 1 0 0 0 0 2 0 -> (Int,Int) 
    index2SparseBlock1Eta (_, x2, _, _, _, _, x7, _) = ((r-1)*4+n,a)
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
    index2SparseCond2 (x1, x2, _, _, x5, _, x7, x8) = (347+(b-1)*40+(k-1)*4+(r-1),21+(a-1)*4+s+1)
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
    index2SparseCond3 (x1, x2, _, _, x5, _, _, _) = (137+(b-1)*10+(k-1),a+1)
                         where 
                             b = 1 + (fromEnum $ getValInd x1 0)
                             a = 1 + (fromEnum $ getValInd x2 0)
                             k = 1 + (fromEnum $ getValInd x5 0)

    mkEqnSparseCond3 :: Tensor 1 1 0 0 1 0 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseCond3 (Tensor map1) = M.mapKeys index2SparseCond3 map1 
                             

    projectorMatrix :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Linds_3 2) Uind_9 -> Tensor 1 0 0 0 1 0 0 6 Rational 
    projectorMatrix map1Area map1Metric = tensorProductWith (*) (interI_Area map1Area) (interI_2 map1Metric)

    index2SparseProjector :: Index 1 0 0 0 1 0 0 6 -> (Int,Int) 
    index2SparseProjector (x1, _, _, _, x5, _, _, x8) = ((a-1)*10+b,i*4^5+j*4^4+k*4^3+l*4^2+p*4+q+1)
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
    index2SparseProjector2 (x1, _, _, _, _, _, _, x8) = (a,i*4^3+j*4^2+k*4+l+1)
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
    index2SparseintCond2_1 (x1, x2, _, _, x5, x6, _, _) = ((e-1)*2100+(f-1)*100+(j-1)*10+k,(a-1)*210+(b-1)*10+i)
                         where 
                             e = 1 + (fromEnum $ getValInd x1 0)
                             f = 1 + (fromEnum $ getValInd x1 1)
                             a = 1 + (fromEnum $ getValInd x2 0)
                             b = 1 + (fromEnum $ getValInd x2 1)
                             j = 1 +  (fromEnum $ getValInd x5 0)
                             k = 1 +  (fromEnum $ getValInd x5 1)
                             i = 1 +  (fromEnum $ getValInd x6 0)

    mkEqnSparseintCond2_1 :: Tensor 2 2 0 0 2 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseintCond2_1 (Tensor map1) = M.mapKeys index2SparseintCond2_1 map1 

    intCond2_1Symbol :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 2 3 0 0 2 1 0 0 Rational 
    intCond2_1Symbol map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) total
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        sym = interI_2 map1Metric
                        flatInter = tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) intArea (flatArea map2Area)
                        tens = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_9 flatInter
                        totalBlockEta = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta sym
                        total = tensorProductWith (*) tens totalBlockEta


    index2SparseintCond2_1Symbol :: M.Map (Int,Int) Int -> Index 2 3 0 0 2 1 0 0 -> (Int,Int) 
    index2SparseintCond2_1Symbol map1 (x1, x2, _, _, x5, x6, x7, x8) = (10*10*21*(e-1)+10*10*(f-1)+10*(j-1)+k,(x-1)*210+(b-1)*10+i)
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
    index2SparseDens1 (_, _, _, _, x5, x6, x7, x8) = ((b-1)*16+(m-1)*4+n,a)
                         where 
                             a = 1 + (fromEnum $ getValInd x6 0)
                             b = 1 + (fromEnum $ getValInd x5 0)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    index2SparseDens2 :: Index 0 0 0 0 2 2 1 1 -> (Int,Int) 
    index2SparseDens2 (_, _, _, _, x5, x6, x7, x8) = ((c-1)*160+(b-1)*16+(m-1)*4+n,(d-1)*10+a)
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
    index2SparseDens1Area (x1 ,x2 , _, _, _, _, x7, x8) = ((b-1)*16+(m-1)*4+n,a)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             b = 1 + (fromEnum $ getValInd x1 0)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    index2SparseDens2Area :: Index 2 2 0 0 0 0 1 1 -> (Int,Int) 
    index2SparseDens2Area (x1, x2, _, _, _, _, x7, x8) = ((c-1)*16*21+(b-1)*16+(m-1)*4+n,(d-1)*21+a)
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
    index2SparseAreaM (_, x2, _, _, x5, x6, x7, x8) = ((j-1)*16+(m-1)*4+n,(a-1)*10+i)
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
    index2SparseAreaMintCond (x1, x2, _, _, x5, x6, _, _) = ((b-1)*100+(j-1)*10+k,(a-1)*10+i)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             b = 1 + (fromEnum $ getValInd x1 0)
                             i = 1 + (fromEnum $ getValInd x6 0)
                             j = 1 + (fromEnum $ getValInd x5 0)
                             k = 1 + (fromEnum $ getValInd x5 1)

    mkEqnSparseAreaMintCond :: Tensor 1 1 0 0 2 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAreaMintCond  (Tensor map1) = M.mapKeys index2SparseAreaMintCond map1


                             
