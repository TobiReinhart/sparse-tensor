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
    intCond1, index2SparseCond1, mkEqnSparseCond1, int1Zero, index2SparseCond1Zero, mkEqnSparseCond1Zero, index2SparseBlock1Eta,
    mkEqnSparseBlock1Eta, intCond2, index2SparseCond2, mkEqnSparseCond2, intCond3, index2SparseCond3, mkEqnSparseCond3
    
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
                             
    
