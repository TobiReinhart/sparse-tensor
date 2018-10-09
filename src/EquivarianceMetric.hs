--this module contains the functions that we need to construct the equivariance equations

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

module EquivarianceMetric (
    eqn1_1M, eqn1_2M, eqn1_3M, eqn2_2M, eqn2_3M, eqn3_3M, 
    index2Sparse1M, index2Sparse2M, index2Sparse3M, index2Sparse4M, index2Sparse5M, index2Sparse6M,
    mkEqn1SparseM, mkEqn2SparseM, mkEqn3SparseM, mkEqn4SparseM, mkEqn5SparseM, mkEqn6SparseM
    
) where

    import Index
    import Tensor
    import BasicTensors
    import Ivar
    import Pde
    import qualified Data.Map as M 

    --the first step is writing functions for constructing the 6 equation blocks that we need

    eqn1_1M ::  M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 0 0 0 0 0 1 1 1 (Ivar Rational)
    eqn1_1M map1Metric map2Metric = tensorContractWith_9 (0,1) addIvar prod 
                    where
                        prod = tensorProductWith sMultIvar (interMetric map1Metric map2Metric) ivar1M

    eqn1_2M ::  M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 0 0 0 0 0 1 1 2 (Ivar Rational)
    eqn1_2M map1Metric map2Metric =  tensorContractWith_9 (0,1) addIvar $ tensorContractWith_3 (1,2) addIvar prod
                    where
                        int1 = tensorProductWith (*) (interMetric map1Metric map2Metric) delta_3 
                        int2 = tensorProductWith (*) ( tensorTranspose 8 (0,1) $ tensorProductWith (*) delta_3 delta_3 ) delta_9
                        intTotal = tensorSub int1 int2 
                        prod = tensorProductWith sMultIvar intTotal ivar2M

    eqn1_3M :: M.Map (Linds_3 2) Uind_9 -> M.Map (Uinds_3 2) Lind_9 -> Tensor 0 0 0 0 0 2 1 1 (Ivar Rational)
    eqn1_3M map1Metric map2Metric  = tensorContractWith_9 (0,2) addIvar $ tensorContractWith_9 (1,3) addIvar prod
                    where
                        int1 = tensorProductWith (*) (interMetric map1Metric map2Metric) delta_9
                        int2 = tensorProductWith (*) delta_9 (interMetric map1Metric map2Metric)
                        intTotal = tensorAdd int1 int2
                        prod = tensorProductWith sMultIvar intTotal ivar3M

    eqn1_4 :: Tensor 0 0 0 0 0 0 1 1 Rational
    eqn1_4 = delta_3

    eqn2_2M :: M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 0 0 0 0 1 1 0 2 (Ivar Rational)
    eqn2_2M map1Metric map2Metric = tensorContractWith_9 (0,1) addIvar prod 
                    where
                        int1 = tensorProductWith (*) (interMetric map1Metric map2Metric) (interI_2 map1Metric)
                        interTotal = tensorContractWith_3 (0,2) (+) int1
                        prod = tensorProductWith sMultIvar interTotal ivar1M
                    
    eqn2_3M :: M.Map (Linds_3 2) Uind_9 -> M.Map (Uinds_3 2) Lind_9 -> Tensor 0 0 0 0 1 2 0 1 (Ivar Rational)
    eqn2_3M map1Metric map2Metric = tensorContractWith_3 (0,1) addIvar $ tensorContractWith_9 (1,2) addIvar prod 
                    where
                        int1_1 = tensorProductWith (*) (interI_2 map1Metric) (interMetric map1Metric map2Metric) 
                        int1_2 = tensorContractWith_3 (0,1) (+) int1_1
                        int1_3 = tensorProductWith (*) (interJ_2 map2Metric) int1_2
                        int1_4 = tensorContractWith_3 (1,0) (+) int1_3
                        int1 = tensorSMult 2 int1_4 
                        int2 = tensorProductWith (*) delta_9 $ tensorProductWith (*) delta_9 delta_3
                        interTotal = tensorSub int1 int2
                        prod = tensorProductWith sMultIvar interTotal ivar2M

    eqn3_3M :: M.Map (Linds_3 3) Uind_19 ->M.Map (Linds_3 2) Uind_9 -> M.Map (Uinds_3 2) Lind_9 -> Tensor 0 0 1 0 0 2 0 1 (Ivar Rational)
    eqn3_3M mapInterI3 map1Metric map2Metric = tensorContractWith_9 (0,2) addIvar prod
                    where
                        int1 = tensorProductWith (*) (interJ_2 map2Metric) (interMetric map1Metric map2Metric)
                        intTotal = tensorContractWith_3 (0,1) (+) $ tensorContractWith_3 (1,2) (+) $ tensorContractWith_3 (2,3) (+) $ tensorProductWith (*) int1 (interI_3 mapInterI3)
                        prod = tensorProductWith sMultIvar intTotal ivar1M

    --we need the Metric versions of these functions

    index2Sparse1M :: Index 0 0 0 0 0 1 1 1 -> (Int,Int) 
    index2Sparse1M (_, _, _, _, _, x6, x7, x8) = ((m-1)*4+n,a+1)
                         where 
                             a = 1 + (fromEnum $ getValInd x6 0)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    --the V_Ai i index always comes after the delta_mn n index (order of indices is important !)

    --check this later on when we have extracted the equations

    index2Sparse2M :: Index 0 0 0 0 0 1 1 2 -> (Int,Int) 
    index2Sparse2M (_, _, _, _, _, x6, x7, x8) = ((m-1)*4+n,10+(a-1)*4+i+1)
                         where 
                             a = 1 + (fromEnum $ getValInd x6 0)
                             i = 1 + (fromEnum $ getValInd x8 1)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    index2Sparse3M :: Index 0 0 0 0 0 2 1 1 -> (Int,Int) 
    index2Sparse3M (_, _, _, _, _, x6, x7, x8) = ((m-1)*4+n,10*5+(a-1)*10+i+1)
                         where 
                             a = 1 + (fromEnum $ getValInd x6 0)
                             i = 1 + (fromEnum $ getValInd x6 1)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    index2Sparse4M :: Index 0 0 0 0 1 1 0 2 -> (Int,Int) 
    index2Sparse4M (_, _, _, _, x5, x6, _, x8) = (16+(j-1)*4+n,10+(a-1)*4+i+1)
                         where 
                             a = 1 + (fromEnum $ getValInd x6 0)
                             j = 1 + (fromEnum $ getValInd x5 0)
                             i = 1 + (fromEnum $ getValInd x8 1)
                             n = 1 + (fromEnum $ getValInd x8 0)

    index2Sparse5M :: Index 0 0 0 0 1 2 0 1 -> (Int,Int) 
    index2Sparse5M (_, _, _, _, x5, x6, _, x8) = (16+(j-1)*4+n,10*5+(a-1)*10+i+1)
                         where 
                             a = 1 + (fromEnum $ getValInd x6 1)
                             j = 1 + (fromEnum $ getValInd x5 0)
                             i = 1 + (fromEnum $ getValInd x6 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    index2Sparse6M :: Index 0 0 1 0 0 2 0 1 -> (Int,Int) 
    index2Sparse6M (_, _, x3, _, _, x6, _, x8) = (56+(j-1)*4+n,10*5+(a-1)*10+i+1)
                         where 
                             a = 1 + (fromEnum $ getValInd x6 1)
                             j = 1 + (fromEnum $ getValInd x3 0)
                             i = 1 + (fromEnum $ getValInd x6 0)
                             n = 1 + (fromEnum $ getValInd x8 0)


    mkEqn1SparseM :: Tensor 0 0 0 0 0 1 1 1 a -> M.Map (Int,Int) a
    mkEqn1SparseM (Tensor map1) = M.mapKeys index2Sparse1M map1 

    mkEqn2SparseM :: Tensor 0 0 0 0 0 1 1 2 a -> M.Map (Int,Int) a
    mkEqn2SparseM (Tensor map1) = M.mapKeys index2Sparse2M map1
    
    mkEqn3SparseM :: Tensor 0 0 0 0 0 2 1 1 a -> M.Map (Int,Int) a
    mkEqn3SparseM (Tensor map1) = M.mapKeys index2Sparse3M map1 

    mkEqn4SparseM :: Tensor 0 0 0 0 1 1 0 2 a -> M.Map (Int,Int) a
    mkEqn4SparseM (Tensor map1) = M.mapKeys index2Sparse4M map1
    
    mkEqn5SparseM :: Tensor 0 0 0 0 1 2 0 1 a -> M.Map (Int,Int) a
    mkEqn5SparseM (Tensor map1) = M.mapKeys index2Sparse5M map1 

    mkEqn6SparseM :: Tensor 0 0 1 0 0 2 0 1 a -> M.Map (Int,Int) a
    mkEqn6SparseM (Tensor map1) = M.mapKeys index2Sparse6M map1 

    