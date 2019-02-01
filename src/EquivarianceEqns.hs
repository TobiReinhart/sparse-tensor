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

module EquivarianceEqns (
    eqn1_1, eqn1_2, eqn1_3, eqn1_4, eqn2_2, eqn2_3, eqn3_3,
    index2Sparse1, index2Sparse2, index2Sparse3, index2Sparse4, index2Sparse5, index2Sparse6,index2SparseConst,
    mkEqn1Sparse, mkEqn2Sparse, mkEqn3Sparse, mkEqn4Sparse, mkEqn5Sparse, mkEqn6Sparse, mkEqnConstSparse, showEqns,
    eqn1_1Flat, eqn2_2Flat, eqn3_3Flat, mkEqnConstSparseFlat, showEqnsFlat, showEqnsFlatFrac, showEqnsFlatMatLab

) where

    import Index
    import Tensor
    import BasicTensors
    import Ivar
    import Pde
    import qualified Data.Map as M 

    --the first step is writing functions for constructing the 6 equation blocks that we need

    eqn1_1 ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> Tensor 0 1 0 0 0 0 1 1 (Ivar Rational)
    eqn1_1 map1Area map2Area = tensorContractWith_20 (0,1) addIvar prod 
                    where
                        prod = tensorProductWith sMultIvar (interArea map1Area map2Area) ivar1

    eqn1_2 ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> Tensor 0 1 0 0 0 0 1 2 (Ivar Rational)
    eqn1_2 map1Area map2Area =  tensorContractWith_20 (0,1) addIvar $ tensorContractWith_3 (1,2) addIvar prod
                    where
                        int1 = tensorProductWith (*) (interArea map1Area map2Area) delta_3 
                        int2 = tensorProductWith (*) ( tensorTranspose 8 (0,1) $ tensorProductWith (*) delta_3 delta_3 ) delta_20
                        intTotal = tensorSub int1 int2 
                        prod = tensorProductWith sMultIvar intTotal ivar2

    eqn1_3 :: M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> Tensor 0 1 0 0 0 1 1 1 (Ivar Rational)
    eqn1_3 map1Metric map2Metric map1Area map2Area = tensorContractWith_20 (0,1) addIvar $ tensorContractWith_9 (0,1) addIvar prod
                    where
                        int1 = tensorProductWith (*) (interArea map1Area map2Area) delta_9
                        int2 = tensorProductWith (*) (interMetric map1Metric map2Metric) delta_20
                        intTotal = tensorAdd int1 int2
                        prod = tensorProductWith sMultIvar intTotal ivar3

    eqn1_4 :: Tensor 0 0 0 0 0 0 1 1 Rational
    eqn1_4 = delta_3

    --there is a problem with the fcators due to symmetrization !!!
    --
    --
    --

    eqn2_2 :: M.Map (Linds_3 2) Uind_9 ->  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> Tensor 0 1 0 0 1 0 0 2 (Ivar Rational)
    eqn2_2 mapInterI2 map1Area map2Area = tensorContractWith_20 (0,1) addIvar prod 
                    where
                        int1 = tensorProductWith (*) (interArea map1Area map2Area) (interI_2 mapInterI2)
                        interTotal = tensorContractWith_3 (0,1) (+) int1
                        prod = tensorProductWith sMultIvar interTotal ivar1
                    
    eqn2_3 :: M.Map (Linds_3 2) Uind_9 -> M.Map (Uinds_3 2) Lind_9 -> M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> Tensor 0 1 0 0 1 1 0 1 (Ivar Rational)
    eqn2_3 mapInterI2 mapInterJ2 map1Area map2Area = tensorContractWith_3 (0,1) addIvar $ tensorContractWith_20 (0,1) addIvar prod 
                    where
                        int1_1 = tensorProductWith (*) (interArea map1Area map2Area) (interI_2 mapInterI2)
                        int1_2 = tensorContractWith_3 (0,1) (+) int1_1
                        int1_3 = tensorProductWith (*) (interJ_2 mapInterJ2) int1_2
                        int1_4 = tensorContractWith_3 (1,1) (+) int1_3
                        int1 = tensorSMult 2 int1_4 
                        int2 = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_9 delta_3
                        interTotal = tensorSub int1 int2
                        prod = tensorProductWith sMultIvar interTotal ivar2

    eqn3_3 :: M.Map (Linds_3 3) Uind_19 -> M.Map (Uinds_3 2) Lind_9 -> M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> Tensor 0 1 1 0 0 1 0 1 (Ivar Rational)
    eqn3_3 mapInterI3 mapInterJ2 map1Area map2Area = tensorContractWith_20 (0,1) addIvar prod
                    where
                        int1 = tensorProductWith (*) (interJ_2 mapInterJ2) (interArea map1Area map2Area)
                        intTotal = tensorContractWith_3 (0,1) (+) $ tensorContractWith_3 (1,2) (+) $ tensorContractWith_3 (2,3) (+) $ tensorProductWith (*) int1 (interI_3 mapInterI3)
                        prod = tensorProductWith sMultIvar intTotal ivar1




    --these functions are specified for area metric (21 dofs) -> can be used to make a sparse matrix map from the 6 tensor maps

    index2Sparse1 :: Index 0 1 0 0 0 0 1 1 -> (Int,Int) 
    index2Sparse1 (Index _  x2  _  _  _  _  x7  x8) = ((m-1)*4+n,a+1)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    --the V_Ai i index always comes after the delta_mn n index (order of indices is important !)

    --check this later on when we have extracted the equations

    index2Sparse2 :: Index 0 1 0 0 0 0 1 2 -> (Int,Int) 
    index2Sparse2 (Index _  x2  _  _  _  _  x7  x8) = ((m-1)*4+n,21+(a-1)*4+i+1)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             i = 1 + (fromEnum $ getValInd x8 1)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    index2Sparse3 :: Index 0 1 0 0 0 1 1 1 -> (Int,Int) 
    index2Sparse3 (Index _  x2  _  _  _  x6  x7  x8) = ((m-1)*4+n,21*5+(a-1)*10+i+1)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             i = 1 + (fromEnum $ getValInd x6 0)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    index2Sparse4 :: Index 0 1 0 0 1 0 0 2 -> (Int,Int) 
    index2Sparse4 (Index _  x2  _  _  x5  _  _  x8) = (16+(j-1)*4+n,21+(a-1)*4+i+1)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             j = 1 + (fromEnum $ getValInd x5 0)
                             i = 1 + (fromEnum $ getValInd x8 1)
                             n = 1 + (fromEnum $ getValInd x8 0)

    index2Sparse5 :: Index 0 1 0 0 1 1 0 1 -> (Int,Int) 
    index2Sparse5 (Index _  x2  _  _  x5  x6  _  x8) = (16+(j-1)*4+n,21*5+(a-1)*10+i+1)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             j = 1 + (fromEnum $ getValInd x5 0)
                             i = 1 + (fromEnum $ getValInd x6 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    index2Sparse6 :: Index 0 1 1 0 0 1 0 1 -> (Int,Int) 
    index2Sparse6 (Index _  x2  x3  _   _  x6  _  x8) = (56+(j-1)*4+n,21*5+(a-1)*10+i+1)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             j = 1 + (fromEnum $ getValInd x3 0)
                             i = 1 + (fromEnum $ getValInd x6 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    index2SparseConst :: Index 0 0 0 0 0 0 1 1 -> (Int,Int)
    index2SparseConst (Index _ _ _ _ _ _ x7 x8) = ((m-1)*4+n,1)
                        where 
                            m = 1 + (fromEnum $ getValInd x7 0)
                            n = 1 + (fromEnum $ getValInd x8 0)

    mkEqn1Sparse :: Tensor 0 1 0 0 0 0 1 1 a -> M.Map (Int,Int) a
    mkEqn1Sparse (Tensor map1) = M.mapKeys index2Sparse1 map1 

    mkEqn2Sparse :: Tensor 0 1 0 0 0 0 1 2 a -> M.Map (Int,Int) a
    mkEqn2Sparse (Tensor map1) = M.mapKeys index2Sparse2 map1
    
    mkEqn3Sparse :: Tensor 0 1 0 0 0 1 1 1 a -> M.Map (Int,Int) a
    mkEqn3Sparse (Tensor map1) = M.mapKeys index2Sparse3 map1 

    mkEqn4Sparse :: Tensor 0 1 0 0 1 0 0 2 a -> M.Map (Int,Int) a
    mkEqn4Sparse (Tensor map1) = M.mapKeys index2Sparse4 map1
    
    mkEqn5Sparse :: Tensor 0 1 0 0 1 1 0 1 a -> M.Map (Int,Int) a
    mkEqn5Sparse (Tensor map1) = M.mapKeys index2Sparse5 map1 

    mkEqn6Sparse :: Tensor 0 1 1 0 0 1 0 1 a -> M.Map (Int,Int) a
    mkEqn6Sparse (Tensor map1) = M.mapKeys index2Sparse6 map1 

    mkEqnConstSparse :: Tensor 0 0 0 0 0 0 1 1 a -> M.Map (Int,Int) (Ivar a)
    mkEqnConstSparse (Tensor map1) = M.map mkConstIvar $ M.mapKeys index2SparseConst map1 

    showEqns :: M.Map (Int,Int) (Ivar Rational) -> String
    showEqns map1 = "{" ++ (tail (concat list2)) ++ "}"
                        where
                            map2 = M.map showIvarRational map1 
                            list1 = M.assocs map2
                            list2 = map (\(x,y) -> "," ++ show x ++ "=" ++ y ++ "\n") list1

    
    --the flat equations

    eqn1_1Flat ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> Tensor 0 1 0 0 0 0 1 1 Rational
    eqn1_1Flat map1Area map2Area = tensorContractWith_20 (0,1) (+) prod 
                    where
                        prod = tensorProductWith (*) (interArea map1Area map2Area) (flatArea map2Area)

                
    eqn2_2Flat :: M.Map (Linds_3 2) Uind_9 ->  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> Tensor 0 1 0 0 1 0 0 2 Rational
    eqn2_2Flat mapInterI2 map1Area map2Area = tensorContractWith_20 (0,1) (+) prod 
                    where
                        int1 = tensorProductWith (*) (interArea map1Area map2Area) (interI_2 mapInterI2)
                        interTotal = tensorContractWith_3 (0,1) (+) int1
                        prod = tensorProductWith (*) interTotal (flatArea map2Area)

    
    eqn3_3Flat :: M.Map (Linds_3 3) Uind_19 -> M.Map (Uinds_3 2) Lind_9 -> M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> Tensor 0 1 1 0 0 1 0 1 Rational
    eqn3_3Flat mapInterI3 mapInterJ2 map1Area map2Area = tensorContractWith_20 (0,1) (+) prod
                    where
                        int1 = tensorProductWith (*) (interJ_2 mapInterJ2) (interArea map1Area map2Area)
                        intTotal = tensorContractWith_3 (0,1) (+) $ tensorContractWith_3 (1,2) (+) $ tensorContractWith_3 (2,3) (+) $ tensorProductWith (*) int1 (interI_3 mapInterI3)
                        prod = tensorProductWith (*) intTotal (flatArea map2Area)

   
    mkEqnConstSparseFlat :: Tensor 0 0 0 0 0 0 1 1 a -> M.Map (Int,Int) a
    mkEqnConstSparseFlat (Tensor map1) = M.mapKeys index2SparseConst map1 

    showEqnsFlat :: M.Map (Int,Int) Rational -> String
    showEqnsFlat map1 = "{" ++ (tail (concat list2)) ++ "}"
                        where
                            map2 = M.map (show.truncate) $ M.filter (/=0) map1 
                            list1 = M.assocs map2
                            list2 = map (\(x,y) -> "," ++ show x ++ "=" ++ y ++ "\n") list1

    showEqnsFlatFrac :: M.Map (Int,Int) Rational -> String
    showEqnsFlatFrac map1 = "{" ++ (tail (concat list2)) ++ "}"
                        where
                            map2 = M.map (show) $ M.filter (/=0) map1 
                            list1 = M.assocs map2
                            list2 = map (\(x,y) -> "," ++ show x ++ "=" ++ y ++ "\n") list1

    
    showEqnsFlatMatLab :: M.Map (Int,Int) Rational -> String
    showEqnsFlatMatLab map1 = (concat list2)
                        where
                            map2 = M.map (show.truncate) $ M.filter (/=0) map1 
                            list1 = M.assocs map2
                            list2 = map (\(x,y) -> (showKeyMatLab x) ++ " " ++ y ++ " " ++ "\n") list1

    showKeyMatLab :: (Int,Int) -> String 
    showKeyMatLab (i,j) = (show i) ++ " " ++ (show j)

    