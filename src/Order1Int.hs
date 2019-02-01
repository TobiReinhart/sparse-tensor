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

module Order1Int (
    ansatzA, mkEqnSparseAnsatzA,
    ansatzAa, mkEqnSparseAnsatzAa, 
    ansatzAI3, mkEqnSparseAnsatzAI,
    int1A, mkEqnSparseint1A
    
    
) where

    import Index
    import Tensor
    import BasicTensors
    import Ivar
    import Pde
    import EquivarianceEqns
    import qualified Data.Map as M 


    --A ansatz Eqn

    ansatzA :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 1 0 0 1 0 0 0 Rational 
    ansatzA map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) prod
                    where
                        intArea = interArea map1Area map2Area
                        antiSym = aSymI_2 map1Metric
                        totalBlock2 = tensorContractWith_3 (1,1) (+) $ tensorProductNumeric invEta antiSym
                        prod = tensorProductNumeric intArea  totalBlock2
                        

    index2SparseAnsatzA :: Index 1 1 0 0 1 0 0 0 -> (Int,Int) 
    index2SparseAnsatzA  (Index x1  x2  _  _  x5  _  _  _) = ((e-1)*10+j,a+1)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      j = 1 +  (fromEnum $ getValInd x5 0)

    mkEqnSparseAnsatzA :: Tensor 1 1 0 0 1 0 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzA (Tensor map1) = M.mapKeys index2SparseAnsatzA map1

    ansatzAa :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 1 0 0 1 0 1 1 Rational 
    ansatzAa map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (1,0) (+) $ tensorContractWith_3 (0,2) (+) prod
                    where
                        int2 = interEqn1_2 map1Area map2Area
                        antiSym = aSymI_2 map1Metric
                        totalBlock2 = tensorContractWith_3 (1,1) (+) $ tensorProductNumeric invEta antiSym
                        prod = tensorProductNumeric int2 totalBlock2
                        

    index2SparseAnsatzAa :: Index 1 1 0 0 1 0 1 1 -> (Int,Int) 
    index2SparseAnsatzAa  (Index x1  x2  _  _  x5  _  x7  x8) = ((e-1)*10*4+(j-1)*4+r,(a-1)*4+s+22)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      j = 1 +  (fromEnum $ getValInd x5 0)
                                                      r = 1 +  (fromEnum $ getValInd x7 0)
                                                      s = 1 +  (fromEnum $ getValInd x8 0)



    mkEqnSparseAnsatzAa :: Tensor 1 1 0 0 1 0 1 1 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzAa (Tensor map1) = M.mapKeys index2SparseAnsatzAa map1

    
    ansatzAI3 :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 1 0 0 2 1 0 0 Rational 
    ansatzAI3 map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) prod
                    where
                        int3 = interEqn1_3 map1Area map2Area map1Metric map2Metric
                        antiSym = aSymI_2 map1Metric
                        totalBlock2 = tensorContractWith_3 (1,1) (+) $ tensorProductNumeric invEta antiSym
                        prod = tensorProductNumeric int3 totalBlock2
                        

    index2SparseAnsatzAI :: Index 1 1 0 0 2 1 0 0 -> (Int,Int) 
    index2SparseAnsatzAI  (Index x1  x2  _  _  x5  x6  _  _) = ((e-1)*100+(j-1)*10+k,(a-1)*10+i+106)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      j = 1 +  (fromEnum $ getValInd x5 0)
                                                      k = 1 +  (fromEnum $ getValInd x5 1)
                                                      i = 1 +  (fromEnum $ getValInd x6 0)




    mkEqnSparseAnsatzAI :: Tensor 1 1 0 0 2 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzAI (Tensor map1) = M.mapKeys index2SparseAnsatzAI map1

    
    int1A :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 0 1 0 0 0 0 2 2 Rational 
    int1A map1Area map2Area map1Metric map2Metric = tensorSub tens tensTrans
                                                    where
                                                        intArea = interArea map1Area map2Area
                                                        flatA = flatArea map2Area
                                                        flatInter = tensorContractWith_20 (0,1) (+) $ tensorProductNumeric intArea flatA
                                                        block1 = tensorProductNumeric delta_3 flatInter
                                                        block2 = tensorContractWith_20 (0,1) (+) $ tensorProductNumeric intArea flatInter
                                                        tens = tensorAdd block1 block2 
                                                        tensTrans = tensorTranspose 7 (0,1) $ tensorTranspose 8 (0,1) $ tens

    index2Sparseint1A :: Index 0 1 0 0 0 0 2 2 -> (Int,Int) 
    index2Sparseint1A  (Index _  x2  _  _  _  _  x7  x8) = ((m-1)*4^3+(n-1)*4^2+(r-1)*4+s,a)
                                                  where 
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      m = 1 +  (fromEnum $ getValInd x7 0)
                                                      r = 1 +  (fromEnum $ getValInd x7 0)
                                                      n = 1 +  (fromEnum $ getValInd x8 0)
                                                      s = 1 +  (fromEnum $ getValInd x8 1)

    mkEqnSparseint1A :: Tensor 0 1 0 0 0 0 2 2 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseint1A (Tensor map1) = M.mapKeys index2Sparseint1A map1