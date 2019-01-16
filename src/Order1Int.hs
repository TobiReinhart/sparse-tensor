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
    ansatzAI, mkEqnSparseAnsatzAI
    
    
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
    index2SparseAnsatzA  (x1, x2, _, _, x5, _, _, _) = ((e-1)*10+j,a)
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
    index2SparseAnsatzAa  (x1, x2, _, _, x5, _, x7, x8) = ((e-1)*10*4+(j-1)*4+r,(a-1)*4+s)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      j = 1 +  (fromEnum $ getValInd x5 0)
                                                      r = 1 +  (fromEnum $ getValInd x7 0)
                                                      s = 1 +  (fromEnum $ getValInd x8 0)



    mkEqnSparseAnsatzAa :: Tensor 1 1 0 0 1 0 1 1 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzAa (Tensor map1) = M.mapKeys index2SparseAnsatzAa map1

    
    ansatzAI2 :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 1 0 0 2 1 0 0 Rational 
    ansatzAI2 map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) prod
                    where
                        int3 = interEqn1_3 map1Area map2Area map1Metric map2Metric
                        antiSym = aSymI_2 map1Metric
                        totalBlock2 = tensorContractWith_3 (1,1) (+) $ tensorProductNumeric invEta antiSym
                        prod = tensorProductNumeric int3 totalBlock2
                        

    index2SparseAnsatzAI :: Index 1 1 0 0 2 1 0 0 -> (Int,Int) 
    index2SparseAnsatzAI  (x1, x2, _, _, x5, x6, _, _) = ((e-1)*100+(j-1)*10+k,(a-1)*10+i)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      j = 1 +  (fromEnum $ getValInd x5 0)
                                                      k = 1 +  (fromEnum $ getValInd x5 1)
                                                      i = 1 +  (fromEnum $ getValInd x6 0)




    mkEqnSparseAnsatzAI :: Tensor 1 1 0 0 2 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzAI (Tensor map1) = M.mapKeys index2SparseAnsatzAI map1
