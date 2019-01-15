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

module Order3Int (
    ansatzABC, mkEqnSparseAnsatzABC2,
    intABC, mkEqnSparseIntABC2,
    ansatzABC2,
    ansatzAaBC, mkEqnSparseAnsatzAaBC,
    ansatzAIBC, mkEqnSparseAnsatzAIBC
    
) where

    import Index
    import Tensor
    import BasicTensors
    import Ivar
    import Pde
    import EquivarianceEqns
    import qualified Data.Map as M 


    --A:B:C ansatz Eqn

    ansatzABC :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 3 3 0 0 1 0 0 0 Rational 
    ansatzABC map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) prod
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        antiSym = aSymI_2 map1Metric
                        block1 = tensorProductNumeric intArea $ tensorProductNumeric delta_20 delta_20 
                        block2 = tensorTranspose 1 (0,2) block1
                        block3 = tensorTranspose 1 (0,1) block2 
                        totalBlock1 = tensorAdd block1 $ tensorAdd block2 block3 
                        totalBlockTrans1 = tensorTranspose 2 (0,1) totalBlock1
                        totalBlockTrans2 = tensorTranspose 2 (0,2) totalBlock1
                        totalBlockTrans3 = tensorTranspose 2 (1,2) totalBlock1
                        totalBlockTrans4 = tensorTranspose 2 (0,2) totalBlockTrans1
                        totalBlockTrans5 = tensorTranspose 2 (1,2) totalBlockTrans1
                        tens = tensorAdd totalBlock1 $ tensorAdd totalBlockTrans1 $ tensorAdd totalBlockTrans2 $ tensorAdd totalBlockTrans3 $ tensorAdd totalBlockTrans4 totalBlockTrans5
                        totalBlock2 = tensorContractWith_3 (1,1) (+) $ tensorProductNumeric invEta antiSym
                        prod = tensorProductNumeric tens totalBlock2

    ansatzABC2 :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 3 3 0 0 1 0 0 0 Rational 
    ansatzABC2 map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ 
        tensorProductNumeric 
        (tensorAdd totalBlock1 
            $ tensorAdd (tensorTranspose 2 (0,1) totalBlock1)
            $ tensorAdd (tensorTranspose 2 (0,2) totalBlock1) 
            $ tensorAdd (tensorTranspose 2 (1,2) totalBlock1) 
            $ tensorAdd (tensorTranspose 2 (0,2) $ tensorTranspose 2 (0,1) totalBlock1) 
            (tensorTranspose 2 (1,2) $ tensorTranspose 2 (0,1) totalBlock1))
            (tensorContractWith_3 (1,1) (+) $ tensorProductNumeric invEta antiSym)
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        antiSym = aSymI_2 map1Metric
                        block1 = tensorProductNumeric intArea $ tensorProductNumeric delta_20 delta_20 
                        block2 = tensorTranspose 1 (0,2) block1
                        block3 = tensorTranspose 1 (0,1) block2 
                        totalBlock1 = tensorAdd block1 $ tensorAdd block2 block3 
                        


    index2SparseAnsatzABC :: Index 3 3 0 0 1 0 0 0 -> (Int,Int) 
    index2SparseAnsatzABC  (x1, x2, _, _, x5, _, _, _) = ((g-1)*21^2*10+(e-1)*210+(f-1)*10+j,(c-1)*21^2+(b-1)*21+a)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      f = 1 + (fromEnum $ getValInd x1 1)
                                                      g = 1 + (fromEnum $ getValInd x1 2)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      b = 1 + (fromEnum $ getValInd x2 1)
                                                      c = 1 + (fromEnum $ getValInd x2 2)
                                                      j = 1 +  (fromEnum $ getValInd x5 0)

    mkEqnSparseAnsatzABC2 :: Tensor 3 3 0 0 1 0 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzABC2 (Tensor map1) = M.mapKeys index2SparseAnsatzABC map1

    intABC :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 2 3 0 0 0 0 2 2 Rational 
    intABC map1Area map2Area map1Metric map2Metric = tensorSub prod prodTrans 
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        flatA = flatArea map2Area
                        flatInt = tensorContractWith_20 (0,1) (+) $ tensorProductNumeric intArea flatA 
                        block0 = tensorTranspose 1 (0,2) $ tensorProductNumeric delta_20 $ tensorProductNumeric delta_20 $ tensorProductNumeric delta_20 delta_3
                        block1 = tensorProductNumeric intArea $ tensorProductNumeric delta_20 delta_20 
                        block2 = tensorTranspose 1 (0,2) block1
                        block3 = tensorTranspose 1 (0,1) block2 
                        totalBlock1 = tensorAdd block0 $ tensorAdd block1 $ tensorAdd block2 block3 
                        totalBlockTrans1 = tensorTranspose 2 (0,1) totalBlock1
                        totalBlockTrans2 = tensorTranspose 2 (0,2) totalBlock1
                        totalBlockTrans3 = tensorTranspose 2 (1,2) totalBlock1
                        totalBlockTrans4 = tensorTranspose 2 (0,2) totalBlockTrans1
                        totalBlockTrans5 = tensorTranspose 2 (1,2) totalBlockTrans1
                        tens = tensorAdd totalBlock1 $ tensorAdd totalBlockTrans1 $ tensorAdd totalBlockTrans2 $ tensorAdd totalBlockTrans3 $ tensorAdd totalBlockTrans4 totalBlockTrans5
                        prod = tensorContractWith_20 (1,3) (+) $ tensorProductNumeric tens flatInt
                        prodTrans = tensorTranspose 7 (0,1) $ tensorTranspose 8 (0,1) prod 

    index2SparseIntABC :: Index 2 3 0 0 0 0 2 2 -> (Int,Int) 
    index2SparseIntABC  (x1, x2, _, _, _, _, x7, x8) = ((e-1)*21*4^4+(f-1)*4^4+(m-1)*4^3+(r-1)*4^2+(n-1)*4+s,(c-1)*21^2+(b-1)*21+a)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      f = 1 + (fromEnum $ getValInd x1 1)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      b = 1 + (fromEnum $ getValInd x2 1)
                                                      c = 1 + (fromEnum $ getValInd x2 2)
                                                      m = 1 + (fromEnum $ getValInd x7 0)
                                                      r = 1 + (fromEnum $ getValInd x7 1)
                                                      n = 1 + (fromEnum $ getValInd x8 0)
                                                      s = 1 + (fromEnum $ getValInd x8 1)


    mkEqnSparseIntABC2 :: Tensor 2 3 0 0 0 0 2 2 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseIntABC2 (Tensor map1) = M.mapKeys index2SparseIntABC map1

    --the Aa:C:D ansatz eqn

    ansatzAaBC :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 3 3 0 0 1 0 1 1 Rational 
    ansatzAaBC map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (1,0) (+) $ tensorContractWith_3 (0,2) (+) prod
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        int2 = interEqn1_2 map1Area map2Area
                        antiSym = aSymI_2 map1Metric
                        block1 = tensorProductNumeric delta_20 $ tensorProductNumeric intArea $ tensorProductNumeric delta_20 delta_3 
                        block2 = tensorTranspose 1 (1,2) block1
                        block3 = tensorProductNumeric int2 $ tensorProductNumeric delta_20 delta_20  
                        totalBlock1 = tensorAdd block1 $ tensorAdd block2 block3 
                        totalBlockTrans = tensorTranspose 2 (0,1) totalBlock1
                        tens = tensorAdd totalBlock1 totalBlockTrans
                        totalBlock2 = tensorContractWith_3 (1,1) (+) $ tensorProductNumeric invEta antiSym
                        prod = tensorProductNumeric tens totalBlock2

    index2SparseAnsatzAaBC :: Index 3 3 0 0 1 0 1 1 -> (Int,Int) 
    index2SparseAnsatzAaBC  (x1, x2, _, _, x5, _, x7, x8) = ((g-1)*21^2*10*4+(e-1)*210*4+(f-1)*10*4+(j-1)*4+r,(c-1)*21^2*4+(b-1)*21*4+(a-1)*4+s)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      f = 1 + (fromEnum $ getValInd x1 1)
                                                      g = 1 + (fromEnum $ getValInd x1 2)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      b = 1 + (fromEnum $ getValInd x2 1)
                                                      c = 1 + (fromEnum $ getValInd x2 2)
                                                      j = 1 +  (fromEnum $ getValInd x5 0)
                                                      r = 1 +  (fromEnum $ getValInd x7 0)
                                                      s = 1 +  (fromEnum $ getValInd x8 0)


    mkEqnSparseAnsatzAaBC :: Tensor 3 3 0 0 1 0 1 1 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzAaBC (Tensor map1) = M.mapKeys index2SparseAnsatzAaBC map1

    ansatzAIBC :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 3 3 0 0 2 1 0 0 Rational 
    ansatzAIBC map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) prod
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        int3 = interEqn1_3 map1Area map2Area map1Metric map2Metric
                        antiSym = aSymI_2 map1Metric
                        block1 = tensorProductNumeric int3 $ tensorProductNumeric delta_20 delta_20 
                        block2 = tensorProductNumeric delta_20 $ tensorProductNumeric delta_9 $ tensorProductNumeric intArea delta_20
                        block3 = tensorTranspose 1 (1,2) block2
                        totalBlock1 = tensorAdd block1 $ tensorAdd block2 block3 
                        totalBlockTrans = tensorTranspose 2 (0,1) totalBlock1
                        tens = tensorAdd totalBlock1 totalBlockTrans
                        totalBlock2 = tensorContractWith_3 (1,1) (+) $ tensorProductNumeric invEta antiSym
                        prod = tensorProductNumeric tens totalBlock2

    index2SparseAnsatzAIBC :: Index 3 3 0 0 2 1 0 0 -> (Int,Int) 
    index2SparseAnsatzAIBC  (x1, x2, _, _, x5, x6, _, _) = ((g-1)*21^2*10*10+(e-1)*210*10+(f-1)*10*10+(j-1)*10+k,(c-1)*21^2*10+(b-1)*21*10+(a-1)*10+i)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      f = 1 + (fromEnum $ getValInd x1 1)
                                                      g = 1 + (fromEnum $ getValInd x1 2)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      b = 1 + (fromEnum $ getValInd x2 1)
                                                      c = 1 + (fromEnum $ getValInd x2 2)
                                                      j = 1 +  (fromEnum $ getValInd x5 0)
                                                      k = 1 +  (fromEnum $ getValInd x5 1)
                                                      i = 1 +  (fromEnum $ getValInd x6 0)


    mkEqnSparseAnsatzAIBC :: Tensor 3 3 0 0 2 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzAIBC (Tensor map1) = M.mapKeys index2SparseAnsatzAIBC map1

