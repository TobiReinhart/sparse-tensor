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

module Order2Int (
    ansatzAB2, mkEqnSparseAnsatzAB2, intAB1, intAB2, mkEqnSparseIntAB,
    ansatzABb2, mkEqnSparseAnsatzABb2, 
    ansatzAIB2, mkEqnSparseAnsatzAIB2, intAIB, mkEqnSparseIntAIB, intAIBsym, intAIBsymZero, intAIBsymRed, mkEqnSparseIntAIBsym,
    intAI, mkEqnSparseIntAI,
    ansatzAI2, mkEqnSparseAnsatzAI2
    
) where

    import Index
    import Tensor
    import BasicTensors
    import Ivar
    import Pde
    import EquivarianceEqns
    import qualified Data.Map as M 


    --A:B ansatz Eqn

    ansatzAB2 :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 2 2 0 0 1 0 0 0 Rational 
    ansatzAB2 map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) prod
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        antiSym = aSymI_2 map1Metric
                        block1 = tensorProductWith (*) intArea delta_20 
                        block2 = tensorTranspose 1 (0,1) block1
                        totalBlock1 = tensorAdd block1 block2
                        totalBlockTrans = tensorTranspose 2 (0,1) totalBlock1
                        tens = tensorAdd totalBlock1 totalBlockTrans
                        totalBlock2 = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta antiSym
                        prod = tensorProductWith (*) tens totalBlock2

    index2SparseAnsatzAB2 :: Index 2 2 0 0 1 0 0 0 -> (Int,Int) 
    index2SparseAnsatzAB2  (x1, x2, _, _, x5, _, _, _) = ((e-1)*210+(f-1)*10+j,(b-1)*21+a)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      f = 1 + (fromEnum $ getValInd x1 1)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      b = 1 + (fromEnum $ getValInd x2 1)
                                                      j = 1 +  (fromEnum $ getValInd x5 0)

    mkEqnSparseAnsatzAB2 :: Tensor 2 2 0 0 1 0 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzAB2 (Tensor map1) = M.mapKeys index2SparseAnsatzAB2 map1

    intAB1 :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 2 0 0 0 0 2 2 Rational 
    intAB1 map1Area map2Area map1Metric map2Metric = tensorSub totalBlock totalBlockTrans  
                            where
                        intArea = interArea map1Area map2Area
                        flatA = flatArea map2Area
                        block1 = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_20 delta_3
                        block2 = tensorTranspose 1 (0,1) $ tensorProductWith (*) intArea delta_20 
                        block3 = tensorProductWith (*) intArea delta_20 
                        totalBlock1 = tensorAdd block1 $ tensorAdd block2 block3
                        block1Trans = tensorTranspose 2 (0,1) totalBlock1
                        block1Sym = tensorAdd totalBlock1 block1Trans
                        totalBlock2 = tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) intArea flatA
                        totalBlock = tensorContractWith_20 (0,2) (+) $ tensorProductWith (*) block1Sym totalBlock2
                        totalBlockTrans = tensorTranspose 7 (0,1) $ tensorTranspose 8 (0,1) totalBlock

    intAB2 :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 2 0 0 0 0 2 2 Rational 
    intAB2 map1Area map2Area map1Metric map2Metric = tensorSub totalBlock totalBlockTrans  
                            where
                        intArea = interArea map1Area map2Area
                        flatA = flatArea map2Area
                        block1 = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_20 delta_3
                        block2 = tensorTranspose 1 (0,1) $ tensorProductWith (*) intArea delta_20 
                        block3 = tensorProductWith (*) intArea delta_20 
                        totalBlock1 = tensorAdd block1 $ tensorAdd block2 block3
                        block1Trans = tensorTranspose 2 (0,1) totalBlock1
                        block1Sym = tensorAdd totalBlock1 block1Trans
                        totalBlock2 = tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) intArea flatA
                        totalBlock = tensorContractWith_20 (1,2) (+) $ tensorProductWith (*) block1Sym totalBlock2
                        totalBlockTrans = tensorTranspose 7 (0,1) $ tensorTranspose 8 (0,1) totalBlock

    index2SparseIntAB :: Index 1 2 0 0 0 0 2 2 -> (Int,Int) 
    index2SparseIntAB  (x1, x2, _, _, _, _, x7, x8) = ((e-1)*4^4+(m-1)*4^3+(n-1)*4^2+(r-1)*4+s,(b-1)*21+a)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      b = 1 + (fromEnum $ getValInd x2 1)
                                                      m = 1 +  (fromEnum $ getValInd x7 0)
                                                      r = 1 +  (fromEnum $ getValInd x7 1)
                                                      n = 1 +  (fromEnum $ getValInd x8 0)
                                                      s = 1 +  (fromEnum $ getValInd x8 1)

    mkEqnSparseIntAB :: Tensor 1 2 0 0 0 0 2 2 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseIntAB (Tensor map1) = M.mapKeys index2SparseIntAB map1


    --the A:Bb prolongation

    ansatzABb2 :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 2 2 0 0 1 0 1 1 Rational 
    ansatzABb2 map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (1,0) (+) $ tensorContractWith_3 (0,2) (+) prod
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        int2 = interEqn1_2 map1Area map2Area  
                        antiSym = aSymI_2 map1Metric
                        block1 = tensorProductWith (*) intArea $ tensorProductWith (*) delta_20 delta_3
                        block2 = tensorProductWith (*) delta_20 int2
                        totalBlock1 = tensorAdd block1 block2
                        totalBlock2 = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta antiSym
                        prod = tensorProductWith (*) totalBlock1 totalBlock2

    index2SparseAnsatzABb2 :: Index 2 2 0 0 1 0 1 1 -> (Int,Int) 
    index2SparseAnsatzABb2 (x1, x2, _, _, x5, _, x7, x8) = ((e-1)*210*4+(f-1)*40+(j-1)*4+s,(a-1)*21*4+(b-1)*4+r)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      f = 1 + (fromEnum $ getValInd x1 1)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      b = 1 + (fromEnum $ getValInd x2 1)
                                                      j = 1 + (fromEnum $ getValInd x5 0)
                                                      s = 1 + (fromEnum $ getValInd x7 0)
                                                      r = 1 + (fromEnum $ getValInd x8 0)

    mkEqnSparseAnsatzABb2 :: Tensor 2 2 0 0 1 0 1 1 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzABb2 (Tensor map1) = M.mapKeys index2SparseAnsatzABb2 map1

    --the C:DK Equation

    ansatzAIB2 :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 2 2 0 0 2 1 0 0 Rational 
    ansatzAIB2 map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) prod
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        int3 = interEqn1_3 map1Area map2Area map1Metric map2Metric 
                        antiSym = aSymI_2 map1Metric
                        block1 = tensorProductWith (*) intArea $ tensorProductWith (*) delta_20 delta_9
                        block2 = tensorProductWith (*) delta_20  int3
                        totalBlock1 = tensorAdd block1 block2
                        totalBlock2 = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta antiSym
                        prod = tensorProductWith (*) totalBlock1 totalBlock2

    
    index2SparseAnsatzAIB2 :: Index 2 2 0 0 2 1 0 0 -> (Int,Int) 
    index2SparseAnsatzAIB2 (x1, x2, _, _, x5, x6, _, _) = ((e-1)*2100+(f-1)*100+(j-1)*10+k,(a-1)*21*10+(b-1)*10+i)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      f = 1 + (fromEnum $ getValInd x1 1)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      b = 1 + (fromEnum $ getValInd x2 1)
                                                      j = 1 +  (fromEnum $ getValInd x5 0)
                                                      k = 1 +  (fromEnum $ getValInd x5 1)
                                                      i = 1 +  (fromEnum $ getValInd x6 0)

    
    mkEqnSparseAnsatzAIB2 :: Tensor 2 2 0 0 2 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzAIB2 (Tensor map1) = M.mapKeys index2SparseAnsatzAIB2 map1

    
    intAIB :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 2 0 0 1 1 2 2 Rational 
    intAIB map1Area map2Area map1Metric map2Metric = tensorSub tens tensTrans  
            where
                intArea = interArea map1Area map2Area
                intMetric = interMetric map1Metric map2Metric
                flatA = flatArea map2Area
                flatIntA = tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) intArea flatA 
                int3 = interEqn1_3 map1Area map2Area map1Metric map2Metric 
                block1 = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_9 delta_3 
                block2 = tensorProductWith (*) intArea $ tensorProductWith (*) delta_20 delta_9
                block3 = tensorProductWith (*) delta_20 int3 
                totalBlock = tensorAdd block1 $ tensorAdd block2 block3 
                tens = tensorContractWith_20 (0,2) (+) $ tensorProductWith (*) totalBlock flatIntA 
                tensTrans = tensorTranspose 7 (0,1) $ tensorTranspose 8 (0,1) tens 


    index2SparseIntAIB :: Index 1 2 0 0 1 1 2 2 -> (Int,Int) 
    index2SparseIntAIB (x1, x2, _, _, x5, x6, x7, x8) = ((c-1)*10*4^4+(j-1)*4^4+(m-1)*4^3+(n-1)*4^2+(r-1)*4+s,(a-1)*21*10+(b-1)*10+i)
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

    mkEqnSparseIntAIB :: Tensor 1 2 0 0 1 1 2 2 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseIntAIB  (Tensor map1) = M.mapKeys index2SparseIntAIB map1

    --for the rest we need to go one order higher !!!

    --try the AIB intCond with symmetrization in m n 

    intAIBsym :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 2 0 0 3 1 0 0 Rational 
    intAIBsym map1Area map2Area map1Metric map2Metric = tensorSub tens tensTrans  
            where
                intArea = interArea map1Area map2Area
                intMetric = interMetric map1Metric map2Metric
                flatA = flatArea map2Area
                sym = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta $ interI_2 map1Metric
                flatIntA = tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) intArea flatA 
                flatSym = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ tensorProductWith (*) flatIntA sym 
                int3 = interEqn1_3 map1Area map2Area map1Metric map2Metric 
                block1 = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_9 delta_3 
                block2 = tensorProductWith (*) intArea $ tensorProductWith (*) delta_20 delta_9
                block3 = tensorProductWith (*) delta_20 int3 
                totalBlock = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ tensorProductWith (*) (tensorAdd block1 $ tensorAdd block2 block3) sym 
                tens = tensorContractWith_20 (0,2) (+) $ tensorProductWith (*) totalBlock flatSym 
                tensTrans = tensorTranspose 5 (1,2) tens 

    --is equivalent to the non sym version

    intAIBsymZero :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 2 0 0 3 1 0 0 Rational 
    intAIBsymZero map1Area map2Area map1Metric map2Metric = tensorSub tens tensTrans  
            where
                intArea = interArea map1Area map2Area
                intMetric = interMetric map1Metric map2Metric
                flatA = flatArea map2Area
                sym = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta $ interI_2 map1Metric
                flatIntA = tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) intArea flatA 
                flatSym = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ tensorProductWith (*) flatIntA sym 
                int3 = interEqn1_3 map1Area map2Area map1Metric map2Metric 
                block1 = tensorProductWith (*) intArea $ tensorProductWith (*) delta_20 delta_9
                totalBlock = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ tensorProductWith (*) block1 sym 
                tens = tensorContractWith_20 (0,2) (+) $ tensorProductWith (*) totalBlock flatSym 
                tensTrans = tensorTranspose 5 (1,2) tens 

    --really yields zero 

    intAIBsymRed :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 2 0 0 3 1 0 0 Rational 
    intAIBsymRed map1Area map2Area map1Metric map2Metric = tensorSub tens tensTrans  
            where
                intArea = interArea map1Area map2Area
                intMetric = interMetric map1Metric map2Metric
                flatA = flatArea map2Area
                sym = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta $ interI_2 map1Metric
                flatIntA = tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) intArea flatA 
                flatSym = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ tensorProductWith (*) flatIntA sym 
                int3 = interEqn1_3 map1Area map2Area map1Metric map2Metric 
                block1 = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_9 delta_3 
                block3 = tensorProductWith (*) delta_20 int3 
                totalBlock = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ tensorProductWith (*) (tensorAdd block1 block3) sym 
                tens = tensorContractWith_20 (0,2) (+) $ tensorProductWith (*) totalBlock flatSym 
                tensTrans = tensorTranspose 5 (1,2) tens 


    index2SparseIntAIBsym :: Index 1 2 0 0 3 1 0 0 -> (Int,Int) 
    index2SparseIntAIBsym (x1, x2, _, _, x5, x6, _, _) = ((c-1)*1000+(j-1)*100+(k-1)*10+l,(a-1)*21*10+(b-1)*10+i)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0) 
                             b = 1 + (fromEnum $ getValInd x2 1) 
                             c = 1 + (fromEnum $ getValInd x1 0)
                             i = 1 + (fromEnum $ getValInd x6 0)
                             j = 1 + (fromEnum $ getValInd x5 0)
                             k = 1 + (fromEnum $ getValInd x5 1)
                             l = 1 + (fromEnum $ getValInd x5 2)
                             

    mkEqnSparseIntAIBsym :: Tensor 1 2 0 0 3 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseIntAIBsym  (Tensor map1) = M.mapKeys index2SparseIntAIBsym map1

    --one last try computing th eback propagated int cond directly

    intAI :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 1 0 0 3 1 0 0 Rational 
    intAI map1Area map2Area map1Metric map2Metric = tensorSub tens tensTrans 
            where
                intArea = interArea map1Area map2Area
                flatA = flatArea map2Area
                sym = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta $ interI_2 map1Metric
                int3 = interEqn1_3 map1Area map2Area map1Metric map2Metric 
                block1 = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ tensorProductWith (*) int3 sym 
                tens = tensorContractWith_9 (0,1) (+) $ tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) block1 block1 
                tensTrans = tensorTranspose 5 (0,2) $ tens

    index2SparseIntAI :: Index 1 1 0 0 3 1 0 0 -> (Int,Int) 
    index2SparseIntAI (x1, x2, _, _, x5, x6, _, _) = ((c-1)*1000+(j-1)*100+(k-1)*10+l,(a-1)*10+i)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0) 
                             c = 1 + (fromEnum $ getValInd x1 0)
                             i = 1 + (fromEnum $ getValInd x6 0)
                             j = 1 + (fromEnum $ getValInd x5 0)
                             k = 1 + (fromEnum $ getValInd x5 1)
                             l = 1 + (fromEnum $ getValInd x5 2)

    mkEqnSparseIntAI :: Tensor 1 1 0 0 3 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseIntAI  (Tensor map1) = M.mapKeys index2SparseIntAI map1

    --the AI Ansatz Eqn

    ansatzAI2 ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 1 0 0 2 1 0 0 Rational 
    ansatzAI2 map1Area map2Area map1Metric map2Metric = tens
            where
                intArea = interArea map1Area map2Area
                flatA = flatArea map2Area
                aSym = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta $ aSymI_2 map1Metric
                int3 = interEqn1_3 map1Area map2Area map1Metric map2Metric 
                block1 = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_9 delta_3
                block2 = int3 
                tens = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) $ tensorProductWith (*) (tensorAdd block1 block2) aSym  

    index2SparseAnsatzAI :: Index 1 1 0 0 2 1 0 0 -> (Int,Int) 
    index2SparseAnsatzAI (x1, x2, _, _, x5, x6, _, _) = ((c-1)*100+(j-1)*10+k,(a-1)*10+i)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0) 
                             c = 1 + (fromEnum $ getValInd x1 0)
                             i = 1 + (fromEnum $ getValInd x6 0)
                             j = 1 + (fromEnum $ getValInd x5 0)
                             k = 1 + (fromEnum $ getValInd x5 1)

    mkEqnSparseAnsatzAI2 :: Tensor 1 1 0 0 2 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzAI2  (Tensor map1) = M.mapKeys index2SparseAnsatzAI map1

    --int Cond does not yield extra condition!!