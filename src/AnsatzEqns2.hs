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

module AnsatzEqns2 (
    ansatzAB, ansatzABb, ansatzAIB, ansatzAaBI, ansatzAaBb, ansatzAIBJ,
    mkEqnSparseAnsatzAB, mkEqnSparseAnsatzABb, mkEqnSparseAnsatzAIB, mkEqnSparseAnsatzAaBI, mkEqnSparseAnsatzAaBb, mkEqnSparseAnsatzAIBJ,
    intCond2NoSym, mkEqnSparseintCond2NoSym, removeAIB, mkEqnSparseRemoveAIB, intCondfirstOrder, mkEqnSparsefirstOrder
   
) where

    import Index
    import Tensor
    import BasicTensors
    import Ivar
    import Pde
    import EquivarianceEqns
    import qualified Data.Map as M 

    --AI:B Ansatz Eqn

    ansatzAIB :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 2 2 0 0 2 1 0 0 Rational 
    ansatzAIB map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) prod
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

    
    index2SparseAnsatzAIB :: M.Map [Int] Int -> Index 2 2 0 0 2 1 0 0 -> (Int,Int) 
    index2SparseAnsatzAIB trian (x1, x2, _, _, x5, x6, _, _) = ((e-1)*2100+(f-1)*100+(j-1)*10+k,316 + x)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      f = 1 + (fromEnum $ getValInd x1 1)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      b = 1 + (fromEnum $ getValInd x2 1)
                                                      j = 1 +  (fromEnum $ getValInd x5 0)
                                                      k = 1 +  (fromEnum $ getValInd x5 1)
                                                      i = 1 +  (fromEnum $ getValInd x6 0)
                                                      x = (M.!) trian [a,105+(b-1)*10+i]

    

    mkEqnSparseAnsatzAIB :: M.Map [Int] Int -> Tensor 2 2 0 0 2 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzAIB trian (Tensor map1) = M.mapKeys (index2SparseAnsatzAIB trian) map1

    --A:B ansatz Eqn

    ansatzAB :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 2 2 0 0 1 0 0 0 Rational 
    ansatzAB map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (0,1) (+) prod
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

    index2SparseAnsatzAB :: M.Map [Int] Int -> Index 2 2 0 0 1 0 0 0 -> (Int,Int) 
    index2SparseAnsatzAB trian (x1, x2, _, _, x5, _, _, _) = ((e-1)*210+(f-1)*10+j,316 + x)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      f = 1 + (fromEnum $ getValInd x1 1)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      b = 1 + (fromEnum $ getValInd x2 1)
                                                      j = 1 +  (fromEnum $ getValInd x5 0)
                                                      x = (M.!) trian [min a b, max a b]

    mkEqnSparseAnsatzAB :: M.Map [Int] Int -> Tensor 2 2 0 0 1 0 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzAB trian (Tensor map1) = M.mapKeys (index2SparseAnsatzAB trian) map1

    --A:Bb Ansatz Equation

    ansatzABb :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 2 2 0 0 1 0 1 1 Rational 
    ansatzABb map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (1,0) (+) $ tensorContractWith_3 (0,2) (+) prod
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

    index2SparseAnsatzABb :: M.Map [Int] Int -> Index 2 2 0 0 1 0 1 1 -> (Int,Int) 
    index2SparseAnsatzABb trian (x1, x2, _, _, x5, _, x7, x8) = ((e-1)*210*4+(f-1)*40+(j-1)*4+s,316 + x)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      f = 1 + (fromEnum $ getValInd x1 1)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      b = 1 + (fromEnum $ getValInd x2 1)
                                                      j = 1 + (fromEnum $ getValInd x5 0)
                                                      s = 1 + (fromEnum $ getValInd x7 0)
                                                      r = 1 + (fromEnum $ getValInd x8 0)
                                                      x = (M.!) trian [a,21+(b-1)*4+r]

    mkEqnSparseAnsatzABb :: M.Map [Int] Int -> Tensor 2 2 0 0 1 0 1 1 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzABb trian (Tensor map1) = M.mapKeys (index2SparseAnsatzABb trian) map1


    --the Aa:Bb ansatz Equation

    ansatzAaBb :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 2 2 0 0 1 0 2 2 Rational 
    ansatzAaBb map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (2,0) (+) $ tensorContractWith_3 (0,3) (+) prod
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        int2 = interEqn1_2 map1Area map2Area  
                        antiSym = aSymI_2 map1Metric
                        block1 = tensorProductWith (*) int2 $ tensorProductWith (*) delta_20 delta_3
                        block2 = tensorTranspose 7 (1,2) $ tensorTranspose 1 (0,1) block1
                        totalBlock1 = tensorAdd block1 block2
                        totalBlockTrans = tensorTranspose 8 (1,2) $ tensorTranspose 2 (0,1) totalBlock1
                        tens = tensorAdd totalBlock1 totalBlockTrans
                        totalBlock2 = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta antiSym
                        prod = tensorProductWith (*) tens totalBlock2

    index2SparseAnsatzAaBb :: M.Map [Int] Int -> Index 2 2 0 0 1 0 2 2 -> (Int,Int) 
    index2SparseAnsatzAaBb trian (x1, x2, _, _, x5, _, x7, x8) = ((e-1)*210*16+(f-1)*160+(j-1)*4+(r-1)*4+s,316 + x)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      f = 1 + (fromEnum $ getValInd x1 1)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      b = 1 + (fromEnum $ getValInd x2 1)
                                                      j = 1 + (fromEnum $ getValInd x5 0)
                                                      r = 1 + (fromEnum $ getValInd x7 0)
                                                      s = 1 + (fromEnum $ getValInd x7 1)
                                                      p = 1 + (fromEnum $ getValInd x8 0)
                                                      q = 1 + (fromEnum $ getValInd x8 1)
                                                      x = (M.!) trian [21 + (min ((a-1)*4+p) ((b-1)*4+q)),21 + (max ((a-1)*4+p) ((b-1)*4+q))]

    mkEqnSparseAnsatzAaBb :: M.Map [Int] Int -> Tensor 2 2 0 0 1 0 2 2 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzAaBb trian (Tensor map1) = M.mapKeys (index2SparseAnsatzAaBb trian) map1

                                                      
    --the Aa:BI Ansatz Equation

    ansatzAaBI :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 2 2 0 0 2 1 1 1 Rational 
    ansatzAaBI map1Area map2Area map1Metric map2Metric = tensorContractWith_3 (1,0) (+) $ tensorContractWith_3 (0,2) (+) prod
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        int2 = interEqn1_2 map1Area map2Area 
                        int3 = interEqn1_3 map1Area map2Area map1Metric map2Metric 
                        antiSym = aSymI_2 map1Metric
                        block1 = tensorProductWith (*) int2 $ tensorProductWith (*) delta_20 delta_9
                        block2 = tensorProductWith (*) delta_20 $ tensorProductWith (*) int3 delta_3
                        totalBlock1 = tensorAdd block1 block2
                        totalBlock2 = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta antiSym
                        prod = tensorProductWith (*) totalBlock1 totalBlock2

    index2SparseAnsatzAaBI :: M.Map [Int] Int -> Index 2 2 0 0 2 1 1 1 -> (Int,Int) 
    index2SparseAnsatzAaBI trian (x1, x2, _, _, x5, x6, x7, x8) = ((e-1)*21*400+(f-1)*400+(j-1)*40+(i-1)*4+r,316 + x)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      f = 1 + (fromEnum $ getValInd x1 1)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      b = 1 + (fromEnum $ getValInd x2 1)
                                                      j = 1 + (fromEnum $ getValInd x5 0)
                                                      i = 1 + (fromEnum $ getValInd x5 1)
                                                      r = 1 + (fromEnum $ getValInd x7 0)
                                                      p = 1 + (fromEnum $ getValInd x8 0)
                                                      k = 1 + (fromEnum $ getValInd x6 0)
                                                      x = (M.!) trian [21+(a-1)*4+p,105+(b-1)*10+k]

    mkEqnSparseAnsatzAaBI :: M.Map [Int] Int -> Tensor 2 2 0 0 2 1 1 1 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzAaBI trian (Tensor map1) = M.mapKeys (index2SparseAnsatzAaBI trian) map1

    --the AI:BJ Ansatz Equation

    ansatzAIBJ :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 2 2 0 0 3 2 0 0 Rational 
    ansatzAIBJ map1Area map2Area map1Metric map2Metric = tens
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        int3 = interEqn1_3 map1Area map2Area map1Metric map2Metric
                        antiSym = aSymI_2 map1Metric
                        totalBlock2 = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta antiSym
                        block1 = tensorProductWith (*) int3 $ tensorProductWith (*) delta_20 delta_9
                        block1Contr = tensorContractWith_3 (0,0) (+) $! tensorContractWith_3 (0,1) (+) $! tensorProductWith (*) block1 totalBlock2
                        block1Trans = tensorTranspose 5 (0,1) $ tensorTranspose 1 (0,1) $! block1Contr
                        totalBlock = tensorAdd block1Contr block1Trans
                        totalBlockTrans = tensorTranspose 6 (0,1) $ tensorTranspose 2 (0,1) $! totalBlock
                        tens = tensorAdd totalBlock totalBlockTrans

    index2SparseAnsatzAIBJ :: M.Map [Int] Int -> Index 2 2 0 0 3 2 0 0 -> (Int,Int) 
    index2SparseAnsatzAIBJ trian (x1, x2, _, _, x5, x6, _, _) = ((e-1)*21000+(f-1)*1000+(r-1)*100+(s-1)*10+t,316 + x)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      f = 1 + (fromEnum $ getValInd x1 1)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      b = 1 + (fromEnum $ getValInd x2 1)
                                                      i = 1 + (fromEnum $ getValInd x6 0)
                                                      j = 1 + (fromEnum $ getValInd x6 1)
                                                      r = 1 + (fromEnum $ getValInd x5 0)
                                                      s = 1 + (fromEnum $ getValInd x5 1)
                                                      t = 1 + (fromEnum $ getValInd x5 2)
                                                      x = (M.!) trian [105 + (min ((a-1)*10+i) ((b-1)*10+j)),105 +  (max ((a-1)*10+i) ((b-1)*10+j))]

    mkEqnSparseAnsatzAIBJ :: M.Map [Int] Int -> Tensor 2 2 0 0 3 2 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzAIBJ trian (Tensor map1) = M.mapKeys (index2SparseAnsatzAIBJ trian) map1   
    
    --int condition

    intCond2NoSym :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 1 2 0 0 1 1 2 2 Rational 
    intCond2NoSym map1Area map2Area map1Metric map2Metric = aSym 
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

    index2SparseintCond2NoSym :: M.Map [Int] Int -> Index 1 2 0 0 1 1 2 2 -> (Int,Int) 
    index2SparseintCond2NoSym trian (x1, x2, _, _, x5, x6, x7, x8) = ((c-1)*10*4^4+(j-1)*4^4+(m-1)*4^3+(n-1)*4^2+(r-1)*4+s,x)
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
                             x = (M.!) trian [b,105+(a-1)*10+i]

    mkEqnSparseintCond2NoSym :: M.Map [Int] Int -> Tensor 1 2 0 0 1 1 2 2 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseintCond2NoSym trian (Tensor map1) = M.mapKeys (index2SparseintCond2NoSym trian) map1

    --eqns that remove the A:BI Ansatz

    removeAIB :: Tensor 2 2 0 0 1 1 0 0 Rational
    removeAIB = tensorProductWith (*) delta_20 $ tensorProductWith (*) delta_20 delta_9
                
    index2SparseRemoveAIB :: M.Map [Int] Int -> Index 2 2 0 0 1 1 0 0 -> (Int,Int) 
    index2SparseRemoveAIB trian (x1, x2, _, _, x5, x6, _, _) = ((e-1)*210+(f-1)*10+j,316 + x)
                                                  where 
                                                      e = 1 + (fromEnum $ getValInd x1 0)
                                                      f = 1 + (fromEnum $ getValInd x1 1)
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      b = 1 + (fromEnum $ getValInd x2 1)
                                                      j = 1 +  (fromEnum $ getValInd x5 0)
                                                      i = 1 +  (fromEnum $ getValInd x6 0)
                                                      x = (M.!) trian [a,105+(b-1)*10+i]

    mkEqnSparseRemoveAIB :: M.Map [Int] Int -> Tensor 2 2 0 0 1 1 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseRemoveAIB trian (Tensor map1) = M.mapKeys (index2SparseRemoveAIB trian) map1

    --sym int Cond for the first prolongation order

    intCondfirstOrder :: M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20 -> M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9 -> Tensor 0 1 0 0 0 0 2 2 Rational 
    intCondfirstOrder map1Area map2Area map1Metric map2Metric = tensorSub total totalTrans 
                where
                    intArea = interArea map1Area map2Area
                    flatA = flatArea map2Area
                    block1 = tensorAdd (tensorProductWith (*) delta_20 delta_3) (intArea)
                    block2 = tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) intArea flatA
                    total = tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) block1 block2
                    totalTrans = tensorTranspose 7 (0,1) $ tensorTranspose 8 (0,1) total

    index2SparsefirstOrder :: Index 0 1 0 0 0 0 2 2 -> (Int,Int) 
    index2SparsefirstOrder (_, x2, _, _, _, _, x7, x8) = ((m-1)*16*4+(r-1)*16+(n-1)*4+s,a)
                                                  where 
                                                      a = 1 + (fromEnum $ getValInd x2 0)
                                                      m = 1 +  (fromEnum $ getValInd x7 0)
                                                      r = 1 +  (fromEnum $ getValInd x7 1)
                                                      n = 1 +  (fromEnum $ getValInd x8 0)
                                                      s = 1 +  (fromEnum $ getValInd x8 1)

    mkEqnSparsefirstOrder :: Tensor 0 1 0 0 0 0 2 2 Rational -> M.Map (Int,Int) Rational
    mkEqnSparsefirstOrder (Tensor map1) = M.mapKeys index2SparsefirstOrder map1


