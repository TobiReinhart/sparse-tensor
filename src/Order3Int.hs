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
                        block1 = tensorProductWith (*) intArea $ tensorProductWith (*) delta_20 delta_20 
                        block2 = tensorTranspose 1 (0,2) block1
                        block3 = tensorTranspose 1 (0,1) block2 
                        totalBlock1 = tensorAdd block1 $ tensorAdd block2 block3 
                        totalBlockTrans1 = tensorTranspose 2 (0,1) totalBlock1
                        totalBlockTrans2 = tensorTranspose 2 (0,2) totalBlock1
                        totalBlockTrans3 = tensorTranspose 2 (1,2) totalBlock1
                        totalBlockTrans4 = tensorTranspose 2 (0,2) totalBlockTrans1
                        totalBlockTrans5 = tensorTranspose 2 (1,2) totalBlockTrans1
                        tens = tensorAdd totalBlock1 $ tensorAdd totalBlockTrans1 $ tensorAdd totalBlockTrans2 $ tensorAdd totalBlockTrans3 $ tensorAdd totalBlockTrans4 totalBlockTrans5
                        totalBlock2 = tensorContractWith_3 (1,1) (+) $ tensorProductWith (*) invEta antiSym
                        prod = tensorProductWith (*) tens totalBlock2

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

    mkEqnSparseAnsatzABC :: Tensor 3 3 0 0 1 0 0 0 Rational -> M.Map (Int,Int) Rational
    mkEqnSparseAnsatzABC (Tensor map1) = M.mapKeys index2SparseAnsatzABC map1
