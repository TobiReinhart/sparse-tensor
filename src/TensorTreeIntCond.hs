--pushes type stuff to kind stuff (prefixed with ')
{-# LANGUAGE DataKinds #-}
--matching on type constructors
{-# LANGUAGE GADTs #-}
--kind signature
{-# LANGUAGE KindSignatures #-}
--type family definitions
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
--infix type plus and mult
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}

module TensorTreeIntCond (
intAIB, triangleMap, ansatzAB, index2SparseAnsatzAB, showMatLab
) where
    
    import TensorTreeNumeric2

    import Data.Ratio
    import qualified Data.Map.Strict as M 

    intAIB :: M.Map (IndList 2 Lind_3) (IndList 1 Uind_9) -> M.Map (IndList 2 Uind_3) (IndList 1 Lind_9) -> M.Map (IndList 4 Lind_3) (IndList 1 Uind_20) -> M.Map (IndList 4 Uind_3) (IndList 1 Lind_20) -> Tensor 1 2 0 0 1 1 2 2 Rational
    intAIB map1Metric map2Metric map1Area map2Area = tensorSub tens tensTrans  
            where
                intArea = interArea map1Area map2Area
                intMetric = interMetric map1Metric map2Metric
                flatIntA = flatInter map1Area map2Area 
                int3 = interEqn3 map1Metric map2Metric map1Area map2Area
                block1 = tensorProd delta20 $ tensorProd delta20 $ tensorProd delta9 delta3 
                block2 = tensorProd intArea $ tensorProd delta20 delta9
                block3 = tensorProd delta20 int3 
                totalBlock = tensorAdd block1 $ tensorAdd block2 block3 
                tens = tensorContr20 (0,2) $ tensorProd totalBlock flatIntA 
                tensTrans = tensorTransU3 (0,1) $ tensorTransL3 (0,1) tens 

    triangleMap2P :: Int -> M.Map [Int] Int 
    triangleMap2P i = M.fromList $ zip j k
                    where
                        j = [ [a,b] | a <- [1..i], b <- [a..i] ]
                        k = [1..]

    triangleMap3P :: Int -> M.Map [Int] Int
    triangleMap3P i = M.fromList $ zip j k
                    where
                        j = [ [a,b,c] | a <- [1..i], b <- [a..i], c <- [b..i] ]
                        k = [1..]

    triangleMap :: Int -> M.Map [Int] Int
    triangleMap i = M.union (triangleMap2P i) (triangleMap3P i)

    ansatzAB :: M.Map (IndList 2 Lind_3) (IndList 1 Uind_9) -> M.Map (IndList 2 Uind_3) (IndList 1 Lind_9) -> M.Map (IndList 4 Lind_3) (IndList 1 Uind_20) -> M.Map (IndList 4 Uind_3) (IndList 1 Lind_20) -> Tensor 2 2 0 0 1 0 0 0 Rational 
    ansatzAB map1Metric map2Metric map1Area map2Area = tensorContr3 (0,0) $ tensorContr3 (0,1) prod
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        antiSym = aSymI2 map1Metric
                        block1 = tensorProd intArea delta20 
                        block2 = tensorTransU20 (0,1) block1
                        totalBlock1 = tensorAdd block1 block2
                        totalBlockTrans = tensorTransL20 (0,1) totalBlock1
                        tens = tensorAdd totalBlock1 totalBlockTrans
                        totalBlock2 = tensorContr3 (1,1) $ tensorProd invEta antiSym
                        prod = tensorProd tens totalBlock2

    index2SparseAnsatzAB :: M.Map [Int] Int -> ([Int],a) -> ((Int,Int),a)
    index2SparseAnsatzAB trian ([e,f,a,b,j],v) = ((e*210+f*10+j+1,1+315+x),v)
                    where
                        a' = 1+a
                        b' = 1+b
                        x = (M.!) trian [min a' b', max a' b']

    showMatLab :: ((Int,Int), Rational) -> String
    showMatLab (_, 0) = error "0 in value"
    showMatLab ((i, j), a) = if denominator a /= 1
                             then error "denominator /= 1"
                             else show i ++ " " ++ show j ++ " " ++ show (numerator a)


    