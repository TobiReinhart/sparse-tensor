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
intAIB, triangleMap, ansatzAB, index2SparseAnsatzAB, showMatLab, ansatzABSym, index2SparseAnsatzABSym, ansatzAaBb, ansatzAIBC, ansatzAIBJCK, index2SparseAnsatzAIBJCKSym, triangleMap3P
) where
    
    import TensorTreeNumeric2

    import Data.Ratio
    import qualified Data.Map.Strict as M 
    import qualified Data.IntMap.Strict as I 
    import Data.Maybe


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


    showMatLab :: ((Int,Int), Rational) -> String
    showMatLab (_, 0) = error "0 in value"
    showMatLab ((i, j), a) = if denominator a /= 1
                             then error "denominator /= 1"
                             else show i ++ " " ++ show j ++ " " ++ show (numerator a)

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

    ansatzABSym :: M.Map (IndList 2 Lind_3) (IndList 1 Uind_9) -> M.Map (IndList 2 Uind_3) (IndList 1 Lind_9) -> M.Map (IndList 4 Lind_3) (IndList 1 Uind_20) -> M.Map (IndList 4 Uind_3) (IndList 1 Lind_20) -> Tensor 2 2 0 0 1 0 0 0 Rational 
    ansatzABSym map1Metric map2Metric map1Area map2Area = tensorAdd result resultTrans
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
                        result = tensorContr3 (0,0) $ tensorContr3 (0,1) prod
                        resultTrans = tensorTransL20 (0,1) result

    index2SparseAnsatzABSym :: M.Map [Int] Int -> ([Int],Rational) -> Maybe ((Int,Int),Rational)
    index2SparseAnsatzABSym trian ([e,f,a,b,j],v) 
            = case matrixInd of
                        (Just x) -> Just ((e*210+f*10+j+1,1+315+x),v')
                        _ -> Nothing
        where
                                v' = if a == b then v/2 else v 
                                a' = 1+a
                                b' = 1+b
                                matrixInd = (M.lookup) [a', b'] trian

    ansatzAaBb :: M.Map (IndList 2 Lind_3) (IndList 1 Uind_9) -> M.Map (IndList 2 Uind_3) (IndList 1 Lind_9) -> M.Map (IndList 4 Lind_3) (IndList 1 Uind_20) -> M.Map (IndList 4 Uind_3) (IndList 1 Lind_20) -> Tensor 2 2 0 0 1 0 2 2 Rational 
    ansatzAaBb map1Metric map2Metric map1Area map2Area = tensorContr3 (2,0) $ tensorContr3 (0,3) prod
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        int2 = interEqn2 map1Area map2Area  
                        antiSym = aSymI2 map1Metric
                        block1 = tensorProd int2 $ tensorProd delta20 delta3
                        block2 = tensorTransU3 (1,2) $ tensorTransU20 (0,1) block1
                        totalBlock1 = tensorAdd block1 block2
                        totalBlockTrans = tensorTransL3 (1,2) $ tensorTransL20 (0,1) totalBlock1
                        tens = tensorAdd totalBlock1 totalBlockTrans
                        totalBlock2 = tensorContr3 (1,1) $ tensorProd  invEta antiSym
                        prod = tensorProd tens totalBlock2

    ansatzAIBC ::  M.Map (IndList 2 Lind_3) (IndList 1 Uind_9) -> M.Map (IndList 2 Uind_3) (IndList 1 Lind_9) -> M.Map (IndList 4 Lind_3) (IndList 1 Uind_20) -> M.Map (IndList 4 Uind_3) (IndList 1 Lind_20) -> Tensor 3 3 0 0 2 1 0 0 Rational 
    ansatzAIBC map1Metric map2Metric map1Area map2Area = tensorContr3 (0,0) $ tensorContr3 (0,1) prod
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        int3 = interEqn3 map1Metric map2Metric map1Area map2Area
                        antiSym = aSymI2 map1Metric
                        block1 = tensorProd int3 $ tensorProd delta20 delta20 
                        block2 = tensorProd delta20 $ tensorProd delta9 $ tensorProd intArea delta20
                        block3 = tensorTransU20 (1,2) block2
                        totalBlock1 = tensorAdd block1 $ tensorAdd block2 block3 
                        totalBlockTrans = tensorTransL20 (1,2) totalBlock1
                        tens = tensorAdd totalBlock1 totalBlockTrans
                        totalBlock2 = tensorContr3 (1,1) $ tensorProd invEta antiSym
                        prod = tensorProd tens totalBlock2

    ansatzAIBJCK :: M.Map (IndList 2 Lind_3) (IndList 1 Uind_9) -> M.Map (IndList 2 Uind_3) (IndList 1 Lind_9) -> M.Map (IndList 4 Lind_3) (IndList 1 Uind_20) -> M.Map (IndList 4 Uind_3) (IndList 1 Lind_20) -> Tensor 3 3 0 0 4 3 0 0 Rational
    ansatzAIBJCK map1Metric map2Metric map1Area map2Area = totalBlock3
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        int3 = interEqn3 map1Metric map2Metric map1Area map2Area
                        antiSym = aSymI2 map1Metric
                        aSym = tensorContr3 (1,1) $ tensorProd invEta antiSym
                        int3Contr = id $! tensorContr3 (0,0) $! tensorContr3 (0,1) $! tensorProd int3 aSym
                        block1 = tensorProd int3Contr $ tensorProd delta20   $ tensorProd delta20   $ tensorProd delta9 delta9
                        block2 = tensorTransU20 (0,2) $ tensorTransU9 (0,3) block1 
                        block3 = tensorTransU20 (0,1) $ tensorTransU9 (0,2) block1 
                        totalBlock1 = tensorAdd block1   $ tensorAdd block2 block3 
                        totalBlock2 = tensorTransL20 (0,2)   $ tensorTransL9 (0,2) totalBlock1
                        totalBlock3 = tensorTransL20 (0,1)   $ tensorTransL9 (0,1) totalBlock1
                        totalBlock4 = tensorTransL20 (1,2)  $ tensorTransL9 (1,2) totalBlock1
                        totalBlock5 = tensorTransL20 (1,2)  $ tensorTransL9 (1,2) totalBlock3
                        totalBlock6 = tensorTransL20 (0,2)  $ tensorTransL9 (0,2) totalBlock3
                        tens = tensorAdd totalBlock1 $ tensorAdd totalBlock2 $ tensorAdd totalBlock3 $ tensorAdd totalBlock4 $ tensorAdd totalBlock5 totalBlock6

    index2SparseAnsatzAIBJCKSym :: M.Map [Int] Int -> ([Int],Rational) -> Maybe ((Int,Int),Rational)
    index2SparseAnsatzAIBJCKSym trian ([d,c,e,a',c',d',l,s,k,m,i',k',l'],v) 
            = case matrixInd of
                        (Just x) -> Just ((d*21^3*1000+c*21^2*1000+e*21*1000+l*1000+k*100+m*10+s+1,1+315+(div (315*316) 2)+x),v)
                        _ -> Nothing
        where
                                ind1 = 105 + a' * 10 + i' +1
                                ind2 = 105 + c' * 10 + k' +1
                                ind3 = 105 + d' *10 + l' +1 
                                v' x
                                    | ind1 == ind2 && ind1 == ind3 = 1/6 *x
                                    | ind1 == ind2 || ind1 == ind3 || ind2 == ind3 = 1/2 *x
                                    | otherwise = x
                                matrixInd = (M.lookup) [ind1, ind2, ind3] trian




    