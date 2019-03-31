{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE FunctionalDependencies #-}



{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver   #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

{-# OPTIONS_GHC -dcore-lint #-}

{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}


module BasicTensor4_2 (

) where 

    import Data.Foldable
    import Data.Ratio
    import Data.List 
    import Control.Applicative
    import Data.Maybe
    import qualified Data.Map.Strict as M
    import qualified Data.IntMap.Strict as I
    import Numeric.Natural
    import GHC.TypeLits
    import Data.Proxy
    import GHC.TypeLits.Normalise
    import GHC.Generics
    import Control.DeepSeq

    import qualified Numeric.LinearAlgebra.Data as HMat
    import qualified Numeric.LinearAlgebra as HLin 
 

    import Data.Serialize

    import Data.Type.Equality

    import Data.Singletons
    import Data.Singletons.Decide
    import Data.Singletons.Prelude.Enum
    import Data.Singletons.TypeLits

    import qualified Data.Eigen.Matrix as Mat 
    import qualified Data.Eigen.SparseMatrix as Sparse
    import qualified Data.Eigen.LA as Sol 

    import Unsafe.Coerce (unsafeCoerce)

    import TensorTreeNumeric4_2 

    --start with deltas 

    delta20 :: ATens 1 1 0 0 0 0 Rational 
    delta20 = fromListT6 $ zip [(singletonInd (Ind20 i),singletonInd (Ind20 i), Empty, Empty, Empty, Empty) | i <- [0..20]] (repeat 1)

    delta9 :: ATens 0 0 1 1 0 0 Rational
    delta9 = fromListT6 $ zip [(Empty, Empty, singletonInd (Ind9 i),singletonInd (Ind9 i), Empty, Empty) | i <- [0..9]] (repeat 1)

    delta3 :: ATens 0 0 0 0 1 1 Rational
    delta3 = fromListT6 $ zip [(Empty, Empty, Empty, Empty, singletonInd (Ind3 i),singletonInd (Ind3 i)) | i <- [0..3]] (repeat 1)

    trianMap2 :: M.Map (IndList 2 Ind3) (IndList 1 Ind9) 
    trianMap2 = M.fromList $ zip [ Append (Ind3 a) $ singletonInd $ Ind3 b | a <- [0..3], b <- [a..3] ] $ map (singletonInd . Ind9) [0..]

    trianMapArea :: M.Map (IndList 4 Ind3) (IndList 1 Ind20)
    trianMapArea = M.fromList $ zip [ Append (Ind3 a) $ Append (Ind3 b) $ Append (Ind3 c) $ singletonInd $ Ind3 d | a <- [0..2], b <- [a+1..3], c <- [a..2], d <- [c+1..3], not $ a == c && b > d ] $ map (singletonInd . Ind20) [0..]
   
    jMult2 :: (Eq a) => IndList 2 a -> Rational 
    jMult2 (Append a (Append b Empty))
            | a == b = 1
            | otherwise = 1/2

    jMult3 :: (Eq a) => IndList 3 a -> Rational
    jMult3 (Append a (Append b (Append c Empty)))
            | i == 1 = 1
            | i == 2 = 1/3
            | otherwise = 1/6
             where 
                i = length $ nub [a,b,c]

    jMultArea :: (Eq a) => IndList 4 a -> Rational
    jMultArea (Append a (Append b (Append c (Append d Empty))))
                | a == c && b == d = 1/4 
                | otherwise = 1/8

    isZeroArea :: (Eq a) => IndList 4 a -> Bool 
    isZeroArea (Append a (Append b (Append c (Append d Empty)))) = a == b || c == d 

    areaSign :: (Eq a, Ord a) => IndList 4 a -> Rational 
    areaSign (Append a (Append b (Append c (Append d Empty)))) = s1 * s2
                 where
                    s1 = pairSign a b
                    s2 = pairSign c d
                    pairSign x y = if x < y then 1 else -1

    canonicalizeArea :: (Eq a, Ord a) => IndList 4 a -> (IndList 4 a, Rational)
    canonicalizeArea (Append a (Append b (Append c (Append d Empty)))) = ((Append a' (Append b' (Append c' (Append d' Empty)))),s)
            where
                s = areaSign (Append a (Append b (Append c (Append d Empty))))
                [[a',b'],[c',d']] = sort $ map sort [[a,b],[c,d]]

    interI2 :: ATens 0 0 1 0 0 2 Rational
    interI2 = fromListT6 $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                trian2 = trianMap2 
                inds = [ (Empty, Empty, (singletonInd $ Ind9 a), Empty, Empty, (Append (Ind3 b) $ singletonInd $ Ind3 c)) | a <- [0..9], b <- [0..3], c <- [0..3]]
                f (_, _, ind1, _, _, ind2)
                    | ind1 == ((M.!) trian2 $ sortInd ind2 ) = 1 
                    | otherwise = 0 

    interJ2 :: ATens 0 0 0 1 2 0 Rational
    interJ2 = fromListT6 $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                trian2 = trianMap2
                inds = [ (Empty, Empty, Empty, (singletonInd $ Ind9 a), (Append (Ind3 b) $ singletonInd $ Ind3 c), Empty) | a <- [0..9], b <- [0..3], c <- [0..3]]
                f (_, _, _, ind1, ind2, _)
                    | ind1 == ((M.!) trian2 $ sortInd ind2 ) = jMult2 ind2  
                    | otherwise = 0 

    interIArea :: ATens 1 0 0 0 0 4  Rational
    interIArea = fromListT6 $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                trianArea = trianMapArea
                inds = [ ((singletonInd $ Ind20 a), Empty, Empty, Empty, Empty, (Append (Ind3 b) $ Append (Ind3 c) $ Append (Ind3 d) $ singletonInd $ Ind3 e)) | a <- [0..20], b <- [0..3], c <- [0..3], d <- [0..3], e <- [0..3], not (b == c || d == e)]
                f (ind1, _, _, _, _, ind2)
                    | ind1 == ((M.!) trianArea indArea) = s
                    | otherwise = 0
                        where
                            (indArea, s) = canonicalizeArea ind2 

    interJArea :: ATens 0 1 0 0 4 0 Rational
    interJArea = fromListT6 $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                trianArea = trianMapArea
                inds = [  (Empty, (singletonInd $ Ind20 a), Empty, Empty, (Append (Ind3 b) $ Append (Ind3 c) $ Append (Ind3 d) $ singletonInd $ Ind3 e), Empty) | a <- [0..20], b <- [0..3], c <- [0..3], d <- [0..3], e <- [0..3], not (b == c || d == e)]
                f (_, ind1, _, _, ind2, _)
                    | ind1 == ((M.!) trianArea indArea) = s * (jMultArea indArea)
                    | otherwise = 0
                        where
                            (indArea, s) = canonicalizeArea ind2 

    interMetric :: ATens 0 0 1 1 1 1 Rational
    interMetric = (-2) &. (contrATens3 (0,0) $ interI2 &* interJ2 )

    interArea :: ATens 1 1 0 0 1 1 Rational
    interArea = (-4) &. (contrATens3 (1,1) $ contrATens3 (2,2) $ contrATens3 (3,3) $ interIArea &* interJArea)

    interEqn2 :: ATens 1 1 0 0 2 2 Rational
    interEqn2 = int1 &- int2
            where
                int1 = interArea &* delta3
                int2 = (tensorTrans6 (0,1) $ delta3 &* delta3 ) &* delta20

    interEqn3 :: ATens 1 1 1 1 1 1 Rational
    interEqn3 = int1 &+ int2 
            where
                int1 = interArea &* delta9
                int2 = interMetric &* delta20

    interEqn4 :: ATens 1 1 0 1 3 1 Rational
    interEqn4 = block1 &- block2 
            where
                block1' = interJ2 &* interArea
                block1 = block1' &+ (tensorTrans5 (1,2) block1') 
                block2 = delta20 &* delta3 &* interJ2


