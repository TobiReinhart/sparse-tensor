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


module BasicTensors4_2 (
    flatInter, interArea, interEqn5, flatArea, interEqn3, interEqn4, interEqn2, invEta,
    genericArea, genericAreaDerivative1, genericAreaDerivative2, generic4Ansatz

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

    import Data.Serialize
    import Unsafe.Coerce (unsafeCoerce)

    import Data.Type.Equality
    import Data.Singletons
    import Data.Singletons.Decide
    import Data.Singletons.Prelude.Enum
    import Data.Singletons.TypeLits

    import qualified Data.Eigen.Matrix as Mat 
    import qualified Data.Eigen.SparseMatrix as Sparse
    import qualified Data.Eigen.LA as Sol 


    import TensorTreeNumeric4_2 

    --start with deltas 

    delta20 :: ATens 1 1 0 0 0 0 Rational 
    delta20 = fromListT6 $ zip [(singletonInd (Ind20 i),singletonInd (Ind20 i), Empty, Empty, Empty, Empty) | i <- [0..20]] (repeat 1)

    delta9 :: ATens 0 0 1 1 0 0 Rational
    delta9 = fromListT6 $ zip [(Empty, Empty, singletonInd (Ind9 i),singletonInd (Ind9 i), Empty, Empty) | i <- [0..9]] (repeat 1)

    delta3 :: ATens 0 0 0 0 1 1 Rational
    delta3 = fromListT6 $ zip [(Empty, Empty, Empty, Empty, singletonInd (Ind3 i),singletonInd (Ind3 i)) | i <- [0..3]] (repeat 1)

    --eta and inverse eta (numerical)

    eta :: ATens 0 0 0 0 0 2 Rational
    eta =  fromListT6 l 
                where
                    l = map (\(x,y,z) -> ((Empty,Empty,Empty,Empty,Empty,Append (Ind3 x) $ Append (Ind3 y) Empty),z)) [(0,0,-1),(1,1,1),(2,2,1),(3,3,1)]

    invEta :: ATens 0 0 0 0 2 0 Rational 
    invEta =  fromListT6 l 
                where
                    l = map (\(x,y,z) -> ((Empty,Empty,Empty,Empty,Append (Ind3 x) $ Append (Ind3 y) Empty,Empty),z)) [(0,0,-1),(1,1,1),(2,2,1),(3,3,1)]

    --epsilon and inverse epsilon (numerical)

    epsilon :: ATens 0 0 0 0 0 4 Rational 
    epsilon = fromListT6 $ map (\([i,j,k,l],v) -> ((Empty, Empty, Empty, Empty, Empty, Append (Ind3 i) $ Append (Ind3 j) $ Append (Ind3 k) $ singletonInd (Ind3 l)),v)) epsL 
                    where
                       epsSign [i,j,k,l] = (-1)^(length $  filter (==True) [j>i,k>i,l>i,k>j,l>j,l>k])
                       epsL = map (\x -> (x, epsSign x)) $ permutations [0,1,2,3]

    epsilonInv :: ATens 0 0 0 0 4 0 Rational 
    epsilonInv = fromListT6 $ map (\([i,j,k,l],v) -> ((Empty, Empty, Empty, Empty, Append (Ind3 i) $ Append (Ind3 j) $ Append (Ind3 k) $ singletonInd (Ind3 l), Empty),v)) epsL 
                    where
                       epsSign [i,j,k,l] = (-1)^(length $  filter (==True) [j>i,k>i,l>i,k>j,l>j,l>k])
                       epsL = map (\x -> (x, epsSign x)) $ permutations [0,1,2,3]

    --now the earea metric tesnors 

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

    interEqn5 :: ATens 1 1 0 1 3 1 Rational 
    interEqn5 = cyclicSymATens5 [0,1,2] intA1 
            where
                intA1 = interJ2 &* interArea 

   
    --generic Ansätze up to prolongation order 2

    --A
    generic4Ansatz :: ATens 1 0 0 0 0 0 AnsVar
    generic4Ansatz = fromListT6 list 
         where 
             list = [ let varMap = I.singleton (dof a) 1
                      in ((singletonInd (Ind20 $ a-1), Empty, Empty, Empty, Empty, Empty), varMap)
                      | a <- [1..21] ]
             dof a = a

    --Aa
    generic5Ansatz :: ATens 1 0 0 0 1 0 AnsVar
    generic5Ansatz = fromListT6 list
          where 
             list = [ let varMap = I.singleton (dof a p) 1
                      in ((singletonInd (Ind20 $ a-1), Empty, Empty, Empty, singletonInd (Ind3 $ p-1 ), Empty), varMap)
                      | a <- [1..21], p <- [1..4] ]
             dof a p = 1 + 21 + 4*(a-1) + (p-1)

    --AI
    generic6Ansatz :: ATens 1 0 1 0 0 0 AnsVar
    generic6Ansatz = fromListT6 list
         where 
            list = [ let varMap = I.singleton (dof a i) 1
                     in ((singletonInd (Ind20 $ a-1), Empty, singletonInd (Ind9 $ i-1), Empty, Empty, Empty), varMap)
                     | a <- [1..21], i <- [1..10] ]
            dof a i = 1 + 21 + 84 + 10*(a-1) + (i-1)
    --AB
    generic8Ansatz :: ATens 2 0 0 0 0 0 AnsVar
    generic8Ansatz = fromListT6 list
         where 
            list = [ let varMap = I.singleton (dof a b) 1
                     in ((Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, Empty, Empty, Empty, Empty), varMap)
                     | a <- [1..21], b <- [1..21] ]
            dof a b = let a' = min a b
                          b' = max a b
                      in trian M.! [a',b'] + 315
            trian = M.fromList $ zip j k
                      where
                          j = [ [a,b] | a <- [1..315], b <- [a..315] ]
                          k = [1..]

    --ABb 
    generic9Ansatz :: ATens 2 0 0 0 1 0 AnsVar 
    generic9Ansatz = fromListT6 list
        where
            list = [ let varMap = I.singleton (dof a b p) 1
                    in ((Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, Empty, Empty, singletonInd (Ind3 $ p-1), Empty), varMap)
                    | a <- [1..21], b <- [1..21], p <- [1..4]]
            dof a b p = trian M.! [a,1 + 21 + 4*(b-1) + (p-1)] + 315
            trian = M.fromList $ zip j k
                    where
                        j = [ [a,b] | a <- [1..315], b <- [a..315] ]
                        k = [1..]

    --AaBb 
    generic10_1Ansatz :: ATens 2 0 0 0 2 0 AnsVar 
    generic10_1Ansatz = fromListT6 list
        where
            list = [ let varMap = I.singleton (dof a b p q) 1
                    in ((Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, Empty, Empty, Append (Ind3 $ p-1) $ singletonInd (Ind3 $ q-1), Empty), varMap)
                    | a <- [1..21], b <- [1..21], p <- [1..4], q <- [1..4]]
            dof a b p q = let 
                    a' = min (1 + 21 + 4*(a-1) + (p-1)) (1 + 21 + 4*(b-1) + (q-1)) 
                    b' = max (1 + 21 + 4*(a-1) + (p-1)) (1 + 21 + 4*(b-1) + (q-1)) 
                    in trian M.! [a',b'] + 315
            trian = M.fromList $ zip j k
                    where
                        j = [ [a,b] | a <- [1..315], b <- [a..315] ]
                        k = [1..]

    --ABI 
    generic10_2Ansatz :: ATens 2 0 1 0 0 0 AnsVar 
    generic10_2Ansatz = fromListT6 list
         where
             list = [ let varMap = I.singleton (dof a b i) 1
                     in ((Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, singletonInd (Ind9 $ i -1), Empty, Empty, Empty), varMap)
                     | a <- [1..21], b <- [1..21], i <- [1..10]]
             dof a b i = trian M.! [a,1 + 105 + 10*(b-1) + (i-1)] + 315
             trian = M.fromList $ zip j k
                     where
                         j = [ [a,b] | a <- [1..315], b <- [a..315] ]
                         k = [1..]
 
    --ApBI 
    generic11Ansatz :: ATens 2 0 1 0 1 0 AnsVar 
    generic11Ansatz = fromListT6 list
         where
             list = [ let varMap = I.singleton (dof a b i p) 1
                     in ((Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, singletonInd (Ind9 $ i -1), Empty, singletonInd (Ind3 $ p-1), Empty), varMap)
                     | a <- [1..21], b <- [1..21], i <- [1..10], p <- [1..4]]
             dof a b i p = trian M.! [1 + 21 + 4*(a-1) + (p-1),1 + 105 + 10*(b-1) + (i-1)] + 315
             trian = M.fromList $ zip j k
                     where
                         j = [ [a,b] | a <- [1..315], b <- [a..315] ]
                         k = [1..]

    --AIBJ 
    generic12Ansatz :: ATens 2 0 2 0 0 0 AnsVar 
    generic12Ansatz = fromListT6 list
        where
            list = [ let varMap = I.singleton (dof a b i j) 1
                    in ((Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, Append (Ind9 $ i-1) $ singletonInd (Ind9 $ j -1), Empty, Empty, Empty), varMap)
                    | a <- [1..21], b <- [1..21], i <- [1..10], j <- [1..10]]
            dof a b i j = let 
                    a' =  min (1 + 105 + 10*(a-1) + (i-1)) (1 + 105 + 10*(b-1) + (j-1))
                    b' =  max (1 + 105 + 10*(a-1) + (i-1)) (1 + 105 + 10*(b-1) + (j-1))
                    in trian M.! [a',b'] + 315
            trian = M.fromList $ zip j k
                    where
                        j = [ [a,b] | a <- [1..315], b <- [a..315] ]
                        k = [1..]

    flatArea :: ATens 0 1 0 0 0 0 Rational 
    flatArea = fromListT6 $ map (\(i,v) -> ( (Empty, (singletonInd $ Ind20 i), Empty, Empty, Empty, Empty), v)) [(0,-1),(5,-1),(6,-1),(9,1),(11,-1),(12,-1),(15,1),(18,1),(20,1)]

    flatInter :: ATens 0 1 0 0 1 1 Rational 
    flatInter = contrATens1 (0,1) $ interArea &* flatArea

    genericArea :: ATens 0 1 0 0 0 0 (AreaVar Rational) 
    genericArea = fromListT6 assocs 
            where 
                dofs = map (\x -> AreaVar 0 $ I.singleton x 1) [1..21] 
                inds = map (\i -> (Empty, (singletonInd $ Ind20 i), Empty, Empty, Empty, Empty)) [0..20]
                assocs = zip inds dofs
 
    genericAreaFlat :: ATens 0 1 0 0 0 0 (AreaVar Rational)
    genericAreaFlat = fromListT6 $
                      map (\(i,v) -> ( (Empty, (singletonInd $ Ind20 i), Empty, Empty, Empty, Empty), v))
                                    [(0, AreaVar 0 $ I.singleton 1 (-1)),(5, AreaVar 0 $ I.singleton 4 1),(6, AreaVar 0 $ I.singleton 2 (-1)),(9, AreaVar 0 $ I.singleton 5 (-1)),(11, AreaVar 0 $ I.singleton 3 (-1)),(12, AreaVar 0 $ I.singleton 6 1),(15, AreaVar 0 $ I.singleton 1 1),(18, AreaVar 0 $ I.singleton 2 1),(20, AreaVar 0 $ I.singleton 3 1)]

    genericAreaDerivative1 :: ATens 0 1 0 0 0 1 (AreaVar Rational)
    genericAreaDerivative1 = fromListT6 assocs
                where 
                    dofs = map (\x -> AreaVar 0 $ I.singleton x 1) [22..105]
                    inds = map (\(a,p) -> (Empty, (singletonInd $ Ind20 a), Empty, Empty, Empty, (singletonInd $ Ind3 p))) $ [ (a,p) | a <- [0..20], p <- [0..3]]
                    assocs = zip inds dofs 

    genericAreaDerivative2 :: ATens 0 1 0 1 0 0 (AreaVar Rational)
    genericAreaDerivative2 = fromListT6 assocs
                where 
                    dofs = map (\x -> AreaVar 0 $ I.singleton x 1) [106..315]
                    inds = map (\(a,i) -> (Empty, (singletonInd $ Ind20 a), Empty, (singletonInd $ Ind9 i), Empty, Empty)) $ [ (a,i) | a <- [0..20], i <- [0..9]]
                    assocs = zip inds dofs 


    

    