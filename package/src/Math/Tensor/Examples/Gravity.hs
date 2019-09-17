-----------------------------------------------------------------------------
-- |
-- Module      :  Math.Tensor.Examples.Gravity
-- Copyright   :  (c) 2019 Tobias Reinhart and Nils Alex
-- License     :  MIT
-- Maintainer  :  tobi.reinhart@fau.de, nils.alex@fau.de
--
--
-- This module provides a variety of @'Tensor'@s that are currently predefined in the sparse-tensor package.
--
-- Amongst many standard tensor from differential geometry and classical field theories such as Kronecker deltas \(\delta^a_b \) in multiple different dimensions,
-- the Levi-Civita symbol \(\epsilon^{abcd} \) and the Minkowski metric \(\eta_{ab}\) and its inverse \(\eta^{ab}\), most included tensors were implemented during
-- the initial use of the sparse-tensor package, the perturbative construction of generalized gravity theories. Thus many of the included tensors stem from this area of research.
--
-- Additionally to providing basic predefined @'Tensor'@s for further computations this module also nicely illustrates how the construction of @'Tensor'@s is achieved.
--
-- The majority of the tensors in this module are defined as type @'ATens'@ which describes a tensor that takes the three different index types
-- @'Ind20'@, @'Ind9'@, @'Ind3'@ each one appearing in contravariant and covariant position. If in the following expression that are formed from such tensors are additionally
-- explained via their algebraic expression using appropriate symbols for the individual tensors we label indices of type @'Ind20'@ by \(A,B,C,D,...\), indices of type
-- \(I,J,K,L,...\) and spacetime indices of type @'ind3'@ are labeled by \(a,b,c,d,...\). Hence a general such tensor is displayed as \(T^{A_1...A_m I_1...I_r a_1...a_p}_{B_1...B_n J_1...J_s b_1...b_s} \).
-- Such a tensor then has the type @'ATens' m n r s p q@.
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Math.Tensor.Examples.Gravity (
-- * Standard Tensors
-- ** Kronecker Delta
delta3, delta9, delta20,
delta3A,
-- ** Minkowski Metric
eta, invEta, etaA, invEtaA, etaAbs,
-- ** Levi-Civita Symbol
epsilon, epsilonInv, epsilonA, epsilonInvA,
-- ** Generators of the Lorentz Group
-- | The following six tensors are a choice of generators of the Lorentz group \( \mathrm{SO}(3,1)\), i.e. they constitute a basis of the
-- corresponding Lie algebra \( \mathrm{so}(3,1)\).
--
-- The Lie algebra \( \mathrm{so}(3,1)\) is isomorphic to the algebra of \(\eta_{ab}\) anti symmetric matrices.
-- Thus the following six tensors \( (K_i)^a_b \) for \( i = 1,...,6 \) all satisfy \( (K_i)^a_{b} \eta_{ca} = - (K_i)^a_{c} \eta_{ba}  \).
--
-- The six generators are obtained by \(2 (K_1)^a_b = \eta_{b0} \delta^ a_{1} - \eta_{b0} \delta^ a_{1} \), and similar for
-- the remaining @5@ independent components of the anti symmetric index pair.
lorentzJ1, lorentzJ2, lorentzJ3, lorentzK1, lorentzK2, lorentzK3,
-- ** Area Metric
flatArea,
-- * Constructive Gravity Specific Tensors
-- ** Intertwiners
-- | The following tensors are used to relate the abstract indices of type @'Ind9'@ to symmetric pairs of spacetime indices of type @'Ind3'@.
interI2, interJ2,
-- | The following tensors are used to relate the abstract indices of type @'Ind20'@ to blocks of @4@ spacetime indices \( (abcd)\) of type @'Ind3'@, that are anti symmetric in
-- \( a \leftrightarrow b \), anti symmetric in \( c \leftrightarrow d \) and further symmetric w.r.t. \( (ab) \leftrightarrow (cd) \).
interIArea, interJArea,
-- ** Infinitesimal Diffeomorphisms
-- | The following two tensors \(C^{Am}_{Bn} \) and \(K^{Im}_{Jn}\) encode the infinitesimal transformation behavior of tensors of type @'ATens' 0 0 0 1 0 0@  and tensors of type
-- @'ATens' 0 1 0 0 0 0@ respectively under spacetime diffeomorphisms. They are related to the Lie derivative via \(\mathscr{L}_{\xi}G_A = \partial_m G_A \cdot \xi^m + C^{Bm}_{An} G_B \cdot \partial_m \xi ^n \).
interArea, interMetric,
-- | __ Further such Tensors__
flatInterMetric, flatInter,
interEqn2, interEqn3, interEqn4, interEqn5,
interEqn2Metric, interEqn3Metric, interEqn4Metric, interEqn5Metric,
-- ** Random Tensor
-- | The following tensors are filled with random components. They can for instance be used to test ranks of tensorial equations.
randArea, randFlatArea, randAreaDerivative1, randAreaDerivative2, randMetric, randAxon,
-- * Unknown Tensors
-- The following tensors have all there individual components filled with different variables using the @'AnsVarR'@ type.
-- Thus they represent general tensors with unknown components.
generic4Ansatz, generic5Ansatz, generic6Ansatz,
generic8Ansatz, generic9Ansatz, generic10_1Ansatz, generic10_2Ansatz, generic11Ansatz, generic12_1Ansatz,
) where

import System.Random.TF.Instances (randomRs)
import System.Random.TF.Init (newTFGen)

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as I

import Data.List (permutations, nub, sort)

import Math.Tensor

liftSTtoATens :: STTens n1 n2 v -> ATens 0 0 0 0 n1 n2 v
liftSTtoATens = Scalar . Scalar . Scalar . Scalar

--start with deltas

-- | Standard spacetime Kronecker delta \(\delta^a_b\) as @'STTens' 1 1 ('SField' 'Rational')@.
--
-- > delta3 = fromListT2 $ zip [(singletonInd (Ind3 i),singletonInd (Ind3 i)) | i <- [0..3]] (repeat $ SField 1)
delta3 :: STTens 1 1 (SField Rational)
delta3 = fromListT2 $ zip [(singletonInd (Ind3 i),singletonInd (Ind3 i)) | i <- [0..3]] (repeat $ SField 1)

-- | Spacetime Kronecker delta as @'ATens'@.
delta3A :: ATens 0 0 0 0 1 1 (SField Rational)
delta3A = liftSTtoATens delta3

-- | Standard Kronecker delta for the @'Ind9'@ index type \(\delta^I_J\) as @'ATens' 0 0 1 1 0 0 ('SField' 'Rational')@.
--
-- > delta9 = fromListT6 $ zip [(Empty, Empty, singletonInd (Ind9 i),singletonInd (Ind9 i), Empty, Empty) | i <- [0..9]] (repeat $ SField 1)
delta9 :: ATens 0 0 1 1 0 0 (SField Rational)
delta9 = fromListT6 $ zip [(Empty, Empty, singletonInd (Ind9 i),singletonInd (Ind9 i), Empty, Empty) | i <- [0..9]] (repeat $ SField 1)

-- | Standard Kronecker delta for the @'Ind20'@ index type \(\delta^A_B\) as @'ATens' 1 1 0 0 0 0 ('SField' 'Rational')@.
--
-- > delta20 = fromListT6 $ zip [(singletonInd (Ind20 i),singletonInd (Ind20 i), Empty, Empty, Empty, Empty) | i <- [0..20]] (repeat $ SField 1)
delta20 :: ATens 1 1 0 0 0 0 (SField Rational)
delta20 = fromListT6 $ zip [(singletonInd (Ind20 i),singletonInd (Ind20 i), Empty, Empty, Empty, Empty) | i <- [0..20]] (repeat $ SField 1)

-- | Spacetime Minkowski metric \(\eta_{ab}\) as @'ATens' 0 0 0 0 0 2 ('SField' 'Rational')@. The Minkowski metric could
-- also be defined as @'STTens' 0 2 ('SField' 'Rational')@ in similar fashion.
--
-- > eta =  fromListT2 map (\(x,y,z) -> ((Empty,Append (Ind3 x) $ Append (Ind3 y) Empty),SField z)) [(0,0,-1),(1,1,1),(2,2,1),(3,3,1)]
eta :: STTens 0 2 (SField Rational)
eta =  fromListT2 l
            where
                l = map (\(x,y,z) -> ((Empty,Append (Ind3 x) $ Append (Ind3 y) Empty),SField z))
                    [(0,0,-1),(1,1,1),(2,2,1),(3,3,1)]

-- | Minkowski metric lifted to @'ATens'@.
etaA :: ATens 0 0 0 0 0 2 (SField Rational)
etaA = liftSTtoATens eta

-- | Inverse spacetime Minkowski metric \(\eta^{ab}\) as @'ATens' 0 0 0 0 2 0 ('SField' 'Rational')@. The inverse Minkowski metric could
-- also be defined as @'STTens' 2 0 ('SField' 'Rational')@ in similar fashion.
--
-- > invEta = fromListT2 $ map (\(x,y,z) -> ((Append (Ind3 x) $ Append (Ind3 y) Empty,Empty),SField z)) [(0,0,-1),(1,1,1),(2,2,1),(3,3,1)]
invEta :: STTens 2 0 (SField Rational)
invEta =  fromListT2 l
            where
                l = map (\(x,y,z) -> ((Append (Ind3 x) $ Append (Ind3 y) Empty,Empty),SField z))
                    [(0,0,-1),(1,1,1),(2,2,1),(3,3,1)]

-- | Inverse Minkowski metric lifted to @'ATens'@.
invEtaA :: ATens 0 0 0 0 2 0 (SField Rational)
invEtaA = liftSTtoATens invEta

-- | The tensor \(\eta_I\) provides an equivalent version of the Minkowski metric that uses an index of type @'Ind9'@ to label the @10@ different values of the symmetric spacetime index pair.
etaAbs :: ATens 0 0 0 1 0 0 (SField Rational)
etaAbs = fromListT6 l
            where
                l = map (\(x,y) -> ((Empty, Empty, Empty, singletonInd $ Ind9 x, Empty, Empty),SField y))
                    [(0,-1),(4,1),(7,1),(9,1)]

-- | Covariant spacetime Levi-Civita symbol \(\epsilon_{abcd}\) as type @'ATTens' 0 4 ('SField' 'Rational')@.
epsilon :: STTens 0 4 (SField Rational)
epsilon = fromListT2 ls
                where
                   ls = map (\([i,j,k,l],v) -> ((Empty, Append (Ind3 i) $ Append (Ind3 j) $ Append (Ind3 k) $ singletonInd (Ind3 l)),SField v)) epsL
                   epsSign i j k l = (-1) ^ length (filter (==True) [j>i,k>i,l>i,k>j,l>j,l>k])
                   epsL = map (\x@[i,j,k,l] -> (x, epsSign i j k l)) $ permutations [0,1,2,3]

-- | Covariant Levi-Civita symbol lifted to @'ATens'@.
epsilonA :: ATens 0 0 0 0 0 4 (SField Rational)
epsilonA = liftSTtoATens epsilon

-- | Contravariant spacetime Levi-Civita symbol \(\epsilon^{abcd}\) as type @'STTens'4 0 ('SField' 'Rational')@. T
epsilonInv :: STTens 4 0 (SField Rational)
epsilonInv = fromListT2 ls
                where
                   ls = map (\([i,j,k,l],v) -> ((Append (Ind3 i) $ Append (Ind3 j) $ Append (Ind3 k) $ singletonInd (Ind3 l), Empty),SField v)) epsL
                   epsSign i j k l = (-1) ^ length (filter (==True) [j>i,k>i,l>i,k>j,l>j,l>k])
                   epsL = map (\x@[i,j,k,l] -> (x, epsSign i j k l)) $ permutations [0,1,2,3]

-- | Contravariant Levi-Civita symbol lifted to @'ATens'@.
epsilonInvA :: ATens 0 0 0 0 4 0 (SField Rational)
epsilonInvA = liftSTtoATens epsilonInv

--generators of the Lorentz group lie algebra (for flat metric eta)

lorentzJ1 :: ATens 0 0 0 0 1 1 (SField Rational)
lorentzJ1 = fromListT6 l
        where
            l = map (\(x,y,z) -> ((Empty,Empty,Empty,Empty,singletonInd $ Ind3 x,singletonInd $ Ind3 y), SField z)) [(3,2,1),(2,3,-1)]

lorentzJ2 :: ATens 0 0 0 0 1 1 (SField Rational)
lorentzJ2 = fromListT6 l
        where
            l = map (\(x,y,z) -> ((Empty,Empty,Empty,Empty,singletonInd $ Ind3 x,singletonInd $ Ind3 y), SField z)) [(3,1,-1),(1,3,1)]

lorentzJ3 :: ATens 0 0 0 0 1 1 (SField Rational)
lorentzJ3 = fromListT6 l
        where
            l = map (\(x,y,z) -> ((Empty,Empty,Empty,Empty,singletonInd $ Ind3 x,singletonInd $ Ind3 y), SField z)) [(2,1,1),(1,2,-1)]

lorentzK1 :: ATens 0 0 0 0 1 1 (SField Rational)
lorentzK1 = fromListT6 l
        where
            l = map (\(x,y,z) -> ((Empty,Empty,Empty,Empty,singletonInd $ Ind3 x,singletonInd $ Ind3 y), SField z)) [(0,1,1),(1,0,1)]

lorentzK2 :: ATens 0 0 0 0 1 1 (SField Rational)
lorentzK2 = fromListT6 l
        where
            l = map (\(x,y,z) -> ((Empty,Empty,Empty,Empty,singletonInd $ Ind3 x,singletonInd $ Ind3 y), SField z)) [(0,2,1),(2,0,1)]

lorentzK3 :: ATens 0 0 0 0 1 1 (SField Rational)
lorentzK3 = fromListT6 l
        where
            l = map (\(x,y,z) -> ((Empty,Empty,Empty,Empty,singletonInd $ Ind3 x,singletonInd $ Ind3 y), SField z)) [(0,3,1),(3,0,1)]

-- Area Metric

-- | Flat area metric tensor. Can be obtained via the @'interJArea'@ intertwiner \( J_A^{abcd}\) as: \( N_A = J_A^{abcd} \left ( \eta_{ac} \eta_{bd} - \eta_{ad} \eta_{bc} - \epsilon_{abcd} \right ) \).
flatArea :: ATens 0 1 0 0 0 0 (SField Rational)
flatArea = fromListT6 $ map (\(i,v) -> ( (Empty, singletonInd $ Ind20 i, Empty, Empty, Empty, Empty), SField v))
                        [(0,-1),(5,-1),(6,-1),(9,1),(11,-1),(12,-1),(15,1),(18,1),(20,1)]


--now the Area metric tensors

trianMap2 :: M.Map (IndList 2 Ind3) (IndList 1 Ind9)
trianMap2 = M.fromList $ zip [ Append (Ind3 a) $ singletonInd $ Ind3 b | a <- [0..3], b <- [a..3] ] $ map (singletonInd . Ind9) [0..]

trianMapArea :: M.Map (IndList 4 Ind3) (IndList 1 Ind20)
trianMapArea = M.fromList $ zip [ Append (Ind3 a) $ Append (Ind3 b) $ Append (Ind3 c) $ singletonInd $ Ind3 d | a <- [0..2], b <- [a+1..3], c <- [a..2], d <- [c+1..3], not $ a == c && b > d ] $ map (singletonInd . Ind20) [0..]

jMult2 :: (Eq a) => IndList 2 a -> Rational
jMult2 (Append a (Append b Empty))
        | a == b = 1
        | otherwise = 1/2

_jMult3 :: (Eq a) => IndList 3 a -> Rational
_jMult3 (Append a (Append b (Append c Empty)))
        | i == 1 = 1
        | i == 2 = 1/3
        | otherwise = 1/6
         where
            i = length $ nub [a,b,c]

jMultArea :: (Eq a) => IndList 4 a -> Rational
jMultArea (Append a (Append b (Append c (Append d Empty))))
            | a == c && b == d = 1/4
            | otherwise = 1/8

_isZeroArea :: (Eq a) => IndList 4 a -> Bool
_isZeroArea (Append a (Append b (Append c (Append d Empty)))) = a == b || c == d

areaSign :: (Eq a, Ord a) => IndList 4 a -> Rational
areaSign (Append a (Append b (Append c (Append d Empty)))) = s1 * s2
             where
                s1 = pairSign a b
                s2 = pairSign c d
                pairSign x y = if x < y then 1 else -1

canonicalizeArea :: (Eq a, Ord a) => IndList 4 a -> (IndList 4 a, Rational)
canonicalizeArea (Append a (Append b (Append c (Append d Empty)))) = (Append a' (Append b' (Append c' (Append d' Empty))),s)
        where
            s = areaSign (Append a (Append b (Append c (Append d Empty))))
            [[a',b'],[c',d']] = sort $ map sort [[a,b],[c,d]]

-- | The tensor \(I^I_{ab} \) maps between covariant @'Ind9'@ indices and symmetric pairs of covariant @'Ind3'@ indices.
interI2 :: ATens 0 0 1 0 0 2 (SField Rational)
interI2 = fromListT6 $ fmap (fmap SField) $ filter (\(_,k) -> k /= 0) $ map (\x -> (x,f x)) inds
        where
            trian2 = trianMap2
            inds = [ (Empty, Empty, singletonInd $ Ind9 a, Empty, Empty, Append (Ind3 b) $ singletonInd $ Ind3 c) | a <- [0..9], b <- [0..3], c <- [0..3]]
            f (_, _, ind1, _, _, ind2)
                | ind1 == (M.!) trian2 (sortInd ind2) = 1
                | otherwise = 0

-- | The tensor \(J_I^{ab} \) maps between covariant @'Ind9'@ indices and pairs of covariant @'Ind3'@ indices.
interJ2 :: ATens 0 0 0 1 2 0 (SField Rational)
interJ2 = fromListT6 $ fmap (fmap SField) $ filter (\(_,k) -> k /= 0) $ map (\x -> (x,f x)) inds
        where
            trian2 = trianMap2
            inds = [ (Empty, Empty, Empty, singletonInd $ Ind9 a, Append (Ind3 b) $ singletonInd $ Ind3 c, Empty) | a <- [0..9], b <- [0..3], c <- [0..3]]
            f (_, _, _, ind1, ind2, _)
                | ind1 == (M.!) trian2 (sortInd ind2) = jMult2 ind2
                | otherwise = 0

-- | The tensor \( I^A_{abcd}\) maps between covariant @'Ind20'@ indices and blocks of @4@ of covariant @'Ind3'@ indices.
interIArea :: ATens 1 0 0 0 0 4  (SField Rational)
interIArea = fromListT6 $ fmap (fmap SField) $ filter (\(_,k) -> k /= 0) $ map (\x -> (x,f x)) inds
        where
            trianArea = trianMapArea
            inds = [ (singletonInd (Ind20 a), Empty, Empty, Empty, Empty, Append (Ind3 b) $ Append (Ind3 c) $ Append (Ind3 d) $ singletonInd $ Ind3 e) | a <- [0..20], b <- [0..3], c <- [0..3], d <- [0..3], e <- [0..3], not (b == c || d == e)]
            f (ind1, _, _, _, _, ind2)
                | ind1 == (M.!) trianArea indArea = s
                | otherwise = 0
                    where
                        (indArea, s) = canonicalizeArea ind2

-- | The tensor \( J_A^{abcd}\) maps between contravariant @'Ind20'@ indices and blocks of @4@ of contravariant @'Ind3'@ indices.
interJArea :: ATens 0 1 0 0 4 0 (SField Rational)
interJArea = fromListT6 $ fmap (fmap SField) $ filter (\(_,k) -> k /= 0) $ map (\x -> (x,f x)) inds
        where
            trianArea = trianMapArea
            inds = [  (Empty, singletonInd $ Ind20 a, Empty, Empty, Append (Ind3 b) $ Append (Ind3 c) $ Append (Ind3 d) $ singletonInd $ Ind3 e, Empty) | a <- [0..20], b <- [0..3], c <- [0..3], d <- [0..3], e <- [0..3], not (b == c || d == e)]
            f (_, ind1, _, _, ind2, _)
                | ind1 == (M.!) trianArea indArea = s * jMultArea indArea
                | otherwise = 0
                    where
                        (indArea, s) = canonicalizeArea ind2

-- |  Can be obtained as: \(C^{Am}_{Bn} = -4 \cdot I^A_{nbcd} J_B^{mbcd}  \)
--
-- > interArea = SField (-4 :: Rational) &. contrATens3 (1,1) (contrATens3 (2,2) $ contrATens3 (3,3) $ interIArea &* interJArea
interArea :: ATens 1 1 0 0 1 1 (SField Rational)
interArea = SField (-4 :: Rational) &. contrATens3 (1,1) (contrATens3 (2,2) $ contrATens3 (3,3) $ interIArea &* interJArea)

-- | Can be obtained as : \(K^{Im}_{Jn} = -2 \cdot I^I_{nb} J_J^{mb}  \)
--
-- > interMetric = SField (-2 :: Rational) &. contrATens3 (0,0) (interI2 &* interJ2)
interMetric :: ATens 0 0 1 1 1 1 (SField Rational)
interMetric = SField (-2 :: Rational) &. contrATens3 (0,0) (interI2 &* interJ2)

-- | Is given by: \( C^m_{Bn} = C^{Am}_{Bn} N_A \)
--
-- > flatInter = contrATens1 (0,1) $ interArea &* flatArea
flatInter :: ATens 0 1 0 0 1 1 (SField Rational)
flatInter = contrATens1 (0,1) $ interArea &* flatArea

-- | Is given by: \( K^m_{Jn} = K^{Im}_{Jn} \eta_I\)
--
-- > flatInterMetric = contrATens2 (0,1) $ interMetric &* etaAbs
flatInterMetric :: ATens 0 0 0 1 1 1 (SField Rational)
flatInterMetric = contrATens2 (0,1) $ interMetric &* etaAbs

-- | Is given by: \(  C_{An}^{Bm} \delta_p^q - \delta_A^B \delta_p^m \delta_n^q \)
interEqn2 :: ATens 1 1 0 0 2 2 (SField Rational)
interEqn2 = int1 &- int2
        where
            int1 = interArea &* delta3A
            int2 = tensorTrans6 (0,1) (delta3A &* delta3A) &* delta20

-- | Is given by: \(  K_{In}^{Jm} \delta_p^q - \delta_I^J \delta_p^m \delta_n^q \)
interEqn2Metric :: ATens 0 0 1 1 2 2 (SField Rational)
interEqn2Metric = int1 &- int2
        where
            int1 = interMetric &* delta3A
            int2 = tensorTrans6 (0,1) (delta3A &* delta3A) &* delta9

-- | Is given by: \(  C_{An}^{Bm} \delta_I^J + \delta_A^B K^{Im}_{Jn}\)
interEqn3 :: ATens 1 1 1 1 1 1 (SField Rational)
interEqn3 = int1 &+ int2
        where
            int1 = interArea &* delta9
            int2 = interMetric &* delta20

-- | Is given by: \(  K_{In}^{Jm} \delta_K^L + \delta_I^J K^{Km}_{Ln}\)
interEqn3Metric :: ATens 0 0 2 2 1 1 (SField Rational)
interEqn3Metric = int1 &+ int2
        where
            int1 = interMetric &* delta9
            int2 = delta9 &* interMetric

-- | Is given by: \( C_{An}^{B(m\vert} 2 J_I^{\vert p) q} - \delta^B_A J_I ^{pm} \delta_n^q  \)
interEqn4 :: ATens 1 1 0 1 3 1 (SField Rational)
interEqn4 = block1 &- block2
        where
            block1' = interJ2 &* interArea
            block1 = block1' &+ tensorTrans5 (1,2) block1'
            block2 = delta20 &* delta3A &* interJ2

-- | Is given by: \( K_{In}^{J(m\vert} 2 J_L^{\vert p) q} - \delta^I_J J_L ^{pm} \delta_n^q  \)
interEqn4Metric :: ATens 0 0 1 2 3 1 (SField Rational)
interEqn4Metric = block1 &- block2
        where
            block1' = interJ2 &* interMetric
            block1 = block1' &+ tensorTrans5 (1,2) block1'
            block2 = delta3A &* interJ2 &* delta9

-- | Is given by: \( C_{An}^{B(m\vert} J_I^{\vert p q )} \)
interEqn5 :: ATens 1 1 0 1 3 1 (SField Rational)
interEqn5 = cyclicSymATens5 [0,1,2] intA1
        where
            intA1 = interJ2 &* interArea

--derivative indices are left metric indices right !!

-- | Is given by: \( K_{In}^{J(m\vert} J_L^{\vert p q )} \)
interEqn5Metric :: ATens 0 0 1 2 3 1 (SField Rational)
interEqn5Metric = cyclicSymATens5 [0,1,2] intA1
        where
            intA1 = interJ2 &* interMetric


--generic Ansätze up to prolongation order 2

--A
-- |
-- prop> tensorRank6' generic4Ansatz = 21
generic4Ansatz :: ATens 1 0 0 0 0 0 (AnsVar (SField Rational))
generic4Ansatz = fromListT6 list
     where
         list = [ let varMap = AnsVar $ I.singleton (dof a) $ SField 1
                  in ((singletonInd (Ind20 $ a-1), Empty, Empty, Empty, Empty, Empty), varMap)
                  | a <- [1..21] ]
         dof a = a

--Aa
-- |
-- prop> tensorRank6' generic5Ansatz = 21*4
generic5Ansatz :: ATens 1 0 0 0 1 0 (AnsVar (SField Rational))
generic5Ansatz = fromListT6 list
      where
         list = [ let varMap = AnsVar $ I.singleton (dof a p) $ SField 1
                  in ((singletonInd (Ind20 $ a-1), Empty, Empty, Empty, singletonInd (Ind3 $ p-1 ), Empty), varMap)
                  | a <- [1..21], p <- [1..4] ]
         dof a p = 1 + 21 + 4*(a-1) + (p-1)

--AI
-- |
-- prop> tensorRank6' generic5Ansatz = 21*10
generic6Ansatz :: ATens 1 0 1 0 0 0 (AnsVar (SField Rational))
generic6Ansatz = fromListT6 list
     where
        list = [ let varMap = AnsVar $ I.singleton (dof a i) $ SField 1
                 in ((singletonInd (Ind20 $ a-1), Empty, singletonInd (Ind9 $ i-1), Empty, Empty, Empty), varMap)
                 | a <- [1..21], i <- [1..10] ]
        dof a i = 1 + 21 + 84 + 10*(a-1) + (i-1)
--AB
-- |
-- prop> tensorRank6' generic8Ansatz = 21*22/2
generic8Ansatz :: ATens 2 0 0 0 0 0 (AnsVar (SField Rational))
generic8Ansatz = fromListT6 list
     where
        list = [ let varMap = AnsVar $ I.singleton (dof a b) $ SField 1
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
-- |
-- prop> tensorRank6' generic21Ansatz = 21*21*4
generic9Ansatz :: ATens 2 0 0 0 1 0 (AnsVar (SField Rational))
generic9Ansatz = fromListT6 list
    where
        list = [ let varMap = AnsVar $ I.singleton (dof a b p) $ SField 1
                in ((Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, Empty, Empty, singletonInd (Ind3 $ p-1), Empty), varMap)
                | a <- [1..21], b <- [1..21], p <- [1..4]]
        dof a b p = trian M.! [a,1 + 21 + 4*(b-1) + (p-1)] + 315
        trian = M.fromList $ zip j k
                where
                    j = [ [a,b] | a <- [1..315], b <- [a..315] ]
                    k = [1..]

--AaBb
-- |
-- prop> tensorRank6' generic5Ansatz = 84*85/2
generic10_1Ansatz :: ATens 2 0 0 0 2 0 (AnsVar (SField Rational))
generic10_1Ansatz = fromListT6 list
    where
        list = [ let varMap = AnsVar $ I.singleton (dof a b p q) $ SField 1
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
-- |
-- prop> tensorRank6' generic5Ansatz = 21*21*10
generic10_2Ansatz :: ATens 2 0 1 0 0 0 (AnsVar (SField Rational))
generic10_2Ansatz = fromListT6 list
     where
         list = [ let varMap = AnsVar $ I.singleton (dof a b i) $ SField 1
                 in ((Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, singletonInd (Ind9 $ i -1), Empty, Empty, Empty), varMap)
                 | a <- [1..21], b <- [1..21], i <- [1..10]]
         dof a b i = trian M.! [a,1 + 105 + 10*(b-1) + (i-1)] + 315
         trian = M.fromList $ zip j k
                 where
                     j = [ [a,b] | a <- [1..315], b <- [a..315] ]
                     k = [1..]

--ApBI
-- |
-- prop> tensorRank6' generic5Ansatz = 21*21*10*4
generic11Ansatz :: ATens 2 0 1 0 1 0 (AnsVar (SField Rational))
generic11Ansatz = fromListT6 list
     where
         list = [ let varMap = AnsVar $ I.singleton (dof a b i p) $ SField 1
                 in ((Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, singletonInd (Ind9 $ i -1), Empty, singletonInd (Ind3 $ p-1), Empty), varMap)
                 | a <- [1..21], b <- [1..21], i <- [1..10], p <- [1..4]]
         dof a b i p = trian M.! [1 + 21 + 4*(a-1) + (p-1),1 + 105 + 10*(b-1) + (i-1)] + 315
         trian = M.fromList $ zip j k
                 where
                     j = [ [a,b] | a <- [1..315], b <- [a..315] ]
                     k = [1..]

--AIBJ
-- |
-- prop> tensorRank6' generic5Ansatz = 210*211/2
generic12_1Ansatz :: ATens 2 0 2 0 0 0 (AnsVar (SField Rational))
generic12_1Ansatz = fromListT6 list
    where
        list = [ let varMap = AnsVar $ I.singleton (dof a b i j) $ SField 1
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

-- Random Tensors

randRats :: IO [Rational]
randRats = do
            gen <- newTFGen
            let randList' = randomRs (-10000,10000) gen :: [Int]
            let randList = map fromIntegral randList'
            return randList

randArea :: IO (ATens 0 1 0 0 0 0 (SField Rational))
randArea = do
              randList <- randRats
              let inds = map (\i -> (Empty, singletonInd $ Ind20 i, Empty, Empty, Empty, Empty)) [0..20]
              return $ fromListT6 $ zip inds $ fmap SField randList

randAxon :: IO (ATens 0 1 0 0 0 0 (SField Rational))
randAxon = do
              randList <- randRats
              let inds = map (\i -> (Empty, singletonInd $ Ind20 i, Empty, Empty, Empty, Empty)) [5,9,12]
              let randInd = SField $ head randList
              let assocs = zip inds [-randInd, randInd, -randInd]
              let tens = fromListT6 assocs
              return tens

randFlatArea :: IO (ATens 0 1 0 0 0 0 (SField Rational))
randFlatArea = do
                  randList <- randRats
                  let assocs = map (\(i,v) -> ( (Empty, singletonInd $ Ind20 i, Empty, Empty, Empty, Empty), SField v))
                         [(0, -1 * head randList),(5, randList !! 3),(6, -1 * randList !! 1),(9, -1 * randList !! 4),(11, -1 * randList !! 2),(12, randList !! 5),(15, head randList),(18, randList !! 1),(20, randList !! 2)]
                  let tens = fromListT6 assocs
                  return tens


randAreaDerivative1 :: IO (ATens 0 1 0 0 0 1 (SField Rational))
randAreaDerivative1 = do
                         randList <- randRats
                         let inds = [ f a p | a <- [0..20], p <- [0..3]]
                         return $ fromListT6 $ zip inds $ fmap SField randList
                 where f a p = (Empty, singletonInd $ Ind20 a, Empty, Empty, Empty, singletonInd $ Ind3 p)


randAreaDerivative2 :: IO (ATens 0 1 0 1 0 0 (SField Rational))
randAreaDerivative2 = do
                         randList <- randRats
                         let inds = [ f a i | a <- [0..20], i <- [0..9]]
                         return $ fromListT6 $ zip inds $ fmap SField randList
                 where f a i = (Empty, singletonInd $ Ind20 a, Empty, singletonInd $ Ind9 i, Empty, Empty)


randMetric :: IO (ATens 0 0 0 1 0 0 (SField Rational))
randMetric = do
                randList <- randRats
                let inds = map (\i -> (Empty, Empty, Empty, singletonInd $ Ind9 i, Empty, Empty)) [0..20]
                return $ fromListT6 $ zip inds $ fmap SField randList
