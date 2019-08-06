--Implementation of efficient sparse tensor algebra in Haskell

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}

module Math.Tensor (
fromList', singletonInd, sortInd,
Ind1(..), Ind2(..), Ind3(..), Ind9(..), Ind20(..),
STTens, IndTupleST, ATens, IndTupleAbs, IndList(..),
(&+), (&*), (&-), (&.),
Tensor(..), AbsTensor1, AbsTensor2, AbsTensor3, AbsTensor4, AbsTensor5, AbsTensor6, AbsTensor7, AbsTensor8,
TScalar, TAlgebra,
removeZeros1, removeZeros2, removeZeros3, removeZeros4, removeZeros5, removeZeros6, removeZeros7, removeZeros8,
tensorTrans1, tensorTrans2, tensorTrans3, tensorTrans4, tensorTrans5, tensorTrans6, tensorTrans7, tensorTrans8,
tensorBlockTrans1, tensorBlockTrans2, tensorBlockTrans3, tensorBlockTrans4, tensorBlockTrans5, tensorBlockTrans6, tensorBlockTrans7, tensorBlockTrans8,
resortTens1, resortTens2, resortTens3, resortTens4, resortTens5, resortTens6, resortTens7, resortTens8,
evalTens1, evalTens2, evalTens3, evalTens4, evalTens5, evalTens6, evalTens7, evalTens8,
contrATens1, contrATens2, contrATens3, contrATens4,
symATens1, symATens2, symATens3, symATens4, symATens5, symATens6, symATens7, symATens8,
symATensFac1, symATensFac2, symATensFac3, symATensFac4, symATensFac5, symATensFac6, symATensFac7, symATensFac8,
aSymATens1, aSymATens2, aSymATens3, aSymATens4, aSymATens5, aSymATens6, aSymATens7, aSymATens8,
aSymATensFac1, aSymATensFac2, aSymATensFac3, aSymATensFac4, aSymATensFac5, aSymATensFac6, aSymATensFac7, aSymATensFac8,
symBlockATens1, symBlockATens2, symBlockATens3, symBlockATens4, symBlockATens5, symBlockATens6, symBlockATens7, symBlockATens8,
symBlockATensFac1, symBlockATensFac2, symBlockATensFac3, symBlockATensFac4, symBlockATensFac5, symBlockATensFac6, symBlockATensFac7, symBlockATensFac8,
aSymBlockATens1, aSymBlockATens2, aSymBlockATens3, aSymBlockATens4, aSymBlockATens5, aSymBlockATens6, aSymBlockATens7, aSymBlockATens8,
aSymBlockATensFac1, aSymBlockATensFac2, aSymBlockATensFac3, aSymBlockATensFac4, aSymBlockATensFac5, aSymBlockATensFac6, aSymBlockATensFac7, aSymBlockATensFac8,
cyclicSymATens1, cyclicSymATens2, cyclicSymATens3, cyclicSymATens4, cyclicSymATens5, cyclicSymATens6, cyclicSymATens7, cyclicSymATens8,
cyclicSymATensFac1, cyclicSymATensFac2, cyclicSymATensFac3, cyclicSymATensFac4, cyclicSymATensFac5, cyclicSymATensFac6, cyclicSymATensFac7, cyclicSymATensFac8,
cyclicASymATens1, cyclicASymATens2, cyclicASymATens3, cyclicASymATens4, cyclicASymATens5, cyclicASymATens6, cyclicASymATens7, cyclicASymATens8,
cyclicASymATensFac1, cyclicASymATensFac2, cyclicASymATensFac3, cyclicASymATensFac4, cyclicASymATensFac5, cyclicASymATensFac6, cyclicASymATensFac7, cyclicASymATensFac8,
cyclicBlockSymATens1, cyclicBlockSymATens2, cyclicBlockSymATens3, cyclicBlockSymATens4, cyclicBlockSymATens5, cyclicBlockSymATens6, cyclicBlockSymATens7, cyclicBlockSymATens8,
cyclicBlockSymATensFac1, cyclicBlockSymATensFac2, cyclicBlockSymATensFac3, cyclicBlockSymATensFac4, cyclicBlockSymATensFac5, cyclicBlockSymATensFac6, cyclicBlockSymATensFac7, cyclicBlockSymATensFac8,
fromListT1, fromListT2, fromListT3, fromListT4, fromListT5, fromListT6, fromListT7, fromListT8,
fromListT1', fromListT2', fromListT3', fromListT4', fromListT5', fromListT6', fromListT7', fromListT8',
toListShow1, toListShow2, toListShow3, toListShow4, toListShow5, toListShow6, toListShow7, toListShow8,
toListShowVarPretty1, toListShowVarPretty2, toListShowVarPretty3, toListShowVarPretty4, toListShowVarPretty5, toListShowVarPretty6, toListShowVarPretty7, toListShowVarPretty8,
AnsVar(..), showAnsVar,
shiftLabels1, shiftLabels2, shiftLabels3, shiftLabels4, shiftLabels5, shiftLabels6, shiftLabels7, shiftLabels8,
toListShowVar1, toListShowVar2, toListShowVar3, toListShowVar4, toListShowVar5, toListShowVar6, toListShowVar7, toListShowVar8,
TensList1(..), TensList2(..), TensList3(..), TensList4(..), TensList5(..), TensList6(..), TensList7(..), TensList8(..),
singletonTList1, singletonTList2, singletonTList3, singletonTList4, singletonTList5, singletonTList6, singletonTList7, singletonTList8,
(...>), (..&>), (.&.>), (.&&>), (&..>), (&.&>), (&&.>), (&&&>), (...+), (..&+), (.&.+), (.&&+), (&..+), (&.&+), (&&.+), (&&&+),
toEMatrix1, toEMatrix2, toEMatrix3, toEMatrix4, toEMatrix5, toEMatrix6, toEMatrix7, toEMatrix8,
toMatList1, toMatList2, toMatList3, toMatList4, toMatList5, toMatList6, toMatList7, toMatList8,
tensorRank1', tensorRank2', tensorRank3', tensorRank4', tensorRank5', tensorRank6', tensorRank7', tensorRank8',
tensorRank1, tensorRank2, tensorRank3, tensorRank4, tensorRank5, tensorRank6, tensorRank7, tensorRank8
) where

import Data.Foldable (toList)
import Data.Ratio ((%), numerator, denominator)
import Data.List (nubBy, sortOn, intersect)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.IntMap.Strict as I
import qualified Data.Map.Strict as M
import Numeric.Natural (Natural(..))
import GHC.TypeLits
import GHC.Generics (Generic(..))
import Control.DeepSeq (rnf, NFData(..))
import Data.Serialize (encodeLazy, decodeLazy, Serialize(..))
import Data.Singletons (SingI(..), Proxy(..))
import Data.Singletons.Decide
import Data.Singletons.Prelude.Enum
import Data.Singletons.TypeLits
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.ByteString.Lazy as BS (ByteString(..))
import Codec.Compression.GZip (compress, decompress)
import Data.Either (either)

--for Linear Algebra subroutines

import qualified Data.Eigen.SparseMatrix as Sparse
import qualified Data.Eigen.LA as Sol

--ranks of tensors are encoded on type level -> indices are encoded as length typed lists

data IndList n a where
    Empty :: IndList 0 a
    Append :: a -> IndList (n-1) a -> IndList n a

infixr 5 +>

(+>) :: (Enum a) => Int -> IndList (n-1) a -> IndList n a
(+>) i = Append (toEnum i)

singletonInd :: a -> IndList 1 a
singletonInd x = Append x Empty

data IsZero (n :: Nat) where
    Zero :: (0 ~ n)     => IsZero n
    NonZero :: (1 <= n) => IsZero n
deriving instance Show (IsZero n)

isZero :: forall (n :: Nat). SNat n -> IsZero n
isZero n = case n %~ SNat @0
             of Proved Refl -> Zero
                Disproved _ -> unsafeCoerce (NonZero @1)

--construction from untyped lists

fromList :: forall (n :: Nat). SNat n -> forall (a :: *). [a] -> Maybe (IndList n a)
fromList n xs = case isZero n
                  of Zero    -> case xs
                                  of [] -> Just Empty
                                     _  -> Nothing
                     NonZero -> case xs
                                  of []    -> Nothing
                                     x:xs' -> case fromList (sPred n) xs'
                                                of Just v  -> Just (x `Append` v)
                                                   Nothing -> Nothing

fromList' :: forall (n :: Nat). SingI n => forall (a :: *). [a] -> IndList n a
fromList' = \case
               Just v  -> v
               Nothing -> undefined
            . fromList sing

--instances

instance (KnownNat n, Generic a) => Generic (IndList n a) where
    type Rep (IndList n a) = Rep [a]

    to r = fromList' $ to r
    from = from . toList

deriving instance Generic Int
deriving instance (KnownNat n, Generic a, Serialize a) => Serialize (IndList n a)

instance (NFData a) => NFData (IndList n a) where
    rnf Empty = ()
    rnf (Append a i) = rnf a `seq` rnf i

deriving instance (Eq a) => Eq (IndList n a)

deriving instance (Ord a) => Ord (IndList n a)

deriving instance (Show a) => Show (IndList n a)

instance Functor (IndList n) where
    fmap f Empty = Empty
    fmap f (Append x xs) = Append (f x) (fmap f xs)

instance Foldable (IndList n) where
    foldr f y Empty = y
    foldr f y (Append x xs) = f x (foldr f y xs)

--list operations on length typed lists

insertSorted :: (Ord a, Eq a) => a -> IndList n a -> IndList (n+1) a
insertSorted y Empty = Append y Empty
insertSorted y (Append x xs)
    | y <= x = Append y $ Append x xs
    | otherwise = Append x $ insertSorted y xs

sortInd :: (Ord a, Eq a) => IndList n a -> IndList n a
sortInd Empty = Empty
sortInd (Append x xs) = insertSorted x $ sortInd xs

toListInd :: IndList n a -> [a]
toListInd = toList

combineInds :: IndList n a -> IndList m a -> IndList (n+m) a
combineInds Empty l = l
combineInds (Append x xs) l = Append x $ combineInds xs l

headInd :: IndList n a -> a
headInd (Append x xs) = x

tailInd :: IndList n a -> IndList (n-1) a
tailInd (Append x xs) = xs

indexInd :: Int -> IndList n a -> a
indexInd 0 (Append x xs) = x
indexInd i (Append x xs) = indexInd (i-1) xs

updateInd :: Int -> a -> IndList n a -> IndList n a
updateInd 0 s (Append x xs) = Append s xs
updateInd i s (Append x xs) = Append x $ updateInd (i-1) s xs

swapHead :: Int -> IndList n b -> IndList n b
swapHead 1 (Append x xs) = Append (headInd xs) $ Append x (tailInd xs)
swapHead i (Append x xs) = Append val newL
    where
        val = indexInd (i-1) xs
        newL = updateInd (i-1) x xs

removeContractionInd :: (Eq a) => Int -> a -> (IndList n a, c) -> Maybe (IndList (n-1) a,c)
removeContractionInd 0 ind1 (Append x xs, t)
        | ind1 == x = Just (xs,t)
        | otherwise = Nothing
removeContractionInd i ind1 (Append x xs,t) = (\(m,n) -> (Append x m, n)) <$> removeContractionInd (i-1) ind1 (xs,t)

{--
resort inds in IndList accroding to the permutation given by [Int], length of [Int] must be n

example: perm = [1, 2, 0] -> "C is put on 0th pos. A on 1st and B on 2nd"
         indList = [A, B, C]
         algorithm sorts [(1,A), (2,B), (0,C)] on firsts
         => [(0,C), (1,A), (2,B)]
         => [C, A, B]
--}

resortInd :: (SingI n, Ord a) => [Int] -> IndList n a -> IndList n a
resortInd perm indList = newindList
    where
        l' = toList indList
        l'' = if length l' == length perm then zip perm l' else error "permutation has wrong length"
        lReSorted = sortOn fst l''
        newindList = fromList' $ map snd lReSorted

{--
Values of a given tensor should satisfy numberlike properties -> more precisely should constitute an algebra (scaling, addition and multiplication).
It is important to note that only the vector space that tensors of given rank constitute closes on the type level, the product of two tensors with
given rank yields a third tensors with new rank that is hence represented by a different type.
Thus we need the values of tensors to allow for vector space operations
--}

class (Eq a) => TScalar a where
    addS :: a -> a -> a
    subS :: a -> a -> a
    scaleS :: Rational -> a -> a
    scaleZero :: a

--type level function that determines the new type of the product of two tensors with given type

class TAlgebra v v' where
    type TAlg v v' :: *
    prodA :: v -> v' -> TAlg v v'

--Index class

class (Eq a, Ord a, Enum a) => TIndex a where
    indRange :: a -> Int

--a tensor is a sorted list of pairs: (Index,SubTensor). Sorted lists allow for fast insertion and lookup.

type TMap k v = [(k,v)]

--check if the list is sorted

isValidTMap :: (Ord k, Eq v) => TMap k v -> Bool
isValidTMap l = l == sortOn fst l

--insert a new key value pair in the list, if already present the function (v -> v -> v) determines the new value at the ind

insertWithTMap :: (Ord k) => (v -> v -> v) -> k -> v -> TMap k v -> TMap k v
insertWithTMap f key val [] = [(key,val)]
insertWithTMap f key val ((k1,v1):xs)
        | key < k1 = (key,val) : ((k1,v1):xs)
        | key == k1 = (k1,f val v1) : xs
        | otherwise = (k1,v1) : insertWithTMap f key val xs

--combine two sorted lists with combiner function

addTMaps :: (Ord k) => (v -> v -> v) -> TMap k v -> TMap k v -> TMap k v
addTMaps f m1 [] = m1
addTMaps f [] m2 = m2
addTMaps f ((k1,v1):xs) ((k2,v2):ys)
        | k1 < k2 = (k1,v1) : addTMaps f xs ((k2,v2):ys)
        | k2 < k1 = (k2,v2) : addTMaps f ((k1,v1):xs) ys
        | k1 == k2 = (k1, f v1 v2) : addTMaps f xs ys

mapTMap :: (v -> v') -> TMap k v -> TMap k v'
mapTMap f = map (\(k,v) -> (k,f v))

filterTMap :: (v -> Bool) -> TMap k v -> TMap k v
filterTMap f = filter (\(_,v) -> f v)

--finally the tensor data type for a single (contravariant) tensor with n indices is either either identical 0, simply given by a scalar for n=0,
--or a list of pairs (Index,SubTensor)

data Tensor n k v where
    Scalar :: v -> Tensor 0 k v
    Tensor :: TMap k (Tensor n k v) -> Tensor (n+1) k v
    ZeroTensor :: Tensor n k v

--remove zero values in a given Tensor that might be generated during computations

removeZeros :: (TScalar v, TIndex k) => Tensor n k v -> Tensor n k v
removeZeros (Scalar x) = if x == scaleZero then ZeroTensor else Scalar x
removeZeros (Tensor m) = let newMap = filterTMap (/=ZeroTensor) $ mapTMap removeZeros m in if null newMap then ZeroTensor else Tensor newMap
removeZeros ZeroTensor = ZeroTensor

--for converting tensors to bytestrings for saving and loading from file we need a non typesafe data type as intermediate type

data TensorRep k v = ScalarR v | TensorR Natural (TMap k (TensorRep k v)) | ZeroR Natural deriving (Show, Generic, Serialize)

--convert betweeen typesafe and non typesafe tensors

lemma :: forall n m. (n-1 :~: m) -> (m+1 :~: n)
lemma _ = unsafeCoerce (Refl @n)

toRep :: forall n k v. KnownNat n => Tensor n k v -> TensorRep k v
toRep (Scalar v) = ScalarR v
toRep (Tensor m) = -- case isZero (SNat @n)  n == 0 is not possible, because types
                   -- of Zero -> undefined
                   -- NonZero ->
                        case lemma @n Refl
                        of Refl ->
                            let r = fromIntegral $ GHC.TypeLits.natVal (Proxy @n)
                            in TensorR r $ mapTMap (\(t :: Tensor (n-1) k v) -> toRep t) m
toRep ZeroTensor = let r = fromIntegral $ GHC.TypeLits.natVal (Proxy @n)
                in ZeroR r

fromRep :: forall n k v. KnownNat n => TensorRep k v -> Tensor n k v
fromRep (ScalarR v) = case isZero (SNat @n)
                        of Zero    -> Scalar v
                           NonZero -> undefined
fromRep (TensorR r m) = case someNatVal (fromIntegral r)
                        of Just l  -> case l
                                        of SomeNat (_ :: Proxy x) -> case isZero (SNat @x)
                                                                        of NonZero -> case sameNat (Proxy @x) (Proxy @n)
                                                                                        of Nothing   -> undefined
                                                                                           Just Refl -> Tensor (mapTMap (\t -> fromRep t :: Tensor (x-1) k v) m)
                                                                           Zero    -> undefined
                           Nothing -> undefined
fromRep (ZeroR r) = case someNatVal (fromIntegral r)
                    of Just l  -> ZeroTensor
                       Nothing -> undefined

--instances for the tensor data type

instance (NFData k, NFData v) => NFData (Tensor n k v) where
    rnf ZeroTensor = ()
    rnf (Scalar v) = v `seq` rnf v
    rnf (Tensor m) = m `seq` rnf m

instance KnownNat n => Generic (Tensor n k v) where
    type Rep (Tensor n k v) = Rep (TensorRep k v)
    from = from . toRep
    to   = fromRep . to

deriving instance (KnownNat n, Ord k, Serialize k, Serialize v) => Serialize (Tensor n k v)

instance Functor (Tensor n k) where
    fmap f (Scalar x) = Scalar (f x)
    fmap f (Tensor m) = Tensor (mapTMap (fmap f) m)
    fmap f ZeroTensor = ZeroTensor

deriving instance (Show a, Show k) => Show (Tensor n k a)

deriving instance (Eq a, Eq k) => Eq (Tensor n k a)

--in order to construct higher ranked tensors we need the possibility to build tensors with lower order tensors as scalars
--values of given tensor should be instance of both TScalar and TAlgebra to allow for TensorAlgebra and TensorProducts

instance (TIndex k, TScalar v) => TScalar (Tensor n k v) where
    addS = (&+)
    subS = (&-)
    scaleS = (&.)
    scaleZero = ZeroTensor

instance (TIndex k, TAlgebra v v', TScalar v, TScalar v', TScalar (TAlg v v')) => TAlgebra (Tensor n k v) (Tensor m k v') where
    type TAlg (Tensor n k v) (Tensor m k v') = Tensor (n+m) k (TAlg v v')
    prodA = (&*)

instance TScalar Rational where
    addS = (+)
    subS = (-)
    scaleS = (*)
    scaleZero = 0

--for the double instance scaleZero should be the maschine epsilon

instance TScalar Double where
    addS = (+)
    subS = (-)
    scaleS r b = fromRational r * b
    scaleZero = 2.2e-16

instance TAlgebra Rational Rational where
    type TAlg Rational Rational = Rational
    prodA = (*)

instance TAlgebra Double Double where
    type TAlg Double Double = Double
    prodA = (*)

getTensorMap :: Tensor (n+1) k v -> TMap k (Tensor n k v)
getTensorMap (Tensor m) = m

--flatten a tensor to a list containing all (indices,value) pairs

toListT :: Tensor n k v -> [(IndList n k, v)]
toListT (Scalar x) = [(Empty, x)]
toListT (Tensor m) =  concatMap (\(i,t) -> appendF i $ toListT t) m
        where
            appendF i = map (\(l,val) -> (Append i l ,val))
toListT ZeroTensor = []

toListShow :: (TIndex k, TScalar v) => Tensor n k v -> [([Int],v)]
toListShow t = filter (\x -> snd x /= scaleZero) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT t
            showInd i1 = map fromEnum $ toList i1

--construct from typed list

mkTens :: (IndList n k, v) -> Tensor n k v
mkTens (Empty, a) = Scalar a
mkTens (Append x xs, a) = Tensor  [(x, mkTens (xs, a))]

fromListT :: (TIndex k, TScalar v) => [(IndList n k, v)] -> Tensor n k v
fromListT [x] = mkTens x
fromListT (x:xs) = foldr insertOrAdd (mkTens x) xs
fromListT [] = ZeroTensor

--construct from generic list

fromListT' :: forall n k v . (TIndex k, TScalar v, SingI n) => [([k],v)] -> Tensor n k v
fromListT' l = fromListT indList
        where
            indList = map (\(x,y) -> (fromList' x, y)) l

--helper function for tensor addition: adding one indice value pair to the tree structure of a given tensor

insertOrAdd :: (TIndex k, TScalar v) => (IndList n k, v) -> Tensor n k v -> Tensor n k v
insertOrAdd (Empty, a) (Scalar b) = Scalar (addS a b)
insertOrAdd (Append x xs, a) (Tensor m) = Tensor $ insertWithTMap (\_ o -> insertOrAdd (xs, a) o) x indTens m
            where
                indTens = mkTens (xs, a)
insertOrAdd inds ZeroTensor = mkTens inds

--addition for tensors

infixr 6 &+

(&+) :: (TIndex k, TScalar v) => Tensor n k v -> Tensor n k v -> Tensor n k v
(&+) (Scalar a) (Scalar b) = Scalar $ addS a b
(&+) (Tensor m1) (Tensor m2) = Tensor $ addTMaps (&+) m1 m2
(&+) t1 ZeroTensor = t1
(&+) ZeroTensor t2 = t2

--scalar multiplication for tensors

infix 8 &.

(&.) :: (TIndex k, TScalar v) => Rational -> Tensor n k v -> Tensor n k v
(&.) scalar = fmap (scaleS scalar)

--subtraction for tensors

infix 5 &-

(&-) :: (TIndex k, TScalar v) => Tensor n k v -> Tensor n k v -> Tensor n k v
(&-) t1 t2 = t1 &+ (-1) &. t2

--product of tensors

infixr 7 &*

(&*) :: (TIndex k, TAlgebra v v', TScalar v, TScalar v', TScalar (TAlg v v')) => Tensor n k v -> Tensor m k v' -> Tensor (n+m) k (TAlg v v')
(&*) (Scalar x) (Scalar y) = let newVal = prodA x y in if newVal == scaleZero then ZeroTensor else Scalar newVal
(&*) (Scalar x) t2 = fmap (prodA x) t2
(&*) (Tensor m) t2 = Tensor $ mapTMap (&* t2) m
(&*) t1 ZeroTensor = ZeroTensor
(&*) ZeroTensor t2 = ZeroTensor

--transpose a given tensor in 2 of its indices

tensorTrans :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v
tensorTrans (0, j) t = fromListT l
                where
                    l = map (\(x,y) -> (swapHead j x, y)) $ toListT t
tensorTrans (i, j) (Tensor m) = Tensor $ mapTMap (tensorTrans (i-1, j-1)) m
tensorTrans (i ,j) ZeroTensor = ZeroTensor

--transpose a given Tensor in several of its indices

tensorBlockTrans :: (TIndex k, TScalar v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k v
tensorBlockTrans (l1,l2) t = foldr tensorTrans t indList
        where
            indList = if null $ intersect l1 l2 then zip l1 l2 else error "at least one index in the list occurs several times"

--generic resorting of the indices of a given tensor according to permutation given by [Int] -> the most expensive as the whole tensor is flattened to a list
--input is current ordering of tesnor w.r.t. goal orderig, i.e. resorting [A,B,C,D] to [B,C,D,A] is achieved by [3,0,2,1]

resortTens :: (SingI n, TIndex k, TScalar v) => [Int] -> Tensor n k v -> Tensor n k v
resortTens perm t = fromListT $ map (\(x,y) -> (resortInd perm x, y)) $ toListT t

--evaluation of a tensor for a specific index value returning the appropriate subtensor

evalTens :: (SingI (n+1), TIndex k, TScalar v) => Int -> k -> Tensor (n+1) k v -> Tensor n k v
evalTens ind indVal (Tensor m)
            | ind > size -1 || ind < 0 = error "wrong index to evaluate"
            | ind == 0 = fromMaybe ZeroTensor $ lookup indVal m
            | otherwise = fromMaybe ZeroTensor $ lookup indVal (getTensorMap newTens)
            where
                size = length $ fst $ head $ toListShow (Tensor m)
                l = [1..ind] ++ 0 : [ind+1..size -1]
                newTens = resortTens l (Tensor m)

--symmetrization of a given Tensor

symTens :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v
symTens inds t = t &+ tensorTrans inds t

aSymTens :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v
aSymTens inds t = t &- tensorTrans inds t

symTensFac :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v
symTensFac inds t = (1%2) &. symTens inds t

aSymTensFac :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v
aSymTensFac inds t = (1%2) &. aSymTens inds t

--block symmetrization

symBlockTens :: (TIndex k, TScalar v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k v
symBlockTens inds t = t &+ tensorBlockTrans inds t

aSymBlockTens :: (TIndex k, TScalar v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k v
aSymBlockTens inds t = t &- tensorBlockTrans inds t

symBlockTensFac :: (TIndex k, TScalar v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k v
symBlockTensFac inds t = (1%2) &. symBlockTens inds t

aSymBlockTensFac :: (TIndex k, TScalar v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k v
aSymBlockTensFac inds t = (1%2) &. aSymBlockTens inds t

--helper function for cyclic symmetrization: convert all permutations of a given list of indices into an equivalent lists of lists of Swaps of 2 indices

getAllSwaps :: [Int] -> [[(Int,Int)]]
getAllSwaps [x,y] = [[(x,y)]]
getAllSwaps (x:xs) = lNew ++ l' ++ ((:) <$> l <*> l')
        where
            l = zip (repeat x) xs
            lNew = map pure l
            l' = getAllSwaps xs

getAllBlockSwaps :: [[Int]] -> [[([Int],[Int])]]
getAllBlockSwaps [x,y] = [[(x,y)]]
getAllBlockSwaps (x:xs) = lNew ++ l' ++ ((:) <$> l <*> l')
        where
            l = zip (repeat x) xs
            lNew = map pure l
            l' = getAllBlockSwaps xs

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)

--cyclic symmetrization

cyclicSymTens :: (TIndex k, TScalar v) => [Int] -> Tensor n k v -> Tensor n k v
cyclicSymTens inds t = newTens
        where
            swapList = getAllSwaps inds
            tensList = map (foldr tensorTrans t) swapList
            newTens = foldr (&+) t tensList

cyclicASymTens :: (TIndex k, TScalar v) => [Int] -> Tensor n k v -> Tensor n k v
cyclicASymTens inds t = newTens
        where
            swapList = getAllSwaps inds
            signList = map (\x -> (-1) ^ length x) swapList
            tensList' = map (foldr tensorTrans t) swapList
            tensList = zipWith (&.) signList tensList'
            newTens = foldr (&+) t tensList

cyclicBlockSymTens :: (TIndex k, TScalar v) => [[Int]] -> Tensor n k v -> Tensor n k v
cyclicBlockSymTens inds t = newTens
        where
            swapList = getAllBlockSwaps inds
            tensList = map (foldr tensorBlockTrans t) swapList
            newTens = foldr (&+) t tensList

cyclicSymTensFac :: (TIndex k, TScalar v) => [Int] -> Tensor n k v -> Tensor n k v
cyclicSymTensFac inds t = fac &. cyclicSymTens inds t
        where
            fac = 1 % fromIntegral (factorial $ length inds)

cyclicASymTensFac :: (TIndex k, TScalar v) => [Int] -> Tensor n k v -> Tensor n k v
cyclicASymTensFac inds t = fac &. cyclicASymTens inds t
        where
            fac = 1 % fromIntegral (factorial $ length inds)

cyclicBlockSymTensFac :: (TIndex k, TScalar v) => [[Int]] -> Tensor n k v -> Tensor n k v
cyclicBlockSymTensFac inds t = fac &. cyclicBlockSymTens inds t
        where
            fac = 1 % fromIntegral (factorial $ length inds)

--for contraction we need a tensor with upper and lower indices: upper indices always come first

type Tensor2 n1 n2 k v = Tensor n1 k (Tensor n2 k v)

tensorContr :: (TIndex k, TScalar v) => (Int,Int) -> Tensor2 n1 n2 k v -> Tensor2 (n1-1) (n2-1) k v
tensorContr (0,j) t = fromListT tensList
    where
        l = map (\(x,y) -> (x, toListT y)) $ toListT t
        l2 = map (\(x,y) -> (tailInd x,mapMaybe (removeContractionInd j (headInd x)) y)) l
        l3 = filter (\(_,y) -> not (null y)) l2
        tensList = map (\(x,y) -> (x, fromListT y)) l3
tensorContr (i,j) (Tensor m) = Tensor $ mapTMap (tensorContr (i-1,j)) m
tensorContr inds ZeroTensor = ZeroTensor
tensorContr inds (Scalar s) = error "cannot contract scalar!"

--encode and decode tensors as bytestrings for efficient storing

encodeTensor :: (KnownNat n, Ord k, Serialize k, Serialize v) => Tensor n k v -> BS.ByteString
encodeTensor = compress . encodeLazy

decodeTensor :: (KnownNat n, Ord k, Serialize k, Serialize v) => BS.ByteString -> Tensor n k v
decodeTensor bs = either error id $ decodeLazy $ decompress bs

--now a generic abstract Tensor "consists of several" Tensor2s that represent contravariant and covariant tensors in the several abstract indices

type AbsTensor1 n1 k1 v = Tensor n1 k1 v

type AbsTensor2 n1 n2 k1 v = Tensor2 n1 n2 k1 v

type AbsTensor3 n1 n2 n3 k1 k2 v = AbsTensor2 n1 n2 k1 (Tensor n3 k2 v)

type AbsTensor4 n1 n2 n3 n4 k1 k2 v = AbsTensor2 n1 n2 k1 (Tensor2 n3 n4 k2 v)

type AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v = AbsTensor4 n1 n2 n3 n4 k1 k2 (Tensor n5 k3 v)

type AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v = AbsTensor4 n1 n2 n3 n4 k1 k2 (Tensor2 n5 n6 k3 v)

type AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v = AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (Tensor n7 k4 v)

type AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v = AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (Tensor2 n7 n8 k4 v)

--more different indices is probably not realistic but could be easily added by hand

--AbsTensors automatically satisfy TensorAlgebra as we only used type synonyms
--e.g. AbsTensor4 actually looks like Tensor n1 kq1 (Tensor n2 k2 (Tensor n3 k3 (Tensor n4 k4 (Tensor n5 k5 (Tensor n6 k6 (Tensor n7 k7 (Tensor n8 k8 a)))))))

--fmap takes us 1 level deeper -> we get functions that apply a given function to the various subtensors

mapTo1 :: (TScalar v1, TScalar v2, TIndex k) => (v1 -> v2) -> Tensor n1 k v1 -> Tensor n1 k v2
mapTo1 = fmap

mapTo2 :: (TScalar v1, TScalar v2, TIndex k) => (v1 -> v2) -> Tensor2 n1 n2 k v1 -> Tensor2 n1 n2 k v2
mapTo2 f = fmap (fmap f)

mapTo3 :: (TScalar v1, TScalar v2, TIndex k1, TIndex k2) => (v1 -> v2) -> AbsTensor3 n1 n2 n3 k1 k2 v1 -> AbsTensor3 n1 n2 n3 k1 k2 v2
mapTo3 f = fmap (fmap (fmap f))

mapTo4 :: (TScalar v1, TScalar v2, TIndex k1, TIndex k2) => (v1 -> v2) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v1 -> AbsTensor4 n1 n2 n3 n4 k1 k2 v2
mapTo4 f = fmap (fmap (fmap (fmap f)))

mapTo5 :: (TScalar v1, TScalar v2, TIndex k1, TIndex k2, TIndex k3) => (v1 -> v2) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v1 -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v2
mapTo5 f = fmap (fmap (fmap (fmap (fmap f))))

mapTo6 :: (TScalar v1, TScalar v2, TIndex k1, TIndex k2, TIndex k3) => (v1 -> v2) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v1 -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v2
mapTo6 f = fmap (fmap (fmap (fmap (fmap (fmap f)))))

mapTo7 :: (TScalar v1, TScalar v2, TIndex k1, TIndex k2, TIndex k3, TIndex k4) => (v1 -> v2) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v1 -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v2
mapTo7 f = fmap (fmap (fmap (fmap (fmap (fmap (fmap f))))))

mapTo8 :: (TScalar v1, TScalar v2, TIndex k1, TIndex k2, TIndex k3, TIndex k4) => (v1 -> v2) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v1 -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v2
mapTo8 f = fmap (fmap (fmap (fmap (fmap (fmap (fmap (fmap f)))))))

--remove Zero Values at every level of Abstact Tensor

removeZeros1 :: (TScalar v, TIndex k) => AbsTensor1 n1 k v -> AbsTensor1 n1 k v
removeZeros1 = removeZeros

removeZeros2 :: (TScalar v, TIndex k) => AbsTensor2 n1 n2 k v -> AbsTensor2 n1 n2 k v
removeZeros2 = removeZeros . mapTo1 removeZeros

removeZeros3 :: (TScalar v, TIndex k1, TIndex k2) => AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
removeZeros3 = removeZeros . mapTo1 removeZeros . mapTo2 removeZeros

removeZeros4 :: (TScalar v, TIndex k1, TIndex k2) => AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
removeZeros4 = removeZeros . mapTo1 removeZeros . mapTo2 removeZeros . mapTo3 removeZeros

removeZeros5 :: (TScalar v, TIndex k1, TIndex k2, TIndex k3) => AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
removeZeros5 = removeZeros . mapTo1 removeZeros . mapTo2 removeZeros . mapTo3 removeZeros . mapTo4 removeZeros

removeZeros6 :: (TScalar v, TIndex k1, TIndex k2, TIndex k3) => AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
removeZeros6 = removeZeros . mapTo1 removeZeros . mapTo2 removeZeros . mapTo3 removeZeros . mapTo4 removeZeros . mapTo5 removeZeros

removeZeros7 :: (TScalar v, TIndex k1, TIndex k2, TIndex k3, TIndex k4) => AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
removeZeros7 = removeZeros . mapTo1 removeZeros . mapTo2 removeZeros . mapTo3 removeZeros . mapTo4 removeZeros . mapTo5 removeZeros . mapTo6 removeZeros

removeZeros8 :: (TScalar v, TIndex k1, TIndex k2, TIndex k3, TIndex k4) => AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
removeZeros8 = removeZeros . mapTo1 removeZeros . mapTo2 removeZeros . mapTo3 removeZeros . mapTo4 removeZeros . mapTo5 removeZeros . mapTo6 removeZeros . mapTo7 removeZeros

--transpose a general absatract tensor

tensorTrans1 :: (TIndex k1, TScalar v) => (Int,Int) -> AbsTensor1 n1 k1 v -> AbsTensor1 n1 k1 v
tensorTrans1 = tensorTrans

tensorTrans2 :: (TIndex k1, TScalar v) => (Int,Int) -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 n1 n2 k1 v
tensorTrans2 inds = mapTo1 (tensorTrans inds)

tensorTrans3 :: (TIndex k1, TIndex k2, TScalar v) => (Int,Int) -> AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
tensorTrans3 inds = mapTo2 (tensorTrans inds)

tensorTrans4 :: (TIndex k1, TIndex k2, TScalar v) => (Int,Int) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
tensorTrans4 inds = mapTo3 (tensorTrans inds)

tensorTrans5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => (Int,Int) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
tensorTrans5 inds = mapTo4 (tensorTrans inds)

tensorTrans6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => (Int,Int) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
tensorTrans6 inds = mapTo5 (tensorTrans inds)

tensorTrans7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => (Int,Int) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
tensorTrans7 inds = mapTo6 (tensorTrans inds)

tensorTrans8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => (Int,Int) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
tensorTrans8 inds = mapTo7 (tensorTrans inds)

--blockTranspose an AbsTensor

tensorBlockTrans1 :: (TIndex k1, TScalar v) => ([Int],[Int]) -> AbsTensor1 n1 k1 v -> AbsTensor1 n1 k1 v
tensorBlockTrans1 = tensorBlockTrans

tensorBlockTrans2 :: (TIndex k1, TScalar v) => ([Int],[Int]) -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 n1 n2 k1 v
tensorBlockTrans2 inds = mapTo1 (tensorBlockTrans inds)

tensorBlockTrans3 :: (TIndex k1, TIndex k2, TScalar v) => ([Int],[Int]) -> AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
tensorBlockTrans3 inds = mapTo2 (tensorBlockTrans inds)

tensorBlockTrans4 :: (TIndex k1, TIndex k2, TScalar v) => ([Int],[Int]) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
tensorBlockTrans4 inds = mapTo3 (tensorBlockTrans inds)

tensorBlockTrans5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => ([Int],[Int]) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
tensorBlockTrans5 inds = mapTo4 (tensorBlockTrans inds)

tensorBlockTrans6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => ([Int],[Int]) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
tensorBlockTrans6 inds = mapTo5 (tensorBlockTrans inds)

tensorBlockTrans7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => ([Int],[Int]) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
tensorBlockTrans7 inds = mapTo6 (tensorBlockTrans inds)

tensorBlockTrans8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => ([Int],[Int]) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
tensorBlockTrans8 inds = mapTo7 (tensorBlockTrans inds)

--resort an AbsTens

resortTens1 :: (SingI n1, TIndex k1, TScalar v) => [Int] -> AbsTensor1 n1 k1 v -> AbsTensor1 n1 k1 v
resortTens1 = resortTens

resortTens2 :: (SingI n1, SingI n2, TIndex k1, TScalar v) => [Int] -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 n1 n2 k1 v
resortTens2 inds = mapTo1 (resortTens inds)

resortTens3 :: (SingI n1, SingI n2, SingI n3, TIndex k1, TIndex k2, TScalar v) => [Int] -> AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
resortTens3 inds = mapTo2 (resortTens inds)

resortTens4 :: (SingI n1, SingI n2, SingI n3, SingI n4, TIndex k1, TIndex k2, TScalar v) => [Int] -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
resortTens4 inds = mapTo3 (resortTens inds)

resortTens5 :: (SingI n1, SingI n2, SingI n3, SingI n4, SingI n5, TIndex k1, TIndex k2, TIndex k3, TScalar v) => [Int] -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
resortTens5 inds = mapTo4 (resortTens inds)

resortTens6 :: (SingI n1, SingI n2, SingI n3, SingI n4, SingI n5, SingI n6, TIndex k1, TIndex k2, TIndex k3, TScalar v) => [Int] -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
resortTens6 inds = mapTo5 (resortTens inds)

resortTens7 :: (SingI n1, SingI n2, SingI n3, SingI n4, SingI n5, SingI n6, SingI n7, TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => [Int] -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
resortTens7 inds = mapTo6 (resortTens inds)

resortTens8 :: (SingI n1, SingI n2, SingI n3, SingI n4, SingI n5, SingI n6, SingI n7, SingI n8, TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => [Int] -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
resortTens8 inds = mapTo7 (resortTens inds)

--eval an abstract tensor

evalTens1 :: (SingI (n1+1), TIndex k1, TScalar v) => Int -> k1 -> AbsTensor1 (n1+1) k1 v -> AbsTensor1 n1 k1 v
evalTens1 = evalTens

evalTens2 :: (SingI (n2+1), TIndex k1, TScalar v) => Int -> k1 -> AbsTensor2 n1 (n2+1) k1 v  -> AbsTensor2 n1 n2 k1 v
evalTens2 ind indVal = mapTo1 (evalTens ind indVal)

evalTens3 :: (SingI (n3+1), TIndex k1, TIndex k2, TScalar v) => Int -> k2 -> AbsTensor3 n1 n2 (n3+1) k1 k2 v  -> AbsTensor3 n1 n2 n3 k1 k2 v
evalTens3 ind indVal = mapTo2 (evalTens ind indVal)

evalTens4 :: (SingI (n4+1), TIndex k1, TIndex k2, TScalar v) => Int -> k2 -> AbsTensor4 n1 n2 n3 (n4+1) k1 k2 v  -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
evalTens4 ind indVal = mapTo3 (evalTens ind indVal)

evalTens5 :: (SingI (n5+1), TIndex k1, TIndex k2, TIndex k3, TScalar v) => Int -> k3 -> AbsTensor5 n1 n2 n3 n4 (n5+1) k1 k2 k3 v  -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
evalTens5 ind indVal = mapTo4 (evalTens ind indVal)

evalTens6 :: (SingI (n6+1), TIndex k1, TIndex k2, TIndex k3, TScalar v) => Int -> k3 -> AbsTensor6 n1 n2 n3 n4 n5 (n6+1) k1 k2 k3 v  -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
evalTens6 ind indVal = mapTo5 (evalTens ind indVal)

evalTens7 :: (SingI (n7+1), TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => Int -> k4 -> AbsTensor7 n1 n2 n3 n4 n5 n6 (n7+1) k1 k2 k3 k4 v  -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
evalTens7 ind indVal = mapTo6 (evalTens ind indVal)

evalTens8 :: (SingI (n8+1), TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => Int -> k4 -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 (n8+1) k1 k2 k3 k4 v  -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
evalTens8 ind indVal = mapTo7 (evalTens ind indVal)

--symmetrizer functions

symATens1 :: (TIndex k1, TScalar v) =>
             (Int,Int) ->
             AbsTensor1 n1 k1 v ->
             AbsTensor1 n1 k1 v
symATens1 = symTens

symATens2 :: (TIndex k1, TScalar v) =>
             (Int,Int) ->
             AbsTensor2 n1 n2 k1 v ->
             AbsTensor2 n1 n2 k1 v
symATens2 inds = mapTo1 (symTens inds)

symATens3 :: (TIndex k1, TIndex k2, TScalar v) =>
             (Int,Int) ->
             AbsTensor3 n1 n2 n3 k1 k2 v ->
             AbsTensor3 n1 n2 n3 k1 k2 v
symATens3 inds = mapTo2 (symTens inds)

symATens4 :: (TIndex k1, TIndex k2, TScalar v) =>
             (Int,Int) ->
             AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
             AbsTensor4 n1 n2 n3 n4 k1 k2 v
symATens4 inds = mapTo3 (symTens inds)

symATens5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
             (Int,Int) ->
             AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
             AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
symATens5 inds = mapTo4 (symTens inds)

symATens6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
             (Int,Int) ->
             AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
             AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
symATens6 inds = mapTo5 (symTens inds)

symATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
             (Int,Int) ->
             AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
             AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
symATens7 inds = mapTo6 (symTens inds)

symATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
             (Int,Int) ->
             AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
             AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
symATens8 inds = mapTo7 (symTens inds)

--with factor

symATensFac1 :: (TIndex k1, TScalar v) =>
                (Int,Int) ->
                AbsTensor1 n1 k1 v ->
                AbsTensor1 n1 k1 v
symATensFac1 = symTensFac

symATensFac2 :: (TIndex k1, TScalar v) =>
                (Int,Int) ->
                AbsTensor2 n1 n2 k1 v ->
                AbsTensor2 n1 n2 k1 v
symATensFac2 inds = mapTo1 (symTensFac inds)

symATensFac3 :: (TIndex k1,TIndex k2, TScalar v) =>
                (Int,Int) ->
                AbsTensor3 n1 n2 n3 k1 k2 v ->
                AbsTensor3 n1 n2 n3 k1 k2 v
symATensFac3 inds = mapTo2 (symTensFac inds)

symATensFac4 :: (TIndex k1, TIndex k2, TScalar v) =>
                (Int,Int) ->
                AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                AbsTensor4 n1 n2 n3 n4 k1 k2 v
symATensFac4 inds = mapTo3 (symTensFac inds)

symATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                (Int,Int) ->
                AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
symATensFac5 inds = mapTo4 (symTensFac inds)

symATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                (Int,Int) ->
                AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
symATensFac6 inds = mapTo5 (symTensFac inds)

symATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                (Int,Int) ->
                AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
symATensFac7 inds = mapTo6 (symTensFac inds)

symATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                (Int,Int) ->
                AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
symATensFac8 inds = mapTo7 (symTensFac inds)

--antisymmetrization

aSymATens1 :: (TIndex k1, TScalar v) =>
              (Int,Int) ->
              AbsTensor1 n1 k1 v ->
              AbsTensor1 n1 k1 v
aSymATens1 = aSymTens

aSymATens2 :: (TIndex k1, TScalar v) =>
              (Int,Int) ->
              AbsTensor2 n1 n2 k1 v ->
              AbsTensor2 n1 n2 k1 v
aSymATens2 inds = mapTo1 (aSymTens inds)

aSymATens3 :: (TIndex k1, TIndex k2, TScalar v) =>
              (Int,Int) ->
              AbsTensor3 n1 n2 n3 k1 k2 v ->
              AbsTensor3 n1 n2 n3 k1 k2 v
aSymATens3 inds = mapTo2 (aSymTens inds)

aSymATens4 :: (TIndex k1, TIndex k2, TScalar v) =>
              (Int,Int) ->
              AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
              AbsTensor4 n1 n2 n3 n4 k1 k2 v
aSymATens4 inds = mapTo3 (aSymTens inds)

aSymATens5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
              (Int,Int) ->
              AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
              AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
aSymATens5 inds = mapTo4 (aSymTens inds)

aSymATens6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
              (Int,Int) ->
              AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
              AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
aSymATens6 inds = mapTo5 (aSymTens inds)

aSymATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
              (Int,Int) ->
              AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
              AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
aSymATens7 inds = mapTo6 (aSymTens inds)

aSymATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
              (Int,Int) ->
              AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
              AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
aSymATens8 inds = mapTo7 (aSymTens inds)

--with factor

aSymATensFac1 :: (TIndex k1, TScalar v) =>
                 (Int,Int) ->
                 AbsTensor1 n1 k1 v ->
                 AbsTensor1 n1 k1 v
aSymATensFac1 = aSymTensFac

aSymATensFac2 :: (TIndex k1, TScalar v) =>
                 (Int,Int) ->
                 AbsTensor2 n1 n2 k1 v ->
                 AbsTensor2 n1 n2 k1 v
aSymATensFac2 inds = mapTo1 (aSymTensFac inds)

aSymATensFac3 :: (TIndex k1, TIndex k2, TScalar v) =>
                 (Int,Int) ->
                 AbsTensor3 n1 n2 n3 k1 k2 v ->
                 AbsTensor3 n1 n2 n3 k1 k2 v
aSymATensFac3 inds = mapTo2 (aSymTensFac inds)

aSymATensFac4 :: (TIndex k1, TIndex k2, TScalar v) =>
                 (Int,Int) ->
                 AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                 AbsTensor4 n1 n2 n3 n4 k1 k2 v
aSymATensFac4 inds = mapTo3 (aSymTensFac inds)

aSymATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                 (Int,Int) ->
                 AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                 AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
aSymATensFac5 inds = mapTo4 (aSymTensFac inds)

aSymATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                 (Int,Int) ->
                 AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                 AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
aSymATensFac6 inds = mapTo5 (aSymTensFac inds)

aSymATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                 (Int,Int) ->
                 AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                 AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
aSymATensFac7 inds = mapTo6 (aSymTensFac inds)

aSymATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                 (Int,Int) ->
                 AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                 AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
aSymATensFac8 inds = mapTo7 (aSymTensFac inds)

--block symmetrization

symBlockATens1 :: (TIndex k1, TScalar v) =>
                  ([Int],[Int]) ->
                  AbsTensor1 n1 k1 v ->
                  AbsTensor1 n1 k1 v
symBlockATens1 = symBlockTens

symBlockATens2 :: (TIndex k1, TScalar v) =>
                  ([Int],[Int]) ->
                  AbsTensor2 n1 n2 k1 v ->
                  AbsTensor2 n1 n2 k1 v
symBlockATens2 inds = mapTo1 (symBlockTens inds)

symBlockATens3 :: (TIndex k1, TIndex k2, TScalar v) =>
                  ([Int],[Int]) ->
                  AbsTensor3 n1 n2 n3 k1 k2 v ->
                  AbsTensor3 n1 n2 n3 k1 k2 v
symBlockATens3 inds = mapTo2 (symBlockTens inds)

symBlockATens4 :: (TIndex k1, TIndex k2, TScalar v) =>
                  ([Int],[Int]) ->
                  AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                  AbsTensor4 n1 n2 n3 n4 k1 k2 v
symBlockATens4 inds = mapTo3 (symBlockTens inds)

symBlockATens5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                  ([Int],[Int]) ->
                  AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                  AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
symBlockATens5 inds = mapTo4 (symBlockTens inds)

symBlockATens6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                  ([Int],[Int]) ->
                  AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                  AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
symBlockATens6 inds = mapTo5 (symBlockTens inds)

symBlockATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                  ([Int],[Int]) ->
                  AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                  AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
symBlockATens7 inds = mapTo6 (symBlockTens inds)

symBlockATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                  ([Int],[Int]) ->
                  AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                  AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
symBlockATens8 inds = mapTo7 (symBlockTens inds)

--with factor

symBlockATensFac1 :: (TIndex k1, TScalar v) =>
                     ([Int],[Int]) ->
                     AbsTensor1 n1 k1 v ->
                     AbsTensor1 n1 k1 v
symBlockATensFac1 = symBlockTensFac

symBlockATensFac2 :: (TIndex k1, TScalar v) =>
                     ([Int],[Int]) ->
                     AbsTensor2 n1 n2 k1 v ->
                     AbsTensor2 n1 n2 k1 v
symBlockATensFac2 inds = mapTo1 (symBlockTensFac inds)

symBlockATensFac3 :: (TIndex k1, TIndex k2, TScalar v) =>
                     ([Int],[Int]) ->
                     AbsTensor3 n1 n2 n3 k1 k2 v ->
                     AbsTensor3 n1 n2 n3 k1 k2 v
symBlockATensFac3 inds = mapTo2 (symBlockTensFac inds)

symBlockATensFac4 :: (TIndex k1, TIndex k2, TScalar v) =>
                     ([Int],[Int]) ->
                     AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                     AbsTensor4 n1 n2 n3 n4 k1 k2 v
symBlockATensFac4 inds = mapTo3 (symBlockTensFac inds)

symBlockATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                     ([Int],[Int]) ->
                     AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                     AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
symBlockATensFac5 inds = mapTo4 (symBlockTensFac inds)

symBlockATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                     ([Int],[Int]) ->
                     AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                     AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
symBlockATensFac6 inds = mapTo5 (symBlockTensFac inds)

symBlockATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                     ([Int],[Int]) ->
                     AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                     AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
symBlockATensFac7 inds = mapTo6 (symBlockTensFac inds)

symBlockATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                     ([Int],[Int]) ->
                     AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                     AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
symBlockATensFac8 inds = mapTo7 (symBlockTensFac inds)

--antisymmetrization

aSymBlockATens1 :: (TIndex k1, TScalar v) =>
                   ([Int],[Int]) ->
                   AbsTensor1 n1 k1 v ->
                   AbsTensor1 n1 k1 v
aSymBlockATens1 = aSymBlockTens

aSymBlockATens2 :: (TIndex k1, TScalar v) =>
                   ([Int],[Int]) ->
                   AbsTensor2 n1 n2 k1 v ->
                   AbsTensor2 n1 n2 k1 v
aSymBlockATens2 inds = mapTo1 (aSymBlockTens inds)

aSymBlockATens3 :: (TIndex k1, TIndex k2, TScalar v) =>
                   ([Int],[Int]) ->
                   AbsTensor3 n1 n2 n3 k1 k2 v ->
                   AbsTensor3 n1 n2 n3 k1 k2 v
aSymBlockATens3 inds = mapTo2 (aSymBlockTens inds)

aSymBlockATens4 :: (TIndex k1, TIndex k2, TScalar v) =>
                   ([Int],[Int]) ->
                   AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                   AbsTensor4 n1 n2 n3 n4 k1 k2 v
aSymBlockATens4 inds = mapTo3 (aSymBlockTens inds)

aSymBlockATens5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                   ([Int],[Int]) ->
                   AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                   AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
aSymBlockATens5 inds = mapTo4 (aSymBlockTens inds)

aSymBlockATens6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                   ([Int],[Int]) ->
                   AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                   AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
aSymBlockATens6 inds = mapTo5 (aSymBlockTens inds)

aSymBlockATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                   ([Int],[Int]) ->
                   AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                   AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
aSymBlockATens7 inds = mapTo6 (aSymBlockTens inds)

aSymBlockATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                   ([Int],[Int]) ->
                   AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                   AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
aSymBlockATens8 inds = mapTo7 (aSymBlockTens inds)

--with factor

aSymBlockATensFac1 :: (TIndex k1, TScalar v) =>
                      ([Int],[Int]) ->
                      AbsTensor1 n1 k1 v ->
                      AbsTensor1 n1 k1 v
aSymBlockATensFac1 = aSymBlockTensFac

aSymBlockATensFac2 :: (TIndex k1, TScalar v) =>
                      ([Int],[Int]) ->
                      AbsTensor2 n1 n2 k1 v ->
                      AbsTensor2 n1 n2 k1 v
aSymBlockATensFac2 inds = mapTo1 (aSymBlockTensFac inds)

aSymBlockATensFac3 :: (TIndex k1, TIndex k2, TScalar v) =>
                      ([Int],[Int]) ->
                      AbsTensor3 n1 n2 n3 k1 k2 v ->
                      AbsTensor3 n1 n2 n3 k1 k2 v
aSymBlockATensFac3 inds = mapTo2 (aSymBlockTensFac inds)

aSymBlockATensFac4 :: (TIndex k1, TIndex k2, TScalar v) =>
                      ([Int],[Int]) ->
                      AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                      AbsTensor4 n1 n2 n3 n4 k1 k2 v
aSymBlockATensFac4 inds = mapTo3 (aSymBlockTensFac inds)

aSymBlockATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                      ([Int],[Int]) ->
                      AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                      AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
aSymBlockATensFac5 inds = mapTo4 (aSymBlockTensFac inds)

aSymBlockATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                      ([Int],[Int]) ->
                      AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                      AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
aSymBlockATensFac6 inds = mapTo5 (aSymBlockTensFac inds)

aSymBlockATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                      ([Int],[Int]) ->
                      AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                      AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
aSymBlockATensFac7 inds = mapTo6 (aSymBlockTensFac inds)

aSymBlockATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                      ([Int],[Int]) ->
                      AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                      AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
aSymBlockATensFac8 inds = mapTo7 (aSymBlockTensFac inds)

--cyclic symmetrization

cyclicSymATens1 :: (TIndex k1, TScalar v) =>
                   [Int] ->
                   AbsTensor1 n1 k1 v ->
                   AbsTensor1 n1 k1 v
cyclicSymATens1 = cyclicSymTens

cyclicSymATens2 :: (TIndex k1, TScalar v) =>
                   [Int] ->
                   AbsTensor2 n1 n2 k1 v ->
                   AbsTensor2 n1 n2 k1 v
cyclicSymATens2 inds = mapTo1 (cyclicSymTens inds)

cyclicSymATens3 :: (TIndex k1, TIndex k2, TScalar v) =>
                   [Int] ->
                   AbsTensor3 n1 n2 n3 k1 k2 v ->
                   AbsTensor3 n1 n2 n3 k1 k2 v
cyclicSymATens3 inds = mapTo2 (cyclicSymTens inds)

cyclicSymATens4 :: (TIndex k1, TIndex k2, TScalar v) =>
                   [Int] ->
                   AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                   AbsTensor4 n1 n2 n3 n4 k1 k2 v
cyclicSymATens4 inds = mapTo3 (cyclicSymTens inds)

cyclicSymATens5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                   [Int] ->
                   AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                   AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
cyclicSymATens5 inds = mapTo4 (cyclicSymTens inds)

cyclicSymATens6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                   [Int] ->
                   AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                   AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
cyclicSymATens6 inds = mapTo5 (cyclicSymTens inds)

cyclicSymATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                   [Int] ->
                   AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                   AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
cyclicSymATens7 inds = mapTo6 (cyclicSymTens inds)

cyclicSymATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                   [Int] ->
                   AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                   AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
cyclicSymATens8 inds = mapTo7 (cyclicSymTens inds)

--with factor

cyclicSymATensFac1 :: (TIndex k1, TScalar v) =>
                      [Int] ->
                      AbsTensor1 n1 k1 v ->
                      AbsTensor1 n1 k1 v
cyclicSymATensFac1 = cyclicSymTensFac

cyclicSymATensFac2 :: (TIndex k1, TScalar v) =>
                      [Int] ->
                      AbsTensor2 n1 n2 k1 v ->
                      AbsTensor2 n1 n2 k1 v
cyclicSymATensFac2 inds = mapTo1 (cyclicSymTensFac inds)

cyclicSymATensFac3 :: (TIndex k1, TIndex k2, TScalar v) =>
                      [Int] ->
                      AbsTensor3 n1 n2 n3 k1 k2 v ->
                      AbsTensor3 n1 n2 n3 k1 k2 v
cyclicSymATensFac3 inds = mapTo2 (cyclicSymTensFac inds)

cyclicSymATensFac4 :: (TIndex k1, TIndex k2, TScalar v) =>
                      [Int] ->
                      AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                      AbsTensor4 n1 n2 n3 n4 k1 k2 v
cyclicSymATensFac4 inds = mapTo3 (cyclicSymTensFac inds)

cyclicSymATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                      [Int] ->
                      AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                      AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
cyclicSymATensFac5 inds = mapTo4 (cyclicSymTensFac inds)

cyclicSymATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                      [Int] ->
                      AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                      AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
cyclicSymATensFac6 inds = mapTo5 (cyclicSymTensFac inds)

cyclicSymATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                      [Int] ->
                      AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                      AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
cyclicSymATensFac7 inds = mapTo6 (cyclicSymTensFac inds)

cyclicSymATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                      [Int] ->
                      AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                      AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
cyclicSymATensFac8 inds = mapTo7 (cyclicSymTensFac inds)

--cyclic Antisymmetrization

cyclicASymATens1 :: (TIndex k1, TScalar v) =>
                    [Int] ->
                    AbsTensor1 n1 k1 v ->
                    AbsTensor1 n1 k1 v
cyclicASymATens1 = cyclicASymTens

cyclicASymATens2 :: (TIndex k1, TScalar v) =>
                    [Int] ->
                    AbsTensor2 n1 n2 k1 v ->
                    AbsTensor2 n1 n2 k1 v
cyclicASymATens2 inds = mapTo1 (cyclicASymTens inds)

cyclicASymATens3 :: (TIndex k1, TIndex k2, TScalar v) =>
                    [Int] ->
                    AbsTensor3 n1 n2 n3 k1 k2 v ->
                    AbsTensor3 n1 n2 n3 k1 k2 v
cyclicASymATens3 inds = mapTo2 (cyclicASymTens inds)

cyclicASymATens4 :: (TIndex k1, TIndex k2, TScalar v) =>
                    [Int] ->
                    AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                    AbsTensor4 n1 n2 n3 n4 k1 k2 v
cyclicASymATens4 inds = mapTo3 (cyclicASymTens inds)

cyclicASymATens5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                    [Int] ->
                    AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                    AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
cyclicASymATens5 inds = mapTo4 (cyclicASymTens inds)

cyclicASymATens6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                    [Int] ->
                    AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                    AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
cyclicASymATens6 inds = mapTo5 (cyclicASymTens inds)

cyclicASymATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                    [Int] ->
                    AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                    AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
cyclicASymATens7 inds = mapTo6 (cyclicASymTens inds)

cyclicASymATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                    [Int] ->
                    AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                    AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
cyclicASymATens8 inds = mapTo7 (cyclicASymTens inds)

--with factor

cyclicASymATensFac1 :: (TIndex k1, TScalar v) =>
                       [Int] ->
                       AbsTensor1 n1 k1 v ->
                       AbsTensor1 n1 k1 v
cyclicASymATensFac1 = cyclicASymTensFac

cyclicASymATensFac2 :: (TIndex k1, TScalar v) =>
                       [Int] ->
                       AbsTensor2 n1 n2 k1 v ->
                       AbsTensor2 n1 n2 k1 v
cyclicASymATensFac2 inds = mapTo1 (cyclicASymTensFac inds)

cyclicASymATensFac3 :: (TIndex k1, TIndex k2, TScalar v) =>
                       [Int] ->
                       AbsTensor3 n1 n2 n3 k1 k2 v ->
                       AbsTensor3 n1 n2 n3 k1 k2 v
cyclicASymATensFac3 inds = mapTo2 (cyclicASymTensFac inds)

cyclicASymATensFac4 :: (TIndex k1, TIndex k2, TScalar v) =>
                       [Int] ->
                       AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                       AbsTensor4 n1 n2 n3 n4 k1 k2 v
cyclicASymATensFac4 inds = mapTo3 (cyclicASymTensFac inds)

cyclicASymATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                       [Int] ->
                       AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                       AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
cyclicASymATensFac5 inds = mapTo4 (cyclicASymTensFac inds)

cyclicASymATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                       [Int] ->
                       AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                       AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
cyclicASymATensFac6 inds = mapTo5 (cyclicASymTensFac inds)

cyclicASymATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                       [Int] ->
                       AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                       AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
cyclicASymATensFac7 inds = mapTo6 (cyclicASymTensFac inds)

cyclicASymATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                       [Int] ->
                       AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                       AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
cyclicASymATensFac8 inds = mapTo7 (cyclicASymTensFac inds)

--cyclic block symmetrization

cyclicBlockSymATens1 :: (TIndex k1, TScalar v) =>
                        [[Int]] ->
                        AbsTensor1 n1 k1 v ->
                        AbsTensor1 n1 k1 v
cyclicBlockSymATens1 = cyclicBlockSymTens

cyclicBlockSymATens2 :: (TIndex k1, TScalar v) =>
                        [[Int]] ->
                        AbsTensor2 n1 n2 k1 v ->
                        AbsTensor2 n1 n2 k1 v
cyclicBlockSymATens2 inds = mapTo1 (cyclicBlockSymTens inds)

cyclicBlockSymATens3 :: (TIndex k1, TIndex k2, TScalar v) =>
                        [[Int]] ->
                        AbsTensor3 n1 n2 n3 k1 k2 v ->
                        AbsTensor3 n1 n2 n3 k1 k2 v
cyclicBlockSymATens3 inds = mapTo2 (cyclicBlockSymTens inds)

cyclicBlockSymATens4 :: (TIndex k1, TIndex k2, TScalar v) =>
                        [[Int]] ->
                        AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                        AbsTensor4 n1 n2 n3 n4 k1 k2 v
cyclicBlockSymATens4 inds = mapTo3 (cyclicBlockSymTens inds)

cyclicBlockSymATens5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                        [[Int]] ->
                        AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                        AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
cyclicBlockSymATens5 inds = mapTo4 (cyclicBlockSymTens inds)

cyclicBlockSymATens6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                        [[Int]] ->
                        AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                        AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
cyclicBlockSymATens6 inds = mapTo5 (cyclicBlockSymTens inds)

cyclicBlockSymATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                        [[Int]] ->
                        AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                        AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
cyclicBlockSymATens7 inds = mapTo6 (cyclicBlockSymTens inds)

cyclicBlockSymATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                        [[Int]] ->
                        AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                        AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
cyclicBlockSymATens8 inds = mapTo7 (cyclicBlockSymTens inds)

--with factor

cyclicBlockSymATensFac1 :: (TIndex k1, TScalar v) =>
                           [[Int]] ->
                           AbsTensor1 n1 k1 v ->
                           AbsTensor1 n1 k1 v
cyclicBlockSymATensFac1 = cyclicBlockSymTensFac

cyclicBlockSymATensFac2 :: (TIndex k1, TScalar v) =>
                           [[Int]] ->
                           AbsTensor2 n1 n2 k1 v ->
                           AbsTensor2 n1 n2 k1 v
cyclicBlockSymATensFac2 inds = mapTo1 (cyclicBlockSymTensFac inds)

cyclicBlockSymATensFac3 :: (TIndex k1, TIndex k2, TScalar v) =>
                           [[Int]] ->
                           AbsTensor3 n1 n2 n3 k1 k2 v ->
                           AbsTensor3 n1 n2 n3 k1 k2 v
cyclicBlockSymATensFac3 inds = mapTo2 (cyclicBlockSymTensFac inds)

cyclicBlockSymATensFac4 :: (TIndex k1, TIndex k2, TScalar v) =>
                           [[Int]] ->
                           AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                           AbsTensor4 n1 n2 n3 n4 k1 k2 v
cyclicBlockSymATensFac4 inds = mapTo3 (cyclicBlockSymTensFac inds)

cyclicBlockSymATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                           [[Int]] ->
                           AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                           AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
cyclicBlockSymATensFac5 inds = mapTo4 (cyclicBlockSymTensFac inds)

cyclicBlockSymATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) =>
                           [[Int]] ->
                           AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                           AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
cyclicBlockSymATensFac6 inds = mapTo5 (cyclicBlockSymTensFac inds)

cyclicBlockSymATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                           [[Int]] ->
                           AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                           AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
cyclicBlockSymATensFac7 inds = mapTo6 (cyclicBlockSymTensFac inds)

cyclicBlockSymATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) =>
                           [[Int]] ->
                           AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                           AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
cyclicBlockSymATensFac8 inds = mapTo7 (cyclicBlockSymTensFac inds)

--contraction for general tensors

contrATens1 :: (TIndex k1, TScalar v) => (Int,Int) -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 (n1-1) (n2-1) k1 v
contrATens1 = tensorContr

contrATens2 :: (TIndex k1, TIndex k2, TScalar v) => (Int,Int) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 (n3-1) (n4-1) k1 k2 v
contrATens2 inds = mapTo2 (tensorContr inds)

contrATens3 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => (Int,Int) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 (n5-1) (n6-1) k1 k2 k3 v
contrATens3 inds = mapTo4 (tensorContr inds)

contrATens4 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => (Int,Int) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 (n7-1) (n8-1) k1 k2 k3 k4 v
contrATens4 inds = mapTo6 (tensorContr inds)

--construct tensors from lists of (indices,value) pairs

type IndTuple1 n1 k1 = IndList n1 k1

type IndTuple2 n1 n2 k1 = (IndList n1 k1, IndList n2 k1)

type IndTuple3 n1 n2 n3 k1 k2 = (IndList n1 k1, IndList n2 k1, IndList n3 k2)

type IndTuple4 n1 n2 n3 n4 k1 k2 = (IndList n1 k1, IndList n2 k1, IndList n3 k2, IndList n4 k2)

type IndTuple5 n1 n2 n3 n4 n5 k1 k2 k3 = (IndList n1 k1, IndList n2 k1, IndList n3 k2, IndList n4 k2, IndList n5 k3)

type IndTuple6 n1 n2 n3 n4 n5 n6 k1 k2 k3 = (IndList n1 k1, IndList n2 k1, IndList n3 k2, IndList n4 k2, IndList n5 k3, IndList n6 k3)

type IndTuple7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 = (IndList n1 k1, IndList n2 k1, IndList n3 k2, IndList n4 k2, IndList n5 k3, IndList n6 k3, IndList n7 k4)

type IndTuple8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 = (IndList n1 k1, IndList n2 k1, IndList n3 k2, IndList n4 k2, IndList n5 k3, IndList n6 k3, IndList n7 k4, IndList n8 k4)

--construct a tensor with a single value

mkTens1 :: (TIndex k1, TScalar v) => (IndTuple1 n1 k1, v) -> AbsTensor1 n1 k1 v
mkTens1 = mkTens

mkTens2 :: (TIndex k1, TScalar v) => (IndTuple2 n1 n2 k1, v) -> AbsTensor2 n1 n2 k1 v
mkTens2 ((i1,i2),s) = mkTens (i1,mkTens (i2,s))

mkTens3 :: (TIndex k1, TIndex k2, TScalar v) => (IndTuple3 n1 n2 n3 k1 k2, v) -> AbsTensor3 n1 n2 n3 k1 k2 v
mkTens3 ((i1,i2,i3),s) = mkTens (i1,mkTens (i2,mkTens (i3,s)))

mkTens4 :: (TIndex k1, TIndex k2, TScalar v) => (IndTuple4 n1 n2 n3 n4 k1 k2, v) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
mkTens4 ((i1,i2,i3,i4),s) = mkTens (i1,mkTens (i2,mkTens (i3,mkTens (i4,s))))

mkTens5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => (IndTuple5 n1 n2 n3 n4 n5 k1 k2 k3, v) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
mkTens5 ((i1,i2,i3,i4,i5),s) = mkTens (i1,mkTens (i2,mkTens (i3,mkTens (i4,mkTens (i5,s)))))

mkTens6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => (IndTuple6 n1 n2 n3 n4 n5 n6 k1 k2 k3, v) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
mkTens6 ((i1,i2,i3,i4,i5,i6),s) = mkTens (i1,mkTens (i2,mkTens (i3,mkTens (i4,mkTens (i5,mkTens (i6,s))))))

mkTens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => (IndTuple7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4, v) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
mkTens7 ((i1,i2,i3,i4,i5,i6,i7),s) = mkTens (i1,mkTens (i2,mkTens (i3,mkTens (i4,mkTens (i5,mkTens (i6,mkTens (i7,s)))))))

mkTens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => (IndTuple8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4, v) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
mkTens8 ((i1,i2,i3,i4,i5,i6,i7,i8),s) = mkTens (i1,mkTens (i2,mkTens (i3,mkTens (i4,mkTens (i5,mkTens (i6,mkTens (i7,mkTens (i8,s))))))))

--construct a tensor from a list of typed index, value pairs -> ' version uses nontyped indices

fromListT1 :: (TIndex k1, TScalar v) => [(IndTuple1 n1 k1, v)] -> AbsTensor1 n1 k1 v
fromListT1 = fromListT

fromListT1' :: forall n1 k1 v . (SingI n1, TIndex k1, TScalar v) => [([k1],v)] -> AbsTensor1 n1 k1 v
fromListT1' = fromListT'

fromListT2 :: (TIndex k1, TScalar v) => [(IndTuple2 n1 n2 k1, v)] -> AbsTensor2 n1 n2 k1 v
fromListT2 l = foldr (&+) ZeroTensor tensList
    where
        tensList = map mkTens2 l

fromListT2' :: forall n1 n2 k1 v . (SingI n1, SingI n2, TIndex k1, TScalar v) => [(([k1],[k1]),v)] -> AbsTensor2 n1 n2 k1 v
fromListT2' l = fromListT2 indList
        where
            indList = map (\((x1,x2),y) -> ((fromList' x1, fromList' x2),y)) l

fromListT3 :: (TIndex k1, TIndex k2,  TScalar v) => [(IndTuple3 n1 n2 n3 k1 k2, v)] -> AbsTensor3 n1 n2 n3 k1 k2 v
fromListT3 l = foldr (&+) ZeroTensor tensList
    where
        tensList = map mkTens3 l

fromListT3' :: forall n1 n2 n3 k1 k2 v . (SingI n1, SingI n2, SingI n3, TIndex k1, TIndex k2, TScalar v) => [(([k1],[k1],[k2]),v)] -> AbsTensor3 n1 n2 n3 k1 k2 v
fromListT3' l = fromListT3 indList
        where
            indList = map (\((x1,x2,x3),y) -> ((fromList' x1, fromList' x2, fromList' x3),y)) l

fromListT4 :: (TIndex k1, TIndex k2,  TScalar v) => [(IndTuple4 n1 n2 n3 n4 k1 k2, v)] -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
fromListT4 l = foldr (&+) ZeroTensor tensList
    where
        tensList = map mkTens4 l

fromListT4' :: forall n1 n2 n3 n4 k1 k2 v . (SingI n1, SingI n2, SingI n3, SingI n4, TIndex k1, TIndex k2, TScalar v) => [(([k1],[k1],[k2],[k2]),v)] -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
fromListT4' l = fromListT4 indList
        where
            indList = map (\((x1,x2,x3,x4),y) -> ((fromList' x1, fromList' x2, fromList' x3, fromList' x4),y)) l

fromListT5 :: (TIndex k1, TIndex k2, TIndex k3,  TScalar v) => [(IndTuple5 n1 n2 n3 n4 n5 k1 k2 k3, v)] -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
fromListT5 l = foldr (&+) ZeroTensor tensList
    where
        tensList = map mkTens5 l

fromListT5' :: forall n1 n2 n3 n4 n5 k1 k2 k3 v . (SingI n1, SingI n2, SingI n3, SingI n4, SingI n5,  TIndex k1, TIndex k2, TIndex k3, TScalar v) => [(([k1],[k1],[k2],[k2],[k3]),v)] -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
fromListT5' l = fromListT5 indList
        where
            indList = map (\((x1,x2,x3,x4,x5),y) -> ((fromList' x1, fromList' x2, fromList' x3, fromList' x4, fromList' x5),y)) l

fromListT6 :: (TIndex k1, TIndex k2, TIndex k3,  TScalar v) => [(IndTuple6 n1 n2 n3 n4 n5 n6 k1 k2 k3, v)] -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
fromListT6 l = foldr (&+) ZeroTensor tensList
    where
        tensList = map mkTens6 l

fromListT6' :: forall n1 n2 n3 n4 n5 n6 k1 k2 k3 v . (SingI n1, SingI n2, SingI n3, SingI n4, SingI n5, SingI n6, TIndex k1, TIndex k2, TIndex k3, TScalar v) => [(([k1],[k1],[k2],[k2],[k3],[k3]),v)] -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
fromListT6' l = fromListT6 indList
        where
            indList = map (\((x1,x2,x3,x4,x5,x6),y) -> ((fromList' x1, fromList' x2, fromList' x3, fromList' x4, fromList' x5, fromList' x6),y)) l

fromListT7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4,  TScalar v) => [(IndTuple7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4, v)] -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
fromListT7 l = foldr (&+) ZeroTensor tensList
    where
        tensList = map mkTens7 l

fromListT7' :: forall n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v . (SingI n1, SingI n2, SingI n3, SingI n4, SingI n5, SingI n6, SingI n7, TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => [(([k1],[k1],[k2],[k2],[k3],[k3],[k4]),v)] -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
fromListT7' l = fromListT7 indList
        where
            indList = map (\((x1,x2,x3,x4,x5,x6,x7),y) -> ((fromList' x1, fromList' x2, fromList' x3, fromList' x4, fromList' x5, fromList' x6, fromList' x7),y)) l

fromListT8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4,  TScalar v) => [(IndTuple8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4, v)] -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
fromListT8 l = foldr (&+) ZeroTensor tensList
    where
        tensList = map mkTens8 l

fromListT8' :: forall n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v . (SingI n1, SingI n2, SingI n3, SingI n4, SingI n5, SingI n6, SingI n7, SingI n8, TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => [(([k1],[k1],[k2],[k2],[k3],[k3],[k4],[k4]),v)] -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
fromListT8' l = fromListT8 indList
        where
            indList = map (\((x1,x2,x3,x4,x5,x6,x7,x8),y) -> ((fromList' x1, fromList' x2, fromList' x3, fromList' x4, fromList' x5, fromList' x6, fromList' x7, fromList' x8),y)) l

--convert a tensor to corresponding assocs list

toListT1 :: AbsTensor1 n1 k1 v -> [(IndTuple1 n1 k1, v)]
toListT1 = toListT

toListT2 :: AbsTensor2 n1 n2 k1 v -> [(IndTuple2 n1 n2 k1, v)]
toListT2 t = concatMap (\(x,y) -> appendT1 x $ toListT y ) $ toListT t

toListT3 :: AbsTensor3 n1 n2 n3 k1 k2 v -> [(IndTuple3 n1 n2 n3 k1 k2, v)]
toListT3 t = concatMap (\(x,y) -> appendT2 x $ toListT y ) $
             concatMap (\(x,y) -> appendT1 x $ toListT y ) $ toListT t

toListT4 :: AbsTensor4 n1 n2 n3 n4 k1 k2 v -> [(IndTuple4 n1 n2 n3 n4 k1 k2, v)]
toListT4 t = concatMap (\(x,y) -> appendT3 x $ toListT y ) $
             concatMap (\(x,y) -> appendT2 x $ toListT y ) $
             concatMap (\(x,y) -> appendT1 x $ toListT y ) $ toListT t

toListT5 :: AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> [(IndTuple5 n1 n2 n3 n4 n5 k1 k2 k3, v)]
toListT5 t = concatMap (\(x,y) -> appendT4 x $ toListT y ) $
             concatMap (\(x,y) -> appendT3 x $ toListT y ) $
             concatMap (\(x,y) -> appendT2 x $ toListT y ) $
             concatMap (\(x,y) -> appendT1 x $ toListT y ) $ toListT t

toListT6 :: AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> [(IndTuple6 n1 n2 n3 n4 n5 n6 k1 k2 k3, v)]
toListT6 t = concatMap (\(x,y) -> appendT5 x $ toListT y ) $
             concatMap (\(x,y) -> appendT4 x $ toListT y ) $
             concatMap (\(x,y) -> appendT3 x $ toListT y ) $
             concatMap (\(x,y) -> appendT2 x $ toListT y ) $
             concatMap (\(x,y) -> appendT1 x $ toListT y ) $ toListT t

toListT7 :: AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> [(IndTuple7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4, v)]
toListT7 t = concatMap (\(x,y) -> appendT6 x $ toListT y ) $
             concatMap (\(x,y) -> appendT5 x $ toListT y ) $
             concatMap (\(x,y) -> appendT4 x $ toListT y ) $
             concatMap (\(x,y) -> appendT3 x $ toListT y ) $
             concatMap (\(x,y) -> appendT2 x $ toListT y ) $
             concatMap (\(x,y) -> appendT1 x $ toListT y ) $ toListT t

toListT8 :: AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> [(IndTuple8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4, v)]
toListT8 t = concatMap (\(x,y) -> appendT7 x $ toListT y ) $
             concatMap (\(x,y) -> appendT6 x $ toListT y ) $
             concatMap (\(x,y) -> appendT5 x $ toListT y ) $
             concatMap (\(x,y) -> appendT4 x $ toListT y ) $
             concatMap (\(x,y) -> appendT3 x $ toListT y ) $
             concatMap (\(x,y) -> appendT2 x $ toListT y ) $
             concatMap (\(x,y) -> appendT1 x $ toListT y ) $ toListT t

appendT1 i = map (\(x,y) -> ((i,x),y))
appendT2 (i1,i2) = map (\(x,y) -> ((i1,i2,x),y))
appendT3 (i1,i2,i3) = map (\(x,y) -> ((i1,i2,i3,x),y))
appendT4 (i1,i2,i3,i4) = map (\(x,y) -> ((i1,i2,i3,i4,x),y))
appendT5 (i1,i2,i3,i4,i5) = map (\(x,y) -> ((i1,i2,i3,i4,i5,x),y))
appendT6 (i1,i2,i3,i4,i5,i6) = map (\(x,y) -> ((i1,i2,i3,i4,i5,i6,x),y))
appendT7 (i1,i2,i3,i4,i5,i6,i7) = map (\(x,y) -> ((i1,i2,i3,i4,i5,i6,i7,x),y))

--convert to non type safe assocs list, all indices regardeless of their type are collected in the [Int] list

toListShow1 :: (TIndex k1, TScalar v) => AbsTensor1 n1 k1 v -> [([Int],v)]
toListShow1 = toListShow

toListShow2 :: (TIndex k1, TScalar v) => AbsTensor2 n1 n2 k1 v -> [([Int],v)]
toListShow2 t = filter (\x -> snd x /= scaleZero) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT2 t
            showInd (i1,i2) = map fromEnum (toList i1) ++ map fromEnum (toList i2)

toListShow3 :: (TIndex k1, TIndex k2, TScalar v) => AbsTensor3 n1 n2 n3 k1 k2 v -> [([Int],v)]
toListShow3 t = filter (\x -> snd x /= scaleZero) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT3 t
            showInd (i1,i2,i3) = map fromEnum (toList i1) ++ map fromEnum (toList i2) ++
                                 map fromEnum (toList i3)

toListShow4 :: (TIndex k1, TIndex k2, TScalar v) => AbsTensor4 n1 n2 n3 n4 k1 k2 v -> [([Int],v)]
toListShow4 t = filter (\x -> snd x /= scaleZero) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT4 t
            showInd (i1,i2,i3,i4) = map fromEnum (toList i1) ++ map fromEnum (toList i2) ++
                                    map fromEnum (toList i3) ++ map fromEnum (toList i4)

toListShow5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> [([Int],v)]
toListShow5 t = filter (\x -> snd x /= scaleZero) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT5 t
            showInd (i1,i2,i3,i4,i5) = map fromEnum (toList i1) ++ map fromEnum (toList i2) ++
                                    map fromEnum (toList i3) ++ map fromEnum (toList i4) ++
                                    map fromEnum (toList i5)

toListShow6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> [([Int],v)]
toListShow6 t = filter (\x -> snd x /= scaleZero) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT6 t
            showInd (i1,i2,i3,i4,i5,i6) = map fromEnum (toList i1) ++ map fromEnum (toList i2) ++
                                        map fromEnum (toList i3) ++ map fromEnum (toList i4) ++
                                        map fromEnum (toList i5) ++ map fromEnum (toList i6)

toListShow7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> [([Int],v)]
toListShow7 t = filter (\x -> snd x /= scaleZero) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT7 t
            showInd (i1,i2,i3,i4,i5,i6,i7) = map fromEnum (toList i1) ++ map fromEnum (toList i2) ++
                                        map fromEnum (toList i3) ++ map fromEnum (toList i4) ++
                                        map fromEnum (toList i5) ++ map fromEnum (toList i6) ++
                                        map fromEnum (toList i7)

toListShow8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> [([Int],v)]
toListShow8 t = filter (\x -> snd x /= scaleZero) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT8 t
            showInd (i1,i2,i3,i4,i5,i6,i7,i8) = map fromEnum (toList i1) ++ map fromEnum (toList i2) ++
                                        map fromEnum (toList i3) ++ map fromEnum (toList i4) ++
                                        map fromEnum (toList i5) ++ map fromEnum (toList i6) ++
                                        map fromEnum (toList i7) ++ map fromEnum (toList i8)


--AnsVar represents the variables in the tensor ansätze -> simply instance of TScalar that represents a single variable that can only occur linearly

newtype AnsVar a = AnsVar (I.IntMap a) deriving (Eq)

shiftVarLabels :: Int -> AnsVar a -> AnsVar a
shiftVarLabels s (AnsVar v) = AnsVar $ I.mapKeys (s +) v

shiftLabels1 :: (TIndex k1, TScalar a) => Int -> AbsTensor1 n1 k1 (AnsVar a) -> AbsTensor1 n1 k1 (AnsVar a)
shiftLabels1 s = mapTo1 (shiftVarLabels s)

shiftLabels2 :: (TIndex k1, TScalar a) => Int -> AbsTensor2 n1 n2 k1 (AnsVar a) -> AbsTensor2 n1 n2 k1 (AnsVar a)
shiftLabels2 s = mapTo2 (shiftVarLabels s)

shiftLabels3 :: (TIndex k1, TIndex k2, TScalar a) =>Int -> AbsTensor3 n1 n2 n3 k1 k2 (AnsVar a) -> AbsTensor3 n1 n2 n3 k1 k2 (AnsVar a)
shiftLabels3 s = mapTo3 (shiftVarLabels s)

shiftLabels4 :: (TIndex k1, TIndex k2, TScalar a) => Int -> AbsTensor4 n1 n2 n3 n4 k1 k2 (AnsVar a) -> AbsTensor4 n1 n2 n3 n4 k1 k2 (AnsVar a)
shiftLabels4 s = mapTo4 (shiftVarLabels s)

shiftLabels5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar a) => Int -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (AnsVar a) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (AnsVar a)
shiftLabels5 s = mapTo5 (shiftVarLabels s)

shiftLabels6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar a) => Int -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (AnsVar a) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (AnsVar a)
shiftLabels6 s = mapTo6 (shiftVarLabels s)

shiftLabels7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar a) => Int -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (AnsVar a) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (AnsVar a)
shiftLabels7 s = mapTo7 (shiftVarLabels s)

shiftLabels8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar a) => Int -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (AnsVar a) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (AnsVar a)
shiftLabels8 s = mapTo8 (shiftVarLabels s)

instance (TScalar a) => TScalar (AnsVar a) where
    addS (AnsVar v1) (AnsVar v2) = AnsVar $ I.unionWith addS v1 v2
    subS (AnsVar v1) (AnsVar v2) = AnsVar $ I.unionWith addS v1 $ I.map (scaleS(-1)) v2
    scaleS s (AnsVar v) = AnsVar $ I.map (scaleS s) v
    scaleZero = AnsVar I.empty

instance TAlgebra Rational (AnsVar Rational) where
    type TAlg Rational (AnsVar Rational) = AnsVar Rational
    prodA = scaleS

instance TAlgebra (AnsVar Rational) Rational where
    type TAlg (AnsVar Rational) Rational = AnsVar Rational
    prodA = flip scaleS

showAnsVar :: Char -> AnsVar Rational -> String
showAnsVar varLabel (AnsVar linMap)
    | null assocs = " "
    | otherwise = tail assocs
        where
            assocs = concatMap (\(x,y) -> "+" ++ showFrac y ++ "*" ++ [varLabel] ++ "[" ++ show x ++ "]") $ filter (\(_,y) -> y /= 0) $ I.assocs linMap
            showSigned x = if x < 0 then "(" ++ show x ++ ")" else show x
            showFrac x = if denominator x == 1 then showSigned (numerator x) else showSigned (numerator x) ++ "/" ++ show (denominator x)

--flatten tensor with ansVar values to assocs list

toListShowVar1 :: (TIndex k1, TScalar a) => AbsTensor1 n1 k1 (AnsVar a) -> [([Int], [(Int, a)])]
toListShowVar1 t = filter (\(_,c) -> c /= []) $ map (\(a,AnsVar b) -> (a, filter (\(_,b) -> b/= scaleZero) $ I.assocs b)) l
        where
            l = toListShow1 t

toListShowVar2 :: (TIndex k1, TScalar a) => AbsTensor2 n1 n2 k1 (AnsVar a) -> [([Int], [(Int, a)])]
toListShowVar2 t = filter (\(_,c) -> c /= []) $ map (\(a,AnsVar b) -> (a, filter (\(_,b) -> b/= scaleZero) $ I.assocs b)) l
        where
            l = toListShow2 t

toListShowVar3 :: (TIndex k1, TIndex k2, TScalar a) => AbsTensor3 n1 n2 n3 k1 k2 (AnsVar a) -> [([Int], [(Int, a)])]
toListShowVar3 t = filter (\(_,c) -> c /= []) $ map (\(a,AnsVar b) -> (a, filter (\(_,b) -> b/= scaleZero) $ I.assocs b)) l
        where
            l = toListShow3 t

toListShowVar4 :: (TIndex k1, TIndex k2, TScalar a) => AbsTensor4 n1 n2 n3 n4 k1 k2 (AnsVar a) -> [([Int], [(Int, a)])]
toListShowVar4 t = filter (\(_,c) -> c /= []) $ map (\(a,AnsVar b) -> (a, filter (\(_,b) -> b/= scaleZero) $ I.assocs b)) l
        where
            l = toListShow4 t

toListShowVar5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar a) => AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (AnsVar a) -> [([Int], [(Int, a)])]
toListShowVar5 t = filter (\(_,c) -> c /= []) $ map (\(a,AnsVar b) -> (a, filter (\(_,b) -> b/= scaleZero) $ I.assocs b)) l
        where
            l = toListShow5 t

toListShowVar6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar a) => AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (AnsVar a)  -> [([Int], [(Int, a)])]
toListShowVar6 t = filter (\(_,c) -> c /= []) $ map (\(a,AnsVar b) -> (a, filter (\(_,b) -> b/= scaleZero) $ I.assocs b)) l
        where
            l = toListShow6 t

toListShowVar7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar a) => AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (AnsVar a) -> [([Int], [(Int, a)])]
toListShowVar7 t = filter (\(_,c) -> c /= []) $ map (\(a,AnsVar b) -> (a, filter (\(_,b) -> b/= scaleZero) $ I.assocs b)) l
        where
            l = toListShow7 t

toListShowVar8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar a) => AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (AnsVar a) -> [([Int], [(Int, a)])]
toListShowVar8 t = filter (\(_,c) -> c /= []) $ map (\(a,AnsVar b) -> (a, filter (\(_,b) -> b/= scaleZero) $ I.assocs b)) l
        where
            l = toListShow8 t

toListShowVarPretty1 :: (TIndex k1) => AbsTensor1 n1 k1 (AnsVar Rational) -> Char -> [([Int], String)]
toListShowVarPretty1 t varLabel = map (\(x,y) -> (x, showAnsVar varLabel y)) $ filter (\(_,AnsVar c) -> c /= I.empty) $ map (\(a,AnsVar b) -> (a, AnsVar $ I.filter (/= scaleZero) b)) l
        where
            l = toListShow1 t

toListShowVarPretty2 :: (TIndex k1) => AbsTensor2 n1 n2 k1 (AnsVar Rational) -> Char -> [([Int], String)]
toListShowVarPretty2 t varLabel = map (\(x,y) -> (x, showAnsVar varLabel y)) $ filter (\(_,AnsVar c) -> c /= I.empty) $ map (\(a,AnsVar b) -> (a, AnsVar $ I.filter (/= scaleZero) b)) l
        where
            l = toListShow2 t

toListShowVarPretty3 :: (TIndex k1, TIndex k2) => AbsTensor3 n1 n2 n3 k1 k2 (AnsVar Rational) -> Char -> [([Int], String)]
toListShowVarPretty3 t varLabel = map (\(x,y) -> (x, showAnsVar varLabel y)) $ filter (\(_,AnsVar c) -> c /= I.empty) $ map (\(a,AnsVar b) -> (a, AnsVar $ I.filter (/= scaleZero) b)) l
        where
            l = toListShow3 t

toListShowVarPretty4 :: (TIndex k1, TIndex k2, TScalar a) => AbsTensor4 n1 n2 n3 n4 k1 k2 (AnsVar Rational) -> Char -> [([Int], String)]
toListShowVarPretty4 t varLabel = map (\(x,y) -> (x, showAnsVar varLabel y)) $ filter (\(_,AnsVar c) -> c /= I.empty) $ map (\(a,AnsVar b) -> (a, AnsVar $ I.filter (/= scaleZero) b)) l
        where
            l = toListShow4 t

toListShowVarPretty5 :: (TIndex k1, TIndex k2, TIndex k3) => AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (AnsVar Rational) -> Char -> [([Int], String)]
toListShowVarPretty5 t varLabel = map (\(x,y) -> (x, showAnsVar varLabel y)) $ filter (\(_,AnsVar c) -> c /= I.empty) $ map (\(a,AnsVar b) -> (a, AnsVar $ I.filter (/= scaleZero) b)) l
        where
            l = toListShow5 t

toListShowVarPretty6 :: (TIndex k1, TIndex k2, TIndex k3) => AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (AnsVar Rational) -> Char -> [([Int], String)]
toListShowVarPretty6 t varLabel = map (\(x,y) -> (x, showAnsVar varLabel y)) $ filter (\(_,AnsVar c) -> c /= I.empty) $ map (\(a,AnsVar b) -> (a, AnsVar $ I.filter (/= scaleZero) b)) l
        where
            l = toListShow6 t

toListShowVarPretty7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (AnsVar Rational) -> Char -> [([Int], String)]
toListShowVarPretty7 t varLabel = map (\(x,y) -> (x, showAnsVar varLabel y)) $ filter (\(_,AnsVar c) -> c /= I.empty) $ map (\(a,AnsVar b) -> (a, AnsVar $ I.filter (/= scaleZero) b)) l
        where
            l = toListShow7 t

toListShowVarPretty8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (AnsVar Rational) -> Char -> [([Int], String)]
toListShowVarPretty8 t varLabel = map (\(x,y) -> (x, showAnsVar varLabel y)) $ filter (\(_,AnsVar c) -> c /= I.empty) $ map (\(a,AnsVar b) -> (a, AnsVar $ I.filter (/= scaleZero) b)) l
        where
            l = toListShow8 t

--write the tensor data into a matrix: columns label the occuring AnsVars (note that this is possible as we restricted to the case where the vars only occur linearly)
--rows label the non zero entries in the tensor

toMatList1' :: (TIndex k1, TScalar a) => AbsTensor1 n1 k1 (AnsVar a) -> [[(Int, a)]]
toMatList1' t = map snd $ toListShowVar1 t

toMatList2' :: (TIndex k1, TScalar a) => AbsTensor2 n1 n2 k1 (AnsVar a) -> [[(Int, a)]]
toMatList2' t = map snd $ toListShowVar2 t

toMatList3' :: (TIndex k1, TIndex k2, TScalar a) => AbsTensor3 n1 n2 n3 k1 k2 (AnsVar a) -> [[(Int, a)]]
toMatList3' t = map snd $ toListShowVar3 t

toMatList4' :: (TIndex k1, TIndex k2, TScalar a) => AbsTensor4 n1 n2 n3 n4 k1 k2 (AnsVar a) -> [[(Int, a)]]
toMatList4' t = map snd $ toListShowVar4 t

toMatList5' :: (TIndex k1, TIndex k2, TIndex k3, TScalar a) => AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (AnsVar a) -> [[(Int, a)]]
toMatList5' t = map snd $ toListShowVar5 t

toMatList6' :: (TIndex k1, TIndex k2, TIndex k3, TScalar a) => AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (AnsVar a) -> [[(Int, a)]]
toMatList6' t = map snd $ toListShowVar6 t

toMatList7' :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar a) => AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (AnsVar a) -> [[(Int, a)]]
toMatList7' t = map snd $ toListShowVar7 t

toMatList8' :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar a) => AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (AnsVar a) -> [[(Int, a)]]
toMatList8' t = map snd $ toListShowVar8 t

normalize :: [(Int,Rational)] -> ([(Int,Rational)],Rational)
normalize [] = ([],1)
normalize ((a,b) : xs) = ((a,1) : map (\(x,y) -> (x,y / b)) xs,b)

--convert several tensor to one matrixList
--types for heterogenic lists of tensors of different rank but of the syme algebraic type, i.e. same index spaces

data TensList1 k1 v where
    EmptyTList1 :: TensList1 k1 v
    AppendTList1 :: AbsTensor1 n1 k1 v -> TensList1 k1 v -> TensList1 k1 v

data TensList2 k1 v where
    EmptyTList2 :: TensList2 k1 v
    AppendTList2 :: AbsTensor2 n1 n2 k1 v -> TensList2 k1 v -> TensList2 k1 v

data TensList3 k1 k2 v where
    EmptyTList3 :: TensList3 k1 k2 v
    AppendTList3 :: AbsTensor3 n1 n2 n3 k1 k2 v -> TensList3 k1 k2 v -> TensList3 k1 k2 v

data TensList4 k1 k2 v where
    EmptyTList4 :: TensList4 k1 k2 v
    AppendTList4 :: AbsTensor4 n1 n2 n3 n4 k1 k2 v -> TensList4 k1 k2 v -> TensList4 k1 k2 v

data TensList5 k1 k2 k3 v where
    EmptyTList5 :: TensList5 k1 k2 k3 v
    AppendTList5 :: AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> TensList5 k1 k2 k3 v -> TensList5 k1 k2 k3 v

data TensList6 k1 k2 k3 v where
    EmptyTList6 :: TensList6 k1 k2 k3 v
    AppendTList6 :: AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> TensList6 k1 k2 k3 v -> TensList6 k1 k2 k3 v

data TensList7 k1 k2 k3 k4 v where
    EmptyTList7 :: TensList7 k1 k2 k3 k4 v
    AppendTList7 :: AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> TensList7 k1 k2 k3 k4 v -> TensList7 k1 k2 k3 k4 v

data TensList8 k1 k2 k3 k4 v where
    EmptyTList8 :: TensList8 k1 k2 k3 k4 v
    AppendTList8 :: AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> TensList8 k1 k2 k3 k4 v -> TensList8 k1 k2 k3 k4 v

--map over heterogenic lists

mapTensList1 :: (forall n1. AbsTensor1 n1 k1 v -> b ) -> TensList1 k1 v -> [b]
mapTensList1 f EmptyTList1 = []
mapTensList1 f (AppendTList1 t l) = f t : mapTensList1 f l

mapTensList2 :: (forall n1 n2. AbsTensor2 n1 n2 k1 v -> b ) -> TensList2 k1 v -> [b]
mapTensList2 f EmptyTList2 = []
mapTensList2 f (AppendTList2 t l) = f t : mapTensList2 f l

mapTensList3 :: (forall n1 n2 n3. AbsTensor3 n1 n2 n3 k1 k2 v -> b ) -> TensList3 k1 k2 v -> [b]
mapTensList3 f EmptyTList3 = []
mapTensList3 f (AppendTList3 t l) = f t : mapTensList3 f l

mapTensList4 :: (forall n1 n2 n3 n4. AbsTensor4 n1 n2 n3 n4 k1 k2 v -> b ) -> TensList4 k1 k2 v -> [b]
mapTensList4 f EmptyTList4 = []
mapTensList4 f (AppendTList4 t l) = f t : mapTensList4 f l

mapTensList5 :: (forall n1 n2 n3 n4 n5. AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> b ) -> TensList5 k1 k2 k3 v -> [b]
mapTensList5 f EmptyTList5 = []
mapTensList5 f (AppendTList5 t l) = f t : mapTensList5 f l

mapTensList6 :: (forall n1 n2 n3 n4 n5 n6. AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> b ) -> TensList6 k1 k2 k3 v -> [b]
mapTensList6 f EmptyTList6 = []
mapTensList6 f (AppendTList6 t l) = f t : mapTensList6 f l

mapTensList7 :: (forall n1 n2 n3 n4 n5 n6 n7. AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> b ) -> TensList7 k1 k2 k3 k4 v -> [b]
mapTensList7 f EmptyTList7 = []
mapTensList7 f (AppendTList7 t l) = f t : mapTensList7 f l

mapTensList8 :: (forall n1 n2 n3 n4 n5 n6 n7 n8. AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> b ) -> TensList8 k1 k2 k3 k4 v -> [b]
mapTensList8 f EmptyTList8 = []
mapTensList8 f (AppendTList8 t l) = f t : mapTensList8 f l

--heterogenic tensor list construction from tensors

infixr 5 ...>

(...>) :: AbsTensor1 n1 k1 v -> TensList1 k1 v -> TensList1 k1 v
(...>) = AppendTList1

singletonTList1 :: AbsTensor1 n1 k1 v -> TensList1 k1 v
singletonTList1 t = t ...> EmptyTList1

infixr 6 ...+

(...+) :: TensList1 k1 v -> TensList1 k1 v -> TensList1 k1 v
(...+) EmptyTList1 t1 = t1
(...+) t1 EmptyTList1 = t1
(...+) (AppendTList1 t1 EmptyTList1) t2 = AppendTList1 t1 t2
(...+) (AppendTList1 t1 t1') t2 = AppendTList1 t1 (t1' ...+ t2)


infixr 5 ..&>

(..&>) :: AbsTensor2 n1 n2 k1 v -> TensList2 k1 v -> TensList2 k1 v
(..&>) = AppendTList2

singletonTList2 :: AbsTensor2 n1 n2 k1 v -> TensList2 k1 v
singletonTList2 t = t ..&> EmptyTList2

infixr 6 ..&+

(..&+) :: TensList2 k1 v -> TensList2 k1 v -> TensList2 k1 v
(..&+) EmptyTList2 t1 = t1
(..&+) t1 EmptyTList2 = t1
(..&+) (AppendTList2 t1 EmptyTList2) t2 = AppendTList2 t1 t2
(..&+) (AppendTList2 t1 t1') t2 = AppendTList2 t1 (t1' ..&+ t2)


infixr 5 .&.>

(.&.>) :: AbsTensor3 n1 n2 n3 k1 k2 v -> TensList3 k1 k2 v -> TensList3 k1 k2 v
(.&.>) = AppendTList3

singletonTList3 :: AbsTensor3 n1 n2 n3 k1 k2 v -> TensList3 k1 k2 v
singletonTList3 t = t .&.> EmptyTList3

infixr 6 .&.+

(.&.+) :: TensList3 k1 k2 v -> TensList3 k1 k2 v -> TensList3 k1 k2 v
(.&.+) EmptyTList3 t1 = t1
(.&.+) t1 EmptyTList3 = t1
(.&.+) (AppendTList3 t1 EmptyTList3) t2 = AppendTList3 t1 t2
(.&.+) (AppendTList3 t1 t1') t2 = AppendTList3 t1 (t1' .&.+ t2)


infixr 5 .&&>

(.&&>) :: AbsTensor4 n1 n2 n3 n4 k1 k2 v -> TensList4 k1 k2 v -> TensList4 k1 k2 v
(.&&>) = AppendTList4

singletonTList4 :: AbsTensor4 n1 n2 n3 n4 k1 k2 v -> TensList4 k1 k2 v
singletonTList4 t = t .&&> EmptyTList4

infixr 6 .&&+

(.&&+) :: TensList4 k1 k2 v -> TensList4 k1 k2 v -> TensList4 k1 k2 v
(.&&+) EmptyTList4 t1 = t1
(.&&+) t1 EmptyTList4 = t1
(.&&+) (AppendTList4 t1 EmptyTList4) t2 = AppendTList4 t1 t2
(.&&+) (AppendTList4 t1 t1') t2 = AppendTList4 t1 (t1' .&&+ t2)


infixr 5 &..>

(&..>) :: AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> TensList5 k1 k2 k3 v -> TensList5 k1 k2 k3 v
(&..>) = AppendTList5

singletonTList5 :: AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> TensList5 k1 k2 k3 v
singletonTList5 t = t &..> EmptyTList5

infixr 6 &..+

(&..+) :: TensList5 k1 k2 k3 v -> TensList5 k1 k2 k3 v -> TensList5 k1 k2 k3 v
(&..+) EmptyTList5 t1 = t1
(&..+) t1 EmptyTList5 = t1
(&..+) (AppendTList5 t1 EmptyTList5) t2 = AppendTList5 t1 t2
(&..+) (AppendTList5 t1 t1') t2 = AppendTList5 t1 (t1' &..+ t2)


infixr 5 &.&>

(&.&>) :: AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> TensList6 k1 k2 k3 v -> TensList6 k1 k2 k3 v
(&.&>) = AppendTList6

singletonTList6 :: AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> TensList6 k1 k2 k3 v
singletonTList6 t = t &.&> EmptyTList6

infixr 6 &.&+

(&.&+) :: TensList6 k1 k2 k3 v -> TensList6 k1 k2 k3 v -> TensList6 k1 k2 k3 v
(&.&+) EmptyTList6 t1 = t1
(&.&+) t1 EmptyTList6 = t1
(&.&+) (AppendTList6 t1 EmptyTList6) t2 = AppendTList6 t1 t2
(&.&+) (AppendTList6 t1 t1') t2 = AppendTList6 t1 (t1' &.&+ t2)


infixr 5 &&.>

(&&.>) :: AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> TensList7 k1 k2 k3 k4 v -> TensList7 k1 k2 k3 k4 v
(&&.>) = AppendTList7

singletonTList7 :: AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> TensList7 k1 k2 k3 k4 v
singletonTList7 t = t &&.> EmptyTList7

infixr 6 &&.+

(&&.+) :: TensList7 k1 k2 k3 k4 v -> TensList7 k1 k2 k3 k4 v -> TensList7 k1 k2 k3 k4 v
(&&.+) EmptyTList7 t1 = t1
(&&.+) t1 EmptyTList7 = t1
(&&.+) (AppendTList7 t1 EmptyTList7) t2 = AppendTList7 t1 t2
(&&.+) (AppendTList7 t1 t1') t2 = AppendTList7 t1 (t1' &&.+ t2)


infixr 5 &&&>

(&&&>) :: AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> TensList8 k1 k2 k3 k4 v -> TensList8 k1 k2 k3 k4 v
(&&&>) = AppendTList8

singletonTList8 :: AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> TensList8 k1 k2 k3 k4 v
singletonTList8 t = t &&&> EmptyTList8

infixr 6 &&&+

(&&&+) :: TensList8 k1 k2 k3 k4 v -> TensList8 k1 k2 k3 k4 v -> TensList8 k1 k2 k3 k4 v
(&&&+) EmptyTList8 t1 = t1
(&&&+) t1 EmptyTList8 = t1
(&&&+) (AppendTList8 t1 EmptyTList8) t2 = AppendTList8 t1 t2
(&&&+) (AppendTList8 t1 t1') t2 = AppendTList8 t1 (t1' &&&+ t2)


--collect data of heterogenic tensor list in one sparse matrix assocs list
--intendet for evaluating tensorial equations: the values are only collected up to overall factors

collectMatList :: [[(Int, Rational)]] -> [((Int, Int), Rational)]
collectMatList matList = l'
    where
        l2 = nubBy (\(a,_) (b,_) -> a == b) $ map normalize matList
        l = map (\(x,y) -> map (\(a,b) -> (a,b*y)) x) l2
        l' = concat $ zipWith (\r z -> map (\(x,y) -> ((z, x), y)) r) l [1..]

toMatList1 :: (TIndex k1) => TensList1 k1 (AnsVar Rational) -> [((Int,Int),Rational)]
toMatList1 t = collectMatList matList
    where
        matList = concat $ mapTensList1 toMatList1' t

toMatList2 :: (TIndex k1) => TensList2 k1 (AnsVar Rational) -> [((Int,Int),Rational)]
toMatList2 t = collectMatList matList
    where
        matList = concat $ mapTensList2 toMatList2' t

toMatList3 :: (TIndex k1, TIndex k2) => TensList3 k1 k2 (AnsVar Rational) -> [((Int,Int),Rational)]
toMatList3 t = collectMatList matList
    where
        matList = concat $ mapTensList3 toMatList3' t

toMatList4 :: (TIndex k1, TIndex k2) => TensList4 k1 k2 (AnsVar Rational) -> [((Int,Int),Rational)]
toMatList4 t = collectMatList matList
    where
        matList = concat $ mapTensList4 toMatList4' t

toMatList5 :: (TIndex k1, TIndex k2, TIndex k3) => TensList5 k1 k2 k3 (AnsVar Rational) -> [((Int,Int),Rational)]
toMatList5 t = collectMatList matList
    where
        matList = concat $ mapTensList5 toMatList5' t

toMatList6 :: (TIndex k1, TIndex k2, TIndex k3) => TensList6 k1 k2 k3 (AnsVar Rational) -> [((Int,Int),Rational)]
toMatList6 t = collectMatList matList
    where
        matList = concat $ mapTensList6 toMatList6' t

toMatList7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => TensList7 k1 k2 k3 k4 (AnsVar Rational) -> [((Int,Int),Rational)]
toMatList7 t = collectMatList matList
    where
        matList = concat $ mapTensList7 toMatList7' t

toMatList8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => TensList8 k1 k2 k3 k4 (AnsVar Rational) -> [((Int,Int),Rational)]
toMatList8 t = collectMatList matList
    where
        matList = concat $ mapTensList8 toMatList8' t


--convert to Eigen format for using LA subroutines

dims :: [((Int, Int), a)] -> (Int, Int)
dims xs = (rows, cols)
    where
        rows = maximum $ map (fst.fst) xs
        cols = maximum $ map (snd.fst) xs

assocsToSparse :: [((Int, Int), Rational)] -> Sparse.SparseMatrixXd
assocsToSparse assocs = Sparse.fromList rows cols els
    where
        (rows, cols) = dims assocs
        els          = map (\((x, y), z) -> (x-1, y-1, fromRational z)) assocs

toEMatrix1 :: (TIndex k1) => TensList1 k1 (AnsVar Rational) -> Sparse.SparseMatrixXd
toEMatrix1 = assocsToSparse . toMatList1

toEMatrix2 :: (TIndex k1) => TensList2 k1 (AnsVar Rational) -> Sparse.SparseMatrixXd
toEMatrix2 = assocsToSparse . toMatList2

toEMatrix3 :: (TIndex k1, TIndex k2) => TensList3 k1 k2 (AnsVar Rational) -> Sparse.SparseMatrixXd
toEMatrix3 = assocsToSparse . toMatList3

toEMatrix4 :: (TIndex k1, TIndex k2) => TensList4 k1 k2 (AnsVar Rational) -> Sparse.SparseMatrixXd
toEMatrix4 = assocsToSparse . toMatList4

toEMatrix5 :: (TIndex k1, TIndex k2, TIndex k3) => TensList5 k1 k2 k3 (AnsVar Rational) -> Sparse.SparseMatrixXd
toEMatrix5 = assocsToSparse . toMatList5

toEMatrix6 :: (TIndex k1, TIndex k2, TIndex k3) => TensList6 k1 k2 k3 (AnsVar Rational) -> Sparse.SparseMatrixXd
toEMatrix6 = assocsToSparse . toMatList6

toEMatrix7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => TensList7 k1 k2 k3 k4 (AnsVar Rational) -> Sparse.SparseMatrixXd
toEMatrix7 = assocsToSparse . toMatList7

toEMatrix8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => TensList8 k1 k2 k3 k4 (AnsVar Rational) -> Sparse.SparseMatrixXd
toEMatrix8 = assocsToSparse . toMatList8

--rank of the tensor can be computed with rank Sol.FullPivLU or Sol.JakobiSVD

tensorRank1' :: (TIndex k1) => AbsTensor1 n1 k1 (AnsVar Rational) -> Int
tensorRank1' t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrix1 (singletonTList1 t)

tensorRank1 :: (TIndex k1) => TensList1 k1 (AnsVar Rational) -> Int
tensorRank1 t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrix1 t


tensorRank2' :: (TIndex k1) => AbsTensor2 n1 n2 k1 (AnsVar Rational) -> Int
tensorRank2' t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrix2 (singletonTList2 t)

tensorRank2 :: (TIndex k1) => TensList2 k1 (AnsVar Rational) -> Int
tensorRank2 t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrix2 t


tensorRank3' :: (TIndex k1, TIndex k2) => AbsTensor3 n1 n2 n3 k1 k2 (AnsVar Rational) -> Int
tensorRank3' t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrix3 (singletonTList3 t)

tensorRank3 :: (TIndex k1, TIndex k2) =>  TensList3 k1 k2 (AnsVar Rational) -> Int
tensorRank3 t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrix3 t


tensorRank4' :: (TIndex k1, TIndex k2) =>  AbsTensor4 n1 n2 n3 n4 k1 k2 (AnsVar Rational) -> Int
tensorRank4' t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrix4 (singletonTList4 t)

tensorRank4 :: (TIndex k1, TIndex k2) =>  TensList4 k1 k2 (AnsVar Rational) -> Int
tensorRank4 t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrix4 t


tensorRank5' :: (TIndex k1, TIndex k2, TIndex k3) =>  AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (AnsVar Rational) -> Int
tensorRank5' t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrix5 (singletonTList5 t)

tensorRank5 :: (TIndex k1, TIndex k2, TIndex k3) => TensList5 k1 k2 k3 (AnsVar Rational) -> Int
tensorRank5 t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrix5 t


tensorRank6' :: (TIndex k1, TIndex k2, TIndex k3) => AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (AnsVar Rational) -> Int
tensorRank6' t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrix6 (singletonTList6 t)

tensorRank6 :: (TIndex k1, TIndex k2, TIndex k3) => TensList6 k1 k2 k3 (AnsVar Rational) -> Int
tensorRank6 t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrix6 t


tensorRank7' :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (AnsVar Rational) -> Int
tensorRank7' t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrix7 (singletonTList7 t)

tensorRank7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => TensList7 k1 k2 k3 k4 (AnsVar Rational) -> Int
tensorRank7 t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrix7 t


tensorRank8' :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (AnsVar Rational) -> Int
tensorRank8' t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrix8 (singletonTList8 t)

tensorRank8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => TensList8 k1 k2 k3 k4 (AnsVar Rational) -> Int
tensorRank8 t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrix8 t


--finally explicitly provide the necessary instances for standard spacetime tensors and some further standard tensors
--Indi has a range from 0 to i

newtype Ind1 =  Ind1 {indVal1 :: Int}
    deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)

instance TIndex Ind1 where
    indRange x = 2

instance Enum Ind1 where
    toEnum = Ind1
    fromEnum = indVal1


newtype Ind2 =  Ind2 {indVal2 :: Int}
    deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)

instance TIndex Ind2 where
    indRange x = 3

instance Enum Ind2 where
    toEnum = Ind2
    fromEnum = indVal2


newtype Ind3 =  Ind3 {indVal3 :: Int}
    deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)

instance TIndex Ind3 where
    indRange x = 4

instance Enum Ind3 where
    toEnum = Ind3
    fromEnum = indVal3


newtype Ind9 =  Ind9 {indVal9 :: Int}
    deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)

instance TIndex Ind9 where
        indRange x = 10

instance Enum Ind9 where
        toEnum = Ind9
        fromEnum = indVal9

newtype Ind20 =  Ind20 {indVal20 :: Int}
    deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)

instance TIndex Ind20 where
        indRange x = 21

instance Enum Ind20 where
        toEnum = Ind20
        fromEnum = indVal20

type STTens n1 n2 v = AbsTensor2 n1 n2 Ind3 v

type IndTupleST n1 n2 = (IndList n1 Ind3, IndList n2 Ind3)

type ATens n1 n2 n3 n4 n5 n6 v = AbsTensor6 n1 n2 n3 n4 n5 n6 Ind20 Ind9 Ind3 v

type IndTupleAbs n1 n2 n3 n4 n5 n6 = (IndList n1 Ind20, IndList n2 Ind20 , IndList n3 Ind9, IndList n4 Ind9, IndList n5 Ind3, IndList n6 Ind3)
