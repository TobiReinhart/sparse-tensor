-----------------------------------------------------------------------------
-- |
-- Module      :  Math.Tensor
-- Copyright   :  (c) 2019 Tobias Reinhart and Nils Alex
-- License     :  MIT
-- Maintainer  :  tobi.reinhart@fau.de, nils.alex@fau.de
--
--
-- This module defines the basic data types and functions of the sparse-tensor package.
--
-- The @'Tensor' n k v@  data type provides the fundamental building block of all further tensor types.
-- It represents a general tensor that takes @n@ individual indices all belonging
-- to the same index typ @k@ and retrieves values of type @v@. As such the @'Tensor'@ type can be thought of representing a single tensor that only features
-- contravariant indices.
--
-- @'Tensor' n k v@ is internally implemented as an ordered forest with nodes of type @'k'@ and leafs of type @'v'@.
--
-- Additional covariant indices can be incorporated by adjoining leafs of type @'Tensor' n2 k v@ to a @'Tensor' n1 k v@.
-- This yields the type @'Tensor2' n1 n2 k v@ which thus is used to represent the tensors in the traditional sense that include contravariant and covariant indices of the same type, i.e. running over the same index range.
--
-- Recursively appending further @'Tensor'@s as leafs each one with possibly different
-- index types and different ranks allows the sparse-tensor package cover the treatment of more complicated tensors with an arbitrary number of different indices.
--
-- The @'Tensor'@ data type directly incorporates its rank in form of a type level natural number @n@. This results in added type safety when performing the usual
-- tensor algebra operations. To provide a simple example the addition of two tensors is only meaningful if the ranks of the two tensors coincide. Hence then sparse-tensor package only incorporates an addition of @'Tensor'@s with the same type.
-- Unintentional additions of @'Tensor'@s with different type then immediately yields a type error thus preventing the mistake.
--
-- Furthermore the @'Tensor'@ type employs a sparse storage paradigm in the sense that when constructing @'Tensor'@s only non zero values must be specified.
-- Missing values are then taken as vanishing automatically.
-----------------------------------------------------------------------------

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

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}

module Math.Tensor (
-- * Tensor Data Types
-- ** Length Typed Index List
--
-- | @'Tensor'@s provide information regarding their number of indices, i.e. their rank in their type. Consequently when constructing them from lists of indices value pairs and
-- converting them to such it is advantageous when also these lists contain the additional length information in their type.
IndList(..),
--
singletonInd, (+>), fromList, fromListMaybe,
headInd, tailInd, sortInd, updateInd,
-- ** The Tensor Type
--
-- | The basic tensor type @'Tensor' n k v@ represents a tensor that takes @n@ indices of type @k@ and maps them to values of type @v@.
-- This type can be thought of as representing a single, purely contravariant tensor that features @n@ indices.
--
-- A general abstract tensor with multiple possibly different indices is obtained by simply adjoining the appropriate number of individual basic tensors.
--
-- The @'Tensor'@ type is internally implemented as ordered forest with nodes being the individual indices and leafs given by the corresponding values.
--
TMap,
Tensor(..),
--
Tensor2,
--
-- | __The following types are synonyms that are used to represent tensors with multiple different indices. For instance @'AbsTensor4'@ can be used__
-- __to represent any tensor that features two different index types with different index ranges that both appear in contravariant and covariant position.__
AbsTensor1, AbsTensor2, AbsTensor3, AbsTensor4, AbsTensor5, AbsTensor6, AbsTensor7, AbsTensor8,
--
STTens, ATens,
--
-- ** Index Type Class
--
TIndex(..),
--
Ind3(..), Ind9(..), Ind20(..),
--
-- ** Tensor Value Type Class
--
-- | Types that any @'Tensor'@ might use as values must satisfy number like properties, more precisely they must constitute an additive group.
-- This requirement is encoded in the @'TAdd'@ type class.
--
-- Allowing further for the computation of tensor products the @'Tensor'@ types resemble the structure of the usual graded tensor algebra on the type level.
-- When computing tensor products of different ranked tensors and also tensors with different value type it is thus necessary that the two @'Tensor'@ types provide the
-- information regarding the explicit type of their product. This is the case if the two value types provide an instance of the @'Prod'@ type class.
--
-- If the values of a given tensor are instances of these two type classes also tensor type  @'Tensor'@ itself represents an instance of them.
-- This allows the use of the basic tensor algebra functions and also many further tensor functions irrespective of the values of a given @'Tensor'@
-- are themselves @'Tensor'@s or represent simple scalar values.
--
TAdd(..),
--
Prod(..),
--
-- ** Tensor Value Instances
-- *** Scalar Values
--
SField(..),
--
-- *** Linear Variables
AnsVar(..), AnsVarR,
--
shiftVarLabels,
--
-- | __The following functions apply this shift of the variable labels, i.e. the function @'shiftVarLabels'@ to all @'AnsVar'@s that are contained in a @'Tensor'@.__
shiftLabels1, shiftLabels2, shiftLabels3, shiftLabels4, shiftLabels5, shiftLabels6, shiftLabels7, shiftLabels8,
--
-- *** Functions as Values
--
CFun(..), evalSec,
--
-- ** Construction of Tensor
-- | @'Tensor'@s can most easily be constructed from key value list where the keys are tuples of either @'IndList'@s or simply traditional lists that encode the values of the
-- various indices and the values then are the corresponding @'Tensor'@ values. While the construction from tuples of normal lists is certainly more flexible
-- it lacks the desired type safety that is incorporated when construction is achieved from tuples of length typed @'IndList'@s.
--
-- __The following are type synonyms that are frequently for these key value lists.__
IndTuple1, IndTuple2, IndTuple3, IndTuple4, IndTuple5, IndTuple6, IndTuple7, IndTuple8,
--
IndTupleST, IndTupleAbs,
--
fromListT,
--
-- | __The following functions employ the construction of different @'Tensor'@ types from such typed key value lists.__
fromListT1, fromListT2, fromListT3, fromListT4, fromListT5, fromListT6, fromListT7, fromListT8,
--
fromListT',
--
-- | __The following functions employ the construction of different @'Tensor'@ types from un-typed key value lists.__
fromListT1', fromListT2', fromListT3', fromListT4', fromListT5', fromListT6', fromListT7', fromListT8',
--
-- ** Accessing Deeper Leaf Levels
-- | Given @'Tensor'@s with multiple index types, i.e. @'Tensor'@s that themselves have further @'Tensor'@s attached as their leafs, i.e. as their @'Scalar'@s one needs a way of accessing these deeper @'Tensor'@ levels. This is precisely achieved by the following functions.
-- They allow a general function that takes @'Tensor'@s as arguments to be applied to @'Tensor'@s that are attached in deeper levels.
--
mapTo1, mapTo2, mapTo3, mapTo4, mapTo5, mapTo6, mapTo7, mapTo8,
--
-- * Tensor Algebra
--
-- ** Basic Tensor Algebra Operations
--
-- | The following functions provide the @'Tensor'@ type with the structure of the usual tensor algebra.
(&+), negateTens, (&*), (&-), (&.),
--
--
tensorContr,
--
-- | __If the indices that are to be contracted do not correspond to the first index type of the given @'Tensor'@ the following functions can be used.__
contrATens1, contrATens2, contrATens3, contrATens4,
--
-- ** Rearranging indices
--
-- *** Swapping 2 Indices
tensorTrans,
--
-- | __If the indices that are to be transposed do not correspond to the first index type of the given @'Tensor'@ the following functions can be used.__
tensorTrans1, tensorTrans2, tensorTrans3, tensorTrans4, tensorTrans5, tensorTrans6, tensorTrans7, tensorTrans8,
--
-- *** Swapping 2 index Blocks
tensorBlockTrans,
--
-- | __If the index blocks that are to be transposed do not correspond to the first index type of the given @'Tensor'@ the following functions can be used.__
tensorBlockTrans1, tensorBlockTrans2, tensorBlockTrans3, tensorBlockTrans4, tensorBlockTrans5, tensorBlockTrans6, tensorBlockTrans7, tensorBlockTrans8,
--
-- *** Permuting indices
resortTens,
--
-- | __If the indices that are to be permuted do not correspond to the first index type of the given @'Tensor'@ the following functions can be used.__
resortTens1, resortTens2, resortTens3, resortTens4, resortTens5, resortTens6, resortTens7, resortTens8,
--
-- ** Symmetrization of Tensors
--
-- *** Pair Symmetrization
symTens,
--
-- | __If the indices in which the @'Tensor'@ shall be symmetrized do not correspond to the first index type of the given @'Tensor'@ the following functions can be used.__
symATens1, symATens2, symATens3, symATens4, symATens5, symATens6, symATens7, symATens8,
--
symTensFac,
--
-- | __If the indices in which the @'Tensor'@ shall be symmetrized do not correspond to the first index type of the given @'Tensor'@ the following functions can be used.__
symATensFac1, symATensFac2, symATensFac3, symATensFac4, symATensFac5, symATensFac6, symATensFac7, symATensFac8,
--
-- *** Pair Anti Symmetrization
aSymTens,
--
-- | __If the indices in which the @'Tensor'@ shall be anti symmetrized do not correspond to the first index type of the given @'Tensor'@ the following functions can be used.__
aSymATens1, aSymATens2, aSymATens3, aSymATens4, aSymATens5, aSymATens6, aSymATens7, aSymATens8,
--
aSymTensFac,
--
-- | __If the indices in which the @'Tensor'@ shall be anti symmetrized do not correspond to the first index type of the given @'Tensor'@ the following functions can be used.__
aSymATensFac1, aSymATensFac2, aSymATensFac3, aSymATensFac4, aSymATensFac5, aSymATensFac6, aSymATensFac7, aSymATensFac8,
--
-- *** Block Exchange Symmetrization
symBlockTens,
--
-- | __If the index blocks in which the @'Tensor'@ shall be symmetrized do not correspond to the first index type of the given @'Tensor'@ the following functions can be used.__
symBlockATens1, symBlockATens2, symBlockATens3, symBlockATens4, symBlockATens5, symBlockATens6, symBlockATens7, symBlockATens8,
--
symBlockTensFac,
--
-- | __If the index blocks in which the @'Tensor'@ shall be symmetrized do not correspond to the first index type of the given @'Tensor'@ the following functions can be used.__
symBlockATensFac1, symBlockATensFac2, symBlockATensFac3, symBlockATensFac4, symBlockATensFac5, symBlockATensFac6, symBlockATensFac7, symBlockATensFac8,
--
-- *** Block Exchange Anti Symmetrization
aSymBlockTens,
--
-- | __If the index blocks in which the @'Tensor'@ shall be anti symmetrized do not correspond to the first index type of the given @'Tensor'@ the following functions can be used.__
aSymBlockATens1, aSymBlockATens2, aSymBlockATens3, aSymBlockATens4, aSymBlockATens5, aSymBlockATens6, aSymBlockATens7, aSymBlockATens8,
--
aSymBlockTensFac,
--
-- | __If the index blocks in which the @'Tensor'@ shall be anti symmetrized do not correspond to the first index type of the given @'Tensor'@ the following functions can be used.__
aSymBlockATensFac1, aSymBlockATensFac2, aSymBlockATensFac3, aSymBlockATensFac4, aSymBlockATensFac5, aSymBlockATensFac6, aSymBlockATensFac7, aSymBlockATensFac8,
--
-- *** Cyclic Symmetrization
cyclicSymTens,
--
-- | __If the indices in which the @'Tensor'@ shall be symmetrized do not correspond to the first index type of the given @'Tensor'@ the following functions can be used.__
cyclicSymATens1, cyclicSymATens2, cyclicSymATens3, cyclicSymATens4, cyclicSymATens5, cyclicSymATens6, cyclicSymATens7, cyclicSymATens8,
--
cyclicSymTensFac,
--
-- | __If the indices in which the @'Tensor'@ shall be symmetrized do not correspond to the first index type of the given @'Tensor'@ the following functions can be used.__
cyclicSymATensFac1, cyclicSymATensFac2, cyclicSymATensFac3, cyclicSymATensFac4, cyclicSymATensFac5, cyclicSymATensFac6, cyclicSymATensFac7, cyclicSymATensFac8,
--
-- *** Cyclic Anti Symmetrization
cyclicASymTens,
--
-- | __If the indices in which the @'Tensor'@ shall be anti symmetrized do not correspond to the first index type of the given @'Tensor'@ the following functions can be used.__
cyclicASymATens1, cyclicASymATens2, cyclicASymATens3, cyclicASymATens4, cyclicASymATens5, cyclicASymATens6, cyclicASymATens7, cyclicASymATens8,
--
cyclicASymTensFac,
--
-- | __If the indices in which the @'Tensor'@ shall be anti symmetrized do not correspond to the first index type of the given @'Tensor'@ the following functions can be used.__
cyclicASymATensFac1, cyclicASymATensFac2, cyclicASymATensFac3, cyclicASymATensFac4, cyclicASymATensFac5, cyclicASymATensFac6, cyclicASymATensFac7, cyclicASymATensFac8,
--
-- *** Cyclic Block Symmetrization
cyclicBlockSymTens,
--
-- | __If the index blocks in which the @'Tensor'@ shall be symmetrized do not correspond to the first index type of the given @'Tensor'@ the following functions can be used.__
cyclicBlockSymATens1, cyclicBlockSymATens2, cyclicBlockSymATens3, cyclicBlockSymATens4, cyclicBlockSymATens5, cyclicBlockSymATens6, cyclicBlockSymATens7, cyclicBlockSymATens8,
--
cyclicBlockSymTensFac,
--
-- | __If the index blocks in which the @'Tensor'@ shall be symmetrized do not correspond to the first index type of the given @'Tensor'@ the following functions can be used.__
cyclicBlockSymATensFac1, cyclicBlockSymATensFac2, cyclicBlockSymATensFac3, cyclicBlockSymATensFac4, cyclicBlockSymATensFac5, cyclicBlockSymATensFac6, cyclicBlockSymATensFac7, cyclicBlockSymATensFac8,
--
-- * Tensor Functions
-- ** Lists of multiple Tensors
--
-- | Sometimes it is convenient to collect multiple @'Tensors'@s with the same value type. To allow the individual @'Tensor'@s to have different rank these lists are heterogeneous.
TensList1(..), TensList2(..), TensList3(..), TensList4(..), TensList5(..), TensList6(..), TensList7(..), TensList8(..),
--
-- | __Construction of a such heterogeneous list with a single @'Tensor'@ as Entry.__
singletonTList1, singletonTList2, singletonTList3, singletonTList4, singletonTList5, singletonTList6, singletonTList7, singletonTList8,
--
-- | __Infix Synonyms for the various constructors such as @'AppendTList1'@, @'AppendTList2'@, etc.__
(...>), (..&>), (.&.>), (.&&>), (&..>), (&.&>), (&&.>), (&&&>),
--
-- | __Combining functions for the separate heterogeneous tensor lists.__
(...+), (..&+), (.&.+), (.&&+), (&..+), (&.&+), (&&.+), (&&&+),
--
-- ** Conversion of Tensors
-- *** To List
--
toListT,
--
-- | __Converting the various @'Tensors'@ with multiple different index types to such typed lists.__
toListT1, toListT2, toListT3, toListT4, toListT5, toListT6, toListT7, toListT8,
--
toListT',
--
-- | __Converting the various @'Tensors'@ with multiple different index types to such non-typed lists.__
toListT1', toListT2', toListT3', toListT4', toListT5', toListT6', toListT7', toListT8',
--
-- *** To Matrix
-- | Convert a tensor that stores @'AnsVar'@ values to a matrix where the columns label the variables in @'AnsVar'@
--   and the rows label independent components of the tensor.
--
-- __ Convert a @'Tensor'@ to a sparse matrix assocs list.__
toMatListT1', toMatListT2', toMatListT3', toMatListT4', toMatListT5', toMatListT6', toMatListT7', toMatListT8',
--
-- __ Convert a @'Tensor'@ to a sparse "Eigen.SparseMatrix".__
toEMatrixT1', toEMatrixT2', toEMatrixT3', toEMatrixT4', toEMatrixT5', toEMatrixT6', toEMatrixT7', toEMatrixT8',
--
-- | __ Convert all @'Tensors'@ of a heterogeneous tensor List to a combined sparse matrix assocs list.__
toMatListT1, toMatListT2, toMatListT3, toMatListT4, toMatListT5, toMatListT6, toMatListT7, toMatListT8,
--
-- | __ Convert all @'Tensors'@ of a heterogeneous tensor List to a combined sparse "Eigen.SparseMatrix".__
toEMatrixT1, toEMatrixT2, toEMatrixT3, toEMatrixT4, toEMatrixT5, toEMatrixT6, toEMatrixT7, toEMatrixT8,
--
-- ** Tensor utility functions
--
-- *** Removing Zeros
removeZeros,
--
-- | __The following functions allow the removal of zero values of @'Tensor'@s with multiple different index types.__
removeZeros1, removeZeros2, removeZeros3, removeZeros4, removeZeros5, removeZeros6, removeZeros7, removeZeros8,
--
-- *** Evaluation
evalTens,
--
-- | __The following functions can be used if the @'Tensor'@ shall be evaluated for an index type that is different from the first index type of the @'Tensor'@.__
evalTens1, evalTens2, evalTens3, evalTens4, evalTens5, evalTens6, evalTens7, evalTens8,
--
-- *** Rank Computations
-- | Compute the rank of a tensor of @'AnsVarR'@ values. The tensor is converted to a matrix with columns labeling the individual variables that occur in 'ansVarR'
-- and rows labeling the independent tensor components. The rank is then computed using 'Eigen' subroutines. These functions is for instance useful
-- when determining the rank of tensorial equations.
--
-- __ Compute the rank of a single @'Tensor'@.__
tensorRank1', tensorRank2', tensorRank3', tensorRank4', tensorRank5', tensorRank6', tensorRank7', tensorRank8',
--
-- | __Compute the combined rank of a heterogeneous list of multiple tensors.__
tensorRank1, tensorRank2, tensorRank3, tensorRank4, tensorRank5, tensorRank6, tensorRank7, tensorRank8,
--
-- *** Save and Load Tensors
encodeTensor, decodeTensor,
--
-- ** Tensor Differentiation
-- *** Partial Derivatives
partial
) where

import Data.Foldable (toList)
import Control.Applicative (liftA2)
import Data.Ratio ((%), numerator, denominator)
import Data.List (nubBy, sortOn, intersect)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.IntMap.Strict as I
import qualified Data.Map.Strict as M
import Numeric.Natural (Natural(..))
import qualified Numeric.AD.Rank1.Forward as AD
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

--Length typed lists for the tensor indices.

data IndList n a where
    Empty :: IndList 0 a
    Append :: a -> IndList (n-1) a -> IndList n a

infixr 5 +>

-- | Infix synonym for @'Append'@.

(+>) :: (Enum a) => Int -> IndList (n-1) a -> IndList n a
(+>) i = Append (toEnum i)

-- | Construct an @'IndList'@ that only contains a single value.

singletonInd :: a -> IndList 1 a
singletonInd x = Append x Empty

--Helper functions for the construction from untyped lists.

data IsZero (n :: Nat) where
    Zero :: (0 ~ n)     => IsZero n
    NonZero :: (1 <= n) => IsZero n
deriving instance Show (IsZero n)

isZero :: forall (n :: Nat). SNat n -> IsZero n
isZero n = case n %~ SNat @0
             of Proved Refl -> Zero
                Disproved _ -> unsafeCoerce (NonZero @1)

fromList' :: forall (n :: Nat). SNat n -> forall (a :: *). [a] -> Maybe (IndList n a)
fromList' n xs = case isZero n
                  of Zero    -> case xs
                                  of [] -> Just Empty
                                     _  -> Nothing
                     NonZero -> case xs
                                  of []    -> Nothing
                                     x:xs' -> case fromList' (sPred n) xs'
                                                of Just v  -> Just (x `Append` v)
                                                   Nothing -> Nothing

-- | Construction of a legth typed @'IndList'@ from an untyped list.

fromList :: forall (n :: Nat). SingI n => forall (a :: *). [a] -> IndList n a
fromList = \case
               Just v  -> v
               Nothing -> undefined
            . fromList' sing

-- | Construction from untyped lists returning a @'Maybe'@ value.

fromListMaybe :: forall (n :: Nat). SingI n => forall (a :: *). [a] -> Maybe (IndList n a)
fromListMaybe = fromList' sing

--Instances of the length typed lists.

instance (KnownNat n, Generic a) => Generic (IndList n a) where
    type Rep (IndList n a) = Rep [a]

    to r = fromList $ to r
    from = from . toList

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

-- | Sorts an @'IndList'@ of elements that satisfy the @'Ord'@ constraint by implementing a version of insertion sort.

sortInd :: (Ord a, Eq a) => IndList n a -> IndList n a
sortInd Empty = Empty
sortInd (Append x xs) = insertSorted x $ sortInd xs

toListInd :: IndList n a -> [a]
toListInd = toList

combineInds :: IndList n a -> IndList m a -> IndList (n+m) a
combineInds Empty l = l
combineInds (Append x xs) l = Append x $ combineInds xs l

-- | An implementation of the usual head function from "Data.List" for @'IndList'@s. The function returns the first element of the @'IndList'@.

headInd :: IndList n a -> a
headInd (Append x xs) = x

-- | An implementation of the usual tail function from "Data.List" for @'IndList'@s. The function removes the first element of the @'IndList'@.

tailInd :: IndList n a -> IndList (n-1) a
tailInd (Append x xs) = xs

-- | An implementation of the usual @(!!)@ function from "Data.List" for @'IndList'@s. The function returns the list element that is specified by the provided integer value.
--  Indices start from zero.

indexInd :: Int -> IndList n a -> a
indexInd 0 (Append x xs) = x
indexInd i (Append x xs) = indexInd (i-1) xs
indexInd _ _ = error "Index is too large!"

-- | The function replaces the element at index/position specified by its first argument with the element that is specified by its second argument.

updateInd :: Int -> a -> IndList n a -> IndList n a
updateInd 0 s (Append x xs) = Append s xs
updateInd i s (Append x xs) = Append x $ updateInd (i-1) s xs
updateInd _ _  _ = error "Index is too large!"

-- Special functions for the contraction of tensors.

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
resort Inds in IndList according to the permutation given by [Int], length of [Int] must be n
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
        newindList = fromList $ map snd lReSorted

--Index type class

-- | The @'TIndex'@ type class collects a number of class constraints that any index type must satisfy.
class (Eq a, Ord a, Enum a) => TIndex a where

-- | Newtype wrapper for an index type that is used to represent indices with a range from @0@ to @3@.
newtype Ind3 =  Ind3 {indVal3 :: Int}
    deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)

instance TIndex Ind3 where

instance Enum Ind3 where
    toEnum = Ind3
    fromEnum = indVal3

-- | Newtype wrapper for an index type that is used to represent indices with a range from @0@ to @9@.
newtype Ind9 =  Ind9 {indVal9 :: Int}
    deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)

instance TIndex Ind9 where

instance Enum Ind9 where
        toEnum = Ind9
        fromEnum = indVal9

-- | Newtype wrapper for an index type that is used to represent indices with a from @0@ to @20@.
newtype Ind20 =  Ind20 {indVal20 :: Int}
    deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)

instance TIndex Ind20 where

instance Enum Ind20 where
        toEnum = Ind20
        fromEnum = indVal20

-- | Index tuple type for a @'Tensor'@ that provides only contravariant and covariant spacetime indices, i.e. a @'STTens'@.
type IndTupleST n1 n2 = (IndList n1 Ind3, IndList n2 Ind3)

-- | Index tuple type for a @'Tensor'@ with indices of type @'Ind20'@, @'ind9'@ and @'Ind3'@ each one appearing contravariantly and covariantly, i.e. a ‘@ATens'@.
type IndTupleAbs n1 n2 n3 n4 n5 n6 = (IndList n1 Ind20, IndList n2 Ind20 , IndList n3 Ind9, IndList n4 Ind9, IndList n5 Ind3, IndList n6 Ind3)

{--
Values of a given tensor should satisfy number-like properties -> more precisely should constitute an algebra (scaling, addition and multiplication).
It is important to note that only the vector space that tensors of given rank constitute closes on the type level, the product of two tensors with
given rank yields a third tensors with new rank that is hence represented by a different type.
Thus we need the values of tensors to allow for vector space operations
--}

-- Tensor Value type classes.

-- | Type class that encodes the additive group structure of possible @'Tensor'@ values, i.e. addition, neutral element and existence of inverse elements.
-- Each possible @'Tensor'@ value must allow for these basic group operations and hence be an instance of this type class.
class TAdd a where
    -- | Test whether the given element is zero, i.e. the neutral element.
    scaleZero :: a -> Bool
    -- | Addition of two elements.
    addS :: a -> a -> a
    -- | Maps an element to its additive inverse.
    negateS :: a -> a
    -- | Subtraction of two elements.
    subS :: a -> a -> a
    subS a b = a `addS` negateS b

-- | Newtype wrapper that is used for representing scalar values.
newtype SField a = SField a deriving (Show, Eq, Ord)

instance Functor SField where
    fmap f (SField a) = SField $ f a

instance Applicative SField where
    pure = SField
    (<*>) (SField f) = fmap f

instance Num a => Num (SField a) where
    (+) = liftA2 (+)
    (-) = liftA2 (+)
    (*) = liftA2 (+)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

-- tensor type must be instance of both.

class Epsilon a where
    nearZero :: a -> Bool

instance Epsilon Double where
    nearZero d = abs d < 1e-12

instance Epsilon Float where
    nearZero d = abs d < 1e-5

instance Epsilon Rational where
    nearZero r = r == 0

instance Epsilon Int where
    nearZero i = i == 0

instance Epsilon Integer where
    nearZero i = i == 0

instance (Num a, Eq a) => TAdd (SField a) where
    addS (SField a) (SField b) = SField $ a + b
    negateS (SField a) = SField $ negate a
    scaleZero (SField a) = a == 0

instance (TIndex k, TAdd v) => TAdd (Tensor n k v) where
    addS = (&+)
    negateS = negateTens
    scaleZero = \case
                    ZeroTensor -> True
                    _          -> False

-- | Type class for product of two (possibly different) types. The resulting type depends on the types that are given as input.
class Prod v v' where
    -- | Type level function that returns the type of the result of @'prod'@.
    type TProd v v' :: *
    -- | Product function.
    prod :: v -> v' -> TProd v v'

instance Num a => Prod (SField a) (SField a) where
    type TProd (SField a) (SField a) = SField a
    prod = (*)

instance (TIndex k, Prod (SField s) v) => Prod (SField s) (Tensor n k v) where
    type TProd (SField s) (Tensor n k v) = Tensor n k (TProd (SField s) v)
    prod = (&.)

instance (TIndex k, Prod (AnsVar s) v) => Prod (AnsVar s) (Tensor n k v) where
    type TProd (AnsVar s) (Tensor n k v) = Tensor n k (TProd (AnsVar s) v)
    prod = (&.)

instance (TIndex k, Prod v v') => Prod (Tensor n k v) (Tensor n' k v') where
    type TProd (Tensor n k v) (Tensor n' k v') = Tensor (n+n') k (TProd v v')
    prod = (&*)

-- | The @'AnsVar' a@ type represents a basic type for variables that must only occur linearly. As such they
-- can for instance be used whenever tensorial expression involves a tensor with unknown components. This tensor can then be described as
-- having values of type @'AnsVar'@. The @'AnsVar'@ type can for instance be useful when the tensorial expression that involves the @'Tensor'@ with
-- @'AnsVar'@ values describes a linear equation system. Using the functions @'toMatListT1'@, ... this equation system can
-- then be transformed into a matrix with columns labeling the individual @'AnsVar'@s.

newtype AnsVar a = AnsVar (I.IntMap a) deriving Show

type AnsVarR = AnsVar (SField Rational)

-- | Shifts the labels of the variables that are contained in the @'AnsVar'@ type towards larger index labels by the amount specified.
shiftVarLabels :: Int -> AnsVar a -> AnsVar a
shiftVarLabels s (AnsVar v) = AnsVar $ I.mapKeys (s +) v

-- | > shiftLabels1 s = mapTo1 (shiftVarLabels s)
shiftLabels1 :: Int -> AbsTensor1 n1 k1 (AnsVar a) -> AbsTensor1 n1 k1 (AnsVar a)
shiftLabels1 s = mapTo1 (shiftVarLabels s)

-- | > shiftLabels2 s = mapTo2 (shiftVarLabels s)
shiftLabels2 :: Int -> AbsTensor2 n1 n2 k1 (AnsVar a) -> AbsTensor2 n1 n2 k1 (AnsVar a)
shiftLabels2 s = mapTo2 (shiftVarLabels s)

-- | > shiftLabels3 s = mapTo3 (shiftVarLabels s)
shiftLabels3 :: Int -> AbsTensor3 n1 n2 n3 k1 k2 (AnsVar a) -> AbsTensor3 n1 n2 n3 k1 k2 (AnsVar a)
shiftLabels3 s = mapTo3 (shiftVarLabels s)

-- | > shiftLabels4 s = mapTo4 (shiftVarLabels s)
shiftLabels4 :: Int -> AbsTensor4 n1 n2 n3 n4 k1 k2 (AnsVar a) -> AbsTensor4 n1 n2 n3 n4 k1 k2 (AnsVar a)
shiftLabels4 s = mapTo4 (shiftVarLabels s)

-- | > shiftLabels5 s = mapTo5 (shiftVarLabels s)
shiftLabels5 :: Int -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (AnsVar a) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (AnsVar a)
shiftLabels5 s = mapTo5 (shiftVarLabels s)

-- | > shiftLabels6 s = mapTo6 (shiftVarLabels s)
shiftLabels6 :: Int -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (AnsVar a) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (AnsVar a)
shiftLabels6 s = mapTo6 (shiftVarLabels s)

-- | > shiftLabels7 s = mapTo7 (shiftVarLabels s)
shiftLabels7 :: Int -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (AnsVar a) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (AnsVar a)
shiftLabels7 s = mapTo7 (shiftVarLabels s)

-- | > shiftLabels8 s = mapTo8 (shiftVarLabels s)
shiftLabels8 :: Int -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (AnsVar a) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (AnsVar a)
shiftLabels8 s = mapTo8 (shiftVarLabels s)

instance TAdd a => TAdd (AnsVar a) where
    addS (AnsVar v1) (AnsVar v2) = AnsVar $ I.unionWith addS v1 v2
    negateS (AnsVar v1) = AnsVar $ I.map negateS v1
    scaleZero (AnsVar v) = I.null v

instance Prod (SField v) (SField v') => Prod (SField v) (AnsVar (SField v')) where
    type TProd (SField v) (AnsVar (SField v')) = AnsVar (TProd (SField v) (SField v'))
    prod v (AnsVar v') = AnsVar $ I.map (prod v) v'

instance Prod (SField v') (SField v) => Prod (AnsVar (SField v)) (SField v') where
    type TProd (AnsVar (SField v)) (SField v') = AnsVar (TProd (SField v') (SField v))
    prod (AnsVar v) v' = AnsVar $ I.map (prod v') v

-- | Type for representation of functions as @'Tensor'@ values.
newtype CFun a b = CFun (a -> b)

instance Functor (CFun a) where
    fmap f (CFun g) = CFun $ f . g

instance Applicative (CFun a) where
    pure = CFun . const
    (CFun f) <*> (CFun g) = CFun $ \x -> f x (g x)

instance Num b => Num (CFun a b) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = CFun . const . fromInteger

instance Num b => TAdd (CFun a b) where
    addS = (+)
    negateS = negate
    scaleZero = const False

instance Num b => Prod (CFun a b) (CFun a b) where
    type TProd (CFun a b) (CFun a b) = CFun a b
    prod = (*)

instance Num b => Prod (SField b) (CFun a b) where
    type TProd (SField b) (CFun a b) = CFun a b
    prod (SField s) (CFun f) = CFun $ (*s) . f

-- | evaluate a tensor section, i.e. a @'CFun'@-valued @'STTens'@, at given spacetime point
evalSec :: (Num b, Eq b, Epsilon b) => STTens n1 n2 (CFun a b) -> a -> STTens n1 n2 (SField b)
evalSec tens p = tens'
    where
        tList = toListT2 tens
        tList' = fmap (fmap (\(CFun f) -> f p)) tList
        tList'' = filter (\(_, v) -> not $ nearZero v) tList'
        tList''' = fmap (fmap SField) tList''
        tens' = fromListT2 tList'''

-- compute gradient as [[a] -> a] instead of [a] -> [a]
myGrad :: Num a => [Int] -> ([AD.Forward a] -> AD.Forward a) -> [(Int, [a] -> a)]
myGrad is f = map (\i -> (i, (!!i) . g)) is
    where
        g = AD.grad f

-- | Additional type for the sorted tensor forest. @'TMap' k v@ represents an ordered list of key value pairs. Ordering is always defined w.r.t. the keys.
--  All future functions maintain this order when acting on a valid, i.e. ordered @'TMap'@s.
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

-- | Basic tensor data types.
data Tensor n k v where
    -- | Constructor of leaf values.
    Scalar :: v -> Tensor 0 k v
    -- | Constructs a @'Tensor'@ from a @'TMap'@ of index sub tensor pairs.
    Tensor :: TMap k (Tensor n k v) -> Tensor (n+1) k v
    -- | Represents a @'Tensor'@ that is identical zero.
    ZeroTensor :: Tensor n k v


-- | Represents a @'Tensor'@ with attached @'Scalar'@s being again of @'Tensor'@ type and taking the same index type. This type can be used
-- to represent a general @'Tensor'@ that takes contravariant and covariant indices.
type Tensor2 n1 n2 k v = Tensor n1 k (Tensor n2 k v)

type AbsTensor1 n1 k1 v = Tensor n1 k1 v

type AbsTensor2 n1 n2 k1 v = Tensor2 n1 n2 k1 v

type AbsTensor3 n1 n2 n3 k1 k2 v = AbsTensor2 n1 n2 k1 (Tensor n3 k2 v)

type AbsTensor4 n1 n2 n3 n4 k1 k2 v = AbsTensor2 n1 n2 k1 (Tensor2 n3 n4 k2 v)

type AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v = AbsTensor4 n1 n2 n3 n4 k1 k2 (Tensor n5 k3 v)

type AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v = AbsTensor4 n1 n2 n3 n4 k1 k2 (Tensor2 n5 n6 k3 v)

type AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v = AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (Tensor n7 k4 v)

type AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v = AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (Tensor2 n7 n8 k4 v)

-- | Type synonym for a @'Tensor'@ with contravariant and covariant spacetime indices.
type STTens n1 n2 v = AbsTensor2 n1 n2 Ind3 v

-- | Type synonym for a @'Tensor'@ with three different index types ranging from @0@ to @20@, from @0@ to @9@ and from @0@ to @3@ each one
--   appearing contravariantly and covariantly.
type ATens n1 n2 n3 n4 n5 n6 v = AbsTensor6 n1 n2 n3 n4 n5 n6 Ind20 Ind9 Ind3 v

--for converting tensor to @'Bytestring'@s for saving and loading from file we need a non typesafe data type as intermediate type
data TensorRep k v = ScalarR v | TensorR Natural (TMap k (TensorRep k v)) | ZeroR Natural deriving (Show, Generic, Serialize)

--convert between typesafe and non typesafe tensors

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

getTensorMap :: Tensor (n+1) k v -> TMap k (Tensor n k v)
getTensorMap (Tensor m) = m


toMatListT1' :: (TIndex k1, TAdd a) => AbsTensor1 n1 k1 (AnsVar a) -> [((Int,Int),a)]
toMatListT1' = collectMatList . toMatList1'

toMatListT2' :: (TIndex k1, TAdd a) => AbsTensor2 n1 n2 k1 (AnsVar a) -> [((Int,Int),a)]
toMatListT2' = collectMatList . toMatList2'

toMatListT3' :: (TIndex k1, TIndex k2, TAdd a) => AbsTensor3 n1 n2 n3 k1 k2 (AnsVar a) -> [((Int,Int),a)]
toMatListT3' = collectMatList . toMatList3'

toMatListT4' :: (TIndex k1, TIndex k2, TAdd a) => AbsTensor4 n1 n2 n3 n4 k1 k2 (AnsVar a) -> [((Int,Int),a)]
toMatListT4' = collectMatList . toMatList4'

toMatListT5' :: (TIndex k1, TIndex k2, TIndex k3, TAdd a) => AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (AnsVar a) -> [((Int,Int),a)]
toMatListT5' = collectMatList . toMatList5'

toMatListT6' :: (TIndex k1, TIndex k2, TIndex k3, TAdd a) => AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (AnsVar a) -> [((Int,Int),a)]
toMatListT6' = collectMatList . toMatList6'

toMatListT7' :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd a) => AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (AnsVar a) -> [((Int,Int),a)]
toMatListT7' = collectMatList . toMatList7'

toMatListT8' :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd a) => AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (AnsVar a) -> [((Int,Int),a)]
toMatListT8' = collectMatList . toMatList8'

toEMatrixT1' :: (TIndex k1, Real a) => AbsTensor1 n1 k1 (AnsVar (SField a)) -> Sparse.SparseMatrixXd
toEMatrixT1' = assocsToSparse . toMatListT1'

toEMatrixT2' :: (TIndex k1, Real a) => AbsTensor2 n1 n2 k1 (AnsVar (SField a)) -> Sparse.SparseMatrixXd
toEMatrixT2' = assocsToSparse . toMatListT2'

toEMatrixT3' :: (TIndex k1, TIndex k2, Real a) => AbsTensor3 n1 n2 n3 k1 k2 (AnsVar (SField a)) -> Sparse.SparseMatrixXd
toEMatrixT3' = assocsToSparse . toMatListT3'

toEMatrixT4' :: (TIndex k1, TIndex k2, Real a) => AbsTensor4 n1 n2 n3 n4 k1 k2 (AnsVar (SField a)) -> Sparse.SparseMatrixXd
toEMatrixT4' = assocsToSparse . toMatListT4'

toEMatrixT5' :: (TIndex k1, TIndex k2, TIndex k3, Real a) => AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (AnsVar (SField a)) -> Sparse.SparseMatrixXd
toEMatrixT5' = assocsToSparse . toMatListT5'

toEMatrixT6' :: (TIndex k1, TIndex k2, TIndex k3, Real a) => AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (AnsVar (SField a)) -> Sparse.SparseMatrixXd
toEMatrixT6' = assocsToSparse . toMatListT6'

toEMatrixT7' :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, Real a) => AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (AnsVar (SField a)) -> Sparse.SparseMatrixXd
toEMatrixT7' = assocsToSparse . toMatListT7'

toEMatrixT8' :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, Real a) => AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (AnsVar (SField a)) -> Sparse.SparseMatrixXd
toEMatrixT8' = assocsToSparse . toMatListT8'



--construct from typed list

mkTens :: (IndList n k, v) -> Tensor n k v
mkTens (Empty, a) = Scalar a
mkTens (Append x xs, a) = Tensor  [(x, mkTens (xs, a))]

-- | This functions construction @'Tensor'@s with a single index type that only appears contravariantly from a list of key value pairs, where the keys are provided by a @'IndList'@ that contains the indices
-- under which the corresponding value shall be stored.
fromListT :: (TIndex k, TAdd v) => [(IndList n k, v)] -> Tensor n k v
fromListT [x] = mkTens x
fromListT (x:xs) = foldr insertOrAdd (mkTens x) xs
fromListT [] = ZeroTensor

-- | Same functionality as @'fromListT'@ but the indices of the various values are specified by usual list. This is certainly more flexible
-- however all at the cost of reduced type safety compared to @'fromListT'@.
fromListT' :: forall n k v b. (TIndex k, TAdd v, SingI n) => [([k],v)] -> Tensor n k v
fromListT' l = fromListT indList
        where
            indList = map (\(x,y) -> (fromList x, y)) l

fromListT1 :: (TIndex k1, TAdd v) => [(IndTuple1 n1 k1, v)] -> AbsTensor1 n1 k1 v
fromListT1 = fromListT

fromListT1' :: forall n1 k1 v b. (SingI n1, TIndex k1, TAdd v) => [([k1],v)] -> AbsTensor1 n1 k1 v
fromListT1' = fromListT'

fromListT2 :: (TIndex k1, TAdd v) => [(IndTuple2 n1 n2 k1, v)] -> AbsTensor2 n1 n2 k1 v
fromListT2 l = foldr (&+) ZeroTensor tensList
    where
        tensList = map mkTens2 l

fromListT2' :: forall n1 n2 k1 v b. (SingI n1, SingI n2, TIndex k1, TAdd v) => [(([k1],[k1]),v)] -> AbsTensor2 n1 n2 k1 v
fromListT2' l = fromListT2 indList
        where
            indList = map (\((x1,x2),y) -> ((fromList x1, fromList x2),y)) l

fromListT3 :: (TIndex k1, TIndex k2, TAdd v) => [(IndTuple3 n1 n2 n3 k1 k2, v)] -> AbsTensor3 n1 n2 n3 k1 k2 v
fromListT3 l = foldr (&+) ZeroTensor tensList
    where
        tensList = map mkTens3 l

fromListT3' :: forall n1 n2 n3 k1 k2 v b. (SingI n1, SingI n2, SingI n3, TIndex k1, TIndex k2, TAdd v) => [(([k1],[k1],[k2]),v)] -> AbsTensor3 n1 n2 n3 k1 k2 v
fromListT3' l = fromListT3 indList
        where
            indList = map (\((x1,x2,x3),y) -> ((fromList x1, fromList x2, fromList x3),y)) l

fromListT4 :: (TIndex k1, TIndex k2, TAdd v) => [(IndTuple4 n1 n2 n3 n4 k1 k2, v)] -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
fromListT4 l = foldr (&+) ZeroTensor tensList
    where
        tensList = map mkTens4 l

fromListT4' :: forall n1 n2 n3 n4 k1 k2 v b. (SingI n1, SingI n2, SingI n3, SingI n4, TIndex k1, TIndex k2, TAdd v) => [(([k1],[k1],[k2],[k2]),v)] -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
fromListT4' l = fromListT4 indList
        where
            indList = map (\((x1,x2,x3,x4),y) -> ((fromList x1, fromList x2, fromList x3, fromList x4),y)) l

fromListT5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) => [(IndTuple5 n1 n2 n3 n4 n5 k1 k2 k3, v)] -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
fromListT5 l = foldr (&+) ZeroTensor tensList
    where
        tensList = map mkTens5 l

fromListT5' :: forall n1 n2 n3 n4 n5 k1 k2 k3 v b. (SingI n1, SingI n2, SingI n3, SingI n4, SingI n5,  TIndex k1, TIndex k2, TIndex k3, TAdd v) => [(([k1],[k1],[k2],[k2],[k3]),v)] -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
fromListT5' l = fromListT5 indList
        where
            indList = map (\((x1,x2,x3,x4,x5),y) -> ((fromList x1, fromList x2, fromList x3, fromList x4, fromList x5),y)) l

fromListT6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) => [(IndTuple6 n1 n2 n3 n4 n5 n6 k1 k2 k3, v)] -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
fromListT6 l = foldr (&+) ZeroTensor tensList
    where
        tensList = map mkTens6 l

fromListT6' :: forall n1 n2 n3 n4 n5 n6 k1 k2 k3 v b. (SingI n1, SingI n2, SingI n3, SingI n4, SingI n5, SingI n6, TIndex k1, TIndex k2, TIndex k3, TAdd v) => [(([k1],[k1],[k2],[k2],[k3],[k3]),v)] -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
fromListT6' l = fromListT6 indList
        where
            indList = map (\((x1,x2,x3,x4,x5,x6),y) -> ((fromList x1, fromList x2, fromList x3, fromList x4, fromList x5, fromList x6),y)) l

fromListT7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) => [(IndTuple7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4, v)] -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
fromListT7 l = foldr (&+) ZeroTensor tensList
    where
        tensList = map mkTens7 l

fromListT7' :: forall n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v b. (SingI n1, SingI n2, SingI n3, SingI n4, SingI n5, SingI n6, SingI n7, TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) => [(([k1],[k1],[k2],[k2],[k3],[k3],[k4]),v)] -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
fromListT7' l = fromListT7 indList
        where
            indList = map (\((x1,x2,x3,x4,x5,x6,x7),y) -> ((fromList x1, fromList x2, fromList x3, fromList x4, fromList x5, fromList x6, fromList x7),y)) l

fromListT8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) => [(IndTuple8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4, v)] -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
fromListT8 l = foldr (&+) ZeroTensor tensList
    where
        tensList = map mkTens8 l

fromListT8' :: forall n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v b. (SingI n1, SingI n2, SingI n3, SingI n4, SingI n5, SingI n6, SingI n7, SingI n8, TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) => [(([k1],[k1],[k2],[k2],[k3],[k3],[k4],[k4]),v)] -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
fromListT8' l = fromListT8 indList
        where
            indList = map (\((x1,x2,x3,x4,x5,x6,x7,x8),y) -> ((fromList x1, fromList x2, fromList x3, fromList x4, fromList x5, fromList x6, fromList x7, fromList x8),y)) l


--helper function for tensor addition: adding one indices value pair to the tree structure of a given tensor

insertOrAdd :: (TIndex k, TAdd v) => (IndList n k, v) -> Tensor n k v -> Tensor n k v
insertOrAdd (Empty, a) (Scalar b) = Scalar $ a `addS` b
insertOrAdd (Append x xs, a) (Tensor m) = Tensor $ insertWithTMap (\_ o -> insertOrAdd (xs, a) o) x indTens m
            where
                indTens = mkTens (xs, a)
insertOrAdd inds ZeroTensor = mkTens inds

--addition for tensors

infixl 6 &+

-- | Addition of two arbitrary tensors. The only requirement is that the corresponding values satisfy the @'TAdd'@ constraint.
-- In particular this function can also be used for adding tensors that themselves contain tensors as values.

(&+) :: (TIndex k, TAdd v) => Tensor n k v -> Tensor n k v -> Tensor n k v
(&+) (Scalar a) (Scalar b) = Scalar $ a `addS` b
(&+) (Tensor m1) (Tensor m2) = Tensor $ addTMaps (&+) m1 m2
(&+) t1 ZeroTensor = t1
(&+) ZeroTensor t2 = t2

-- | Scalar multiplication of an arbitrary @'Tensor'@. Only requirement is that the corresponding values and the scalar type satisfy the
-- @'Prod'@ constraint.

infix 8 &.

(&.) :: (TIndex k, Prod s v) => s -> Tensor n k v -> Tensor n k (TProd s v)
(&.) scalar = fmap (prod scalar)

-- | Negation of an arbitrary @'Tensor'@. The function uses the required group property of the tensor values to map each value of the tensors to its
-- additive inverse.

negateTens :: (TIndex k, TAdd v) => Tensor n k v -> Tensor n k v
negateTens = fmap negateS

-- | Subtraction of two arbitrary @'Tensor'@s. The only requirement is that the corresponding values satisfy the @'TAdd'@ constraint.
-- In particular this function can also be used for adding tensors that themselves contain tensors as values.

infixl 6 &-

(&-) :: (TIndex k, TAdd v) => Tensor n k v -> Tensor n k v -> Tensor n k v
(&-) (Scalar a) (Scalar b) = Scalar $ subS a b
(&-) (Tensor m1) (Tensor m2) = Tensor $ addTMaps (&-) m1 m2
(&-) t1 ZeroTensor = t1
(&-) ZeroTensor t2 = negateS t2

-- | Tensor product of two @'Tensor'@s. In the result for each index type the indices of the first @'Tensor'@ are arranged left of those in the second @'Tensor'@ argument.
-- The values of the two @'Tensor'@s must satisfy the @'Prod'@ constraint.

infixl 7 &*

(&*) :: (TIndex k, Prod v v') => Tensor n k v -> Tensor m k v' -> TProd (Tensor n k v) (Tensor m k v')
(&*) (Scalar x) (Scalar y) = Scalar $ prod x y
(&*) (Scalar x) t2 = fmap (prod x) t2
(&*) (Tensor m) t2 = Tensor $ mapTMap (&* t2) m
(&*) t1 ZeroTensor = ZeroTensor
(&*) ZeroTensor t2 = ZeroTensor

--encode and decode tensors as bytestrings for efficient storing

-- | Utility function to serialize and compress a @'Tensor'@ into a @'Data.ByteString.Lazy.ByteString'@, making use of the @'Serialize'@ instance.
encodeTensor :: (KnownNat n, Ord k, Serialize k, Serialize v) => Tensor n k v -> BS.ByteString
encodeTensor = compress . encodeLazy

-- | Utility function to decompress and de-serialize a @'Data.ByteString.Lazy.ByteString'@ into a @'Tensor'@, making use of the @'Serialize'@ instance.
decodeTensor :: (KnownNat n, Ord k, Serialize k, Serialize v) => BS.ByteString -> Tensor n k v
decodeTensor bs = either error id $ decodeLazy $ decompress bs


-- | The function computes partial derivatives of spacetime tensor of type @'STTens'@ with @'CFun'@ values.
partial :: Num a => STTens n1 n2 (CFun [AD.Forward a] (AD.Forward a)) -> STTens n1 (n2+1) (CFun [a] a)
partial tens = tens'
    where
        tList = toListT2 tens
        grads = map (\(is, CFun v) -> (is, myGrad [0..3] v)) tList
        tList' = concatMap (\((i1, i2), gs) -> map (\(ig, g) -> ((i1, Ind3 ig `Append` i2), CFun g)) gs) grads
        tens' = fromListT2 tList'

--AbsTensors automatically satisfy TensorAlgebra as we only used type synonyms
--e.g. AbsTensor4 actually looks like Tensor n1 kq1 (Tensor n2 k2 (Tensor n3 k3 (Tensor n4 k4 (Tensor n5 k5 (Tensor n6 k6 (Tensor n7 k7 (Tensor n8 k8 a)))))))

--fmap takes us 1 level deeper -> we get functions that apply a given function to the various sub tensors

-- | Synonym for @fmap@. @'mapTo1'@ applies a function to the values of a tensor.
--
-- > mapTo1 = fmap
mapTo1 :: (v1 -> v2) -> Tensor n1 k v1 -> Tensor n1 k v2
mapTo1 = fmap

-- | Applies a function one level deeper than @'mapTo1'@. In other words the leaf values of the @'Tensor'@ at hand must themselves be of type
-- @'Tensor'@, @'mapTo2'@ then applies a function to the values of these leaf @'Tensor'@s.
--
-- > mapTo2 = fmap . fmap
mapTo2 :: (v1 -> v2) -> Tensor2 n1 n2 k v1 -> Tensor2 n1 n2 k v2
mapTo2 = fmap . fmap

-- | Maps a function to the 3rd leaf level.
--
-- > mapTo3 = fmap . fmap . fmap
mapTo3 :: (v1 -> v2) -> AbsTensor3 n1 n2 n3 k1 k2 v1 -> AbsTensor3 n1 n2 n3 k1 k2 v2
mapTo3 = fmap . fmap . fmap

-- | Maps a function to the 4th leaf level.
--
-- > mapTo4 = fmap . fmap . fmap . fmap
mapTo4 :: (v1 -> v2) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v1 -> AbsTensor4 n1 n2 n3 n4 k1 k2 v2
mapTo4 = fmap . fmap . fmap . fmap

-- | Maps a function to the 5th leaf level.
--
-- > mapTo5 = fmap . fmap . fmap . fmap . fmap
mapTo5 :: (v1 -> v2) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v1 -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v2
mapTo5 = fmap . fmap . fmap . fmap . fmap

-- | Maps a function to the 6th leaf level.
--
-- > mapTo6 = fmap . fmap . fmap . fmap . fmap . fmap
mapTo6 :: (v1 -> v2) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v1 -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v2
mapTo6 = fmap . fmap . fmap . fmap . fmap . fmap

-- | Maps a function to the 7th leaf level.
--
-- > mapTo7 = fmap . fmap . fmap . fmap . fmap . fmap . fmap
mapTo7 :: (v1 -> v2) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v1 -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v2
mapTo7 = fmap . fmap . fmap . fmap . fmap . fmap . fmap

-- | Maps a function to the 8th leaf level.
--
-- > mapTo8 = fmap . fmap . fmap . fmap . fmap . fmap . fmap . fmap
mapTo8 :: (v1 -> v2) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v1 -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v2
mapTo8 = fmap . fmap . fmap . fmap . fmap . fmap . fmap . fmap

--remove Zero Values at every level of Abstract Tensor

-- | Applying these functions on a given tensor removes all zero values from it. Even if zero values are not included in a given tensor when it is constructed
-- they might occur during computations. If so they are not automatically removed from the tensor.
removeZeros :: TAdd v => Tensor n k v -> Tensor n k v
removeZeros (Scalar x) = if scaleZero x then ZeroTensor else Scalar x
removeZeros (Tensor m) = let newMap = filterTMap
                                (\case
                                    ZeroTensor -> False
                                    _          -> True) $ mapTMap removeZeros m in if null newMap then ZeroTensor else Tensor newMap
removeZeros ZeroTensor = ZeroTensor

-- | >  removeZeros1 = removeZeros
removeZeros1 :: (TAdd v, TIndex k) => AbsTensor1 n1 k v -> AbsTensor1 n1 k v
removeZeros1 = removeZeros

-- | > removeZeros2 = removeZeros . mapTo1 removeZeros
removeZeros2 :: (TAdd v, TIndex k) => AbsTensor2 n1 n2 k v -> AbsTensor2 n1 n2 k v
removeZeros2 = removeZeros . mapTo1 removeZeros

-- | > removeZeros3 = removeZeros . mapTo1 removeZeros . mapTo2 removeZeros
removeZeros3 :: (TAdd v, TIndex k1, TIndex k2) => AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
removeZeros3 = removeZeros . mapTo1 removeZeros . mapTo2 removeZeros

-- | > removeZeros4 = removeZeros . mapTo1 removeZeros . mapTo2 removeZeros . mapTo3 removeZeros
removeZeros4 :: (TAdd v, TIndex k1, TIndex k2) => AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
removeZeros4 = removeZeros . mapTo1 removeZeros . mapTo2 removeZeros . mapTo3 removeZeros

-- | > removeZeros5 = removeZeros . mapTo1 removeZeros . mapTo2 removeZeros . mapTo3 removeZeros . mapTo4 removeZeros
removeZeros5 :: (TAdd v, TIndex k1, TIndex k2, TIndex k3) => AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
removeZeros5 = removeZeros . mapTo1 removeZeros . mapTo2 removeZeros . mapTo3 removeZeros . mapTo4 removeZeros

-- | > removeZeros6 = removeZeros . mapTo1 removeZeros . mapTo2 removeZeros . mapTo3 removeZeros . mapTo4 removeZeros . mapTo5 removeZeros
removeZeros6 :: (TAdd v, TIndex k1, TIndex k2, TIndex k3) => AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
removeZeros6 = removeZeros . mapTo1 removeZeros . mapTo2 removeZeros . mapTo3 removeZeros . mapTo4 removeZeros . mapTo5 removeZeros

-- | > removeZeros7 = removeZeros . mapTo1 removeZeros . mapTo2 removeZeros . mapTo3 removeZeros . mapTo4 removeZeros . mapTo5 removeZeros . mapTo6 removeZeros
removeZeros7 :: (TAdd v, TIndex k1, TIndex k2, TIndex k3, TIndex k4) => AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
removeZeros7 = removeZeros . mapTo1 removeZeros . mapTo2 removeZeros . mapTo3 removeZeros . mapTo4 removeZeros . mapTo5 removeZeros . mapTo6 removeZeros

-- | > removeZeros8 = removeZeros . mapTo1 removeZeros . mapTo2 removeZeros . mapTo3 removeZeros . mapTo4 removeZeros . mapTo5 removeZeros . mapTo6 removeZeros . mapTo7 removeZeros
removeZeros8 :: (TAdd v, TIndex k1, TIndex k2, TIndex k3, TIndex k4) => AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
removeZeros8 = removeZeros . mapTo1 removeZeros . mapTo2 removeZeros . mapTo3 removeZeros . mapTo4 removeZeros . mapTo5 removeZeros . mapTo6 removeZeros . mapTo7 removeZeros


-- | Transpose a @'Tensor'@ in two specified contravariant indices of the first index type indices.
-- The result is simply the tensor with the two indices with position specified by the two @'Int'@ values swapped.
tensorTrans :: (TIndex k, TAdd v) => (Int,Int) -> Tensor n k v -> Tensor n k v
tensorTrans (0, j) t = fromListT l
                where
                    l = map (\(x,y) -> (swapHead j x, y)) $ toListT t
tensorTrans (i, j) (Tensor m) = Tensor $ mapTMap (tensorTrans (i-1, j-1)) m
tensorTrans (i ,j) ZeroTensor = ZeroTensor

-- | > tensorTrans1 = tensorTrans
tensorTrans1 :: (TIndex k1, TAdd v) => (Int,Int) -> AbsTensor1 n1 k1 v -> AbsTensor1 n1 k1 v
tensorTrans1 = tensorTrans

-- | > tensorTrans2 mapTo1 . tensorTrans
tensorTrans2 :: (TIndex k1, TAdd v) => (Int,Int) -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 n1 n2 k1 v
tensorTrans2 = mapTo1 . tensorTrans

-- | > tensorTrans3 = mapTo2 . tensorTrans
tensorTrans3 :: (TIndex k1, TIndex k2, TAdd v) => (Int,Int) -> AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
tensorTrans3 = mapTo2 . tensorTrans

-- | > tensorTrans4 = mapTo3 tensorTrans
tensorTrans4 :: (TIndex k1, TIndex k2, TAdd v) => (Int,Int) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
tensorTrans4 = mapTo3 . tensorTrans

-- | > tensorTrans5 = mapTo4 . tensorTrans
tensorTrans5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) => (Int,Int) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
tensorTrans5 = mapTo4 . tensorTrans

-- | > tensorTrans6 = mapTo5 . tensorTrans
tensorTrans6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) => (Int,Int) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
tensorTrans6 = mapTo5 . tensorTrans

-- | > tensorTrans7 = mapTo6 . tensorTrans
tensorTrans7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) => (Int,Int) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
tensorTrans7 = mapTo6 . tensorTrans

-- | > tensorTrans8 = mapTo7 . tensorTrans
tensorTrans8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) => (Int,Int) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
tensorTrans8 = mapTo7 . tensorTrans


-- | Swap, i.e. transpose two index blocks in a given @'Tensor'@. The two index blocks must be disjoint.
tensorBlockTrans :: (TIndex k, TAdd v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k v
tensorBlockTrans (l1,l2) t = foldr tensorTrans t indList
        where
            indList = if null $ intersect l1 l2 then zip l1 l2 else error "at least one index in the list occurs several times"

-- | > tensorBlockTrans1 = tensorBlockTrans
tensorBlockTrans1 :: (TIndex k1, TAdd v) => ([Int],[Int]) -> AbsTensor1 n1 k1 v -> AbsTensor1 n1 k1 v
tensorBlockTrans1 = tensorBlockTrans

-- | > tensorBlockTrans2 = mapTo1 tensorBlockTrans
tensorBlockTrans2 :: (TIndex k1, TAdd v) => ([Int],[Int]) -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 n1 n2 k1 v
tensorBlockTrans2 = mapTo1 . tensorBlockTrans

-- | > tensorBlockTrans3 = mapTo2 . tensorBlockTrans
tensorBlockTrans3 :: (TIndex k1, TIndex k2, TAdd v) => ([Int],[Int]) -> AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
tensorBlockTrans3 = mapTo2 . tensorBlockTrans

-- | > tensorBlockTrans4 = mapTo3 . tensorBlockTrans
tensorBlockTrans4 :: (TIndex k1, TIndex k2, TAdd v) => ([Int],[Int]) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
tensorBlockTrans4 = mapTo3 . tensorBlockTrans

-- | > tensorBlockTrans5 = mapTo4 . tensorBlockTrans
tensorBlockTrans5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) => ([Int],[Int]) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
tensorBlockTrans5 = mapTo4 . tensorBlockTrans

-- | > tensorBlockTrans6 = mapTo5 . tensorBlockTrans
tensorBlockTrans6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) => ([Int],[Int]) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
tensorBlockTrans6 = mapTo5 . tensorBlockTrans

-- | > tensorBlockTrans7 = mapTo6 . tensorBlockTrans
tensorBlockTrans7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) => ([Int],[Int]) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
tensorBlockTrans7 = mapTo6 . tensorBlockTrans

-- | > tensorBlockTrans8 = mapTo7 tensorBlockTrans
tensorBlockTrans8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) => ([Int],[Int]) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
tensorBlockTrans8 = mapTo7 . tensorBlockTrans

--resort an AbsTens

-- | Completely permute the indices of a given tensor. The new index order is specified by a list @'[Int]'@ that must be of length given by the
-- number of indices the tensor contains. The list then specifies in its i-th element the position on which the i-th index of the tensor
-- shall be sorted.
--
-- > resortTens [1,2,0,3] (fromListT' [([0,1,2,3],1)] :: Tensor 4 Ind3 Rational) = (fromListT' [([2,0,1,3],1)] :: Tensor 4 Ind3 Rational)
resortTens :: (KnownNat n, TIndex k, TAdd v) => [Int] -> Tensor n k v -> Tensor n k v
resortTens perm t = fromListT $ map (\(x,y) -> (resortInd perm x, y)) $ toListT t

-- | > resortTens1 = resortTens
resortTens1 :: (KnownNat n1, TIndex k1, TAdd v) => [Int] -> AbsTensor1 n1 k1 v -> AbsTensor1 n1 k1 v
resortTens1 = resortTens

-- | > resortTens2 = mapTo1 . resortTens
resortTens2 :: (KnownNat n2, TIndex k1, TAdd v) => [Int] -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 n1 n2 k1 v
resortTens2 = mapTo1 . resortTens

-- | > resortTens3 = mapTo2 . resortTens
resortTens3 :: (KnownNat n3, TIndex k1, TIndex k2, TAdd v) => [Int] -> AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
resortTens3 = mapTo2 . resortTens

-- | > resortTens4 = mapTo3 . resortTens
resortTens4 :: (KnownNat n4, TIndex k1, TIndex k2, TAdd v) => [Int] -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
resortTens4 = mapTo3 . resortTens

-- | > resortTens5 = mapTo4 . resortTens
resortTens5 :: (KnownNat n5, TIndex k1, TIndex k2, TIndex k3, TAdd v) => [Int] -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
resortTens5 = mapTo4 . resortTens

-- | > resortTens6 = mapTo5 . resortTens
resortTens6 :: (KnownNat n6, TIndex k1, TIndex k2, TIndex k3, TAdd v) => [Int] -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
resortTens6 = mapTo5 . resortTens

-- | > resortTens7 = mapTo6 . resortTens
resortTens7 :: (KnownNat n7, TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) => [Int] -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
resortTens7 = mapTo6 . resortTens

-- | > resortTens8 = mapTo7 . resortTens
resortTens8 :: (KnownNat n8, TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) => [Int] -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
resortTens8 = mapTo7 . resortTens

--evaluation of a tensor for a specific index value returning the appropriate sub tensor

-- | Evaluate a @'Tensor'@ for a specific value of its first contravariant index type returning the corresponding sub @'Tensor'@.
-- The additional functions specified below apply the evaluation function @'evalTens'@ to the deeper @'Tensor'@ levels.
evalTens :: (KnownNat (n+1), TIndex k, TAdd v) => Int -> k -> Tensor (n+1) k v -> Tensor n k v
evalTens ind indVal (Tensor m)
            | ind > size -1 || ind < 0 = error "wrong index to evaluate"
            | ind == 0 = fromMaybe ZeroTensor $ lookup indVal m
            | otherwise = fromMaybe ZeroTensor $ lookup indVal (getTensorMap newTens)
            where
                size = length $ fst $ head $ toListT' (Tensor m)
                l = [1..ind] ++ 0 : [ind+1..size -1]
                newTens = resortTens l (Tensor m)

-- | > evalTens1 = evalTens
evalTens1 :: (KnownNat n1, TIndex k1, TAdd v) => Int -> k1 -> AbsTensor1 (n1+1) k1 v -> AbsTensor1 n1 k1 v
evalTens1 = evalTens

-- | > evalTens2 ind indVal = mapTo1 (evalTens ind indVal)
evalTens2 :: (KnownNat n2, TIndex k1, TAdd v) => Int -> k1 -> AbsTensor2 n1 (n2+1) k1 v  -> AbsTensor2 n1 n2 k1 v
evalTens2 ind indVal = mapTo1 (evalTens ind indVal)

-- | > evalTens3 ind indVal = mapTo2 (evalTens ind indVal)
evalTens3 :: (KnownNat n3, TIndex k1, TIndex k2, TAdd v) => Int -> k2 -> AbsTensor3 n1 n2 (n3+1) k1 k2 v  -> AbsTensor3 n1 n2 n3 k1 k2 v
evalTens3 ind indVal = mapTo2 (evalTens ind indVal)

-- | > evalTens4 ind indVal = mapTo3 (evalTens ind indVal)
evalTens4 :: (KnownNat n4, TIndex k1, TIndex k2, TAdd v) => Int -> k2 -> AbsTensor4 n1 n2 n3 (n4+1) k1 k2 v  -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
evalTens4 ind indVal = mapTo3 (evalTens ind indVal)

-- | > evalTens5 ind indVal = mapTo4 (evalTens ind indVal)
evalTens5 :: (KnownNat n5, TIndex k1, TIndex k2, TIndex k3, TAdd v) => Int -> k3 -> AbsTensor5 n1 n2 n3 n4 (n5+1) k1 k2 k3 v  -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
evalTens5 ind indVal = mapTo4 (evalTens ind indVal)

-- | > evalTens6 ind indVal = mapTo5 (evalTens ind indVal)
evalTens6 :: (KnownNat n6, TIndex k1, TIndex k2, TIndex k3, TAdd v) => Int -> k3 -> AbsTensor6 n1 n2 n3 n4 n5 (n6+1) k1 k2 k3 v  -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
evalTens6 ind indVal = mapTo5 (evalTens ind indVal)

-- | > evalTens7 ind indVal = mapTo6 (evalTens ind indVal)
evalTens7 :: (KnownNat n7, TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) => Int -> k4 -> AbsTensor7 n1 n2 n3 n4 n5 n6 (n7+1) k1 k2 k3 k4 v  -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
evalTens7 ind indVal = mapTo6 (evalTens ind indVal)

-- | > evalTens8 ind indVal = mapTo7 (evalTens ind indVal)
evalTens8 :: (KnownNat n8, TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) => Int -> k4 -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 (n8+1) k1 k2 k3 k4 v  -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
evalTens8 ind indVal = mapTo7 (evalTens ind indVal)

--symmetrizer functions

-- | Basic symmetrization in a pair of contravariant indices of the 1st index type. Usual factors of \( \frac{1}{2} \) are not include in this function. The resulting function hence no longer satisfies the property of
-- a projection. The following functions apply @'symTens'@ to the index types of the deeper leaf levels, i.e. covariant indices of the 1st index type, contravariant indices of the 2nd index type, etc.
symTens :: (TIndex k, TAdd v) => (Int,Int) -> Tensor n k v -> Tensor n k v
symTens inds t = t &+ tensorTrans inds t

-- | > symATens1 = symTens
symATens1 :: (TIndex k1, TAdd v) =>
             (Int,Int) ->
             AbsTensor1 n1 k1 v ->
             AbsTensor1 n1 k1 v
symATens1 = symTens

-- | > symATens2 = mapTo1 . symTens
symATens2 :: (TIndex k1, TAdd v) =>
             (Int,Int) ->
             AbsTensor2 n1 n2 k1 v ->
             AbsTensor2 n1 n2 k1 v
symATens2 = mapTo1 . symTens

-- | > symATens3 = mapTo2 . symTens
symATens3 :: (TIndex k1, TIndex k2, TAdd v) =>
             (Int,Int) ->
             AbsTensor3 n1 n2 n3 k1 k2 v ->
             AbsTensor3 n1 n2 n3 k1 k2 v
symATens3 = mapTo2 . symTens

-- | > symATens4 = mapTo3 . symTens
symATens4 :: (TIndex k1, TIndex k2, TAdd v) =>
             (Int,Int) ->
             AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
             AbsTensor4 n1 n2 n3 n4 k1 k2 v
symATens4 = mapTo3 . symTens

-- | > symATens5 = mapTo4 . symTens
symATens5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) =>
             (Int,Int) ->
             AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
             AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
symATens5 = mapTo4 . symTens

-- | > symATens6 = mapTo5 . symTens
symATens6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) =>
             (Int,Int) ->
             AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
             AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
symATens6 = mapTo5 . symTens

-- | > symATens7 = mapTo6 . symTens
symATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) =>
             (Int,Int) ->
             AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
             AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
symATens7 = mapTo6 . symTens

-- | > symATens8 = mapTo7 . symTens
symATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) =>
             (Int,Int) ->
             AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
             AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
symATens8 = mapTo7 . symTens

--with factor

-- | Same functionality as @'symTens'@ but including the \( \frac{1}{2} \) in the result and thus defining a projection.
-- The following functions apply @'symTensFac'@ to the index types of the deeper leaf levels, i.e. covariant indices of the 1st index type, contravariant indices of the 2nd index type, etc.
--
-- > symTensFac inds t = (SField $ 1%2) &. symTens inds t
symTensFac :: (TIndex k, TAdd v, Prod (SField Rational) v) => (Int,Int) -> Tensor n k v -> Tensor n k (TProd (SField Rational) v)
symTensFac inds t = (SField $ 1%2) &. symTens inds t

-- | > symATensFac1 = symTensFac
symATensFac1 :: (TIndex k1, TAdd v, Prod (SField Rational) v) =>
                (Int,Int) ->
                AbsTensor1 n1 k1 v ->
                AbsTensor1 n1 k1 (TProd (SField Rational) v)
symATensFac1 = symTensFac

-- | > symATensFac2 = mapTo1 . symTensFac
symATensFac2 :: (TIndex k1, TAdd v, Prod (SField Rational) v) =>
                (Int,Int) ->
                AbsTensor2 n1 n2 k1 v ->
                AbsTensor2 n1 n2 k1 (TProd (SField Rational) v)
symATensFac2 = mapTo1 . symTensFac

-- | > symATensFac3 = mapTo2 . symTensFac
symATensFac3 :: (TIndex k1,TIndex k2, TAdd v, Prod (SField Rational) v) =>
                (Int,Int) ->
                AbsTensor3 n1 n2 n3 k1 k2 v ->
                AbsTensor3 n1 n2 n3 k1 k2 (TProd (SField Rational) v)
symATensFac3 = mapTo2 . symTensFac

-- | > symATensFac4 = mapTo3 . symTensFac
symATensFac4 :: (TIndex k1, TIndex k2, TAdd v, Prod (SField Rational) v) =>
                (Int,Int) ->
                AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                AbsTensor4 n1 n2 n3 n4 k1 k2 (TProd (SField Rational) v)
symATensFac4 = mapTo3 . symTensFac

-- | > symATensFac5 = mapTo4 . symTensFac
symATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v, Prod (SField Rational) v) =>
                (Int,Int) ->
                AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (TProd (SField Rational) v)
symATensFac5 = mapTo4 . symTensFac

-- | > symATensFac6 = mapTo5 . symTensFac
symATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v, Prod (SField Rational) v) =>
                (Int,Int) ->
                AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (TProd (SField Rational) v)
symATensFac6 = mapTo5 . symTensFac

-- | > symATensFac7 = mapTo6 . symTensFac
symATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v, Prod (SField Rational) v) =>
                (Int,Int) ->
                AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (TProd (SField Rational) v)
symATensFac7 = mapTo6 . symTensFac

-- | > symATensFac8 = mapTo7 . symTensFac
symATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v, Prod (SField Rational) v) =>
                (Int,Int) ->
                AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (TProd (SField Rational) v)
symATensFac8 = mapTo7 . symTensFac

--anti-symmetrization

-- | Basic anti symmetrization in a pair of contravariant indices of the 1st index type. Usual factors of \( \frac{1}{2} \) are not include in this function. The resulting function hence no longer satisfies the property of
-- a projection. The following functions apply @'aSymTens'@ to the index types of the deeper leaf levels, i.e. covariant indices of the 1st index type, contravariant indices of the 2nd index type, etc.
aSymTens :: (TIndex k, TAdd v) => (Int,Int) -> Tensor n k v -> Tensor n k v
aSymTens inds t = t &- tensorTrans inds t

-- | > aSymATens1 = aSymTens
aSymATens1 :: (TIndex k1, TAdd v) =>
              (Int,Int) ->
              AbsTensor1 n1 k1 v ->
              AbsTensor1 n1 k1 v
aSymATens1 = aSymTens

-- | > aSymATens2 = mapTo1 . aSymTens
aSymATens2 :: (TIndex k1, TAdd v) =>
              (Int,Int) ->
              AbsTensor2 n1 n2 k1 v ->
              AbsTensor2 n1 n2 k1 v
aSymATens2 = mapTo1 . aSymTens

-- | > aSymATens3 = mapTo2 . aSymTens
aSymATens3 :: (TIndex k1, TIndex k2, TAdd v) =>
              (Int,Int) ->
              AbsTensor3 n1 n2 n3 k1 k2 v ->
              AbsTensor3 n1 n2 n3 k1 k2 v
aSymATens3 = mapTo2 . aSymTens

-- | > aSymATens4 = mapTo3 . aSymTens
aSymATens4 :: (TIndex k1, TIndex k2, TAdd v) =>
              (Int,Int) ->
              AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
              AbsTensor4 n1 n2 n3 n4 k1 k2 v
aSymATens4 = mapTo3 . aSymTens

-- | > aSymATens5 = mapTo4 . aSymTens
aSymATens5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) =>
              (Int,Int) ->
              AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
              AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
aSymATens5 = mapTo4 . aSymTens

-- | > aSymATens6 = mapTo5 . aSymTens
aSymATens6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) =>
              (Int,Int) ->
              AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
              AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
aSymATens6 = mapTo5 . aSymTens

-- | > aSymATens7 = mapTo6 . aSymTens
aSymATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) =>
              (Int,Int) ->
              AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
              AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
aSymATens7 = mapTo6 . aSymTens

-- | > aSymATens8 = mapTo7 . aSymTens
aSymATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) =>
              (Int,Int) ->
              AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
              AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
aSymATens8 = mapTo7 . aSymTens

--with factor

-- | Same functionality as @'aSymTens'@ but including the \( \frac{1}{2} \) factors in the result and thus defining a projection.
-- The following functions apply @'aSymTensFac'@ to the index types of the deeper leaf levels, i.e. covariant indices of the 1st index type, contravariant indices of the 2nd index type, etc.
--
-- > aSymTensFac inds t = (SField $ 1%2)&. aSymTens inds t
aSymTensFac :: (TIndex k, TAdd v, Prod (SField Rational) v) => (Int,Int) -> Tensor n k v -> Tensor n k (TProd (SField Rational) v)
aSymTensFac inds t = (SField $ 1%2)&. aSymTens inds t

-- | > aSymATensFac1 = aSymTensFac
aSymATensFac1 :: (TIndex k1, TAdd v, Prod (SField Rational) v) =>
                 (Int,Int) ->
                 AbsTensor1 n1 k1 v ->
                 AbsTensor1 n1 k1 (TProd (SField Rational) v)
aSymATensFac1 = aSymTensFac

-- | > aSymATensFac2 = mapTo1 . aSymTensFac
aSymATensFac2 :: (TIndex k1, TAdd v, Prod (SField Rational) v) =>
                 (Int,Int) ->
                 AbsTensor2 n1 n2 k1 v ->
                 AbsTensor2 n1 n2 k1 (TProd (SField Rational) v)
aSymATensFac2 = mapTo1 . aSymTensFac

-- | > aSymATensFac3 = mapTo2 . aSymTensFac
aSymATensFac3 :: (TIndex k1, TIndex k2, TAdd v, Prod (SField Rational) v) =>
                 (Int,Int) ->
                 AbsTensor3 n1 n2 n3 k1 k2 v ->
                 AbsTensor3 n1 n2 n3 k1 k2 (TProd (SField Rational) v)
aSymATensFac3 = mapTo2 . aSymTensFac

-- | > aSymATensFac4 = mapTo3 . aSymTensFac
aSymATensFac4 :: (TIndex k1, TIndex k2, TAdd v, Prod (SField Rational) v) =>
                 (Int,Int) ->
                 AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                 AbsTensor4 n1 n2 n3 n4 k1 k2 (TProd (SField Rational) v)
aSymATensFac4 = mapTo3 . aSymTensFac

-- | > aSymATensFac5 = mapTo4 . aSymTensFac
aSymATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v, Prod (SField Rational) v) =>
                 (Int,Int) ->
                 AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                 AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (TProd (SField Rational) v)
aSymATensFac5 = mapTo4 . aSymTensFac

-- | > aSymATensFac6 = mapTo5 . aSymTensFac
aSymATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v, Prod (SField Rational) v) =>
                 (Int,Int) ->
                 AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                 AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (TProd (SField Rational) v)
aSymATensFac6 = mapTo5 . aSymTensFac

-- | > aSymATensFac7 = mapTo6 . aSymTensFac
aSymATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v, Prod (SField Rational) v) =>
                 (Int,Int) ->
                 AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                 AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (TProd (SField Rational) v)
aSymATensFac7 = mapTo6 . aSymTensFac

-- | > aSymATensFac8 = mapTo7 . aSymTensFac
aSymATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v, Prod (SField Rational) v) =>
                 (Int,Int) ->
                 AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                 AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (TProd (SField Rational) v)
aSymATensFac8 = mapTo7 . aSymTensFac

--block symmetrization

-- | Symmetrization w.r.t. the exchange of two blocks of contravariant indices of the 1st index type. The index blocks must be disjoint. These function does not include the usual \( \frac{1}{2} \) factors and thus does not
-- define a projection. The following functions apply @'symBlockTens'@ to the index types of the deeper leaf levels, i.e. covariant indices of the 1st index type, contravariant indices of the 2nd index type, etc.
symBlockTens :: (TIndex k, TAdd v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k v
symBlockTens inds t = t &+ tensorBlockTrans inds t

-- | > symBlockATens1 = symBlockTens
symBlockATens1 :: (TIndex k1, TAdd v) =>
                  ([Int],[Int]) ->
                  AbsTensor1 n1 k1 v ->
                  AbsTensor1 n1 k1 v
symBlockATens1 = symBlockTens

-- | > symBlockATens2 = mapTo1 . symBlockTens
symBlockATens2 :: (TIndex k1, TAdd v) =>
                  ([Int],[Int]) ->
                  AbsTensor2 n1 n2 k1 v ->
                  AbsTensor2 n1 n2 k1 v
symBlockATens2 = mapTo1 . symBlockTens

-- | > symBlockATens3 = mapTo2 . symBlockTens
symBlockATens3 :: (TIndex k1, TIndex k2, TAdd v) =>
                  ([Int],[Int]) ->
                  AbsTensor3 n1 n2 n3 k1 k2 v ->
                  AbsTensor3 n1 n2 n3 k1 k2 v
symBlockATens3 = mapTo2 . symBlockTens

-- | > symBlockATens4 = mapTo3 . symBlockTens
symBlockATens4 :: (TIndex k1, TIndex k2, TAdd v) =>
                  ([Int],[Int]) ->
                  AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                  AbsTensor4 n1 n2 n3 n4 k1 k2 v
symBlockATens4 = mapTo3 . symBlockTens

-- | > symBlockATens5 = mapTo4 . symBlockTens
symBlockATens5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) =>
                  ([Int],[Int]) ->
                  AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                  AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
symBlockATens5 = mapTo4 . symBlockTens

-- | > symBlockATens6 = mapTo5 . symBlockTens
symBlockATens6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) =>
                  ([Int],[Int]) ->
                  AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                  AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
symBlockATens6 = mapTo5 . symBlockTens

-- | > symBlockATens7 = mapTo6 . symBlockTens
symBlockATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) =>
                  ([Int],[Int]) ->
                  AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                  AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
symBlockATens7 = mapTo6 . symBlockTens

-- | > symBlockATens8 = mapTo7 . symBlockTens
symBlockATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) =>
                  ([Int],[Int]) ->
                  AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                  AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
symBlockATens8 = mapTo7 . symBlockTens

--with factor

-- | Same functionality as @'symBlockTens'@ but including the usual factors of \( \frac{1}{2} \) in the result and thus defining a projection.
-- The following functions apply @'symBlockTensFac'@ to the index types of the deeper leaf levels, i.e. covariant indices of the 1st index type, contravariant indices of the 2nd index type, etc.
--
-- > symBlockTensFac inds t = (SField $ 1%2)&. symBlockTens inds t
symBlockTensFac :: (TIndex k, TAdd v, Prod (SField Rational) v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k (TProd (SField Rational) v)
symBlockTensFac inds t = (SField $ 1%2)&. symBlockTens inds t

-- | > symBlockATensFac1 = symBlockTensFac
symBlockATensFac1 :: (TIndex k1, TAdd v, Prod (SField Rational) v) =>
                     ([Int],[Int]) ->
                     AbsTensor1 n1 k1 v ->
                     AbsTensor1 n1 k1 (TProd (SField Rational) v)
symBlockATensFac1 = symBlockTensFac

-- | > symBlockATensFac2 = mapTo1 . symBlockTensFac
symBlockATensFac2 :: (TIndex k1, TAdd v, Prod (SField Rational) v) =>
                     ([Int],[Int]) ->
                     AbsTensor2 n1 n2 k1 v ->
                     AbsTensor2 n1 n2 k1 (TProd (SField Rational) v)
symBlockATensFac2 = mapTo1 . symBlockTensFac

-- | > symBlockATensFac3 = mapTo2 . symBlockTensFac
symBlockATensFac3 :: (TIndex k1, TIndex k2, TAdd v, Prod (SField Rational) v) =>
                     ([Int],[Int]) ->
                     AbsTensor3 n1 n2 n3 k1 k2 v ->
                     AbsTensor3 n1 n2 n3 k1 k2 (TProd (SField Rational) v)
symBlockATensFac3 = mapTo2 . symBlockTensFac

-- | > symBlockATensFac4 = mapTo3 . symBlockTensFac
symBlockATensFac4 :: (TIndex k1, TIndex k2, TAdd v, Prod (SField Rational) v) =>
                     ([Int],[Int]) ->
                     AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                     AbsTensor4 n1 n2 n3 n4 k1 k2 (TProd (SField Rational) v)
symBlockATensFac4 = mapTo3 . symBlockTensFac

-- | > symBlockATensFac5 = mapTo4 . symBlockTensFac
symBlockATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v, Prod (SField Rational) v) =>
                     ([Int],[Int]) ->
                     AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                     AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (TProd (SField Rational) v)
symBlockATensFac5 = mapTo4 . symBlockTensFac

-- | > symBlockATensFac6 = mapTo5 . symBlockTensFac
symBlockATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v, Prod (SField Rational) v) =>
                     ([Int],[Int]) ->
                     AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                     AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (TProd (SField Rational) v)
symBlockATensFac6 = mapTo5 . symBlockTensFac

-- | > symBlockATensFac7 = mapTo6 . symBlockTensFac
symBlockATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v, Prod (SField Rational) v) =>
                     ([Int],[Int]) ->
                     AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                     AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (TProd (SField Rational) v)
symBlockATensFac7 = mapTo6 . symBlockTensFac

-- | > symBlockATensFac8 = mapTo7 . symBlockTensFac
symBlockATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v, Prod (SField Rational) v) =>
                     ([Int],[Int]) ->
                     AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                     AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (TProd (SField Rational) v)
symBlockATensFac8 = mapTo7 . symBlockTensFac

--anti symmetrization

-- | Anti symmetrization w.r.t. the exchange of two blocks of contravariant indices of the 1st index type. The index blocks must be disjoint. These function does not include the usual \( \frac{1}{2} \) factors and thus does not
-- define a projection. The following functions apply @'aSymBlockTens'@ to the index types of the deeper leaf levels, i.e. covariant indices of the 1st index type, contravariant indices of the 2nd index type, etc.
aSymBlockTens :: (TIndex k, TAdd v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k v
aSymBlockTens inds t = t &- tensorBlockTrans inds t

-- | > aSymBlockATens1 = aSymBlockTens
aSymBlockATens1 :: (TIndex k1, TAdd v) =>
                   ([Int],[Int]) ->
                   AbsTensor1 n1 k1 v ->
                   AbsTensor1 n1 k1 v
aSymBlockATens1 = aSymBlockTens

-- | > aSymBlockATens2 = mapTo1 . aSymBlockTens
aSymBlockATens2 :: (TIndex k1, TAdd v) =>
                   ([Int],[Int]) ->
                   AbsTensor2 n1 n2 k1 v ->
                   AbsTensor2 n1 n2 k1 v
aSymBlockATens2 = mapTo1 . aSymBlockTens

-- | > aSymBlockATens3 = mapTo2 . aSymBlockTens
aSymBlockATens3 :: (TIndex k1, TIndex k2, TAdd v) =>
                   ([Int],[Int]) ->
                   AbsTensor3 n1 n2 n3 k1 k2 v ->
                   AbsTensor3 n1 n2 n3 k1 k2 v
aSymBlockATens3 = mapTo2 . aSymBlockTens

-- | > aSymBlockATens4 = mapTo3 . aSymBlockTens
aSymBlockATens4 :: (TIndex k1, TIndex k2, TAdd v) =>
                   ([Int],[Int]) ->
                   AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                   AbsTensor4 n1 n2 n3 n4 k1 k2 v
aSymBlockATens4 = mapTo3 . aSymBlockTens

-- | > aSymBlockATens5 = mapTo4 . aSymBlockTens
aSymBlockATens5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) =>
                   ([Int],[Int]) ->
                   AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                   AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
aSymBlockATens5 = mapTo4 . aSymBlockTens

-- | > aSymBlockATens6 = mapTo5 . aSymBlockTens
aSymBlockATens6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) =>
                   ([Int],[Int]) ->
                   AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                   AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
aSymBlockATens6 = mapTo5 . aSymBlockTens

-- | > aSymBlockATens7 = mapTo6 . aSymBlockTens
aSymBlockATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) =>
                   ([Int],[Int]) ->
                   AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                   AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
aSymBlockATens7 = mapTo6 . aSymBlockTens

-- | > aSymBlockATens8 = mapTo7 . aSymBlockTens
aSymBlockATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) =>
                   ([Int],[Int]) ->
                   AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                   AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
aSymBlockATens8 = mapTo7 . aSymBlockTens

--with factor

-- | Same functionality as @'aSymBlockTens'@ but including the usual factors of \( \frac{1}{2} \) in the result and thus defining a projection.
-- The following functions apply @'aSymBlockTensFac'@ to the index types of the deeper leaf levels, i.e. covariant indices of the 1st index type, contravariant indices of the 2nd index type, etc.
--
-- > aSymBlockTensFac inds t = (SField $ 1%2)&. aSymBlockTens inds t
aSymBlockTensFac :: (TIndex k, TAdd v, Prod (SField Rational) v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k (TProd (SField Rational) v)
aSymBlockTensFac inds t = (SField $ 1%2)&. aSymBlockTens inds t

-- | > aSymBlockATensFac1 = aSymBlockTensFac
aSymBlockATensFac1 :: (TIndex k1, TAdd v, Prod (SField Rational) v) =>
                      ([Int],[Int]) ->
                      AbsTensor1 n1 k1 v ->
                      AbsTensor1 n1 k1 (TProd (SField Rational) v)
aSymBlockATensFac1 = aSymBlockTensFac

-- | > aSymBlockATensFac2 = mapTo1 . aSymBlockTensFac
aSymBlockATensFac2 :: (TIndex k1, TAdd v, Prod (SField Rational) v) =>
                      ([Int],[Int]) ->
                      AbsTensor2 n1 n2 k1 v ->
                      AbsTensor2 n1 n2 k1 (TProd (SField Rational) v)
aSymBlockATensFac2 = mapTo1 . aSymBlockTensFac

-- | > aSymBlockATensFac3 = mapTo2 . aSymBlockTensFac
aSymBlockATensFac3 :: (TIndex k1, TIndex k2, TAdd v, Prod (SField Rational) v) =>
                      ([Int],[Int]) ->
                      AbsTensor3 n1 n2 n3 k1 k2 v ->
                      AbsTensor3 n1 n2 n3 k1 k2 (TProd (SField Rational) v)
aSymBlockATensFac3 = mapTo2 . aSymBlockTensFac

-- | > aSymBlockATensFac4 = mapTo3 . aSymBlockTensFac
aSymBlockATensFac4 :: (TIndex k1, TIndex k2, TAdd v, Prod (SField Rational) v) =>
                      ([Int],[Int]) ->
                      AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                      AbsTensor4 n1 n2 n3 n4 k1 k2 (TProd (SField Rational) v)
aSymBlockATensFac4 = mapTo3 . aSymBlockTensFac

-- | > aSymBlockATensFac5 = mapTo4 . aSymBlockTensFac
aSymBlockATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v, Prod (SField Rational) v) =>
                      ([Int],[Int]) ->
                      AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                      AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (TProd (SField Rational) v)
aSymBlockATensFac5 = mapTo4 . aSymBlockTensFac

-- | > aSymBlockATensFac6 = mapTo5 . aSymBlockTensFac
aSymBlockATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v, Prod (SField Rational) v) =>
                      ([Int],[Int]) ->
                      AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                      AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (TProd (SField Rational) v)
aSymBlockATensFac6 = mapTo5 . aSymBlockTensFac

-- | > aSymBlockATensFac7 = mapTo6 . aSymBlockTensFac
aSymBlockATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v, Prod (SField Rational) v) =>
                      ([Int],[Int]) ->
                      AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                      AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (TProd (SField Rational) v)
aSymBlockATensFac7 = mapTo6 . aSymBlockTensFac

-- | > aSymBlockATensFac8 = mapTo7 . aSymBlockTensFac
aSymBlockATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v, Prod (SField Rational) v) =>
                      ([Int],[Int]) ->
                      AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                      AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (TProd (SField Rational) v)
aSymBlockATensFac8 = mapTo7 . aSymBlockTensFac

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

-- | Cyclic symmetrization of the specified subset of contravariant @'Tensor'@ indices of the first index type. The function does not include
-- usual factors of \( \frac{1}{i!} \) where \( i \) is the number of indices w.r.t. which the symmetrization is performed.
-- Further functions that are displayed below apply @'cyclicSymTens'@ to the various deeper @'Tensor'@ levels.
cyclicSymTens :: (TIndex k, TAdd v) => [Int] -> Tensor n k v -> Tensor n k v
cyclicSymTens inds t = newTens
        where
            swapList = getAllSwaps inds
            tensList = map (foldr tensorTrans t) swapList
            newTens = foldr (&+) t tensList

-- | > cyclicSymATens1 = cyclicSymTens
cyclicSymATens1 :: (TIndex k1, TAdd v) =>
                   [Int] ->
                   AbsTensor1 n1 k1 v ->
                   AbsTensor1 n1 k1 v
cyclicSymATens1 = cyclicSymTens

-- | > cyclicSymATens2 = mapTo1 . cyclicSymTens
cyclicSymATens2 :: (TIndex k1, TAdd v) =>
                   [Int] ->
                   AbsTensor2 n1 n2 k1 v ->
                   AbsTensor2 n1 n2 k1 v
cyclicSymATens2 = mapTo1 . cyclicSymTens

-- | > cyclicSymATens3 = mapTo2 . cyclicSymTens
cyclicSymATens3 :: (TIndex k1, TIndex k2, TAdd v) =>
                   [Int] ->
                   AbsTensor3 n1 n2 n3 k1 k2 v ->
                   AbsTensor3 n1 n2 n3 k1 k2 v
cyclicSymATens3 = mapTo2 . cyclicSymTens

-- | > cyclicSymATens4 = mapTo3 . cyclicSymTens
cyclicSymATens4 :: (TIndex k1, TIndex k2, TAdd v) =>
                   [Int] ->
                   AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                   AbsTensor4 n1 n2 n3 n4 k1 k2 v
cyclicSymATens4 = mapTo3 . cyclicSymTens

-- | > cyclicSymATens5 = mapTo4 . cyclicSymTens
cyclicSymATens5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) =>
                   [Int] ->
                   AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                   AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
cyclicSymATens5 = mapTo4 . cyclicSymTens

-- | > cyclicSymATens6 = mapTo5 . cyclicSymTens
cyclicSymATens6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) =>
                   [Int] ->
                   AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                   AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
cyclicSymATens6 = mapTo5 . cyclicSymTens

-- | > cyclicSymATens7 = mapTo6 . cyclicSymTens
cyclicSymATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) =>
                   [Int] ->
                   AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                   AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
cyclicSymATens7 = mapTo6 . cyclicSymTens

-- | > cyclicSymATens8 = mapTo7 . cyclicSymTens
cyclicSymATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) =>
                   [Int] ->
                   AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                   AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
cyclicSymATens8 = mapTo7 . cyclicSymTens

--with factor

-- | This function provides the same functionality as @'cyclicSymTens'@ with the only difference that it includes the \(\frac{1}{i!}\) factors.
-- The functions that are further provided apply @'cyclicSymTensFac'@ to deeper @'Tensor'@ levels.
--
-- > cyclicSymTensFac inds t = fac &. cyclicSymTens inds t
cyclicSymTensFac :: (TIndex k, TAdd v, Prod (SField Rational) v) => [Int] -> Tensor n k v -> Tensor n k (TProd (SField Rational) v)
cyclicSymTensFac inds t = fac &. cyclicSymTens inds t
        where
            fac = SField $ 1 % fromIntegral (factorial $ length inds)

-- | > cyclicSymATensFac1 = cyclicSymTensFac
cyclicSymATensFac1 :: (TIndex k1, TAdd v, Prod (SField Rational) v) =>
                      [Int] ->
                      AbsTensor1 n1 k1 v ->
                      AbsTensor1 n1 k1 (TProd (SField Rational) v)
cyclicSymATensFac1 = cyclicSymTensFac

-- | > cyclicSymATensFac2 = mapTo1 . cyclicSymTensFac
cyclicSymATensFac2 :: (TIndex k1, TAdd v, Prod (SField Rational) v) =>
                      [Int] ->
                      AbsTensor2 n1 n2 k1 v ->
                      AbsTensor2 n1 n2 k1 (TProd (SField Rational) v)
cyclicSymATensFac2 = mapTo1 . cyclicSymTensFac

-- | > cyclicSymATensFac3 = mapTo2 . cyclicSymTensFac
cyclicSymATensFac3 :: (TIndex k1, TIndex k2, TAdd v, Prod (SField Rational) v) =>
                      [Int] ->
                      AbsTensor3 n1 n2 n3 k1 k2 v ->
                      AbsTensor3 n1 n2 n3 k1 k2 (TProd (SField Rational) v)
cyclicSymATensFac3 = mapTo2 . cyclicSymTensFac

-- | > cyclicSymATensFac4 = mapTo3 . cyclicSymTensFac
cyclicSymATensFac4 :: (TIndex k1, TIndex k2, TAdd v, Prod (SField Rational) v) =>
                      [Int] ->
                      AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                      AbsTensor4 n1 n2 n3 n4 k1 k2 (TProd (SField Rational) v)
cyclicSymATensFac4 = mapTo3 . cyclicSymTensFac

-- | > cyclicSymATensFac5 = mapTo4 . cyclicSymTensFac
cyclicSymATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v, Prod (SField Rational) v) =>
                      [Int] ->
                      AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                      AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (TProd (SField Rational) v)
cyclicSymATensFac5 = mapTo4 . cyclicSymTensFac

-- | > cyclicSymATensFac6 = mapTo5 . cyclicSymTensFac
cyclicSymATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v, Prod (SField Rational) v) =>
                      [Int] ->
                      AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                      AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (TProd (SField Rational) v)
cyclicSymATensFac6 = mapTo5 . cyclicSymTensFac

-- | > cyclicSymATensFac7 = mapTo6 . cyclicSymTensFac
cyclicSymATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v, Prod (SField Rational) v) =>
                      [Int] ->
                      AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                      AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (TProd (SField Rational) v)
cyclicSymATensFac7 = mapTo6 . cyclicSymTensFac

-- | > cyclicSymATensFac8 = mapTo7 . cyclicSymTensFac
cyclicSymATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v, Prod (SField Rational) v) =>
                      [Int] ->
                      AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                      AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (TProd (SField Rational) v)
cyclicSymATensFac8 = mapTo7 . cyclicSymTensFac

--cyclic anti symmetrization

-- | Cyclic anti symmetrization of the specified subset of contravariant @'Tensor'@ indices of the first index type. The function does not include
-- usual factors of \( \frac{1}{i!} \) where \( i \) is the number of indices w.r.t. which the symmetrization is performed.
-- Further functions that are displayed below apply @'cyclicASymTens'@ to the various deeper @'Tensor'@ levels.
cyclicASymTens :: (TIndex k, TAdd v) => [Int] -> Tensor n k v -> Tensor n k v
cyclicASymTens inds t = newTens
        where
            swapList = getAllSwaps inds
            signList = map (\x -> (-1) ^ length x) swapList
            tensList' = map (foldr tensorTrans t) swapList
            tensList = zipWith (\s v -> if s == -1 then negateS v else v) signList tensList'
            newTens = foldr (&+) t tensList

-- | > cyclicASymATens1 = cyclicASymTens
cyclicASymATens1 :: (TIndex k1, TAdd v) =>
                    [Int] ->
                    AbsTensor1 n1 k1 v ->
                    AbsTensor1 n1 k1 v
cyclicASymATens1 = cyclicASymTens

-- | > cyclicASymATens2 = mapTo1 . cyclicASymTens
cyclicASymATens2 :: (TIndex k1, TAdd v) =>
                    [Int] ->
                    AbsTensor2 n1 n2 k1 v ->
                    AbsTensor2 n1 n2 k1 v
cyclicASymATens2 = mapTo1 . cyclicASymTens

-- | > cyclicASymATens3 = mapTo2 . cyclicASymTens
cyclicASymATens3 :: (TIndex k1, TIndex k2, TAdd v) =>
                    [Int] ->
                    AbsTensor3 n1 n2 n3 k1 k2 v ->
                    AbsTensor3 n1 n2 n3 k1 k2 v
cyclicASymATens3 = mapTo2 . cyclicASymTens

-- | > cyclicASymATens4 = mapTo3 . cyclicASymTens
cyclicASymATens4 :: (TIndex k1, TIndex k2, TAdd v) =>
                    [Int] ->
                    AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                    AbsTensor4 n1 n2 n3 n4 k1 k2 v
cyclicASymATens4 = mapTo3 . cyclicASymTens

-- | > cyclicASymATens5 = mapTo4 . cyclicASymTens
cyclicASymATens5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) =>
                    [Int] ->
                    AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                    AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
cyclicASymATens5 = mapTo4 . cyclicASymTens

-- | > cyclicASymATens6 = mapTo5 . cyclicASymTens
cyclicASymATens6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) =>
                    [Int] ->
                    AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                    AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
cyclicASymATens6 = mapTo5 . cyclicASymTens

-- | > cyclicASymATens7 = mapTo6 . cyclicASymTens
cyclicASymATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) =>
                    [Int] ->
                    AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                    AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
cyclicASymATens7 = mapTo6 . cyclicASymTens

-- | > cyclicASymATens8 = mapTo7 . cyclicASymTens
cyclicASymATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) =>
                    [Int] ->
                    AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                    AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
cyclicASymATens8 = mapTo7 . cyclicASymTens

--with factor

-- | This function provides the same functionality as @'cyclicASymTens'@ with the only difference that it includes the \(\frac{1}{i!}\) factors.
-- The functions that are further provided apply @'cyclicASymTensFac'@ to deeper @'Tensor'@ levels.
--
-- > cyclicASymTensFac inds t = fac &. cyclicASymTens inds t
cyclicASymTensFac :: (TIndex k, TAdd v, Prod (SField Rational) v) => [Int] -> Tensor n k v -> Tensor n k (TProd (SField Rational) v)
cyclicASymTensFac inds t = fac &. cyclicASymTens inds t
        where
            fac = SField $ 1 % fromIntegral (factorial $ length inds)

-- | > cyclicASymATensFac1 = cyclicASymTensFac
cyclicASymATensFac1 :: (TIndex k1, TAdd v, Prod (SField Rational) v) =>
                       [Int] ->
                       AbsTensor1 n1 k1 v ->
                       AbsTensor1 n1 k1 (TProd (SField Rational) v)
cyclicASymATensFac1 = cyclicASymTensFac

-- | > cyclicASymATensFac2 = mapTo1 . cyclicASymTensFac
cyclicASymATensFac2 :: (TIndex k1, TAdd v, Prod (SField Rational) v) =>
                       [Int] ->
                       AbsTensor2 n1 n2 k1 v ->
                       AbsTensor2 n1 n2 k1 (TProd (SField Rational) v)
cyclicASymATensFac2 = mapTo1 . cyclicASymTensFac

-- | > cyclicASymATensFac3 = mapTo2 . cyclicASymTensFac
cyclicASymATensFac3 :: (TIndex k1, TIndex k2, TAdd v, Prod (SField Rational) v) =>
                       [Int] ->
                       AbsTensor3 n1 n2 n3 k1 k2 v ->
                       AbsTensor3 n1 n2 n3 k1 k2 (TProd (SField Rational) v)
cyclicASymATensFac3 = mapTo2 . cyclicASymTensFac

-- | > cyclicASymATensFac4 = mapTo3 . cyclicASymTensFac
cyclicASymATensFac4 :: (TIndex k1, TIndex k2, TAdd v, Prod (SField Rational) v) =>
                       [Int] ->
                       AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                       AbsTensor4 n1 n2 n3 n4 k1 k2 (TProd (SField Rational) v)
cyclicASymATensFac4 = mapTo3 . cyclicASymTensFac

-- | > cyclicASymATensFac5 = mapTo4 . cyclicASymTensFac
cyclicASymATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v, Prod (SField Rational) v) =>
                       [Int] ->
                       AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                       AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (TProd (SField Rational) v)
cyclicASymATensFac5 = mapTo4 . cyclicASymTensFac

-- | > cyclicASymATensFac6 = mapTo5 . cyclicASymTensFac
cyclicASymATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v, Prod (SField Rational) v) =>
                       [Int] ->
                       AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                       AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (TProd (SField Rational) v)
cyclicASymATensFac6 = mapTo5 . cyclicASymTensFac

-- | > cyclicASymATensFac7 = mapTo6 . cyclicASymTensFac
cyclicASymATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v, Prod (SField Rational) v) =>
                       [Int] ->
                       AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                       AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (TProd (SField Rational) v)
cyclicASymATensFac7 = mapTo6 . cyclicASymTensFac

-- | > cyclicASymATensFac8 = mapTo7 . cyclicASymTensFac
cyclicASymATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v, Prod (SField Rational) v) =>
                       [Int] ->
                       AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                       AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (TProd (SField Rational) v)
cyclicASymATensFac8 = mapTo7 . cyclicASymTensFac

--cyclic block symmetrization

-- | Cyclic block symmetrization, i.e. symmetrization w.r.t. any permutation of the specified index blocks of contravariant indices of the first index type.
-- Usual factors of \( \frac{1}{i!} \) where \( i \) is the number of index blocks w.r.t. which the symmetrization is performed are not included.
-- The functions that are displayed further below apply @'cyclicBlockSymTens'@ to the various deeper @'Tensor'@ levels.
cyclicBlockSymTens :: (TIndex k, TAdd v) => [[Int]] -> Tensor n k v -> Tensor n k v
cyclicBlockSymTens inds t = newTens
        where
            swapList = getAllBlockSwaps inds
            tensList = map (foldr tensorBlockTrans t) swapList
            newTens = foldr (&+) t tensList

-- | > cyclicBlockSymATens1 = cyclicBlockSymTens
cyclicBlockSymATens1 :: (TIndex k1, TAdd v) =>
                        [[Int]] ->
                        AbsTensor1 n1 k1 v ->
                        AbsTensor1 n1 k1 v
cyclicBlockSymATens1 = cyclicBlockSymTens

-- | > cyclicBlockSymATens2 = mapTo1 . cyclicBlockSymTens
cyclicBlockSymATens2 :: (TIndex k1, TAdd v) =>
                        [[Int]] ->
                        AbsTensor2 n1 n2 k1 v ->
                        AbsTensor2 n1 n2 k1 v
cyclicBlockSymATens2 = mapTo1 . cyclicBlockSymTens

-- | > cyclicBlockSymATens3 = mapTo2 . cyclicBlockSymTens
cyclicBlockSymATens3 :: (TIndex k1, TIndex k2, TAdd v) =>
                        [[Int]] ->
                        AbsTensor3 n1 n2 n3 k1 k2 v ->
                        AbsTensor3 n1 n2 n3 k1 k2 v
cyclicBlockSymATens3 = mapTo2 . cyclicBlockSymTens

-- | > cyclicBlockSymATens4 = mapTo3 . cyclicBlockSymTens
cyclicBlockSymATens4 :: (TIndex k1, TIndex k2, TAdd v) =>
                        [[Int]] ->
                        AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                        AbsTensor4 n1 n2 n3 n4 k1 k2 v
cyclicBlockSymATens4 = mapTo3 . cyclicBlockSymTens

-- | > cyclicBlockSymATens5 = mapTo4 . cyclicBlockSymTens
cyclicBlockSymATens5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) =>
                        [[Int]] ->
                        AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                        AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
cyclicBlockSymATens5 = mapTo4 . cyclicBlockSymTens

-- | > cyclicBlockSymATens6 = mapTo5 . cyclicBlockSymTens
cyclicBlockSymATens6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) =>
                        [[Int]] ->
                        AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                        AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
cyclicBlockSymATens6 = mapTo5 . cyclicBlockSymTens

-- | > cyclicBlockSymATens7 = mapTo6 . cyclicBlockSymTens
cyclicBlockSymATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) =>
                        [[Int]] ->
                        AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                        AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
cyclicBlockSymATens7 = mapTo6 . cyclicBlockSymTens

-- | > cyclicBlockSymATens8 = mapTo7 . cyclicBlockSymTens
cyclicBlockSymATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) =>
                        [[Int]] ->
                        AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                        AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
cyclicBlockSymATens8 = mapTo7 . cyclicBlockSymTens

--with factor

-- | This function provides the same functionality as @'cyclicBlockSymTens'@ with the only difference that it includes the \(\frac{1}{i!}\) factors.
-- The functions that are further provided apply @'cyclicASymTensFac'@ to deeper @'Tensor'@ levels.
--
-- > cyclicBlockSymTensFac inds t = fac &. cyclicBlockSymTens inds t
cyclicBlockSymTensFac :: (TIndex k, TAdd v, Prod (SField Rational) v) => [[Int]] -> Tensor n k v -> Tensor n k (TProd (SField Rational) v)
cyclicBlockSymTensFac inds t = fac &. cyclicBlockSymTens inds t
        where
            fac = SField $ 1 % fromIntegral (factorial $ length inds)

-- | > cyclicBlockSymATensFac1 = cyclicBlockSymTensFac
cyclicBlockSymATensFac1 :: (TIndex k1, TAdd v, Prod (SField Rational) v) =>
                           [[Int]] ->
                           AbsTensor1 n1 k1 v ->
                           AbsTensor1 n1 k1 (TProd (SField Rational) v)
cyclicBlockSymATensFac1 = cyclicBlockSymTensFac

-- | > cyclicBlockSymATensFac2 = mapTo1 . cyclicBlockSymTensFac
cyclicBlockSymATensFac2 :: (TIndex k1, TAdd v, Prod (SField Rational) v) =>
                           [[Int]] ->
                           AbsTensor2 n1 n2 k1 v ->
                           AbsTensor2 n1 n2 k1 (TProd (SField Rational) v)
cyclicBlockSymATensFac2 = mapTo1 . cyclicBlockSymTensFac

-- | > cyclicBlockSymATensFac3 = mapTo2 . cyclicBlockSymTensFac
cyclicBlockSymATensFac3 :: (TIndex k1, TIndex k2, TAdd v, Prod (SField Rational) v) =>
                           [[Int]] ->
                           AbsTensor3 n1 n2 n3 k1 k2 v ->
                           AbsTensor3 n1 n2 n3 k1 k2 (TProd (SField Rational) v)
cyclicBlockSymATensFac3 = mapTo2 . cyclicBlockSymTensFac

-- | > cyclicBlockSymATensFac4 = mapTo3 . cyclicBlockSymTensFac
cyclicBlockSymATensFac4 :: (TIndex k1, TIndex k2, TAdd v, Prod (SField Rational) v) =>
                           [[Int]] ->
                           AbsTensor4 n1 n2 n3 n4 k1 k2 v ->
                           AbsTensor4 n1 n2 n3 n4 k1 k2 (TProd (SField Rational) v)
cyclicBlockSymATensFac4 = mapTo3 . cyclicBlockSymTensFac

-- | > cyclicBlockSymATensFac5 = mapTo4 . cyclicBlockSymTensFac
cyclicBlockSymATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v, Prod (SField Rational) v) =>
                           [[Int]] ->
                           AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v ->
                           AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (TProd (SField Rational) v)
cyclicBlockSymATensFac5 = mapTo4 . cyclicBlockSymTensFac

-- | > cyclicBlockSymATensFac6 = mapTo5 . cyclicBlockSymTensFac
cyclicBlockSymATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v, Prod (SField Rational) v) =>
                           [[Int]] ->
                           AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v ->
                           AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (TProd (SField Rational) v)
cyclicBlockSymATensFac6 = mapTo5 . cyclicBlockSymTensFac

-- | > cyclicBlockSymATensFac7 = mapTo6 . cyclicBlockSymTensFac
cyclicBlockSymATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v, Prod (SField Rational) v) =>
                           [[Int]] ->
                           AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v ->
                           AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (TProd (SField Rational) v)
cyclicBlockSymATensFac7 = mapTo6 . cyclicBlockSymTensFac

-- | > cyclicBlockSymATensFac8 = mapTo7 . cyclicBlockSymTensFac
cyclicBlockSymATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v, Prod (SField Rational) v) =>
                           [[Int]] ->
                           AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v ->
                           AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (TProd (SField Rational) v)
cyclicBlockSymATensFac8 = mapTo7 . cyclicBlockSymTensFac

--contraction for general tensors

-- | This functions compute the contraction of a @'Tensor'@ in two of its indices of the same index type, i.e. the function sets the two indices equal and sums over their whole index range.
tensorContr :: (TIndex k, TAdd v) => (Int,Int) -> Tensor2 n1 n2 k v -> Tensor2 (n1-1) (n2-1) k v
tensorContr (0,j) t = fromListT tensList
    where
        l = map (\(x,y) -> (x, toListT y)) $ toListT t
        l2 = map (\(x,y) -> (tailInd x,mapMaybe (removeContractionInd j (headInd x)) y)) l
        l3 = filter (\(_,y) -> not (null y)) l2
        tensList = map (\(x,y) -> (x, fromListT y)) l3
tensorContr (i,j) (Tensor m) = Tensor $ mapTMap (tensorContr (i-1,j)) m
tensorContr inds ZeroTensor = ZeroTensor
tensorContr inds (Scalar s) = error "cannot contract scalar!"

-- | @'contrATens1'@ is a synonym for @'tensorContr'@. It applies the contraction to the 1st index type of a tensor.
--
-- > contrATens1 = tensorContr
contrATens1 :: (TIndex k1, TAdd v) => (Int,Int) -> AbsTensor2 (n1+1) (n2+1) k1 v -> AbsTensor2 n1 n2 k1 v
contrATens1 = tensorContr

-- | @'contrATens2'@ applies the contraction to the 2nd index type of a tensor.
--
-- > contrATens2 = mapTo2 . tensorContr
contrATens2 :: (TIndex k1, TIndex k2, TAdd v) => (Int,Int) -> AbsTensor4 n1 n2 (n3+1) (n4+1) k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
contrATens2 = mapTo2 . tensorContr

-- | @'contrATens3'@ applies the contraction to the 3rd index type of a tensor.
--
-- > contrATens3 = mapTo4 . tensorContr
contrATens3 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) => (Int,Int) -> AbsTensor6 n1 n2 n3 n4 (n5+1) (n6+1) k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
contrATens3 = mapTo4 . tensorContr

-- | @'contrATens4'@ applies the contraction to the 4th index type of a tensor.
--
-- > contrATens4 = mapTo6 . tensorContr
contrATens4 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) => (Int,Int) -> AbsTensor8 n1 n2 n3 n4 n5 n6 (n7+1) (n8+1) k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
contrATens4 = mapTo6 . tensorContr

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

mkTens1 :: (IndTuple1 n1 k1, v) -> AbsTensor1 n1 k1 v
mkTens1 = mkTens

mkTens2 :: (IndTuple2 n1 n2 k1, v) -> AbsTensor2 n1 n2 k1 v
mkTens2 ((i1,i2),s) = mkTens (i1,mkTens (i2,s))

mkTens3 :: (IndTuple3 n1 n2 n3 k1 k2, v) -> AbsTensor3 n1 n2 n3 k1 k2 v
mkTens3 ((i1,i2,i3),s) = mkTens (i1,mkTens (i2,mkTens (i3,s)))

mkTens4 :: (IndTuple4 n1 n2 n3 n4 k1 k2, v) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
mkTens4 ((i1,i2,i3,i4),s) = mkTens (i1,mkTens (i2,mkTens (i3,mkTens (i4,s))))

mkTens5 :: (IndTuple5 n1 n2 n3 n4 n5 k1 k2 k3, v) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
mkTens5 ((i1,i2,i3,i4,i5),s) = mkTens (i1,mkTens (i2,mkTens (i3,mkTens (i4,mkTens (i5,s)))))

mkTens6 :: (IndTuple6 n1 n2 n3 n4 n5 n6 k1 k2 k3, v) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
mkTens6 ((i1,i2,i3,i4,i5,i6),s) = mkTens (i1,mkTens (i2,mkTens (i3,mkTens (i4,mkTens (i5,mkTens (i6,s))))))

mkTens7 :: (IndTuple7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4, v) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
mkTens7 ((i1,i2,i3,i4,i5,i6,i7),s) = mkTens (i1,mkTens (i2,mkTens (i3,mkTens (i4,mkTens (i5,mkTens (i6,mkTens (i7,s)))))))

mkTens8 :: (IndTuple8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4, v) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
mkTens8 ((i1,i2,i3,i4,i5,i6,i7,i8),s) = mkTens (i1,mkTens (i2,mkTens (i3,mkTens (i4,mkTens (i5,mkTens (i6,mkTens (i7,mkTens (i8,s))))))))

--convert a tensor to corresponding assocs list

-- | The following function converts a @'Tensor'@ to a typed index tuple list, i.e. a list of key value pairs with keys of type @'IndList'@ and
-- values being the corresponding @'Tensor'@ value.
toListT :: Tensor n k v -> [(IndList n k, v)]
toListT (Scalar x) = [(Empty, x)]
toListT (Tensor m) =  concatMap (\(i,t) -> appendF i $ toListT t) m
        where
            appendF i = map (\(l,val) -> (Append i l ,val))
toListT ZeroTensor = []

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

-- | This function converts a given @'Tensor'@ to a non-typed index tuple list.
toListT' :: (TIndex k, TAdd v) => Tensor n k v -> [([Int],v)]
toListT' t = filter (not . scaleZero . snd) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT t
            showInd i1 = map fromEnum $ toList i1

toListT1' :: (TIndex k1, TAdd v) => AbsTensor1 n1 k1 v -> [([Int],v)]
toListT1' = toListT'

toListT2' :: (TIndex k1, TAdd v) => AbsTensor2 n1 n2 k1 v -> [(([Int], [Int]), v)]
toListT2' t = filter (not . scaleZero . snd) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT2 t
            showInd (i1,i2) = (map fromEnum (toList i1), map fromEnum (toList i2))

toListT3' :: (TIndex k1, TIndex k2, TAdd v) => AbsTensor3 n1 n2 n3 k1 k2 v -> [(([Int], [Int], [Int]),v)]
toListT3' t = filter (not . scaleZero . snd) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT3 t
            showInd (i1,i2,i3) = (map fromEnum (toList i1), map fromEnum (toList i2),
                                 map fromEnum (toList i3))

toListT4' :: (TIndex k1, TIndex k2, TAdd v) => AbsTensor4 n1 n2 n3 n4 k1 k2 v -> [(([Int], [Int], [Int], [Int]),v)]
toListT4' t = filter (not . scaleZero . snd) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT4 t
            showInd (i1,i2,i3,i4) = (map fromEnum (toList i1), map fromEnum (toList i2),
                                    map fromEnum (toList i3), map fromEnum (toList i4))

toListT5' :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) => AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> [(([Int], [Int], [Int], [Int], [Int]),v)]
toListT5' t = filter (not . scaleZero . snd) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT5 t
            showInd (i1,i2,i3,i4,i5) = (map fromEnum (toList i1), map fromEnum (toList i2),
                                    map fromEnum (toList i3), map fromEnum (toList i4),
                                    map fromEnum (toList i5))

toListT6' :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) => AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> [(([Int], [Int], [Int], [Int], [Int], [Int]),v)]
toListT6' t = filter (not . scaleZero . snd) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT6 t
            showInd (i1,i2,i3,i4,i5,i6) = (map fromEnum (toList i1), map fromEnum (toList i2),
                                        map fromEnum (toList i3), map fromEnum (toList i4),
                                        map fromEnum (toList i5), map fromEnum (toList i6))

toListT7' :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) => AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> [(([Int], [Int], [Int], [Int], [Int], [Int], [Int]),v)]
toListT7' t = filter (not . scaleZero . snd) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT7 t
            showInd (i1,i2,i3,i4,i5,i6,i7) = (map fromEnum (toList i1), map fromEnum (toList i2),
                                        map fromEnum (toList i3), map fromEnum (toList i4),
                                        map fromEnum (toList i5), map fromEnum (toList i6),
                                        map fromEnum (toList i7))

toListT8' :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) => AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> [(([Int], [Int], [Int], [Int], [Int], [Int], [Int], [Int]),v)]
toListT8' t = filter (not . scaleZero . snd) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT8 t
            showInd (i1,i2,i3,i4,i5,i6,i7,i8) = (map fromEnum (toList i1), map fromEnum (toList i2),
                                        map fromEnum (toList i3), map fromEnum (toList i4),
                                        map fromEnum (toList i5), map fromEnum (toList i6),
                                        map fromEnum (toList i7), map fromEnum (toList i8))


--convert to non type safe assocs list, all indices regardless of their type are collected in the [Int] list

toListShow1 :: (TIndex k1, TAdd v) => AbsTensor1 n1 k1 v -> [([Int],v)]
toListShow1 = toListT'

toListShow2 :: (TIndex k1, TAdd v) => AbsTensor2 n1 n2 k1 v -> [([Int],v)]
toListShow2 t = filter (not . scaleZero . snd) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT2 t
            showInd (i1,i2) = map fromEnum (toList i1) ++ map fromEnum (toList i2)

toListShow3 :: (TIndex k1, TIndex k2, TAdd v) => AbsTensor3 n1 n2 n3 k1 k2 v -> [([Int],v)]
toListShow3 t = filter (not . scaleZero . snd) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT3 t
            showInd (i1,i2,i3) = map fromEnum (toList i1) ++ map fromEnum (toList i2) ++
                                 map fromEnum (toList i3)

toListShow4 :: (TIndex k1, TIndex k2, TAdd v) => AbsTensor4 n1 n2 n3 n4 k1 k2 v -> [([Int],v)]
toListShow4 t = filter (not . scaleZero . snd) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT4 t
            showInd (i1,i2,i3,i4) = map fromEnum (toList i1) ++ map fromEnum (toList i2) ++
                                    map fromEnum (toList i3) ++ map fromEnum (toList i4)

toListShow5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) => AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> [([Int],v)]
toListShow5 t = filter (not . scaleZero . snd) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT5 t
            showInd (i1,i2,i3,i4,i5) = map fromEnum (toList i1) ++ map fromEnum (toList i2) ++
                                    map fromEnum (toList i3) ++ map fromEnum (toList i4) ++
                                    map fromEnum (toList i5)

toListShow6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd v) => AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> [([Int],v)]
toListShow6 t = filter (not . scaleZero . snd) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT6 t
            showInd (i1,i2,i3,i4,i5,i6) = map fromEnum (toList i1) ++ map fromEnum (toList i2) ++
                                        map fromEnum (toList i3) ++ map fromEnum (toList i4) ++
                                        map fromEnum (toList i5) ++ map fromEnum (toList i6)

toListShow7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) => AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> [([Int],v)]
toListShow7 t = filter (not . scaleZero . snd) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT7 t
            showInd (i1,i2,i3,i4,i5,i6,i7) = map fromEnum (toList i1) ++ map fromEnum (toList i2) ++
                                        map fromEnum (toList i3) ++ map fromEnum (toList i4) ++
                                        map fromEnum (toList i5) ++ map fromEnum (toList i6) ++
                                        map fromEnum (toList i7)

toListShow8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd v) => AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> [([Int],v)]
toListShow8 t = filter (not . scaleZero . snd) $ map (\(x,y) -> (showInd x, y)) l
        where
            l = toListT8 t
            showInd (i1,i2,i3,i4,i5,i6,i7,i8) = map fromEnum (toList i1) ++ map fromEnum (toList i2) ++
                                        map fromEnum (toList i3) ++ map fromEnum (toList i4) ++
                                        map fromEnum (toList i5) ++ map fromEnum (toList i6) ++
                                        map fromEnum (toList i7) ++ map fromEnum (toList i8)


--flatten tensor with ansVar values to assocs list

toListShowVar1 :: (TIndex k1, TAdd a) => AbsTensor1 n1 k1 (AnsVar a) -> [([Int], [(Int, a)])]
toListShowVar1 t = filter (not . null . snd) $ map (\(a,AnsVar b) -> (a, filter (not . scaleZero . snd) $ I.assocs b)) l
        where
            l = toListShow1 t

toListShowVar2 :: (TIndex k1, TAdd a) => AbsTensor2 n1 n2 k1 (AnsVar a) -> [([Int], [(Int, a)])]
toListShowVar2 t = filter (not . null . snd) $ map (\(a,AnsVar b) -> (a, filter (not . scaleZero . snd) $ I.assocs b)) l
        where
            l = toListShow2 t

toListShowVar3 :: (TIndex k1, TIndex k2, TAdd a) => AbsTensor3 n1 n2 n3 k1 k2 (AnsVar a) -> [([Int], [(Int, a)])]
toListShowVar3 t = filter (not . null . snd) $ map (\(a,AnsVar b) -> (a, filter (not . scaleZero . snd) $ I.assocs b)) l
        where
            l = toListShow3 t

toListShowVar4 :: (TIndex k1, TIndex k2, TAdd a) => AbsTensor4 n1 n2 n3 n4 k1 k2 (AnsVar a) -> [([Int], [(Int, a)])]
toListShowVar4 t = filter (not . null . snd) $ map (\(a,AnsVar b) -> (a, filter (not . scaleZero . snd) $ I.assocs b)) l
        where
            l = toListShow4 t

toListShowVar5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd a) => AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (AnsVar a) -> [([Int], [(Int, a)])]
toListShowVar5 t = filter (not . null . snd) $ map (\(a,AnsVar b) -> (a, filter (not . scaleZero . snd) $ I.assocs b)) l
        where
            l = toListShow5 t

toListShowVar6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd a) => AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (AnsVar a)  -> [([Int], [(Int, a)])]
toListShowVar6 t = filter (not . null . snd) $ map (\(a,AnsVar b) -> (a, filter (not . scaleZero . snd) $ I.assocs b)) l
        where
            l = toListShow6 t

toListShowVar7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd a) => AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (AnsVar a) -> [([Int], [(Int, a)])]
toListShowVar7 t = filter (not . null . snd) $ map (\(a,AnsVar b) -> (a, filter (not . scaleZero . snd) $ I.assocs b)) l
        where
            l = toListShow7 t

toListShowVar8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd a) => AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (AnsVar a) -> [([Int], [(Int, a)])]
toListShowVar8 t = filter (not . null . snd) $ map (\(a,AnsVar b) -> (a, filter (not . scaleZero . snd) $ I.assocs b)) l
        where
            l = toListShow8 t

--write the tensor data into a matrix: columns label the occurring AnsVars (note that this is possible as we restricted to the case where the vars only occur linearly)
--rows label the non zero entries in the tensor

toMatList1' :: (TIndex k1, TAdd a) => AbsTensor1 n1 k1 (AnsVar a) -> [[(Int, a)]]
toMatList1' t = map snd $ toListShowVar1 t

toMatList2' :: (TIndex k1, TAdd a) => AbsTensor2 n1 n2 k1 (AnsVar a) -> [[(Int, a)]]
toMatList2' t = map snd $ toListShowVar2 t

toMatList3' :: (TIndex k1, TIndex k2, TAdd a) => AbsTensor3 n1 n2 n3 k1 k2 (AnsVar a) -> [[(Int, a)]]
toMatList3' t = map snd $ toListShowVar3 t

toMatList4' :: (TIndex k1, TIndex k2, TAdd a) => AbsTensor4 n1 n2 n3 n4 k1 k2 (AnsVar a) -> [[(Int, a)]]
toMatList4' t = map snd $ toListShowVar4 t

toMatList5' :: (TIndex k1, TIndex k2, TIndex k3, TAdd a) => AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (AnsVar a) -> [[(Int, a)]]
toMatList5' t = map snd $ toListShowVar5 t

toMatList6' :: (TIndex k1, TIndex k2, TIndex k3, TAdd a) => AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (AnsVar a) -> [[(Int, a)]]
toMatList6' t = map snd $ toListShowVar6 t

toMatList7' :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd a) => AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (AnsVar a) -> [[(Int, a)]]
toMatList7' t = map snd $ toListShowVar7 t

toMatList8' :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd a) => AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (AnsVar a) -> [[(Int, a)]]
toMatList8' t = map snd $ toListShowVar8 t

normalize :: [(Int,Rational)] -> ([(Int,Rational)],Rational)
normalize [] = ([],1)
normalize ((a,b) : xs) = ((a,1) : map (\(x,y) -> (x,y / b)) xs,b)

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

-- | Usual map function for heterogeneous tensor lists.

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

infixr 5 ...>

(...>) :: AbsTensor1 n1 k1 v -> TensList1 k1 v -> TensList1 k1 v
(...>) = AppendTList1


singletonTList1 :: AbsTensor1 n1 k1 v -> TensList1 k1 v
singletonTList1 t = t ...> EmptyTList1

infixr 5  ...+

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

infixr 5 ..&+

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

infixr 5 .&.+

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

infixr 5 .&&+

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

infixr 5 &..+

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

infixr 5 &.&+

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

infixr 5 &&.+

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

infixr 5 &&&+

(&&&+) :: TensList8 k1 k2 k3 k4 v -> TensList8 k1 k2 k3 k4 v -> TensList8 k1 k2 k3 k4 v
(&&&+) EmptyTList8 t1 = t1
(&&&+) t1 EmptyTList8 = t1
(&&&+) (AppendTList8 t1 EmptyTList8) t2 = AppendTList8 t1 t2
(&&&+) (AppendTList8 t1 t1') t2 = AppendTList8 t1 (t1' &&&+ t2)


--collect data of heterogeneous tensor list in one sparse matrix assocs list
--intended for evaluating tensorial equations: the values are only collected up to overall factors

collectMatList :: [[(Int, a)]] -> [((Int, Int), a)]
collectMatList matList = l
    where
        -- l2 = nubBy (\(a,_) (b,_) -> a == b) $ map normalize matList
        -- l = map (\(x,y) -> map (\(a,b) -> (a,b*y)) x) l2
        l = concat $ zipWith (\r z -> map (\(x,y) -> ((z, x), y)) r) matList [1..]

toMatListT1 :: (TIndex k1, TAdd a) => TensList1 k1 (AnsVar a) -> [((Int,Int),a)]
toMatListT1 t = collectMatList matList
    where
        matList = concat $ mapTensList1 toMatList1' t

toMatListT2 :: (TIndex k1, TAdd a) => TensList2 k1 (AnsVar a) -> [((Int,Int),a)]
toMatListT2 t = collectMatList matList
    where
        matList = concat $ mapTensList2 toMatList2' t

toMatListT3 :: (TIndex k1, TIndex k2, TAdd a) => TensList3 k1 k2 (AnsVar a) -> [((Int,Int),a)]
toMatListT3 t = collectMatList matList
    where
        matList = concat $ mapTensList3 toMatList3' t

toMatListT4 :: (TIndex k1, TIndex k2, TAdd a) => TensList4 k1 k2 (AnsVar a) -> [((Int,Int),a)]
toMatListT4 t = collectMatList matList
    where
        matList = concat $ mapTensList4 toMatList4' t

toMatListT5 :: (TIndex k1, TIndex k2, TIndex k3, TAdd a) => TensList5 k1 k2 k3 (AnsVar a) -> [((Int,Int),a)]
toMatListT5 t = collectMatList matList
    where
        matList = concat $ mapTensList5 toMatList5' t

toMatListT6 :: (TIndex k1, TIndex k2, TIndex k3, TAdd a) => TensList6 k1 k2 k3 (AnsVar a) -> [((Int,Int),a)]
toMatListT6 t = collectMatList matList
    where
        matList = concat $ mapTensList6 toMatList6' t

toMatListT7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd a) => TensList7 k1 k2 k3 k4 (AnsVar a) -> [((Int,Int),a)]
toMatListT7 t = collectMatList matList
    where
        matList = concat $ mapTensList7 toMatList7' t

toMatListT8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TAdd a) => TensList8 k1 k2 k3 k4 (AnsVar a) -> [((Int,Int),a)]
toMatListT8 t = collectMatList matList
    where
        matList = concat $ mapTensList8 toMatList8' t


--convert to Eigen format for using LA subroutines

dims :: [((Int, Int), a)] -> (Int, Int)
dims xs = (rows, cols)
    where
        rows = maximum $ map (fst.fst) xs
        cols = maximum $ map (snd.fst) xs

assocsToSparse :: Real a => [((Int, Int), SField a)] -> Sparse.SparseMatrixXd
assocsToSparse [] = Sparse.fromList 1 1 [(0,0,0)]
assocsToSparse assocs = Sparse.fromList rows cols els
    where
        (rows, cols) = dims assocs
        els          = map (\((x, y), SField z) -> (x-1, y-1, fromRational $ toRational z)) assocs

toEMatrixT1 :: (TIndex k1) => TensList1 k1 (AnsVar (SField Rational)) -> Sparse.SparseMatrixXd
toEMatrixT1 = assocsToSparse . toMatListT1

toEMatrixT2 :: (TIndex k1) => TensList2 k1 (AnsVar (SField Rational)) -> Sparse.SparseMatrixXd
toEMatrixT2 = assocsToSparse . toMatListT2

toEMatrixT3 :: (TIndex k1, TIndex k2) => TensList3 k1 k2 (AnsVar (SField Rational)) -> Sparse.SparseMatrixXd
toEMatrixT3 = assocsToSparse . toMatListT3

toEMatrixT4 :: (TIndex k1, TIndex k2) => TensList4 k1 k2 (AnsVar (SField Rational)) -> Sparse.SparseMatrixXd
toEMatrixT4 = assocsToSparse . toMatListT4

toEMatrixT5 :: (TIndex k1, TIndex k2, TIndex k3) => TensList5 k1 k2 k3 (AnsVar (SField Rational)) -> Sparse.SparseMatrixXd
toEMatrixT5 = assocsToSparse . toMatListT5

toEMatrixT6 :: (TIndex k1, TIndex k2, TIndex k3) => TensList6 k1 k2 k3 (AnsVar (SField Rational)) -> Sparse.SparseMatrixXd
toEMatrixT6 = assocsToSparse . toMatListT6

toEMatrixT7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => TensList7 k1 k2 k3 k4 (AnsVar (SField Rational)) -> Sparse.SparseMatrixXd
toEMatrixT7 = assocsToSparse . toMatListT7

toEMatrixT8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => TensList8 k1 k2 k3 k4 (AnsVar (SField Rational)) -> Sparse.SparseMatrixXd
toEMatrixT8 = assocsToSparse . toMatListT8

--rank of the tensor can be computed with rank Sol.FullPivLU or Sol.JakobiSVD

tensorRank1' :: (TIndex k1) => AbsTensor1 n1 k1 (AnsVar (SField Rational)) -> Int
tensorRank1' t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrixT1 (singletonTList1 t)

tensorRank1 :: (TIndex k1) => TensList1 k1 (AnsVar (SField Rational)) -> Int
tensorRank1 t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrixT1 t


tensorRank2' :: (TIndex k1) => AbsTensor2 n1 n2 k1 (AnsVar (SField Rational)) -> Int
tensorRank2' t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrixT2 (singletonTList2 t)

tensorRank2 :: (TIndex k1) => TensList2 k1 (AnsVar (SField Rational)) -> Int
tensorRank2 t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrixT2 t


tensorRank3' :: (TIndex k1, TIndex k2) => AbsTensor3 n1 n2 n3 k1 k2 (AnsVar (SField Rational)) -> Int
tensorRank3' t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrixT3 (singletonTList3 t)

tensorRank3 :: (TIndex k1, TIndex k2) =>  TensList3 k1 k2 (AnsVar (SField Rational)) -> Int
tensorRank3 t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrixT3 t


tensorRank4' :: (TIndex k1, TIndex k2) =>  AbsTensor4 n1 n2 n3 n4 k1 k2 (AnsVar (SField Rational)) -> Int
tensorRank4' t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrixT4 (singletonTList4 t)

tensorRank4 :: (TIndex k1, TIndex k2) =>  TensList4 k1 k2 (AnsVar (SField Rational)) -> Int
tensorRank4 t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrixT4 t


tensorRank5' :: (TIndex k1, TIndex k2, TIndex k3) =>  AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (AnsVar (SField Rational)) -> Int
tensorRank5' t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrixT5 (singletonTList5 t)

tensorRank5 :: (TIndex k1, TIndex k2, TIndex k3) => TensList5 k1 k2 k3 (AnsVar (SField Rational)) -> Int
tensorRank5 t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrixT5 t


tensorRank6' :: (TIndex k1, TIndex k2, TIndex k3) => AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (AnsVar (SField Rational)) -> Int
tensorRank6' t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrixT6 (singletonTList6 t)

tensorRank6 :: (TIndex k1, TIndex k2, TIndex k3) => TensList6 k1 k2 k3 (AnsVar (SField Rational)) -> Int
tensorRank6 t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrixT6 t


tensorRank7' :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (AnsVar (SField Rational)) -> Int
tensorRank7' t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrixT7 (singletonTList7 t)

tensorRank7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => TensList7 k1 k2 k3 k4 (AnsVar (SField Rational)) -> Int
tensorRank7 t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrixT7 t


tensorRank8' :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (AnsVar (SField Rational)) -> Int
tensorRank8' t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrixT8 (singletonTList8 t)

tensorRank8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => TensList8 k1 k2 k3 k4 (AnsVar (SField Rational)) -> Int
tensorRank8 t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrixT8 t


