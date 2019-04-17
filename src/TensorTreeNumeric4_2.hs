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
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver   #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -dcore-lint #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}


 module TensorTreeNumeric4_2 (
    Tensor(..), Ind20(..), Ind9(..), Ind3(..), IndList(..), ATens, IndTuple, AnsVar, AreaVar(..),
    (&*), (&+), (&-), (&.),
    fromListT6, singletonInd, (+>), sortInd, toListT6, toListShow6, toListShowVar6,
    tensorTrans1, tensorTrans2, tensorTrans3, tensorTrans4, tensorTrans5, tensorTrans6,
    symATens1, symATens2, symATens3, symATens4, symATens5, symATens6,
    aSymATens1, aSymATens2, aSymATens3, aSymATens4, aSymATens5, aSymATens6,
    cyclicSymATensFac1, cyclicSymATensFac2, cyclicSymATensFac3, cyclicSymATensFac4, cyclicSymATensFac5, cyclicSymATensFac6,
    cyclicSymATens1, cyclicSymATens2, cyclicSymATens3, cyclicSymATens4, cyclicSymATens5, cyclicSymATens6,
    contrATens1, contrATens2, contrATens3,
    decodeTensor, encodeTensor, ansVarToAreaVar, 
    mapTo1, mapTo2, mapTo3, mapTo4, mapTo5, mapTo6,
    resortTens1, resortTens5, fromListT6',
    (&>), (&++), singletonTList, toEMatrix6, shiftLabels6, tensorRank, removeZeros6, removeZeros, toMatList6, toMatList6', mapTensList6
 
    
) where

    import Data.Foldable
    import Data.Ratio
    import Data.List 
    import Control.Applicative
    import Data.Maybe
    import qualified Data.IntMap.Strict as I
    import Numeric.Natural
    import GHC.TypeLits
    import Data.Proxy
    import GHC.TypeLits.Normalise
    import GHC.Generics
    import Control.DeepSeq
    import Data.Serialize
    import Data.Type.Equality
    import Data.Singletons
    import Data.Singletons.Decide
    import Data.Singletons.Prelude.Enum
    import Data.Singletons.TypeLits
    import Unsafe.Coerce (unsafeCoerce)
    import qualified Data.ByteString.Lazy as BS
    import Codec.Compression.GZip
    import Data.Either



    --for Linear Algebra 

    import qualified Data.Eigen.Matrix as Mat 
    import qualified Data.Eigen.SparseMatrix as Sparse
    import qualified Data.Eigen.LA as Sol

    --define lengthtyped lists as indices for a given tensors

    data IndList n a where
        Empty :: IndList 0 a 
        Append :: a -> IndList (n-1) a -> IndList n a 

    infixr 5 +>

    (+>) :: (Enum a) => Int -> IndList (n-1) a -> IndList n a 
    (+>) i l = Append (toEnum i) l 

    singletonInd :: a -> IndList 1 a
    singletonInd x = Append x Empty

    data IsZero (n :: Nat) where
        Zero :: (0 ~ n)     => IsZero n
        NonZero :: (1 <= n) => IsZero n
    deriving instance Show (IsZero n)
    
    isZero :: forall (n :: Nat). SNat n -> IsZero n
    isZero n = case n %~ (SNat @0)
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
        rnf (Empty) = ()
        rnf (Append a i) = (rnf a) `seq` (rnf i)

    deriving instance (Eq a) => Eq (IndList n a)

    deriving instance (Ord a) => Ord (IndList n a)

    deriving instance (Show a) => Show (IndList n a)

    instance Functor (IndList n) where
        fmap f (Empty) = Empty 
        fmap f (Append x xs) = Append (f x) (fmap f xs)

    instance Foldable (IndList n) where
        foldr f y (Empty) = y
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

    removeContractionInd :: (Eq a) => Int -> a -> (IndList n a, c) -> Maybe ((IndList (n-1) a),c)
    removeContractionInd 0 ind1 ((Append x xs), t)
                | ind1 == x = Just $ (xs,t) 
                | otherwise = Nothing 
    removeContractionInd i ind1 ((Append x xs),t) = fmap (\(m,n) -> (Append x m, n)) $ removeContractionInd (i-1) ind1 (xs,t)

    --resort inds in INdList accroding to the permutation given by [Int], length of [Int] must be n

    resortInd :: (SingI n, Ord a) => [Int] -> IndList n a -> IndList n a
    resortInd perm indList = newindList
            where 
                l' = toList indList
                l'' = if length l' == length perm then zip perm l' else error "permutation has wrong length"  
                lReSorted = sortOn fst l'' 
                newindList = fromList' $ map snd lReSorted
 
    -------------------------------------------------------------------------------------------------------------------------------------

    --define a tensor for a single index type

    --values of a given tensor should satisfy numberlike properties

    class (Eq a) => TScalar a where 
        addS :: a -> a -> a 
        subS :: a -> a -> a
        scaleS :: Rational -> a -> a 
        scaleZero :: a 
       
    class TAlgebra v v' where 
        type TAlg v v' :: * 
        prodA :: v -> v' -> TAlg v v'

    class (Eq a, Ord a, Enum a) => TIndex a where 
        indRange :: Int 

    --instead of maps use orderd lists 

    type TMap k v = [(k,v)]

    isValidTMap :: (Ord k, Eq v) => TMap k v -> Bool 
    isValidTMap l = l == (sortOn fst l)

    insertWithTMap :: (Ord k) => (v -> v -> v) -> k -> v -> TMap k v -> TMap k v
    insertWithTMap f key val [] = [(key,val)]
    insertWithTMap f key val ((k1,v1):xs) 
            | key < k1 = (key,val) : ((k1,v1):xs)
            | key == k1 = (k1,f val v1) : xs 
            | otherwise = (k1,v1) : (insertWithTMap f key val xs)

    addTMaps :: (Ord k) => (v -> v -> v) -> TMap k v -> TMap k v -> TMap k v 
    addTMaps f m1 [] = m1 
    addTMaps f [] m2 = m2 
    addTMaps f ((k1,v1):xs) ((k2,v2):ys) 
            | k1 < k2 = (k1,v1) : (addTMaps f xs ((k2,v2):ys))
            | k2 < k1 = (k2,v2) : (addTMaps f ((k1,v1):xs) ys)
            | k1 == k2 = (k1, f v1 v2) : (addTMaps f xs ys)

    mapTMap :: (v -> v') -> TMap k v -> TMap k v' 
    mapTMap f m = map (\(k,v) -> (k,f v)) m  

    filterTMap :: (v -> Bool) -> TMap k v -> TMap k v 
    filterTMap f m = filter (\(_,v) -> f v) m 

    data Tensor n k v where 
        Scalar :: v -> Tensor 0 k v 
        Tensor :: TMap k (Tensor n k v) -> Tensor (n+1) k v
        ZeroTensor :: Tensor n k v

    --remove possible zero values in a given Tensor  

    removeZeros :: (TScalar v, TIndex k) => Tensor n k v -> Tensor n k v
    removeZeros (Scalar x) = if x == scaleZero then ZeroTensor else Scalar x 
    removeZeros (Tensor m) = let newMap = filterTMap (/=ZeroTensor) $ mapTMap removeZeros m in if newMap == [] then ZeroTensor else Tensor newMap
    removeZeros ZeroTensor = ZeroTensor 

    --for converting tensors to bytestrings we need a non typesafe data type as intermediate type

    data TensorRep k v = ScalarR v | TensorR Natural (TMap k (TensorRep k v)) | ZeroR Natural deriving (Show, Generic, Serialize)

    --convert betweeen typesafe and non typesafe tensors

    lemma :: forall n m. (n-1 :~: m) -> (m+1 :~: n)
    lemma _ = unsafeCoerce (Refl @n)

    toRep :: forall n k v. KnownNat n => Tensor n k v -> TensorRep k v
    toRep (Scalar v) = ScalarR v
    toRep (Tensor m) = case isZero (SNat @n)
                       of Zero -> undefined
                          NonZero ->
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
                                                                                                Just Refl -> Tensor (mapTMap (\t -> (fromRep t) :: Tensor (x-1) k v) m)
                                                                                Zero    -> undefined
                                 Nothing -> undefined
    fromRep (ZeroR r) = case someNatVal (fromIntegral r)
                          of Just l  -> ZeroTensor
                             Nothing -> undefined

    --instances

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
        fmap f (ZeroTensor) = ZeroTensor

    deriving instance (Show a, Show k) => Show (Tensor n k a)

    deriving instance (Eq a, Eq k) => Eq (Tensor n k a)

    --in order to construct higher tensors we need the possibility to build tensors with lower order tensors as scalars

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

    instance TAlgebra Rational Rational where 
        type TAlg Rational Rational = Rational
        prodA = (*)

    getTensorMap :: Tensor (n+1) k v -> TMap k (Tensor n k v)
    getTensorMap (Tensor m) = m 

    toListT :: Tensor n k v -> [(IndList n k, v)]
    toListT (Scalar x) = [(Empty, x)]
    toListT (Tensor m) =  concat $ map (\(i,t) -> appendF i $ toListT t) m
            where
                appendF = \i l2 -> map (\(l,val) -> (Append i l ,val)) l2
    toListT ZeroTensor = []
    
    mkTens :: (IndList n k, v) -> Tensor n k v
    mkTens (Empty, a) = Scalar a
    mkTens (Append x xs, a) = Tensor  [(x, mkTens (xs, a))]

    --construct from typed list

    fromListT :: (TIndex k, TScalar v) => [(IndList n k, v)] -> Tensor n k v 
    fromListT [x] = mkTens x 
    fromListT (x:xs) = foldr insertOrAdd (mkTens x) xs 
    fromListT [] = ZeroTensor

    --construct from generic list

    fromListT' :: forall n k v . (TIndex k, TScalar v, SingI n) => [([k],v)] -> Tensor n k v 
    fromListT' l = fromListT indList
            where 
                indList = map (\(x,y) -> (fromList' x, y)) l

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

    infix 8 &. 

    (&.) :: (TIndex k, TScalar v) => Rational -> Tensor n k v -> Tensor n k v 
    (&.) scalar t = fmap (scaleS scalar) t 

    infix 5 &- 
    
    (&-) :: (TIndex k, TScalar v) => Tensor n k v -> Tensor n k v -> Tensor n k v
    (&-) t1 t2 = t1 &+ (-1) &. t2 

    --product of tensors

    infixr 7 &*

    (&*) :: (TIndex k, TAlgebra v v', TScalar v, TScalar v', TScalar (TAlg v v')) => Tensor n k v -> Tensor m k v' -> Tensor (n+m) k (TAlg v v') 
    (&*) (Scalar x) (Scalar y) = let newVal = prodA x y in if newVal == scaleZero then ZeroTensor else Scalar newVal 
    (&*) (Scalar x) t2 = fmap (prodA x) t2 
    (&*) (Tensor m) t2 = Tensor $ mapTMap (\t1 -> (&*) t1 t2) m 
    (&*) t1 ZeroTensor = ZeroTensor 
    (&*) ZeroTensor t2 = ZeroTensor 

    --transpose a given tensor in 2 of its indices

    tensorTrans :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v
    tensorTrans (0, j) t = fromListT l
                    where 
                        l = (map (\(x,y) -> (swapHead j x, y)) $ toListT t)
    tensorTrans (i, j) (Tensor m) = Tensor $ mapTMap (tensorTrans (i-1, j-1)) m 
    tensorTrans (i ,j) ZeroTensor = ZeroTensor

    --transpose a given Tensor in several of its indices (does not work i the same index occurs several times)

    tensorBlockTrans :: (TIndex k, TScalar v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k v
    tensorBlockTrans (l1,l2) t = foldr tensorTrans t indList
            where
                indList = if (intersect l1 l2 == []) then zip l1 l2 else error "at least one indexin the list occurs several times"

    --generic resorting of the indices of a given tensor according to permutation given by [Int] -> the most expensive as the whole tensor is flattened to a list
    --input is current ordering of tesnor w.r.t. goal orderig, i.e. resorting [A,B,C,D] to [B,C,D,A] is achieved by [3,0,2,1]  

    resortTens :: (SingI n, TIndex k, TScalar v) => [Int] -> Tensor n k v -> Tensor n k v 
    resortTens perm t = fromListT $ map (\(x,y) -> (resortInd perm x, y)) $ toListT t

    --symmetrization of a given Tensor with rational Values stored

    symTens :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v 
    symTens inds t = t &+ (tensorTrans inds t) 

    aSymTens :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v 
    aSymTens inds t = t &- (tensorTrans inds t) 

    symTensFac :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v 
    symTensFac inds t = (1%2) &. (symTens inds t)

    aSymTensFac :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v 
    aSymTensFac inds t = (1%2) &. (aSymTens inds t)

    --block symmetrization 

    symBlockTens :: (TIndex k, TScalar v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k v 
    symBlockTens inds t = t &+ (tensorBlockTrans inds t) 

    aSymBlockTens :: (TIndex k, TScalar v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k v 
    aSymBlockTens inds t = t &- (tensorBlockTrans inds t) 

    symBlockTensFac :: (TIndex k, TScalar v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k v 
    symBlockTensFac inds t = (1%2) &. (symBlockTens inds t)

    aSymBlockTensFac :: (TIndex k, TScalar v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k v 
    aSymBlockTensFac inds t = (1%2) &. (aSymBlockTens inds t)

    --cyclic symmetrization

    --convert all permutations of a given list of indices into an equivalent lists of lists of Swaps of 2 indices 

    getAllSwaps :: [Int] -> [[(Int,Int)]]
    getAllSwaps [x,y] = [[(x,y)]] 
    getAllSwaps (x:xs) = lNew ++ l' ++ ((:) <$> l <*> l')
            where 
                l = zip (repeat x) xs 
                lNew = map (\x -> [x]) l
                l' = getAllSwaps xs 

    factorial :: Int -> Int 
    factorial 1 = 1
    factorial n = n*(factorial (n-1))

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
                signList = map (\x -> (-1)^(length x)) swapList
                tensList' = map (foldr tensorTrans t) swapList
                tensList = zipWith (&.) signList tensList' 
                newTens = foldr (&+) t tensList

    cyclicSymTensFac :: (TIndex k, TScalar v) => [Int] -> Tensor n k v -> Tensor n k v 
    cyclicSymTensFac inds t = fac &. (cyclicSymTens inds t)
            where 
                fac = 1%(fromIntegral $ factorial $ length inds)

    cyclicASymTensFac :: (TIndex k, TScalar v) => [Int] -> Tensor n k v -> Tensor n k v 
    cyclicASymTensFac inds t = fac &. (cyclicASymTens inds t)
            where 
                fac = 1%(fromIntegral $ factorial $ length inds)

    --for contraction we need a tensor with upper and lower indices

    type Tensor2 n1 n2 k v = Tensor n1 k (Tensor n2 k v)

    tensorContr :: (TIndex k, TScalar v) => (Int,Int) -> Tensor2 n1 n2 k v -> Tensor2 (n1-1) (n2-1) k v 
    tensorContr (0,j) t = fromListT tensList 
        where
            l = map (\(x,y) -> (x, toListT y)) $ toListT t
            l2 = map (\(x,y) -> (tailInd x,(mapMaybe (removeContractionInd j (headInd x)) y))) l
            l3 = filter (\(_,y) -> length y >= 1) l2 
            tensList = map (\(x,y) -> (x, fromListT y)) l3
    tensorContr (i,j) (Tensor m) = Tensor $ mapTMap (tensorContr (i-1,j)) m
    tensorContr inds ZeroTensor = ZeroTensor 
    tensorContr inds (Scalar s) = error "cannot contract scalar!"   

    --encode and decode tensors as bytestrings 

    encodeTensor :: (KnownNat n, Ord k, Serialize k, Serialize v) => Tensor n k v -> BS.ByteString 
    encodeTensor = compress . encodeLazy 

    decodeTensor :: (KnownNat n, Ord k, Serialize k, Serialize v) => BS.ByteString -> Tensor n k v 
    decodeTensor bs = (fromRight undefined $ decodeLazy $ decompress bs)

    --now a generic abstract Tensor "consists of several" Tensor2s 

    type AbsTensor1 n1 k1 v = Tensor n1 k1 v 

    type AbsTensor2 n1 n2 k1 v = Tensor2 n1 n2 k1 v

    type AbsTensor3 n1 n2 n3 k1 k2 v = AbsTensor2 n1 n2 k1 (Tensor n3 k2 v)

    type AbsTensor4 n1 n2 n3 n4 k1 k2 v = AbsTensor2 n1 n2 k1 (Tensor2 n3 n4 k2 v) 

    type AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v = AbsTensor4 n1 n2 n3 n4 k1 k2 (Tensor n5 k3 v)
    
    type AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v = AbsTensor4 n1 n2 n3 n4 k1 k2 (Tensor2 n5 n6 k3 v)

    type AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v = AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (Tensor n7 k4 v)

    type AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v = AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (Tensor2 n7 n8 k4 v)

    --more different indices is probably not realistic 

    --AbsTensors automatically satisfy TensorAlgebra as we only used type synonyms 
    --e.g. AbsTensor4 actually looks like Tensor n1 kq1 (Tensor n2 k2 (Tensor n3 k3 (Tensor n4 k4 (Tensor n5 k5 (Tensor n6 k6 (Tensor n7 k7 (Tensor n8 k8 a)))))))

    --fmap takes us 1 level deeper 

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
    removeZeros2 = removeZeros . (mapTo1 removeZeros) 

    removeZeros3 :: (TScalar v, TIndex k1, TIndex k2) => AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v 
    removeZeros3 = removeZeros . (mapTo1 removeZeros) . (mapTo2 removeZeros)

    removeZeros4 :: (TScalar v, TIndex k1, TIndex k2) => AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v 
    removeZeros4 = removeZeros . (mapTo1 removeZeros) . (mapTo2 removeZeros) . (mapTo3 removeZeros)

    removeZeros5 :: (TScalar v, TIndex k1, TIndex k2, TIndex k3) => AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v 
    removeZeros5 = removeZeros . (mapTo1 removeZeros) . (mapTo2 removeZeros) . (mapTo3 removeZeros) . (mapTo4 removeZeros)

    removeZeros6 :: (TScalar v, TIndex k1, TIndex k2, TIndex k3) => AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v 
    removeZeros6 = removeZeros . (mapTo1 removeZeros) . (mapTo2 removeZeros) . (mapTo3 removeZeros) . (mapTo4 removeZeros) . (mapTo5 removeZeros)

    removeZeros7 :: (TScalar v, TIndex k1, TIndex k2, TIndex k3, TIndex k4) => AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v 
    removeZeros7 = removeZeros . (mapTo1 removeZeros) . (mapTo2 removeZeros) . (mapTo3 removeZeros) . (mapTo4 removeZeros) . (mapTo5 removeZeros) . (mapTo6 removeZeros)

    removeZeros8 :: (TScalar v, TIndex k1, TIndex k2, TIndex k3, TIndex k4) => AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v 
    removeZeros8 = removeZeros . (mapTo1 removeZeros) . (mapTo2 removeZeros) . (mapTo3 removeZeros) . (mapTo4 removeZeros) . (mapTo5 removeZeros) . (mapTo6 removeZeros) . (mapTo7 removeZeros)

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

    --in total these functions allow to build the symmetrizer and contraction functions for generic tensors 

    --symmetrizer

    symATens1 :: (TIndex k1, TScalar v) => (Int,Int) -> AbsTensor1 n1 k1 v -> AbsTensor1 n1 k1 v
    symATens1 inds = symTens inds
     
    symATens2 :: (TIndex k1, TScalar v) => (Int,Int) -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 n1 n2 k1 v
    symATens2 inds = mapTo1 (symTens inds) 

    symATens3 :: (TIndex k1, TIndex k2, TScalar v) => (Int,Int) -> AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
    symATens3 inds = mapTo2 (symTens inds) 

    symATens4 :: (TIndex k1, TIndex k2, TScalar v) => (Int,Int) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
    symATens4 inds = mapTo3 (symTens inds) 

    symATens5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => (Int,Int) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
    symATens5 inds = mapTo4 (symTens inds) 

    symATens6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => (Int,Int) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
    symATens6 inds = mapTo5 (symTens inds)
     
    symATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => (Int,Int) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
    symATens7 inds = mapTo6 (symTens inds)

    symATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => (Int,Int) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
    symATens8 inds = mapTo7 (symTens inds)
 
    --with factor 

    symATensFac1 :: (TIndex k1, TScalar v) => (Int,Int) -> AbsTensor1 n1 k1 v -> AbsTensor1 n1 k1 v
    symATensFac1 inds = symTensFac inds
     
    symATensFac2 :: (TIndex k1, TScalar v) => (Int,Int) -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 n1 n2 k1 v
    symATensFac2 inds = mapTo1 (symTensFac inds) 

    symATensFac3 :: (TIndex k1,TIndex k2, TScalar v) => (Int,Int) -> AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
    symATensFac3 inds = mapTo2 (symTensFac inds) 

    symATensFac4 :: (TIndex k1, TIndex k2, TScalar v) => (Int,Int) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
    symATensFac4 inds = mapTo3 (symTensFac inds) 

    symATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => (Int,Int) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
    symATensFac5 inds = mapTo4 (symTensFac inds) 

    symATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => (Int,Int) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
    symATensFac6 inds = mapTo5 (symTensFac inds)
     
    symATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => (Int,Int) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
    symATensFac7 inds = mapTo6 (symTensFac inds)

    symATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => (Int,Int) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
    symATensFac8 inds = mapTo7 (symTensFac inds)

    --antisymmetrization

    aSymATens1 :: (TIndex k1, TScalar v) => (Int,Int) -> AbsTensor1 n1 k1 v -> AbsTensor1 n1 k1 v
    aSymATens1 inds = aSymTens inds
     
    aSymATens2 :: (TIndex k1, TScalar v) => (Int,Int) -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 n1 n2 k1 v
    aSymATens2 inds = mapTo1 (aSymTens inds) 

    aSymATens3 :: (TIndex k1, TIndex k2, TScalar v) => (Int,Int) -> AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
    aSymATens3 inds = mapTo2 (aSymTens inds) 

    aSymATens4 :: (TIndex k1, TIndex k2, TScalar v) => (Int,Int) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
    aSymATens4 inds = mapTo3 (aSymTens inds) 

    aSymATens5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => (Int,Int) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
    aSymATens5 inds = mapTo4 (aSymTens inds) 

    aSymATens6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => (Int,Int) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
    aSymATens6 inds = mapTo5 (aSymTens inds)
     
    aSymATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => (Int,Int) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
    aSymATens7 inds = mapTo6 (aSymTens inds)

    aSymATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => (Int,Int) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
    aSymATens8 inds = mapTo7 (aSymTens inds)
 
    --with factor 

    aSymATensFac1 :: (TIndex k1, TScalar v) => (Int,Int) -> AbsTensor1 n1 k1 v -> AbsTensor1 n1 k1 v
    aSymATensFac1 inds = aSymTensFac inds
     
    aSymATensFac2 :: (TIndex k1, TScalar v) => (Int,Int) -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 n1 n2 k1 v
    aSymATensFac2 inds = mapTo1 (aSymTensFac inds) 

    aSymATensFac3 :: (TIndex k1, TIndex k2, TScalar v) => (Int,Int) -> AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
    aSymATensFac3 inds = mapTo2 (aSymTensFac inds) 

    aSymATensFac4 :: (TIndex k1, TIndex k2, TScalar v) => (Int,Int) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
    aSymATensFac4 inds = mapTo3 (aSymTensFac inds) 

    aSymATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => (Int,Int) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
    aSymATensFac5 inds = mapTo4 (aSymTensFac inds) 

    aSymATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => (Int,Int) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
    aSymATensFac6 inds = mapTo5 (aSymTensFac inds)
     
    aSymATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => (Int,Int) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
    aSymATensFac7 inds = mapTo6 (aSymTensFac inds)

    aSymATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => (Int,Int) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
    aSymATensFac8 inds = mapTo7 (aSymTensFac inds)

    --block symmetrization 

    symBlockATens1 :: (TIndex k1, TScalar v) => ([Int],[Int]) -> AbsTensor1 n1 k1 v -> AbsTensor1 n1 k1 v
    symBlockATens1 inds = symBlockTens inds
     
    symBlockATens2 :: (TIndex k1, TScalar v) => ([Int],[Int]) -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 n1 n2 k1 v
    symBlockATens2 inds = mapTo1 (symBlockTens inds) 

    symBlockATens3 :: (TIndex k1, TIndex k2, TScalar v) => ([Int],[Int]) -> AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
    symBlockATens3 inds = mapTo2 (symBlockTens inds) 

    symBlockATens4 :: (TIndex k1, TIndex k2, TScalar v) => ([Int],[Int]) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
    symBlockATens4 inds = mapTo3 (symBlockTens inds) 

    symBlockATens5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => ([Int],[Int]) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
    symBlockATens5 inds = mapTo4 (symBlockTens inds) 

    symBlockATens6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => ([Int],[Int]) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
    symBlockATens6 inds = mapTo5 (symBlockTens inds)
     
    symBlockATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => ([Int],[Int]) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
    symBlockATens7 inds = mapTo6 (symBlockTens inds)

    symBlockATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => ([Int],[Int]) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
    symBlockATens8 inds = mapTo7 (symBlockTens inds)
 
    --with factor 

    symBlockATensFac1 :: (TIndex k1, TScalar v) => ([Int],[Int]) -> AbsTensor1 n1 k1 v -> AbsTensor1 n1 k1 v
    symBlockATensFac1 inds = symBlockTensFac inds
     
    symBlockATensFac2 :: (TIndex k1, TScalar v) => ([Int],[Int]) -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 n1 n2 k1 v
    symBlockATensFac2 inds = mapTo1 (symBlockTensFac inds) 

    symBlockATensFac3 :: (TIndex k1, TIndex k2, TScalar v) => ([Int],[Int]) -> AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
    symBlockATensFac3 inds = mapTo2 (symBlockTensFac inds) 

    symBlockATensFac4 :: (TIndex k1, TIndex k2, TScalar v) => ([Int],[Int]) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
    symBlockATensFac4 inds = mapTo3 (symBlockTensFac inds) 

    symBlockATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => ([Int],[Int]) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
    symBlockATensFac5 inds = mapTo4 (symBlockTensFac inds) 

    symBlockATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => ([Int],[Int]) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
    symBlockATensFac6 inds = mapTo5 (symBlockTensFac inds)
     
    symBlockATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => ([Int],[Int]) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
    symBlockATensFac7 inds = mapTo6 (symBlockTensFac inds)

    symBlockATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => ([Int],[Int]) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
    symBlockATensFac8 inds = mapTo7 (symBlockTensFac inds)

    --antisymmetrization

    aSymBlockATens1 :: (TIndex k1, TScalar v) => ([Int],[Int]) -> AbsTensor1 n1 k1 v -> AbsTensor1 n1 k1 v
    aSymBlockATens1 inds = aSymBlockTens inds
     
    aSymBlockATens2 :: (TIndex k1, TScalar v) => ([Int],[Int]) -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 n1 n2 k1 v
    aSymBlockATens2 inds = mapTo1 (aSymBlockTens inds) 

    aSymBlockATens3 :: (TIndex k1, TIndex k2, TScalar v) => ([Int],[Int]) -> AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
    aSymBlockATens3 inds = mapTo2 (aSymBlockTens inds) 

    aSymBlockATens4 :: (TIndex k1, TIndex k2, TScalar v) => ([Int],[Int]) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
    aSymBlockATens4 inds = mapTo3 (aSymBlockTens inds) 

    aSymBlockATens5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => ([Int],[Int]) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
    aSymBlockATens5 inds = mapTo4 (aSymBlockTens inds) 

    aSymBlockATens6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => ([Int],[Int]) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
    aSymBlockATens6 inds = mapTo5 (aSymBlockTens inds)
     
    aSymBlockATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => ([Int],[Int]) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
    aSymBlockATens7 inds = mapTo6 (aSymBlockTens inds)

    aSymBlockATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => ([Int],[Int]) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
    aSymBlockATens8 inds = mapTo7 (aSymBlockTens inds)
 
    --with factor 

    aSymBlockATensFac1 :: (TIndex k1, TScalar v) => ([Int],[Int]) -> AbsTensor1 n1 k1 v -> AbsTensor1 n1 k1 v
    aSymBlockATensFac1 inds = aSymBlockTensFac inds
     
    aSymBlockATensFac2 :: (TIndex k1, TScalar v) => ([Int],[Int]) -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 n1 n2 k1 v
    aSymBlockATensFac2 inds = mapTo1 (aSymBlockTensFac inds) 

    aSymBlockATensFac3 :: (TIndex k1, TIndex k2, TScalar v) => ([Int],[Int]) -> AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
    aSymBlockATensFac3 inds = mapTo2 (aSymBlockTensFac inds) 

    aSymBlockATensFac4 :: (TIndex k1, TIndex k2, TScalar v) => ([Int],[Int]) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
    aSymBlockATensFac4 inds = mapTo3 (aSymBlockTensFac inds) 

    aSymBlockATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => ([Int],[Int]) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
    aSymBlockATensFac5 inds = mapTo4 (aSymBlockTensFac inds) 

    aSymBlockATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => ([Int],[Int]) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
    aSymBlockATensFac6 inds = mapTo5 (aSymBlockTensFac inds)
     
    aSymBlockATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => ([Int],[Int]) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
    aSymBlockATensFac7 inds = mapTo6 (aSymBlockTensFac inds)

    aSymBlockATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => ([Int],[Int]) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
    aSymBlockATensFac8 inds = mapTo7 (aSymBlockTensFac inds)

    --cyclic symmetrization 

    cyclicSymATens1 :: (TIndex k1, TScalar v) => [Int] -> AbsTensor1 n1 k1 v -> AbsTensor1 n1 k1 v
    cyclicSymATens1 inds = cyclicSymTens inds
     
    cyclicSymATens2 :: (TIndex k1, TScalar v) => [Int] -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 n1 n2 k1 v
    cyclicSymATens2 inds = mapTo1 (cyclicSymTens inds) 

    cyclicSymATens3 :: (TIndex k1, TIndex k2, TScalar v) => [Int] -> AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
    cyclicSymATens3 inds = mapTo2 (cyclicSymTens inds) 

    cyclicSymATens4 :: (TIndex k1, TIndex k2, TScalar v) => [Int] -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
    cyclicSymATens4 inds = mapTo3 (cyclicSymTens inds) 

    cyclicSymATens5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => [Int] -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
    cyclicSymATens5 inds = mapTo4 (cyclicSymTens inds) 

    cyclicSymATens6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => [Int] -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
    cyclicSymATens6 inds = mapTo5 (cyclicSymTens inds)
     
    cyclicSymATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => [Int] -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
    cyclicSymATens7 inds = mapTo6 (cyclicSymTens inds)

    cyclicSymATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => [Int] -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
    cyclicSymATens8 inds = mapTo7 (cyclicSymTens inds)
 
    --with factor 

    cyclicSymATensFac1 :: (TIndex k1, TScalar v) => [Int] -> AbsTensor1 n1 k1 v -> AbsTensor1 n1 k1 v
    cyclicSymATensFac1 inds = cyclicSymTensFac inds
     
    cyclicSymATensFac2 :: (TIndex k1, TScalar v) => [Int] -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 n1 n2 k1 v
    cyclicSymATensFac2 inds = mapTo1 (cyclicSymTensFac inds) 

    cyclicSymATensFac3 :: (TIndex k1, TIndex k2, TScalar v) => [Int] -> AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
    cyclicSymATensFac3 inds = mapTo2 (cyclicSymTensFac inds) 

    cyclicSymATensFac4 :: (TIndex k1, TIndex k2, TScalar v) => [Int] -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
    cyclicSymATensFac4 inds = mapTo3 (cyclicSymTensFac inds) 

    cyclicSymATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => [Int] -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
    cyclicSymATensFac5 inds = mapTo4 (cyclicSymTensFac inds) 

    cyclicSymATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => [Int] -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
    cyclicSymATensFac6 inds = mapTo5 (cyclicSymTensFac inds)
     
    cyclicSymATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => [Int] -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
    cyclicSymATensFac7 inds = mapTo6 (cyclicSymTensFac inds)

    cyclicSymATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => [Int] -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
    cyclicSymATensFac8 inds = mapTo7 (cyclicSymTensFac inds)

    --cyclic Antisymmetrization 

    cyclicASymATens1 :: (TIndex k1, TScalar v) => [Int] -> AbsTensor1 n1 k1 v -> AbsTensor1 n1 k1 v
    cyclicASymATens1 inds = cyclicASymTens inds
     
    cyclicASymATens2 :: (TIndex k1, TScalar v) => [Int] -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 n1 n2 k1 v
    cyclicASymATens2 inds = mapTo1 (cyclicASymTens inds) 

    cyclicASymATens3 :: (TIndex k1, TIndex k2, TScalar v) => [Int] -> AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
    cyclicASymATens3 inds = mapTo2 (cyclicASymTens inds) 

    cyclicASymATens4 :: (TIndex k1, TIndex k2, TScalar v) => [Int] -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
    cyclicASymATens4 inds = mapTo3 (cyclicASymTens inds) 

    cyclicASymATens5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => [Int] -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
    cyclicASymATens5 inds = mapTo4 (cyclicASymTens inds) 

    cyclicASymATens6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => [Int] -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
    cyclicASymATens6 inds = mapTo5 (cyclicASymTens inds)
     
    cyclicASymATens7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => [Int] -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
    cyclicASymATens7 inds = mapTo6 (cyclicASymTens inds)

    cyclicASymATens8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => [Int] -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
    cyclicASymATens8 inds = mapTo7 (cyclicASymTens inds)
 
    --with factor 

    cyclicASymATensFac1 :: (TIndex k1, TScalar v) => [Int] -> AbsTensor1 n1 k1 v -> AbsTensor1 n1 k1 v
    cyclicASymATensFac1 inds = cyclicASymTensFac inds
     
    cyclicASymATensFac2 :: (TIndex k1, TScalar v) => [Int] -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 n1 n2 k1 v
    cyclicASymATensFac2 inds = mapTo1 (cyclicASymTensFac inds) 

    cyclicASymATensFac3 :: (TIndex k1, TIndex k2, TScalar v) => [Int] -> AbsTensor3 n1 n2 n3 k1 k2 v -> AbsTensor3 n1 n2 n3 k1 k2 v
    cyclicASymATensFac3 inds = mapTo2 (cyclicASymTensFac inds) 

    cyclicASymATensFac4 :: (TIndex k1, TIndex k2, TScalar v) => [Int] -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 n3 n4 k1 k2 v
    cyclicASymATensFac4 inds = mapTo3 (cyclicASymTensFac inds) 

    cyclicASymATensFac5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => [Int] -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v
    cyclicASymATensFac5 inds = mapTo4 (cyclicASymTensFac inds) 

    cyclicASymATensFac6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => [Int] -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v
    cyclicASymATensFac6 inds = mapTo5 (cyclicASymTensFac inds)
     
    cyclicASymATensFac7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => [Int] -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v
    cyclicASymATensFac7 inds = mapTo6 (cyclicASymTensFac inds)

    cyclicASymATensFac8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => [Int] -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v
    cyclicASymATensFac8 inds = mapTo7 (cyclicASymTensFac inds)    


    --contraction for general tensors 

    contrATens1 :: (TIndex k1, TScalar v) => (Int,Int) -> AbsTensor2 n1 n2 k1 v -> AbsTensor2 (n1-1) (n2-1) k1 v
    contrATens1 = tensorContr 

    contrATens2 :: (TIndex k1, TIndex k2, TScalar v) => (Int,Int) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v -> AbsTensor4 n1 n2 (n3-1) (n4-1) k1 k2 v
    contrATens2 inds = mapTo2 (tensorContr inds) 

    contrATens3 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => (Int,Int) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> AbsTensor6 n1 n2 n3 n4 (n5-1) (n6-1) k1 k2 k3 v
    contrATens3 inds = mapTo4 (tensorContr inds) 

    contrATens4 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => (Int,Int) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> AbsTensor8 n1 n2 n3 n4 n5 n6 (n7-1) (n8-1) k1 k2 k3 k4 v
    contrATens4 inds = mapTo6 (tensorContr inds) 

    --construct tensors from lists of indices-value pairs 

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
    toListT2 t = concat $ map (\(x,y) -> appendT1 x $ toListT y ) $ toListT t
            where 
                appendT1 = \i l -> map (\(x,y) -> ((i,x),y)) l

    toListT3 :: AbsTensor3 n1 n2 n3 k1 k2 v -> [(IndTuple3 n1 n2 n3 k1 k2, v)]
    toListT3 t = concat $ map (\(x,y) -> appendT2 x $ toListT y ) $
                 concat $ map (\(x,y) -> appendT1 x $ toListT y ) $ toListT t
            where 
                appendT1 = \i l -> map (\(x,y) -> ((i,x),y)) l
                appendT2 = \(i1,i2) l -> map (\(x,y) -> ((i1,i2,x),y)) l

    toListT4 :: AbsTensor4 n1 n2 n3 n4 k1 k2 v -> [(IndTuple4 n1 n2 n3 n4 k1 k2, v)]
    toListT4 t = concat $ map (\(x,y) -> appendT3 x $ toListT y ) $
                 concat $ map (\(x,y) -> appendT2 x $ toListT y ) $
                 concat $ map (\(x,y) -> appendT1 x $ toListT y ) $ toListT t
            where 
                appendT1 = \i l -> map (\(x,y) -> ((i,x),y)) l
                appendT2 = \(i1,i2) l -> map (\(x,y) -> ((i1,i2,x),y)) l
                appendT3 = \(i1,i2,i3) l -> map (\(x,y) -> ((i1,i2,i3,x),y)) l

    toListT5 :: AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> [(IndTuple5 n1 n2 n3 n4 n5 k1 k2 k3, v)]
    toListT5 t = concat $ map (\(x,y) -> appendT4 x $ toListT y ) $
                 concat $ map (\(x,y) -> appendT3 x $ toListT y ) $
                 concat $ map (\(x,y) -> appendT2 x $ toListT y ) $
                 concat $ map (\(x,y) -> appendT1 x $ toListT y ) $ toListT t
            where 
                appendT1 = \i l -> map (\(x,y) -> ((i,x),y)) l
                appendT2 = \(i1,i2) l -> map (\(x,y) -> ((i1,i2,x),y)) l
                appendT3 = \(i1,i2,i3) l -> map (\(x,y) -> ((i1,i2,i3,x),y)) l
                appendT4 = \(i1,i2,i3,i4) l -> map (\(x,y) -> ((i1,i2,i3,i4,x),y)) l

    toListT6 :: AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> [(IndTuple6 n1 n2 n3 n4 n5 n6 k1 k2 k3, v)]
    toListT6 t = concat $ map (\(x,y) -> appendT5 x $ toListT y ) $ 
                 concat $ map (\(x,y) -> appendT4 x $ toListT y ) $
                 concat $ map (\(x,y) -> appendT3 x $ toListT y ) $
                 concat $ map (\(x,y) -> appendT2 x $ toListT y ) $
                 concat $ map (\(x,y) -> appendT1 x $ toListT y ) $ toListT t
            where 
                appendT1 = \i l -> map (\(x,y) -> ((i,x),y)) l
                appendT2 = \(i1,i2) l -> map (\(x,y) -> ((i1,i2,x),y)) l
                appendT3 = \(i1,i2,i3) l -> map (\(x,y) -> ((i1,i2,i3,x),y)) l
                appendT4 = \(i1,i2,i3,i4) l -> map (\(x,y) -> ((i1,i2,i3,i4,x),y)) l
                appendT5 = \(i1,i2,i3,i4,i5) l -> map (\(x,y) -> ((i1,i2,i3,i4,i5,x),y)) l

    toListT7 :: AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> [(IndTuple7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4, v)]
    toListT7 t = concat $ map (\(x,y) -> appendT6 x $ toListT y ) $ 
                 concat $ map (\(x,y) -> appendT5 x $ toListT y ) $ 
                 concat $ map (\(x,y) -> appendT4 x $ toListT y ) $
                 concat $ map (\(x,y) -> appendT3 x $ toListT y ) $
                 concat $ map (\(x,y) -> appendT2 x $ toListT y ) $
                 concat $ map (\(x,y) -> appendT1 x $ toListT y ) $ toListT t
            where 
                appendT1 = \i l -> map (\(x,y) -> ((i,x),y)) l
                appendT2 = \(i1,i2) l -> map (\(x,y) -> ((i1,i2,x),y)) l
                appendT3 = \(i1,i2,i3) l -> map (\(x,y) -> ((i1,i2,i3,x),y)) l
                appendT4 = \(i1,i2,i3,i4) l -> map (\(x,y) -> ((i1,i2,i3,i4,x),y)) l
                appendT5 = \(i1,i2,i3,i4,i5) l -> map (\(x,y) -> ((i1,i2,i3,i4,i5,x),y)) l
                appendT6 = \(i1,i2,i3,i4,i5,i6) l -> map (\(x,y) -> ((i1,i2,i3,i4,i5,i6,x),y)) l

    toListT8 :: AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> [(IndTuple8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4, v)]
    toListT8 t = concat $ map (\(x,y) -> appendT7 x $ toListT y ) $ 
                 concat $ map (\(x,y) -> appendT6 x $ toListT y ) $ 
                 concat $ map (\(x,y) -> appendT5 x $ toListT y ) $ 
                 concat $ map (\(x,y) -> appendT4 x $ toListT y ) $
                 concat $ map (\(x,y) -> appendT3 x $ toListT y ) $
                 concat $ map (\(x,y) -> appendT2 x $ toListT y ) $
                 concat $ map (\(x,y) -> appendT1 x $ toListT y ) $ toListT t
            where 
                appendT1 = \i l -> map (\(x,y) -> ((i,x),y)) l
                appendT2 = \(i1,i2) l -> map (\(x,y) -> ((i1,i2,x),y)) l
                appendT3 = \(i1,i2,i3) l -> map (\(x,y) -> ((i1,i2,i3,x),y)) l
                appendT4 = \(i1,i2,i3,i4) l -> map (\(x,y) -> ((i1,i2,i3,i4,x),y)) l
                appendT5 = \(i1,i2,i3,i4,i5) l -> map (\(x,y) -> ((i1,i2,i3,i4,i5,x),y)) l
                appendT6 = \(i1,i2,i3,i4,i5,i6) l -> map (\(x,y) -> ((i1,i2,i3,i4,i5,i6,x),y)) l
                appendT7 = \(i1,i2,i3,i4,i5,i6,i7) l -> map (\(x,y) -> ((i1,i2,i3,i4,i5,i6,i7,x),y)) l

    --convert to non type safe assocs list, all indices regardeless of their type are collected in the [Int] list 

    toListShow1 :: (TIndex k1, TScalar v) => AbsTensor1 n1 k1 v -> [([Int],v)]
    toListShow1 t = filter (\x -> snd x /= scaleZero) $ map (\(x,y) -> (showInd x, y)) l
            where
                l = toListT1 t 
                showInd i1 = (map fromEnum $ toList i1) 

    toListShow2 :: (TIndex k1, TScalar v) => AbsTensor2 n1 n2 k1 v -> [([Int],v)]
    toListShow2 t = filter (\x -> snd x /= scaleZero) $ map (\(x,y) -> (showInd x, y)) l
            where
                l = toListT2 t 
                showInd (i1,i2) = (map fromEnum $ toList i1) ++ (map fromEnum $ toList i2)

    toListShow3 :: (TIndex k1, TIndex k2, TScalar v) => AbsTensor3 n1 n2 n3 k1 k2 v -> [([Int],v)]
    toListShow3 t = filter (\x -> snd x /= scaleZero) $ map (\(x,y) -> (showInd x, y)) l
            where
                l = toListT3 t 
                showInd (i1,i2,i3) = (map fromEnum $ toList i1) ++ (map fromEnum $ toList i2) ++ 
                                     (map fromEnum $ toList i3)

    toListShow4 :: (TIndex k1, TIndex k2, TScalar v) => AbsTensor4 n1 n2 n3 n4 k1 k2 v -> [([Int],v)]
    toListShow4 t = filter (\x -> snd x /= scaleZero) $ map (\(x,y) -> (showInd x, y)) l
            where
                l = toListT4 t 
                showInd (i1,i2,i3,i4) = (map fromEnum $ toList i1) ++ (map fromEnum $ toList i2) ++ 
                                        (map fromEnum $ toList i3) ++ (map fromEnum $ toList i4)

    toListShow5 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> [([Int],v)]
    toListShow5 t = filter (\x -> snd x /= scaleZero) $ map (\(x,y) -> (showInd x, y)) l
            where
                l = toListT5 t 
                showInd (i1,i2,i3,i4,i5) = (map fromEnum $ toList i1) ++ (map fromEnum $ toList i2) ++ 
                                           (map fromEnum $ toList i3) ++ (map fromEnum $ toList i4) ++ 
                                           (map fromEnum $ toList i5)
                
    toListShow6 :: (TIndex k1, TIndex k2, TIndex k3, TScalar v) => AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> [([Int],v)]
    toListShow6 t = filter (\x -> snd x /= scaleZero) $ map (\(x,y) -> (showInd x, y)) l
            where
                l = toListT6 t 
                showInd (i1,i2,i3,i4,i5,i6) = (map fromEnum $ toList i1) ++ (map fromEnum $ toList i2) ++ 
                                              (map fromEnum $ toList i3) ++ (map fromEnum $ toList i4) ++ 
                                              (map fromEnum $ toList i5) ++ (map fromEnum $ toList i6) 

    toListShow7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> [([Int],v)]
    toListShow7 t = filter (\x -> snd x /= scaleZero) $ map (\(x,y) -> (showInd x, y)) l
            where
                l = toListT7 t 
                showInd (i1,i2,i3,i4,i5,i6,i7) = (map fromEnum $ toList i1) ++ (map fromEnum $ toList i2) ++ 
                                              (map fromEnum $ toList i3) ++ (map fromEnum $ toList i4) ++ 
                                              (map fromEnum $ toList i5) ++ (map fromEnum $ toList i6) ++
                                              (map fromEnum $ toList i7)

    toListShow8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4, TScalar v) => AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> [([Int],v)]
    toListShow8 t = filter (\x -> snd x /= scaleZero) $ map (\(x,y) -> (showInd x, y)) l
            where
                l = toListT8 t 
                showInd (i1,i2,i3,i4,i5,i6,i7,i8) = (map fromEnum $ toList i1) ++ (map fromEnum $ toList i2) ++ 
                                              (map fromEnum $ toList i3) ++ (map fromEnum $ toList i4) ++ 
                                              (map fromEnum $ toList i5) ++ (map fromEnum $ toList i6) ++
                                              (map fromEnum $ toList i7) ++ (map fromEnum $ toList i8)


    --define vars for non numeric tensor computations -> AnsVar represents the variables in the tensor ansätze

    type AnsVar = I.IntMap Rational 

    shiftVarLabels :: Int -> AnsVar -> AnsVar 
    shiftVarLabels s v =  I.mapKeys ((+) s) v

    shiftLabels1 :: (TIndex k1) => Int -> AbsTensor1 n1 k1 AnsVar -> AbsTensor1 n1 k1 AnsVar 
    shiftLabels1 s = mapTo1 (shiftVarLabels s)

    shiftLabels2 :: (TIndex k1) => Int -> AbsTensor2 n1 n2 k1 AnsVar -> AbsTensor2 n1 n2 k1 AnsVar 
    shiftLabels2 s = mapTo2 (shiftVarLabels s)

    shiftLabels3 :: (TIndex k1, TIndex k2) =>Int -> AbsTensor3 n1 n2 n3 k1 k2 AnsVar -> AbsTensor3 n1 n2 n3 k1 k2 AnsVar 
    shiftLabels3 s = mapTo3 (shiftVarLabels s)

    shiftLabels4 :: (TIndex k1, TIndex k2) => Int -> AbsTensor4 n1 n2 n3 n4 k1 k2 AnsVar -> AbsTensor4 n1 n2 n3 n4 k1 k2 AnsVar 
    shiftLabels4 s = mapTo4 (shiftVarLabels s)

    shiftLabels5 :: (TIndex k1, TIndex k2, TIndex k3) => Int -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 AnsVar -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 AnsVar 
    shiftLabels5 s = mapTo5 (shiftVarLabels s)

    shiftLabels6 :: (TIndex k1, TIndex k2, TIndex k3) => Int -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 AnsVar -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 AnsVar 
    shiftLabels6 s = mapTo6 (shiftVarLabels s)

    shiftLabels7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => Int -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 AnsVar -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 AnsVar 
    shiftLabels7 s = mapTo7 (shiftVarLabels s)

    shiftLabels8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => Int -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 AnsVar -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 AnsVar 
    shiftLabels8 s = mapTo8 (shiftVarLabels s)

    instance TScalar AnsVar where 
        addS = I.unionWith (+)
        subS v1 v2 = I.unionWith (+) v1 $ I.map ((*)(-1)) v2 
        scaleS s = I.map ((*) s) 
        scaleZero = I.empty
        

    instance TAlgebra Rational AnsVar where 
        type TAlg Rational AnsVar = AnsVar 
        prodA = scaleS  

    instance TAlgebra AnsVar Rational where 
        type TAlg AnsVar Rational = AnsVar 
        prodA = flip scaleS  

    --variable for generic are metric dofs, i.e parameters that are not obtained by making ansätze for the L derivatives

    data AreaVar a = AreaVar a (I.IntMap a) deriving (Eq)

    instance (TScalar a) => TScalar (AreaVar a) where 
        addS (AreaVar s1 varMap1) (AreaVar s2 varMap2) = AreaVar (addS s1 s2) $ I.unionWith (addS) varMap1 varMap2
        subS (AreaVar s1 varMap1) (AreaVar s2 varMap2) = AreaVar (subS s1 s2) $ I.unionWith (addS) varMap1 $ I.map (scaleS (-1)) varMap2 
        scaleS s (AreaVar s1 varMap1) = AreaVar (scaleS s s1) $ I.map (scaleS s) varMap1
        scaleZero = AreaVar scaleZero I.empty 

    instance (TScalar a) => TAlgebra (AreaVar a) Rational where 
        type TAlg (AreaVar a) Rational = AreaVar a
        prodA = flip scaleS

    instance (TScalar a) => TAlgebra Rational (AreaVar a) where 
        type TAlg Rational (AreaVar a) = AreaVar a
        prodA = scaleS 

    --up to now the same as AnsVar, but if both the ansätze and the dofVars contain parameters always order the ansatzVars to the deeper levels in the extpresion tree

    instance TAlgebra (AreaVar Rational) AnsVar where 
        type TAlg (AreaVar Rational) AnsVar = AreaVar (AnsVar) 
        prodA (AreaVar s1 varMapArea) varMapAns = AreaVar (scaleS s1 varMapAns) $ I.map (\x -> I.map ((*)x) varMapAns) varMapArea 
        
    instance TAlgebra AnsVar (AreaVar Rational) where 
        type TAlg AnsVar (AreaVar Rational) = AreaVar (AnsVar) 
        prodA varMapAns (AreaVar s1 varMapArea) = AreaVar (scaleS s1 varMapAns) $ I.map (\x -> I.map ((*)x) varMapAns) varMapArea 

    --tensors containing parameters in the derivatives must usually be evaluated before we can compute ranks, etc.

    ansVarToAreaVar :: AnsVar -> AreaVar AnsVar 
    ansVarToAreaVar ansV = AreaVar ansV I.empty 

    evalAreaVar :: (TScalar a) => I.IntMap Rational -> AreaVar a -> a 
    evalAreaVar iMap (AreaVar s1 areaMap) = addS s1 (foldr addS fst rest) 
                where 
                    assocs = I.assocs areaMap 
                    newList = map (\(x,y) -> scaleS ((I.!) iMap x) y) assocs
                    ([fst],rest) = splitAt 1 newList 
                    
    evalTensorAreaVar1 :: (TScalar a, TIndex k1) => I.IntMap Rational -> AbsTensor1 n1 k1 (AreaVar a) -> AbsTensor1 n1 k1 a  
    evalTensorAreaVar1 evalMap tens = mapTo1 (evalAreaVar evalMap) tens 

    evalTensorAreaVar2 :: (TScalar a, TIndex k1) => I.IntMap Rational -> AbsTensor2 n1 n2 k1 (AreaVar a) -> AbsTensor2 n1 n2 k1 a  
    evalTensorAreaVar2 evalMap tens = mapTo2 (evalAreaVar evalMap) tens 

    evalTensorAreaVar3 :: (TScalar a, TIndex k1, TIndex k2) => I.IntMap Rational -> AbsTensor3 n1 n2 n3 k1 k2 (AreaVar a) -> AbsTensor3 n1 n2 n3 k1 k2 a  
    evalTensorAreaVar3 evalMap tens = mapTo3 (evalAreaVar evalMap) tens 

    evalTensorAreaVar4 :: (TScalar a, TIndex k1, TIndex k2) => I.IntMap Rational -> AbsTensor4 n1 n2 n3 n4 k1 k2 (AreaVar a) -> AbsTensor4 n1 n2 n3 n4 k1 k2 a  
    evalTensorAreaVar4 evalMap tens = mapTo4 (evalAreaVar evalMap) tens 

    evalTensorAreaVar5 :: (TScalar a, TIndex k1, TIndex k2, TIndex k3) => I.IntMap Rational -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 (AreaVar a) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 a  
    evalTensorAreaVar5 evalMap tens = mapTo5 (evalAreaVar evalMap) tens 

    evalTensorAreaVar6 :: (TScalar a, TIndex k1, TIndex k2, TIndex k3) => I.IntMap Rational -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 (AreaVar a) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 a  
    evalTensorAreaVar6 evalMap tens = mapTo6 (evalAreaVar evalMap) tens 

    evalTensorAreaVar7 :: (TScalar a, TIndex k1, TIndex k2, TIndex k3, TIndex k4) => I.IntMap Rational -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 (AreaVar a) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 a  
    evalTensorAreaVar7 evalMap tens = mapTo7 (evalAreaVar evalMap) tens 

    evalTensorAreaVar8 :: (TScalar a, TIndex k1, TIndex k2, TIndex k3, TIndex k4) => I.IntMap Rational -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 (AreaVar a) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 a  
    evalTensorAreaVar8 evalMap tens = mapTo8 (evalAreaVar evalMap) tens 
                     
    --flatten tensor with ansVar values to assocs list 
    
    toListShowVar1 :: (TIndex k1) => AbsTensor1 n1 k1 AnsVar -> [([Int], [(Int, Rational)])]
    toListShowVar1 t = filter (\(_,c) -> c /= []) $ map (\(a,b) -> (a, filter (\(_,b) -> b/= 0) $ I.assocs b)) l
            where
                l = toListShow1 t 

    toListShowVar2 :: (TIndex k1) => AbsTensor2 n1 n2 k1 AnsVar -> [([Int], [(Int, Rational)])]
    toListShowVar2 t = filter (\(_,c) -> c /= []) $ map (\(a,b) -> (a, filter (\(_,b) -> b/= 0) $ I.assocs b)) l
            where
                l = toListShow2 t 

    toListShowVar3 :: (TIndex k1, TIndex k2) => AbsTensor3 n1 n2 n3 k1 k2 AnsVar -> [([Int], [(Int, Rational)])]
    toListShowVar3 t = filter (\(_,c) -> c /= []) $ map (\(a,b) -> (a, filter (\(_,b) -> b/= 0) $ I.assocs b)) l
            where
                l = toListShow3 t 

    toListShowVar4 :: (TIndex k1, TIndex k2) => AbsTensor4 n1 n2 n3 n4 k1 k2 AnsVar -> [([Int], [(Int, Rational)])]
    toListShowVar4 t = filter (\(_,c) -> c /= []) $ map (\(a,b) -> (a, filter (\(_,b) -> b/= 0) $ I.assocs b)) l
            where
                l = toListShow4 t 

    toListShowVar5 :: (TIndex k1, TIndex k2, TIndex k3) => AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 AnsVar -> [([Int], [(Int, Rational)])]
    toListShowVar5 t = filter (\(_,c) -> c /= []) $ map (\(a,b) -> (a, filter (\(_,b) -> b/= 0) $ I.assocs b)) l
            where
                l = toListShow5 t 

    toListShowVar6 :: (TIndex k1, TIndex k2, TIndex k3) => AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 AnsVar -> [([Int], [(Int, Rational)])]
    toListShowVar6 t = filter (\(_,c) -> c /= []) $ map (\(a,b) -> (a, filter (\(_,b) -> b/= 0) $ I.assocs b)) l
            where
                l = toListShow6 t 

    toListShowVar7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 AnsVar -> [([Int], [(Int, Rational)])]
    toListShowVar7 t = filter (\(_,c) -> c /= []) $ map (\(a,b) -> (a, filter (\(_,b) -> b/= 0) $ I.assocs b)) l
            where
                l = toListShow7 t 

    toListShowVar8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 AnsVar -> [([Int], [(Int, Rational)])]
    toListShowVar8 t = filter (\(_,c) -> c /= []) $ map (\(a,b) -> (a, filter (\(_,b) -> b/= 0) $ I.assocs b)) l
            where
                l = toListShow8 t 

    --write the tensor dat into a matrix 

    toMatList1' :: (TIndex k1) => AbsTensor1 n1 k1 AnsVar -> [[(Int, Rational)]]
    toMatList1' t = map snd $ toListShowVar1 t

    toMatList2' :: (TIndex k1) => AbsTensor2 n1 n2 k1 AnsVar -> [[(Int, Rational)]]
    toMatList2' t = map snd $ toListShowVar2 t

    toMatList3' :: (TIndex k1, TIndex k2) => AbsTensor3 n1 n2 n3 k1 k2 AnsVar -> [[(Int, Rational)]]
    toMatList3' t = map snd $ toListShowVar3 t

    toMatList4' :: (TIndex k1, TIndex k2) => AbsTensor4 n1 n2 n3 n4 k1 k2 AnsVar -> [[(Int, Rational)]]
    toMatList4' t = map snd $ toListShowVar4 t

    toMatList5' :: (TIndex k1, TIndex k2, TIndex k3) => AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 AnsVar -> [[(Int, Rational)]]
    toMatList5' t = map snd $ toListShowVar5 t

    toMatList6' :: (TIndex k1, TIndex k2, TIndex k3) => AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 AnsVar -> [[(Int, Rational)]]
    toMatList6' t = map snd $ toListShowVar6 t

    toMatList7' :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 AnsVar -> [[(Int, Rational)]]
    toMatList7' t = map snd $ toListShowVar7 t

    toMatList8' :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 AnsVar -> [[(Int, Rational)]]
    toMatList8' t = map snd $ toListShowVar8 t

    normalize :: [(Int,Rational)] -> ([(Int,Rational)],Rational)
    normalize [] = ([],1) 
    normalize ((a,b) : xs) = ((a,1) : (map (\(x,y) -> (x,y / b)) xs),b)

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
    mapTensList1 f (AppendTList1 t l) = (f t) : (mapTensList1 f l)

    mapTensList2 :: (forall n1 n2. AbsTensor2 n1 n2 k1 v -> b ) -> TensList2 k1 v -> [b]
    mapTensList2 f EmptyTList2 = [] 
    mapTensList2 f (AppendTList2 t l) = (f t) : (mapTensList2 f l)

    mapTensList3 :: (forall n1 n2 n3. AbsTensor3 n1 n2 n3 k1 k2 v -> b ) -> TensList3 k1 k2 v -> [b]
    mapTensList3 f EmptyTList3 = [] 
    mapTensList3 f (AppendTList3 t l) = (f t) : (mapTensList3 f l)

    mapTensList4 :: (forall n1 n2 n3 n4. AbsTensor4 n1 n2 n3 n4 k1 k2 v -> b ) -> TensList4 k1 k2 v -> [b]
    mapTensList4 f EmptyTList4 = [] 
    mapTensList4 f (AppendTList4 t l) = (f t) : (mapTensList4 f l)

    mapTensList5 :: (forall n1 n2 n3 n4 n5. AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v -> b ) -> TensList5 k1 k2 k3 v -> [b]
    mapTensList5 f EmptyTList5 = [] 
    mapTensList5 f (AppendTList5 t l) = (f t) : (mapTensList5 f l)

    mapTensList6 :: (forall n1 n2 n3 n4 n5 n6. AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v -> b ) -> TensList6 k1 k2 k3 v -> [b]
    mapTensList6 f EmptyTList6 = [] 
    mapTensList6 f (AppendTList6 t l) = (f t) : (mapTensList6 f l)

    mapTensList7 :: (forall n1 n2 n3 n4 n5 n6 n7. AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v -> b ) -> TensList7 k1 k2 k3 k4 v -> [b]
    mapTensList7 f EmptyTList7 = [] 
    mapTensList7 f (AppendTList7 t l) = (f t) : (mapTensList7 f l)

    mapTensList8 :: (forall n1 n2 n3 n4 n5 n6 n7 n8. AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v -> b ) -> TensList8 k1 k2 k3 k4 v -> [b]
    mapTensList8 f EmptyTList8 = [] 
    mapTensList8 f (AppendTList8 t l) = (f t) : (mapTensList8 f l)

    --collect data of heterogenic tensor list in one sparse matrix assocs list 
    
    toMatList1 :: (TIndex k1) => TensList1 k1 AnsVar -> [((Int,Int),Rational)] 
    toMatList1 t = l' 
        where
            matList = concat $ mapTensList1 toMatList1' t 
            l2 = nubBy (\(a,_) (b,_) -> a == b) $ map normalize $ matList 
            l = map (\(x,y) -> map (\(a,b) -> (a,b*y)) x) l2 
            l' = concat $ zipWith (\r z -> map (\(x,y) -> ((z, x), y)) r) l [1..]

    toMatList2 :: (TIndex k1) => TensList2 k1 AnsVar -> [((Int,Int),Rational)] 
    toMatList2 t = l'
        where
            matList = concat $ mapTensList2 toMatList2' t 
            l2 = nubBy (\(a,_) (b,_) -> a == b) $ map normalize $ matList 
            l = map (\(x,y) -> map (\(a,b) -> (a,b*y)) x) l2 
            l' = concat $ zipWith (\r z -> map (\(x,y) -> ((z, x), y)) r) l [1..]

    toMatList3 :: (TIndex k1, TIndex k2) => TensList3 k1 k2 AnsVar -> [((Int,Int),Rational)] 
    toMatList3 t = l'
        where
            matList = concat $ mapTensList3 toMatList3' t 
            l2 = nubBy (\(a,_) (b,_) -> a == b) $ map normalize $ matList 
            l = map (\(x,y) -> map (\(a,b) -> (a,b*y)) x) l2 
            l' = concat $ zipWith (\r z -> map (\(x,y) -> ((z, x), y)) r) l [1..]

    toMatList4 :: (TIndex k1, TIndex k2) => TensList4 k1 k2 AnsVar -> [((Int,Int),Rational)] 
    toMatList4 t = l'
        where
            matList = concat $ mapTensList4 toMatList4' t 
            l2 = nubBy (\(a,_) (b,_) -> a == b) $ map normalize $ matList 
            l = map (\(x,y) -> map (\(a,b) -> (a,b*y)) x) l2 
            l' = concat $ zipWith (\r z -> map (\(x,y) -> ((z, x), y)) r) l [1..]

    toMatList5 :: (TIndex k1, TIndex k2, TIndex k3) => TensList5 k1 k2 k3 AnsVar -> [((Int,Int),Rational)] 
    toMatList5 t = l' 
        where
            matList = concat $ mapTensList5 toMatList5' t 
            l2 = nubBy (\(a,_) (b,_) -> a == b) $ map normalize $ matList 
            l = map (\(x,y) -> map (\(a,b) -> (a,b*y)) x) l2 
            l' = concat $ zipWith (\r z -> map (\(x,y) -> ((z, x), y)) r) l [1..]

    toMatList6 :: (TIndex k1, TIndex k2, TIndex k3) => TensList6 k1 k2 k3 AnsVar -> [((Int,Int),Rational)] 
    toMatList6 t = l'
        where
            matList = concat $ mapTensList6 toMatList6' t 
            l2 = nubBy (\(a,_) (b,_) -> a == b) $ map normalize $ matList 
            l = map (\(x,y) -> map (\(a,b) -> (a,b*y)) x) l2 
            l' = concat $ zipWith (\r z -> map (\(x,y) -> ((z, x), y)) r) l [1..]

    toMatList7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => TensList7 k1 k2 k3 k4 AnsVar -> [((Int,Int),Rational)] 
    toMatList7 t = l'
        where
            matList = concat $ mapTensList7 toMatList7' t 
            l2 = nubBy (\(a,_) (b,_) -> a == b) $ map normalize $ matList 
            l = map (\(x,y) -> map (\(a,b) -> (a,b*y)) x) l2 
            l' = concat $ zipWith (\r z -> map (\(x,y) -> ((z, x), y)) r) l [1..]

    toMatList8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => TensList8 k1 k2 k3 k4 AnsVar -> [((Int,Int),Rational)] 
    toMatList8 t = l'
        where
            matList = concat $ mapTensList8 toMatList8' t 
            l2 = nubBy (\(a,_) (b,_) -> a == b) $ map normalize $ matList 
            l = map (\(x,y) -> map (\(a,b) -> (a,b*y)) x) l2 
            l' = concat $ zipWith (\r z -> map (\(x,y) -> ((z, x), y)) r) l [1..]


    --convert to Eigen format

    toEMatrix1 :: (TIndex k1) => TensList1 k1 AnsVar -> Sparse.SparseMatrixXd
    toEMatrix1 tList =  Sparse.fromList m n l'
            where
                l = toMatList1 tList
                l' = map (\((x,y),z) -> (x-1,y-1,fromRational z)) l
                m = maximum $ map (fst.fst) l
                n = maximum $ map (snd.fst) l 

    toEMatrix2 :: (TIndex k1) => TensList2 k1 AnsVar -> Sparse.SparseMatrixXd
    toEMatrix2 tList =  Sparse.fromList m n l'
            where
                l = toMatList2 tList
                l' = map (\((x,y),z) -> (x-1,y-1,fromRational z)) l
                m = maximum $ map (fst.fst) l
                n = maximum $ map (snd.fst) l 

    toEMatrix3 :: (TIndex k1, TIndex k2) => TensList3 k1 k2 AnsVar -> Sparse.SparseMatrixXd
    toEMatrix3 tList =  Sparse.fromList m n l'
            where
                l = toMatList3 tList
                l' = map (\((x,y),z) -> (x-1,y-1,fromRational z)) l
                m = maximum $ map (fst.fst) l
                n = maximum $ map (snd.fst) l 
            
    toEMatrix4 :: (TIndex k1, TIndex k2) => TensList4 k1 k2 AnsVar -> Sparse.SparseMatrixXd 
    toEMatrix4 tList =  Sparse.fromList m n l'
            where
                l = toMatList4 tList
                l' = map (\((x,y),z) -> (x-1,y-1,fromRational z)) l
                m = maximum $ map (fst.fst) l
                n = maximum $ map (snd.fst) l 

    toEMatrix5 :: (TIndex k1, TIndex k2, TIndex k3) => TensList5 k1 k2 k3 AnsVar -> Sparse.SparseMatrixXd 
    toEMatrix5 tList =  Sparse.fromList m n l'
            where
                l = toMatList5 tList
                l' = map (\((x,y),z) -> (x-1,y-1,fromRational z)) l
                m = maximum $ map (fst.fst) l
                n = maximum $ map (snd.fst) l 

    toEMatrix6 :: (TIndex k1, TIndex k2, TIndex k3) => TensList6 k1 k2 k3 AnsVar -> Sparse.SparseMatrixXd 
    toEMatrix6 tList =  Sparse.fromList m n l'
            where
                l = toMatList6 tList
                l' = map (\((x,y),z) -> (x-1,y-1,fromRational z)) l
                m = maximum $ map (fst.fst) l
                n = maximum $ map (snd.fst) l 

    toEMatrix7 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => TensList7 k1 k2 k3 k4 AnsVar -> Sparse.SparseMatrixXd 
    toEMatrix7 tList =  Sparse.fromList m n l'
            where
                l = toMatList7 tList
                l' = map (\((x,y),z) -> (x-1,y-1,fromRational z)) l
                m = maximum $ map (fst.fst) l
                n = maximum $ map (snd.fst) l 

    toEMatrix8 :: (TIndex k1, TIndex k2, TIndex k3, TIndex k4) => TensList8 k1 k2 k3 k4 AnsVar -> Sparse.SparseMatrixXd 
    toEMatrix8 tList =  Sparse.fromList m n l'
            where
                l = toMatList8 tList
                l' = map (\((x,y),z) -> (x-1,y-1,fromRational z)) l
                m = maximum $ map (fst.fst) l
                n = maximum $ map (snd.fst) l 

    --rank of the equations can be computed with rank Sol.FullPivLU or Sol.JakobySVD

    ------------------------------------------------------------------------------------------------------------------------------------
    --for our concrete purpose we need 3 types of indices 

    data Ind20 =  Ind20 {indVal20 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)
    data Ind9 =  Ind9 {indVal9 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)
    data Ind3 =  Ind3 {indVal3 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)

    instance Enum Ind20 where 
        toEnum = Ind20 
        fromEnum = indVal20 

    instance Enum Ind9 where 
        toEnum = Ind9 
        fromEnum = indVal9 

    instance Enum Ind3 where 
        toEnum = Ind3 
        fromEnum = indVal3 

    instance TIndex Ind20 where
        indRange = 21

    instance TIndex Ind9 where
        indRange = 10

    instance TIndex Ind3 where
        indRange = 4

    type ATens n1 n2 n3 n4 n5 n6 v = AbsTensor6 n1 n2 n3 n4 n5 n6 Ind20 Ind9 Ind3 v 

    --construct ATens6s from assoc lists 

    type IndTuple n1 n2 n3 n4 n5 n6 = (IndList n1 Ind20, IndList n2 Ind20 , IndList n3 Ind9, IndList n4 Ind9, IndList n5 Ind3, IndList n6 Ind3)

    type TList a = TensList6 Ind20 Ind9 Ind3 a 

    infixr 5 &>

    (&>) :: ATens n1 n2 n3 n4 n5 n6 a -> TList a -> TList a  
    (&>) t l = AppendTList6 t l 

    singletonTList :: ATens n1 n2 n3 n4 n5 n6 a -> TList a
    singletonTList t = t &> EmptyTList6

    infixr 6 &++

    (&++) :: TList a -> TList a -> TList a 
    (&++) EmptyTList6 t1 = t1 
    (&++) t1 EmptyTList6 = t1
    (&++) (AppendTList6 t1 EmptyTList6) t2 = AppendTList6 t1 t2 
    (&++) (AppendTList6 t1 t1') t2 = AppendTList6 t1 (t1' &++ t2)

    tensorRank :: ATens n1 n2 n3 n4 n5 n6 AnsVar -> Int 
    tensorRank t = Sol.rank Sol.FullPivLU $ Sparse.toMatrix $ toEMatrix6 (singletonTList t)
 
