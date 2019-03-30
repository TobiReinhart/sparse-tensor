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




 
module TensorTreeNumeric4_2 (
    
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

    --define lengthtyped lists as indices for a given tensors

    data IndList n a where
        Empty :: IndList 0 a 
        Append :: a -> IndList (n-1) a -> IndList n a 

    data IsZero (n :: Nat) where
        Zero :: (0 ~ n)     => IsZero n
        NonZero :: (1 <= n) => IsZero n
    deriving instance Show (IsZero n)
    
    isZero :: forall (n :: Nat). SNat n -> IsZero n
    isZero n = case n %~ (SNat @0)
                 of Proved Refl -> Zero
                    Disproved _ -> unsafeCoerce (NonZero @1)
    
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

    singletonInd :: a -> IndList 1 a
    singletonInd x = Append x Empty

    instance Functor (IndList n) where
        fmap f (Empty) = Empty 
        fmap f (Append x xs) = Append (f x) (fmap f xs)

    instance Foldable (IndList n) where
        foldr f y (Empty) = y
        foldr f y (Append x xs) = f x (foldr f y xs) 

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

    --define a tensor for a single index type

    --values of a given tensor should satisfy numberlike properties

    class (Eq a) => TScalar a where 
        addS :: a -> a -> a 
        subS :: a -> a -> a
        scaleS :: Rational -> a -> a 

    class TAlgebra v v' where 
        type TAlg v v' :: * 
        prodA :: v -> v' -> TAlg v v'

    class (Eq a, Ord a) => TIndex a where 
        indRange :: Int 

    data Tensor n k v where 
        Scalar :: v -> Tensor 0 k v 
        Tensor :: M.Map k (Tensor n k v) -> Tensor (n+1) k v
        ZeroTensor :: Tensor n k v

    --for converting tensors to bytestrings we need a non typesafe data type as intermediate type

    data TensorRep k v = ScalarR v | TensorR Natural (M.Map k (TensorRep k v)) | ZeroR Natural deriving (Show, Generic, Serialize)

    --convert betweeen typesafe and non typesafe tensors

    --do we need the lemma ? 

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
                                   in TensorR r $ fmap (\(t :: Tensor (n-1) k v) -> toRep t) m
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
                                                                                                Just Refl -> Tensor (fmap (\t -> (fromRep t) :: Tensor (x-1) k v) m)
                                                                                Zero    -> undefined
                                 Nothing -> undefined
    fromRep (ZeroR r) = case someNatVal (fromIntegral r)
                          of Just l  -> ZeroTensor
                             Nothing -> undefined
    
    instance KnownNat n => Generic (Tensor n k v) where
        type Rep (Tensor n k v) = Rep (TensorRep k v)

        from = from . toRep
        to   = fromRep . to

    deriving instance (KnownNat n, Ord k, Serialize k, Serialize v) => Serialize (Tensor n k v)

    instance Functor (Tensor n k) where 
        fmap f (Scalar x) = Scalar (f x)
        fmap f (Tensor m) = Tensor (M.map (fmap f) m)
        fmap f (ZeroTensor) = ZeroTensor

    deriving instance (Show a, Show k) => Show (Tensor n k a)

    deriving instance (Eq a, Eq k) => Eq (Tensor n k a)

    --in order to construct higher tensors we need the possibility to build tensors with lower order tensors as scalars

    --values of given tensor should be instance of both TScalar and TAlgebra to allow for TensorAlgebra and TensorProducts

    instance (TIndex k, TScalar v) => TScalar (Tensor n k v) where
        addS = (&+)
        subS = (&-)
        scaleS = (&.)

    instance (TIndex k, TAlgebra v v') => TAlgebra (Tensor n k v) (Tensor m k v') where 
        type TAlg (Tensor n k v) (Tensor m k v') = Tensor (n+m) k (TAlg v v')
        prodA = (&*)

    --for numeric tensors 

    instance TScalar Rational where
        addS = (+)
        subS = (-)
        scaleS = (*)

    instance TAlgebra Rational Rational where 
        type TAlg Rational Rational = Rational
        prodA = (*)

    --these two are probably false

    --instance (TIndex k, TScalar v) => TAlgebra Rational v where 
       -- type TAlg Rational v = v 
        --prodA = scaleS  

    --instance (TIndex k, TScalar v) => TAlgebra v Rational where 
      --  type TAlg v Rational = v
        --prodA = flip scaleS  

    getTensorMap :: Tensor (n+1) k v -> M.Map k (Tensor n k v)
    getTensorMap (Tensor m) = m 

    toListT :: Tensor n k v -> [(IndList n k, v)]
    toListT (Scalar x) = [(Empty, x)]
    toListT (Tensor m) =  concat $ map (\(i,t) -> appendF i $ toListT t) $ M.assocs m
            where
                appendF = \i l2 -> map (\(l,val) -> (Append i l ,val)) l2
    toListT ZeroTensor = []
    
    mkTens :: (IndList n k, v) -> Tensor n k v
    mkTens (Empty, a) = Scalar a
    mkTens (Append x xs, a) = Tensor $ M.singleton x $ mkTens (xs, a)

    fromListT :: (TIndex k, TScalar v) => [(IndList n k, v)] -> Tensor n k v 
    fromListT [x] = mkTens x 
    fromListT (x:xs) = foldr insertOrAdd (mkTens x) xs 
    fromListT [] = ZeroTensor

    insertOrAdd :: (TIndex k, TScalar v) => (IndList n k, v) -> Tensor n k v -> Tensor n k v 
    insertOrAdd (Empty, a) (Scalar b) = Scalar (addS a b)
    insertOrAdd (Append x xs, a) (Tensor m) = Tensor $ M.insertWith (\_ o -> insertOrAdd (xs, a) o) x indTens m 
                where
                    indTens = mkTens (xs, a)
    insertOrAdd inds ZeroTensor = mkTens inds
    

    --addition for tensors

    infixl 6 &+

    (&+) :: (TIndex k, TScalar v) => Tensor n k v -> Tensor n k v -> Tensor n k v 
    (&+) (Scalar a) (Scalar b) = Scalar (addS a b)
    (&+) (Tensor m1) (Tensor m2) = Tensor $ M.unionWith (&+) m1 m2
    (&+) t1 ZeroTensor = t1
    (&+) ZeroTensor t2 = t2

    infix 8 &. 

    (&.) :: (TIndex k, TScalar v) => Rational -> Tensor n k v -> Tensor n k v 
    (&.) scalar t = fmap (scaleS scalar) t 

    infixl 5 &- 
    
    (&-) :: (TIndex k, TScalar v) => Tensor n k v -> Tensor n k v -> Tensor n k v
    (&-) t1 t2 = t1 &+ (-1) &. t2 

    --product of tensors

    infixr 7 &*

    (&*) :: (TIndex k, TAlgebra v v') => Tensor n k v -> Tensor m k v' -> Tensor (n+m) k (TAlg v v') 
    (&*) (Scalar x) (Scalar y) = Scalar (prodA x y)
    (&*) (Scalar x) t2 = fmap (prodA x) t2 
    (&*) (Tensor m) t2 = Tensor $ M.map (\t1 -> (&*) t1 t2) m 
    (&*) t1 ZeroTensor = ZeroTensor 
    (&*) ZeroTensor t2 = ZeroTensor 

    --do we really need this

    tensorProdWith :: (a -> b -> c) -> Tensor n k a -> Tensor m k b -> Tensor (n+m) k c 
    tensorProdWith prodF (Scalar x) (Scalar y) = Scalar (prodF x y)
    tensorProdWith prodF (Scalar x) t2 = fmap (prodF x) t2 
    tensorProdWith prodF (Tensor m) t2 = Tensor $ M.map (\t1 -> tensorProdWith prodF t1 t2) m 
    tensorProdWith prodF t1 ZeroTensor = ZeroTensor 
    tensorProdWith prodF ZeroTensor t2 = ZeroTensor 

    --transpose a given tensor in 2 of its indices

    tensorTrans :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v
    tensorTrans (0, j) t = fromListT l
                    where 
                        l = (map (\(x,y) -> (swapHead j x, y)) $ toListT t)
    tensorTrans (i, j) (Tensor m) = Tensor $ M.map (tensorTrans (i-1, j-1)) m 
    tensorTrans (i ,j) ZeroTensor = ZeroTensor

    --transpose a given Tensor in several of its indices (does not work i the same index occurs several times)

    tensorBlockTrans :: (TIndex k, TScalar v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k v
    tensorBlockTrans (l1,l2) t = foldr tensorTrans t indList
            where
                indList = if (intersect l1 l2 == []) then zip l1 l2 else error "at least one indexin the list occurs several times"

    --symmetrization of a given Tensor with rational Values stored

    symTens :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v 
    symTens inds t = t &+ (tensorTrans inds t) 

    aSymTens :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v 
    aSymTens inds t = t &- (tensorTrans inds t) 

    symTensFac :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v 
    symTensFac inds t = (1%2) &. (symTens inds t)

    aSymTensFac :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v 
    aSymTensFac inds t = (1%2) &. (aSymTens inds t)

    --the next step is block symmetrization 

    symBlockTens :: (TIndex k, TScalar v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k v 
    symBlockTens inds t = t &+ (tensorBlockTrans inds t) 

    aSymBlockTens :: (TIndex k, TScalar v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k v 
    aSymBlockTens inds t = t &- (tensorBlockTrans inds t) 

    symBlockTensFac :: (TIndex k, TScalar v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k v 
    symBlockTensFac inds t = (1%2) &. (symBlockTens inds t)

    aSymBlockTensFac :: (TIndex k, TScalar v) => ([Int],[Int]) -> Tensor n k v -> Tensor n k v 
    aSymBlockTensFac inds t = (1%2) &. (aSymBlockTens inds t)

    --the next step is cyclic symmetrization

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
    tensorContr (i,j) (Tensor m) = Tensor $ M.map (tensorContr (i-1,j)) m
    tensorContr inds ZeroTensor = ZeroTensor    

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

    mapTo1 :: (v1 -> v2) -> Tensor n1 k v1 -> Tensor n1 k v2 
    mapTo1 = fmap 

    mapTo2 :: (v1 -> v2) -> Tensor2 n1 n2 k v1 -> Tensor2 n1 n2 k v2 
    mapTo2 f = fmap (fmap f)

    mapTo3 :: (v1 -> v2) -> AbsTensor3 n1 n2 n3 k1 k2 v1 -> AbsTensor3 n1 n2 n3 k1 k2 v2 
    mapTo3 f = fmap (fmap (fmap f))

    mapTo4 :: (v1 -> v2) -> AbsTensor4 n1 n2 n3 n4 k1 k2 v1 -> AbsTensor4 n1 n2 n3 n4 k1 k2 v2 
    mapTo4 f = fmap (fmap (fmap (fmap f)))

    mapTo5 :: (v1 -> v2) -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v1 -> AbsTensor5 n1 n2 n3 n4 n5 k1 k2 k3 v2 
    mapTo5 f = fmap (fmap (fmap (fmap (fmap f))))

    mapTo6 :: (v1 -> v2) -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v1 -> AbsTensor6 n1 n2 n3 n4 n5 n6 k1 k2 k3 v2 
    mapTo6 f = fmap (fmap (fmap (fmap (fmap (fmap f)))))

    mapTo7 :: (v1 -> v2) -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v1 -> AbsTensor7 n1 n2 n3 n4 n5 n6 n7 k1 k2 k3 k4 v2 
    mapTo7 f = fmap (fmap (fmap (fmap (fmap (fmap (fmap f))))))

    mapTo8 :: (v1 -> v2) -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v1 -> AbsTensor8 n1 n2 n3 n4 n5 n6 n7 n8 k1 k2 k3 k4 v2 
    mapTo8 f = fmap (fmap (fmap (fmap (fmap (fmap (fmap (fmap f)))))))


    --in total these functions allow to build the symmetrizer and contraction functions for generic tensors 

    --for our concrete purpose we need 3 types of indices 

    data Ind20 =  Ind20 {indVal20 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)
    data Ind9 =  Ind9 {indVal9 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)
    data Ind3 =  Ind3 {indVal3 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)

    instance TIndex Ind20 where
        indRange = 21

    instance TIndex Ind9 where
        indRange = 10

    instance TIndex Ind3 where
        indRange = 4

    type ATens n1 n2 n3 n4 n5 n6 v = AbsTensor6 n1 n2 n3 n4 n5 n6 Ind20 Ind9 Ind3 v 

    --symmetrizer

    symATens :: (TScalar v) => Int -> (Int,Int) -> ATens n1 n2 n3 n4 n5 n6 v -> ATens n1 n2 n3 n4 n5 n6 v 
    symATens 1 inds = symTens inds 
    symATens 2 inds = mapTo1 (symTens inds) 
    symATens 3 inds = mapTo2 (symTens inds) 
    symATens 4 inds = mapTo3 (symTens inds) 
    symATens 5 inds = mapTo4 (symTens inds) 
    symATens 6 inds = mapTo5 (symTens inds) 

    symATensFac :: (TScalar v) => Int -> (Int,Int) -> ATens n1 n2 n3 n4 n5 n6 v -> ATens n1 n2 n3 n4 n5 n6 v 
    symATensFac 1 inds = symTensFac inds 
    symATensFac 2 inds = mapTo1 (symTensFac inds) 
    symATensFac 3 inds = mapTo2 (symTensFac inds) 
    symATensFac 4 inds = mapTo3 (symTensFac inds) 
    symATensFac 5 inds = mapTo4 (symTensFac inds) 
    symATensFac 6 inds = mapTo5 (symTensFac inds) 

    aSymATens :: (TScalar v) => Int -> (Int,Int) -> ATens n1 n2 n3 n4 n5 n6 v -> ATens n1 n2 n3 n4 n5 n6 v 
    aSymATens 1 inds = aSymTens inds 
    aSymATens 2 inds = mapTo1 (aSymTens inds) 
    aSymATens 3 inds = mapTo2 (aSymTens inds) 
    aSymATens 4 inds = mapTo3 (aSymTens inds) 
    aSymATens 5 inds = mapTo4 (aSymTens inds) 
    aSymATens 6 inds = mapTo5 (aSymTens inds) 

    aSymATensFac :: (TScalar v) => Int -> (Int,Int) -> ATens n1 n2 n3 n4 n5 n6 v -> ATens n1 n2 n3 n4 n5 n6 v 
    aSymATensFac 1 inds = aSymTensFac inds 
    aSymATensFac 2 inds = mapTo1 (aSymTensFac inds) 
    aSymATensFac 3 inds = mapTo2 (aSymTensFac inds) 
    aSymATensFac 4 inds = mapTo3 (aSymTensFac inds) 
    aSymATensFac 5 inds = mapTo4 (aSymTensFac inds) 
    aSymATensFac 6 inds = mapTo5 (aSymTensFac inds) 

    --block symmetrizer 

    symBlockATens :: (TScalar v) => Int -> ([Int],[Int]) -> ATens n1 n2 n3 n4 n5 n6 v -> ATens n1 n2 n3 n4 n5 n6 v 
    symBlockATens 1 inds = symBlockTens inds 
    symBlockATens 2 inds = mapTo1 (symBlockTens inds) 
    symBlockATens 3 inds = mapTo2 (symBlockTens inds) 
    symBlockATens 4 inds = mapTo3 (symBlockTens inds) 
    symBlockATens 5 inds = mapTo4 (symBlockTens inds) 
    symBlockATens 6 inds = mapTo5 (symBlockTens inds) 

    symBlockATensFac :: (TScalar v) => Int -> ([Int],[Int]) -> ATens n1 n2 n3 n4 n5 n6 v -> ATens n1 n2 n3 n4 n5 n6 v 
    symBlockATensFac 1 inds = symBlockTensFac inds 
    symBlockATensFac 2 inds = mapTo1 (symBlockTensFac inds) 
    symBlockATensFac 3 inds = mapTo2 (symBlockTensFac inds) 
    symBlockATensFac 4 inds = mapTo3 (symBlockTensFac inds) 
    symBlockATensFac 5 inds = mapTo4 (symBlockTensFac inds) 
    symBlockATensFac 6 inds = mapTo5 (symBlockTensFac inds) 

    aSymBlockATens :: (TScalar v) => Int -> ([Int],[Int]) -> ATens n1 n2 n3 n4 n5 n6 v -> ATens n1 n2 n3 n4 n5 n6 v 
    aSymBlockATens 1 inds = aSymBlockTens inds 
    aSymBlockATens 2 inds = mapTo1 (aSymBlockTens inds) 
    aSymBlockATens 3 inds = mapTo2 (aSymBlockTens inds) 
    aSymBlockATens 4 inds = mapTo3 (aSymBlockTens inds) 
    aSymBlockATens 5 inds = mapTo4 (aSymBlockTens inds) 
    aSymBlockATens 6 inds = mapTo5 (aSymBlockTens inds) 

    aSymBlockATensFac :: (TScalar v) => Int -> ([Int],[Int]) -> ATens n1 n2 n3 n4 n5 n6 v -> ATens n1 n2 n3 n4 n5 n6 v 
    aSymBlockATensFac 1 inds = aSymBlockTensFac inds 
    aSymBlockATensFac 2 inds = mapTo1 (aSymBlockTensFac inds) 
    aSymBlockATensFac 3 inds = mapTo2 (aSymBlockTensFac inds) 
    aSymBlockATensFac 4 inds = mapTo3 (aSymBlockTensFac inds) 
    aSymBlockATensFac 5 inds = mapTo4 (aSymBlockTensFac inds) 
    aSymBlockATensFac 6 inds = mapTo5 (aSymBlockTensFac inds) 

    --cyclic Symmetrizer 

    cyclicSymATens :: (TScalar v) => Int -> [Int] -> ATens n1 n2 n3 n4 n5 n6 v -> ATens n1 n2 n3 n4 n5 n6 v 
    cyclicSymATens 1 inds = cyclicSymTens inds 
    cyclicSymATens 2 inds = mapTo1 (cyclicSymTens inds) 
    cyclicSymATens 3 inds = mapTo2 (cyclicSymTens inds) 
    cyclicSymATens 4 inds = mapTo3 (cyclicSymTens inds) 
    cyclicSymATens 5 inds = mapTo4 (cyclicSymTens inds) 
    cyclicSymATens 6 inds = mapTo5 (cyclicSymTens inds) 

    cyclicSymATensFac :: (TScalar v) => Int -> [Int] -> ATens n1 n2 n3 n4 n5 n6 v -> ATens n1 n2 n3 n4 n5 n6 v 
    cyclicSymATensFac 1 inds = cyclicSymTensFac inds 
    cyclicSymATensFac 2 inds = mapTo1 (cyclicSymTensFac inds) 
    cyclicSymATensFac 3 inds = mapTo2 (cyclicSymTensFac inds) 
    cyclicSymATensFac 4 inds = mapTo3 (cyclicSymTensFac inds) 
    cyclicSymATensFac 5 inds = mapTo4 (cyclicSymTensFac inds) 
    cyclicSymATensFac 6 inds = mapTo5 (cyclicSymTensFac inds) 

    cyclicASymATens :: (TScalar v) => Int -> [Int] -> ATens n1 n2 n3 n4 n5 n6 v -> ATens n1 n2 n3 n4 n5 n6 v 
    cyclicASymATens 1 inds = cyclicASymTens inds 
    cyclicASymATens 2 inds = mapTo1 (cyclicASymTens inds) 
    cyclicASymATens 3 inds = mapTo2 (cyclicASymTens inds) 
    cyclicASymATens 4 inds = mapTo3 (cyclicASymTens inds) 
    cyclicASymATens 5 inds = mapTo4 (cyclicASymTens inds) 
    cyclicASymATens 6 inds = mapTo5 (cyclicASymTens inds) 

    cyclicASymATensFac :: (TScalar v) => Int -> [Int] -> ATens n1 n2 n3 n4 n5 n6 v -> ATens n1 n2 n3 n4 n5 n6 v 
    cyclicASymATensFac 1 inds = cyclicASymTensFac inds 
    cyclicASymATensFac 2 inds = mapTo1 (cyclicASymTensFac inds) 
    cyclicASymATensFac 3 inds = mapTo2 (cyclicASymTensFac inds) 
    cyclicASymATensFac 4 inds = mapTo3 (cyclicASymTensFac inds) 
    cyclicASymATensFac 5 inds = mapTo4 (cyclicASymTensFac inds) 
    cyclicASymATensFac 6 inds = mapTo5 (cyclicASymTensFac inds) 

    --general contraction function 

    contrATens20 :: (TScalar v) => (Int,Int) -> ATens n1 n2 n3 n4 n5 n6 v -> ATens (n1-1) (n2-1) n3 n4 n5 n6 v
    contrATens20 = tensorContr 

    contrATens9 :: (TScalar v) => (Int,Int) -> ATens n1 n2 n3 n4 n5 n6 v -> ATens n1 n2 (n3-1) (n4-1) n5 n6 v
    contrATens9 inds = mapTo2 (tensorContr inds)

    contrATens3 :: (TScalar v) => (Int,Int) -> ATens n1 n2 n3 n4 n5 n6 v -> ATens n1 n2 n3 n4 (n5-1) (n6-1) v
    contrATens3 inds = mapTo4 (tensorContr inds)

    --construct ATens6s from assoc lists 

    type IndTuple n1 n2 n3 n4 n5 n6 = (IndList n1 Ind20, IndList n2 Ind20 , IndList n3 Ind9, IndList n4 Ind9, IndList n5 Ind3, IndList n6 Ind3)

    mkTens6 :: (IndTuple n1 n2 n3 n4 n5 n6, a) -> ATens n1 n2 n3 n4 n5 n6 a
    mkTens6 ((i1,i2,i3,i4,i5,i6),s) = mkTens (i1, mkTens (i2, mkTens (i3, mkTens (i4, mkTens (i5, mkTens (i6, s))))))

    fromListT6 :: (TScalar v) => [(IndTuple n1 n2 n3 n4 n5 n6, v)] -> ATens n1 n2 n3 n4 n5 n6 v
    fromListT6 l = foldr (&+) ZeroTensor tensList
            where 
                tensList = map mkTens6 l 
                
    --convert a tensor to corresponding assocs list 

    toListT6 :: ATens n1 n2 n3 n4 n5 n6 a -> [(IndTuple n1 n2 n3 n4 n5 n6, a)]
    toListT6 t = concat $ map (\(x,y) -> appendT5 x $ toListT y ) $ 
                 concat $ map (\(x,y) -> appendT4 x $ toListT y ) $
                 concat $ map (\(x,y) -> appendT3 x $ toListT y ) $
                 concat $ map (\(x,y) -> appendT2 x $ toListT y ) $ concat $
                 map (\(x,y) -> appendT1 x $ toListT y ) $ toListT t
                    where
                        appendT1 = \i l -> map (\(x,y) -> ((i,x),y)) l
                        appendT2 = \(i1,i2) l -> map (\(x,y) -> ((i1,i2,x),y)) l
                        appendT3 = \(i1,i2,i3) l -> map (\(x,y) -> ((i1,i2,i3,x),y)) l
                        appendT4 = \(i1,i2,i3,i4) l -> map (\(x,y) -> ((i1,i2,i3,i4,x),y)) l
                        appendT5 = \(i1,i2,i3,i4,i5) l -> map (\(x,y) -> ((i1,i2,i3,i4,i5,x),y)) l
                
    toListShow6 :: ATens n1 n2 n3 n4 n5 n6 a -> [([Int],a)]
    toListShow6 t = map (\(x,y) -> (showInd x, y)) l
            where
                l = toListT6 t 
                showInd (i1,i2,i3,i4,i5,i6) = (map indVal20 $ toList i1) ++ (map indVal20 $ toList i2) ++ 
                                              (map indVal9 $ toList i3) ++ (map indVal9 $ toList i4) ++ 
                                              (map indVal3 $ toList i5) ++ (map indVal3 $ toList i6) 
    

    --time to test ! 

    delta20 :: ATens 1 1 0 0 0 0 Rational 
    delta20 = fromListT6 $ zip [(singletonInd (Ind20 i),singletonInd (Ind20 i), Empty, Empty, Empty, Empty) | i <- [0..20]] (repeat 1)

    delta9 :: ATens 0 0 1 1 0 0 Rational
    delta9 = fromListT6 $ zip [(Empty, Empty, singletonInd (Ind9 i),singletonInd (Ind9 i), Empty, Empty) | i <- [0..9]] (repeat 1)

    delta3 :: ATens 0 0 0 0 1 1 Rational
    delta3 = fromListT6 $ zip [(Empty, Empty, Empty, Empty, singletonInd (Ind3 i),singletonInd (Ind3 i)) | i <- [0..3]] (repeat 1)












    










    
    


    
    
    



