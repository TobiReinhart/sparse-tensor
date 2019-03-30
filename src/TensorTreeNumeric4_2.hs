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

    removeContractionInd :: (b -> Int) -> (a -> Int) -> Int -> b -> (IndList n a, c) -> Maybe ((IndList (n-1) a),c)
    removeContractionInd g f 0 ind1 ((Append x xs), t)
                | g ind1 == f x = Just $ (xs,t) 
                | otherwise = Nothing 
    removeContractionInd g f i ind1 ((Append x xs),t) = fmap (\(m,n) -> (Append x m, n)) $ removeContractionInd g f (i-1) ind1 (xs,t)

    --define a tensor for a single index type

    class (Eq a) => TScalar a where 
        addS :: a -> a -> a 
        subS :: a -> a -> a
        scaleS :: Rational -> a -> a 
        prodS :: (TScalar b, TScalar c) => a -> b -> c 

    class (Eq a, Ord a) => TIndex a where 
        indRange :: a -> Int 
        upper :: a -> Bool 
        lower :: a -> Bool 
        upper a = not $ lower a 

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

    instance (TIndex k, TScalar v) => TScalar (Tensor n k v) where
        addS = (&+)
        subS = (&-)
        scaleS = (&.)
        prodS = (&*)

    --for numeric tensors 

    instance TScalar Rational where
        addS = (+)
        subS = (-)
        scaleS = (*)
        prodS = (*)

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

    (&*) :: (TIndex k, TScalar v) => Tensor n k v -> Tensor m k v -> Tensor (n+m) k v 
    (&*) (Scalar x) (Scalar y) = Scalar (prodS x y)
    (&*) (Scalar x) t2 = fmap (prodS x) t2 
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

    symTensFac :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v 
    symTensFac inds t = (1%2) &. (t &+ (tensorTrans inds t)) 

    aSymTensFac :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v 
    aSymTensFac inds t = (1%2) &. (t &- (tensorTrans inds t)) 

    --and factorLess -> (wotks also for non rational tensors, i.e. with Vars)

    symTens :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v 
    symTens inds t = t &+ (tensorTrans inds t) 

    aSymTens :: (TIndex k, TScalar v) => (Int,Int) -> Tensor n k v -> Tensor n k v 
    aSymTens inds t = t &- (tensorTrans inds t) 

    --type Tensor2 n1 n2 k v = Tensor n1 k (Tensor n2 k v)
   


    
    
    



