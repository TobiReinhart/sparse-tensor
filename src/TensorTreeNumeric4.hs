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

{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE LambdaCase #-}


{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver   #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

{-# OPTIONS_GHC -dcore-lint #-}

{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}




module TensorTreeNumeric4 (
    toListT8, toListShow8, intAIB, interMetric, interArea, interEqn2, interEqn3, interEqn4, trianMapAreaI, trianMapAreaJ, trianMapI2, trianMapJ2, flatInter,
    interI2, interJ2, aSymI2, interIArea, interJArea, toListShowVar, toMatrix, getTensorRank, getTensorRank2, shiftVarLabels, getTensorRank3, getTensorRank4, toMatrix4, toMatrix3, toMatrix2,
    delta20, delta19, delta9, delta3, tensorContr20, tensorContr19, tensorContr9, tensorContr3, tensorProd8, toMatList, toSparseMatRed,
    tensorTransU20, tensorTransL20, tensorTransU19, tensorTransL19, tensorTransU9, tensorTransL9, tensorTransU3, tensorTransL3, tensorSub8,
    triangleMap3P', ansatzAIBJCK', index2SparseAnsatzAIBJCKSym, VarMap, area18TensList, Tensor(..), Tensor8, IndList(..),
    tensorAdd8, invEta, flatArea, tensorSMult,
    Uind20(..), Lind20(..), Uind19(..), Lind19(..), Uind9(..), Lind9(..), Uind3(..), Lind3(..), IndTuple, fromListT8, fromListTWith8, singletonInd, ansatzAIB,
    ansatzAI, ansatzAB, ansatzAaBb, ansatzABC, ansatzA, tensorTransWithU20, tensorTransWithL20, tensorTransWithU19, tensorTransWithL19, tensorTransWithU9, tensorTransWithL9,
    tensorTransWithU3, tensorTransWithL3, addVarsMap, ansatzAIBJ, ansatzABbCc, ansatzABCI, ansatzApBqCI, ansatzABICJ, ansatzAIBJCK,
    ansatzABCDJ, ansatzABCcDd,
    eqn3, eqn1AI, eqn2Aa, eqn3A, eqn1ABI, eqn2ABb, eqn3AB, eqn1AaBb, eqn1ABCI, eqn1ABbCc, eqn2ABCc, eqn3ABC,
    eqn1, eqn1A, eqn3AI, 
    ansatzAB2, ansatzAIB2_1, ansatzAIB2_2, ansatzAIBJ2 
) where

    import Data.Foldable
    import Data.List 
    import Control.Applicative
    import Data.Maybe
    import qualified Data.Map.Strict as M
    import qualified Data.IntMap.Strict as I
    import qualified Data.Sequence as S
    import Numeric.Natural
    import GHC.TypeLits
    import Data.Proxy
    import GHC.TypeLits.Normalise
    import Data.Foldable
    import GHC.Generics
    import Control.DeepSeq

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

    data Tensor n k v where 
        Scalar :: v -> Tensor 0 k v 
        Tensor :: M.Map k (Tensor n k v) -> Tensor (n+1) k v
        ZeroTensor :: Tensor n k v

    data TensorRep k v = ScalarR v | TensorR Natural (M.Map k (TensorRep k v)) | ZeroR Natural deriving (Show, Generic, Serialize)

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

    fromListT :: (Ord k, Eq k, Eq v) => (v -> v -> v) -> [(IndList n k, v)] -> Tensor n k v 
    fromListT addF [x] = mkTens x 
    fromListT addF (x:xs) = foldr (insertOrAdd addF) (mkTens x) xs 
    fromListT addF [] = ZeroTensor

    insertOrAdd :: (Ord k, Eq k, Eq v) =>  (v -> v -> v) -> (IndList n k, v) -> Tensor n k v -> Tensor n k v 
    insertOrAdd addF (Empty, a) (Scalar b) = Scalar (addF a b)
    insertOrAdd addF (Append x xs, a) (Tensor m) = Tensor $ M.insertWith (\_ o -> insertOrAdd addF (xs, a) o) x indTens m 
                where
                    indTens = mkTens (xs, a)
    insertOrAdd addF inds ZeroTensor = mkTens inds
    

    tensorAdd :: (Ord k, Eq v) => (v -> v -> v) -> Tensor n k v -> Tensor n k v -> Tensor n k v 
    tensorAdd addF (Scalar a) (Scalar b) = Scalar (addF a b)
    tensorAdd addF (Tensor m1) (Tensor m2) = Tensor $ M.unionWith (tensorAdd addF) m1 m2
    tensorAdd addF t1 ZeroTensor = t1
    tensorAdd addF ZeroTensor t2 = t2
    

    --tensorProduct: append the second tensor to the right of the first one 

    tensorProd :: (a -> b -> c) -> Tensor n k a -> Tensor m k b -> Tensor (n+m) k c 
    tensorProd prodF (Scalar x) (Scalar y) = Scalar (prodF x y)
    tensorProd prodF (Scalar x) t2 = fmap (prodF x) t2 
    tensorProd prodF (Tensor m) t2 = Tensor $ M.map (\t1 -> tensorProd prodF t1 t2) m 
    tensorProd prodF t1 ZeroTensor = ZeroTensor 
    tensorProd prodF ZeroTensor t2 = ZeroTensor 

    --could be improved if not the whole tensor but only the part necessary is converted to a list

    tensorTrans :: (Ord k, Eq v) => (v -> v -> v) -> (Int,Int) -> Tensor n k v -> Tensor n k v
    tensorTrans addF (0, j) t = fromListT addF l
                    where 
                        l = (map (\(x,y) -> (swapHead j x, y)) $ toListT t)
    tensorTrans addF (i, j) (Tensor m) = Tensor $ M.map (tensorTrans addF (i-1, j-1)) m 
    tensorTrans addF (i ,j) ZeroTensor = ZeroTensor

    tensorContr :: (Ord k, Ord k', Eq k, Eq v) => (k -> Int) -> (k' -> Int) -> (v -> v -> v) -> (Int, Int) -> Tensor n k (Tensor m k' v) -> Tensor (n-1) k (Tensor (m-1) k' v)
    tensorContr g f addF (0,j) t = fromListT (tensorAdd addF) tensList 
            where
                l = map (\(x,y) -> (x, toListT y)) $ toListT t
                l2 = map (\(x,y) -> (tailInd x,(mapMaybe (removeContractionInd g f j (headInd x)) y))) l
                l3 = filter (\(_,y) -> length y >= 1) l2 
                tensList = map (\(x,y) -> (x, fromListT addF y)) l3
    tensorContr g f addF (i,j) (Tensor m) = Tensor $ M.map (tensorContr g f addF (i-1,j)) m
    tensorContr g f addF inds ZeroTensor = ZeroTensor


    data Uind20 =  Uind20 {indValU20 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)
    data Lind20 =  Lind20 {indValL20 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)
    data Uind19 =  Uind19 {indValU19 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)
    data Lind19 =  Lind19 {indValL19 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)
    data Uind9 =  Uind9 {indValU9 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)
    data Lind9 =  Lind9 {indValL9 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)
    data Uind3 =  Uind3 {indValU3 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)
    data Lind3 =  Lind3 {indValL3 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show, Read, Generic, NFData, Serialize)


    type Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a = Tensor n1 Uind20 (Tensor n2 Lind20 (Tensor n3 Uind19 (Tensor n4 Lind19 (Tensor n5 Uind9 (Tensor n6 Lind9 (Tensor n7 Uind3 (Tensor n8 Lind3 a)))))))
    
    actOnScalar :: (a -> a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a 
    actOnScalar f = fmap (fmap (fmap (fmap (fmap (fmap (fmap (fmap f)))))))

    actOnL3 :: (Tensor n8 Lind3 a -> Tensor m8 Lind3 a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 n6 n7 m8 a
    actOnL3 f = fmap (fmap (fmap (fmap (fmap (fmap (fmap f))))))

    actOnU3 :: (Tensor n7 Uind3 (Tensor n8 Lind3 a) -> Tensor m7 Uind3 (Tensor m8 Lind3 a)) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 n6 m7 m8 a 
    actOnU3 f = fmap (fmap (fmap (fmap (fmap (fmap f)))))

    actOnL9 :: (Tensor n6 Lind9 (Tensor n7 Uind3 (Tensor n8 Lind3 a)) -> Tensor m6 Lind9 (Tensor m7 Uind3 (Tensor m8 Lind3 a))) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 m6 m7 m8 a 
    actOnL9 f = fmap (fmap (fmap (fmap (fmap f))))

    actOnU9 :: (Tensor n5 Uind9 (Tensor n6 Lind9 (Tensor n7 Uind3 (Tensor n8 Lind3 a))) -> Tensor m5 Uind9 (Tensor m6 Lind9 (Tensor m7 Uind3 (Tensor m8 Lind3 a)))) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 m5 m6 m7 m8 a 
    actOnU9 f = fmap (fmap (fmap (fmap f)))

    actOnL19 :: (Tensor n4 Lind19 (Tensor n5 Uind9 (Tensor n6 Lind9 (Tensor n7 Uind3 (Tensor n8 Lind3 a)))) -> Tensor m4 Lind19 (Tensor m5 Uind9 (Tensor m6 Lind9 (Tensor m7 Uind3 (Tensor m8 Lind3 a))))) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 m4 m5 m6 m7 m8 a 
    actOnL19 f = fmap (fmap (fmap f))

    actOnU19 :: (Tensor n3 Uind19 (Tensor n4 Lind19 (Tensor n5 Uind9 (Tensor n6 Lind9 (Tensor n7 Uind3 (Tensor n8 Lind3 a))))) -> Tensor m3 Uind19 (Tensor m4 Lind19 (Tensor m5 Uind9 (Tensor m6 Lind9 (Tensor m7 Uind3 (Tensor m8 Lind3 a)))))) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 m3 m4 m5 m6 m7 m8 a 
    actOnU19 f = fmap (fmap f)

    actOnL20 :: (Tensor n2 Lind20 (Tensor n3 Uind19 (Tensor n4 Lind19 (Tensor n5 Uind9 (Tensor n6 Lind9 (Tensor n7 Uind3 (Tensor n8 Lind3 a)))))) -> Tensor m2 Lind20 (Tensor m3 Uind19 (Tensor m4 Lind19 (Tensor m5 Uind9 (Tensor m6 Lind9 (Tensor m7 Uind3 (Tensor m8 Lind3 a))))))) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 m2 m3 m4 m5 m6 m7 m8 a
    actOnL20 f = fmap f

    actOnU20 :: (Tensor n1 Uind20 (Tensor n2 Lind20 (Tensor n3 Uind19 (Tensor n4 Lind19 (Tensor n5 Uind9 (Tensor n6 Lind9 (Tensor n7 Uind3 (Tensor n8 Lind3 a))))))) -> Tensor m1 Uind20 (Tensor m2 Lind20 (Tensor m3 Uind19 (Tensor m4 Lind19 (Tensor m5 Uind9 (Tensor m6 Lind9 (Tensor m7 Uind3 (Tensor m8 Lind3 a)))))))) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 m1 m2 m3 m4 m5 m6 m7 m8 a 
    actOnU20 f = f

    

    tensorAdd8 :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational 
    tensorAdd8 = tensorAdd (tensorAdd  (tensorAdd (tensorAdd (tensorAdd (tensorAdd (tensorAdd (tensorAdd  (+))))))))

    tensorAddWith8 :: (Eq a) => (a -> a -> a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a 
    tensorAddWith8 f = tensorAdd (tensorAdd  (tensorAdd (tensorAdd (tensorAdd (tensorAdd (tensorAdd (tensorAdd  f)))))))
    
    tensorSub8 :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational 
    tensorSub8 t1 t2 = tensorAdd8 t1 $ actOnScalar ((*) (-1)) t2 

    tensorSubWith8 :: (Eq a) => (a -> a -> a) -> (Rational -> a -> a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a 
    tensorSubWith8 f g t1 t2 = tensorAddWith8 f t1 $ actOnScalar (g (-1)) t2 

    tensorSMult :: Rational -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational
    tensorSMult s t = actOnScalar ((*) s) t

    tensorSMultWith :: Rational -> (Rational -> a -> a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a 
    tensorSMultWith s f t = actOnScalar (f s) t

    tensorTransU20 ::  (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational
    tensorTransU20 inds = actOnU20 (tensorTrans (\t1 t2 -> tensorAdd (tensorAdd (tensorAdd (tensorAdd (tensorAdd (tensorAdd  (tensorAdd (+))))))) t1 t2) inds)

    tensorTransL20 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational 
    tensorTransL20 inds = actOnL20 (tensorTrans (\t1 t2 -> tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd  (+)))))) t1 t2) inds)

    tensorTransU19 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational
    tensorTransU19 inds = actOnU19 (tensorTrans  (\t1 t2 -> tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd (+))))) t1 t2) inds)

    tensorTransL19 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational
    tensorTransL19 inds = actOnL19 (tensorTrans  (\t1 t2 -> tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd (+)))) t1 t2) inds)

    tensorTransU9 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational 
    tensorTransU9 inds = actOnU9 (tensorTrans  (\t1 t2 -> tensorAdd  (tensorAdd  (tensorAdd (+))) t1 t2) inds)

    tensorTransL9 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational 
    tensorTransL9 inds = actOnL9 (tensorTrans  (\t1 t2 -> tensorAdd  (tensorAdd  (+)) t1 t2) inds)

    tensorTransU3 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational 
    tensorTransU3 inds = actOnU3 (tensorTrans  (\t1 t2 -> tensorAdd (+) t1 t2) inds)

    tensorTransL3 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational 
    tensorTransL3 inds = actOnL3 (tensorTrans  (+) inds)

    tensorTransWithU20 :: (Eq a) => (Int,Int) -> (a -> a -> a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorTransWithU20 inds f = actOnU20 (tensorTrans (\t1 t2 -> tensorAdd (tensorAdd (tensorAdd (tensorAdd (tensorAdd (tensorAdd  (tensorAdd f)))))) t1 t2) inds)

    tensorTransWithL20 :: (Eq a) => (Int,Int) -> (a -> a -> a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a 
    tensorTransWithL20 inds f = actOnL20 (tensorTrans (\t1 t2 -> tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd  f))))) t1 t2) inds)

    tensorTransWithU19 :: (Eq a) => (Int,Int) -> (a -> a -> a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorTransWithU19 inds f = actOnU19 (tensorTrans  (\t1 t2 -> tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd f)))) t1 t2) inds)

    tensorTransWithL19 :: (Eq a) => (Int,Int) -> (a -> a -> a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorTransWithL19 inds f = actOnL19 (tensorTrans  (\t1 t2 -> tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd f))) t1 t2) inds)

    tensorTransWithU9 :: (Eq a) => (Int,Int) -> (a -> a -> a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a 
    tensorTransWithU9 inds f = actOnU9 (tensorTrans  (\t1 t2 -> tensorAdd  (tensorAdd  (tensorAdd f)) t1 t2) inds)

    tensorTransWithL9 :: (Eq a) => (Int,Int) -> (a -> a -> a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a 
    tensorTransWithL9 inds f = actOnL9 (tensorTrans  (\t1 t2 -> tensorAdd  (tensorAdd  f) t1 t2) inds)

    tensorTransWithU3 :: (Eq a) => (Int,Int) -> (a -> a -> a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a 
    tensorTransWithU3 inds f = actOnU3 (tensorTrans  (\t1 t2 -> tensorAdd f t1 t2) inds)

    tensorTransWithL3 :: (Eq a) => (Int,Int) -> (a -> a -> a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a 
    tensorTransWithL3 inds f = actOnL3 (tensorTrans  f inds)

    

    tensorContr20 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 (n1-1) (n2-1) n3 n4 n5 n6 n7 n8 Rational
    tensorContr20 inds = actOnU20 (tensorContr  indValU20 indValL20 addF inds)
                where
                    addF = tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd  (+))))))

    tensorContr19 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 n1 n2 (n3-1) (n4-1) n5 n6 n7 n8 Rational 
    tensorContr19 inds = actOnU19 (tensorContr  indValU19 indValL19 addF inds)
                where
                    addF = \t1 t2 -> tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd (+)))) t1 t2

    tensorContr9 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 n1 n2 n3 n4 (n5-1) (n6-1) n7 n8 Rational
    tensorContr9 inds = actOnU9 (tensorContr  indValU9 indValL9 addF inds)
                where
                    addF = \t1 t2 -> tensorAdd  (tensorAdd  (+)) t1 t2
        
    tensorContr3 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 n1 n2 n3 n4 n5 n6 (n7-1) (n8-1) Rational 
    tensorContr3 inds = actOnU3 (tensorContr  indValU3 indValL3 (+) inds)


    tensorContrWith20 :: (Eq a) => (Int,Int) -> (a -> a -> a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 (n1-1) (n2-1) n3 n4 n5 n6 n7 n8 a
    tensorContrWith20 inds f = actOnU20 (tensorContr  indValU20 indValL20 addF inds)
                where
                    addF = tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd  f)))))

    tensorContrWith19 :: (Eq a) => (Int,Int) -> (a -> a -> a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 (n3-1) (n4-1) n5 n6 n7 n8 a
    tensorContrWith19 inds f = actOnU19 (tensorContr  indValU19 indValL19 addF inds)
                where
                    addF = \t1 t2 -> tensorAdd  (tensorAdd  (tensorAdd  (tensorAdd f))) t1 t2

    tensorContrWith9 :: (Eq a) => (Int,Int) -> (a -> a -> a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 (n5-1) (n6-1) n7 n8 a
    tensorContrWith9 inds f = actOnU9 (tensorContr  indValU9 indValL9 addF inds)
                where
                    addF = \t1 t2 -> tensorAdd  (tensorAdd  f) t1 t2
        
    tensorContrWith3 :: (Eq a) => (Int,Int) -> (a -> a -> a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 n6 (n7-1) (n8-1) a 
    tensorContrWith3 inds f = actOnU3 (tensorContr  indValU3 indValL3 f inds)
                


    tensorProd8 :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 m1 m2 m3 m4 m5 m6 m7 m8 Rational -> Tensor8 (n1+m1) (n2+m2) (n3+m3) (n4+m4) (n5+m5) (n6+m6) (n7+m7) (n8+m8) Rational
    tensorProd8 = tensorProd (tensorProd (tensorProd (tensorProd (tensorProd (tensorProd (tensorProd (tensorProd (*))))))))

    tensorProdWith8 :: (a -> b -> c) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 m1 m2 m3 m4 m5 m6 m7 m8 b -> Tensor8 (n1+m1) (n2+m2) (n3+m3) (n4+m4) (n5+m5) (n6+m6) (n7+m7) (n8+m8) c
    tensorProdWith8 f = tensorProd (tensorProd (tensorProd (tensorProd (tensorProd (tensorProd (tensorProd (tensorProd f)))))))

    type IndTuple n1 n2 n3 n4 n5 n6 n7 n8 = (IndList n1 Uind20, IndList n2 Lind20 , IndList n3 Uind19, IndList n4 Lind19, IndList n5 Uind9, IndList n6 Lind9, IndList n7 Uind3, IndList n8 Lind3)

    mkTens8 :: (IndTuple n1 n2 n3 n4 n5 n6 n7 n8, a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a
    mkTens8 ((i1,i2,i3,i4,i5,i6,i7,i8),s) = mkTens (i1, mkTens (i2, mkTens (i3, mkTens (i4, mkTens (i5, mkTens (i6, mkTens (i7, mkTens (i8, s))))))))

    insertOrAdd8 :: (IndTuple n1 n2 n3 n4 n5 n6 n7 n8, Rational) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational
    insertOrAdd8 inds t = tensorAdd8 (mkTens8 inds) t

    insertOrAddWith8 :: (Eq a) => (a -> a -> a) -> (IndTuple n1 n2 n3 n4 n5 n6 n7 n8, a) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a
    insertOrAddWith8 f inds t = tensorAddWith8 f (mkTens8 inds) t

    fromListT8 :: [(IndTuple n1 n2 n3 n4 n5 n6 n7 n8, Rational)] -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational
    fromListT8 l = foldr insertOrAdd8 (mkTens8 $ head p) q 
        where
            (p,q) = splitAt 1 l

    fromListTWith8 :: (Eq a) => (a -> a -> a) -> [(IndTuple n1 n2 n3 n4 n5 n6 n7 n8, a)] -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a
    fromListTWith8 f l = foldr (insertOrAddWith8 f) (mkTens8 $ head p) q 
        where
            (p,q) = splitAt 1 l

    toListT8 :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> [(IndTuple n1 n2 n3 n4 n5 n6 n7 n8, a)]
    toListT8 t = concat $ map (\(x,y) -> appendT7 x $ toListT y ) $ concat $ map (\(x,y) -> appendT6 x $ toListT y ) $ concat $ map (\(x,y) -> appendT5 x $ toListT y ) $ concat $ map (\(x,y) -> appendT4 x $ toListT y ) $ concat $ map (\(x,y) -> appendT3 x $ toListT y ) $ concat $ map (\(x,y) -> appendT2 x $ toListT y ) $ concat $ map (\(x,y) -> appendT1 x $ toListT y ) $ toListT t
            where
                appendT1 = \i l -> map (\(x,y) -> ((i,x),y)) l
                appendT2 = \(i1,i2) l -> map (\(x,y) -> ((i1,i2,x),y)) l
                appendT3 = \(i1,i2,i3) l -> map (\(x,y) -> ((i1,i2,i3,x),y)) l
                appendT4 = \(i1,i2,i3,i4) l -> map (\(x,y) -> ((i1,i2,i3,i4,x),y)) l
                appendT5 = \(i1,i2,i3,i4,i5) l -> map (\(x,y) -> ((i1,i2,i3,i4,i5,x),y)) l
                appendT6 = \(i1,i2,i3,i4,i5,i6) l -> map (\(x,y) -> ((i1,i2,i3,i4,i5,i6,x),y)) l
                appendT7 = \(i1,i2,i3,i4,i5,i6,i7) l -> map (\(x,y) -> ((i1,i2,i3,i4,i5,i6,i7,x),y)) l

    toListShow8 :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a -> [([Int],a)]
    toListShow8 t = map (\(x,y) -> (showInd x, y)) l
            where
                l = toListT8 t 
                showInd (i1,i2,i3,i4,i5,i6,i7,i8) = (map indValU20 $ toList i1) ++ (map indValL20 $ toList i2) ++ (map indValU19 $ toList i3) ++ (map indValL19 $ toList i4) ++ (map indValU9 $ toList i5) ++ (map indValL9 $ toList i6) ++ (map indValU3 $ toList i7) ++ (map indValL3 $ toList i8) 
    
    toListShowVar :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 VarMap -> [([Int], [(Int, Rational)])]
    toListShowVar t = filter (\(_,c) -> c /= []) $ map (\(a,b) -> (a, filter (\(_,b) -> b/= 0) $ I.assocs b)) l
            where
                l = toListShow8 t 

    toMatList :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 VarMap -> [[(Int, Rational)]]
    toMatList t = map snd $ toListShowVar t

    toSparseMat :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 VarMap -> (Int,Int,[((Int, Int), Rational)])
    toSparseMat t = (n,m,l')
            where
                l = toMatList t 
                l' = concat $ zipWith (\r z -> map (\(x,y) -> ((z, x), y)) r) l [1..]
                n = length l 
                m = maximum $ map fst $ concat l 

    normalize :: [(Int,Rational)] -> [(Int,Rational)]
    normalize [] = [] 
    normalize ((a,b) : xs) = (a,1) : (map (\(x,y) -> (x,y / b)) xs)

    toSparseMatRed :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 VarMap -> (Int,Int,[((Int, Int), Rational)])
    toSparseMatRed t = (n,m,l')
            where
                l = nub $ map normalize $ toMatList t 
                l' = concat $ zipWith (\r z -> map (\(x,y) -> ((z, x), y)) r) l [1..]
                n = length l 
                m = maximum $ map fst $ concat l 

    toMatrix' :: [[(Int, Rational)]] -> Mat.MatrixXd 
    toMatrix' l = Sparse.toMatrix $ Sparse.fromList n m l''
                    where
                        l' = concat $ zipWith (\r z -> map (\(x,y) -> (z, x, y)) r) l [1..]
                        n = length l 
                        l'' = map (\(a,b,c) -> (a-1, b-1, fromRational c)) l'
                        m = maximum $ map fst $ concat l 

    toMatrix'' :: [(Int,Int,Rational)] -> Mat.MatrixXd
    toMatrix'' l = Sparse.toMatrix $ Sparse.fromList n m l'
            where
                n = maximum $ map (\(a,b,c) -> a) l 
                m = maximum $ map (\(a,b,c) -> b) l 
                l' = map (\(a,b,c) -> (a-1, b-1, fromRational c)) l

    toMatrix :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 VarMap -> Mat.MatrixXd
    toMatrix t = toMatrix' $ toMatList t

    toMatrix2 :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 VarMap -> Tensor8 m1 m2 m3 m4 m5 m6 m7 m8 VarMap -> Mat.MatrixXd 
    toMatrix2 t1 t2 = toMatrix' l 
            where
                l = toMatList t1 ++ toMatList t2

    toMatrix3 :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 VarMap -> Tensor8 m1 m2 m3 m4 m5 m6 m7 m8 VarMap -> Tensor8 r1 r2 r3 r4 r5 r6 r7 r8 VarMap -> Mat.MatrixXd 
    toMatrix3 t1 t2 t3 = toMatrix' l 
            where
                l = toMatList t1 ++ toMatList t2 ++ toMatList t3

    toMatrix4 :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 VarMap -> Tensor8 m1 m2 m3 m4 m5 m6 m7 m8 VarMap -> Tensor8 r1 r2 r3 r4 r5 r6 r7 r8 VarMap -> Tensor8 s1 s2 s3 s4 s5 s6 s7 s8 VarMap -> Mat.MatrixXd 
    toMatrix4 t1 t2 t3 t4 = toMatrix' l 
            where
                l = toMatList t1 ++ toMatList t2 ++ toMatList t3 ++ toMatList t4

    getTensorRank :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 VarMap -> Int 
    getTensorRank t = Sol.rank Sol.FullPivLU $ toMatrix t

    getTensorRank2 :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 VarMap -> Tensor8 m1 m2 m3 m4 m5 m6 m7 m8 VarMap -> Int 
    getTensorRank2 t1 t2 = Sol.rank Sol.FullPivLU $ toMatrix2 t1 t2

    getTensorRank3 :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 VarMap -> Tensor8 m1 m2 m3 m4 m5 m6 m7 m8 VarMap -> Tensor8 r1 r2 r3 r4 r5 r6 r7 r8 VarMap -> Int 
    getTensorRank3 t1 t2 t3 = Sol.rank Sol.FullPivLU $ toMatrix3 t1 t2 t3

    getTensorRank4 :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 VarMap -> Tensor8 m1 m2 m3 m4 m5 m6 m7 m8 VarMap -> Tensor8 r1 r2 r3 r4 r5 r6 r7 r8 VarMap -> Tensor8 s1 s2 s3 s4 s5 s6 s7 s8 VarMap -> Int 
    getTensorRank4 t1 t2 t3 t4 = Sol.rank Sol.FullPivLU $ toMatrix4 t1 t2 t3 t4 


    type VarMap = I.IntMap Rational   

    shiftVarLabels :: Int -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 VarMap -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 VarMap 
    shiftVarLabels s t = actOnScalar f t 
                where
                    f i = I.mapKeys ((+) s) i

    multVarsMap :: Rational -> VarMap -> VarMap 
    multVarsMap s = I.map ((*) s)

    combineVarsMap :: VarMap -> VarMap -> VarMap 
    combineVarsMap s1 s2 = I.unionWith (*) s1 s2 

    addVarsMap :: VarMap -> VarMap -> VarMap 
    addVarsMap = I.unionWith (+) 

    area18IndList :: [[IndTuple 3 0 0 0 3 0 0 0]]
    area18IndList = map (map (\[(a,i),(b,j),(c,k)] -> (Append (Uind20 a) $ Append (Uind20 b) $ singletonInd (Uind20 c), Empty, Empty, Empty, Append (Uind9 i) $ Append (Uind9 j) $ singletonInd (Uind9 k), Empty, Empty, Empty))) l
            where
                l = [ nub $ permutations [(a,i),(b,j),(c,k)] | a <- [1..21], b <- [a..21], c <- [b..21], i <- [1..10], j <- [1..10], k <- [1..10], not (a==b && i>j), not (b==c && j>k)]

    area18TensList :: [VarMap] -> [(IndTuple 3 0 0 0 3 0 0 0, VarMap)]
    area18TensList vars = concat $ zipWith (\x y -> zip x (repeat y)) area18IndList vars 


    --now the basic tensors

    delta20 :: Tensor8 1 1 0 0 0 0 0 0 Rational 
    delta20 = fromListT8 $ zip [(singletonInd (Uind20 i),singletonInd (Lind20 i), Empty, Empty, Empty, Empty, Empty, Empty) | i <- [0..20]] (repeat 1)

    delta19 :: Tensor8 0 0 1 1 0 0 0 0 Rational
    delta19 = fromListT8 $ zip [(Empty, Empty, singletonInd (Uind19 i),singletonInd (Lind19 i), Empty, Empty, Empty, Empty) | i <- [0..19]] (repeat 1)

    delta9 :: Tensor8 0 0 0 0 1 1 0 0 Rational
    delta9 = fromListT8 $ zip [(Empty, Empty, Empty, Empty, singletonInd (Uind9 i),singletonInd (Lind9 i), Empty, Empty) | i <- [0..9]] (repeat 1)

    delta3 :: Tensor8 0 0 0 0 0 0 1 1 Rational
    delta3 = fromListT8 $ zip [(Empty, Empty, Empty, Empty, Empty, Empty, singletonInd (Uind3 i),singletonInd (Lind3 i)) | i <- [0..3]] (repeat 1)

    trianMapI2 :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) 
    trianMapI2 = M.fromList $ zip [ Append (Lind3 a) $ singletonInd $ Lind3 b | a <- [0..3], b <- [a..3] ] $ map (singletonInd . Uind9) [0..]

    trianMapJ2 :: M.Map (IndList 2 Uind3) (IndList 1 Lind9) 
    trianMapJ2 = M.fromList $ zip [ Append (Uind3 a) $ singletonInd $ Uind3 b | a <- [0..3], b <- [a..3] ] $ map (singletonInd . Lind9) [0..]

    trianMapI3 :: M.Map (IndList 3 Lind3) (IndList 1 Uind19) 
    trianMapI3 = M.fromList $ zip [ Append (Lind3 a) $ Append (Lind3 b) $ singletonInd $ Lind3 c | a <- [0..3], b <- [a..3], c <- [b..3] ] $ map (singletonInd . Uind19) [0..]

    trianMapJ3 :: M.Map (IndList 3 Uind3) (IndList 1 Lind19) 
    trianMapJ3 = M.fromList $ zip [ Append (Uind3 a) $ Append (Uind3 b) $ singletonInd $ Uind3 c | a <- [0..3], b <- [a..3], c <- [b..3] ] $ map (singletonInd . Lind19) [0..]

    trianMapAreaI :: M.Map (IndList 4 Lind3) (IndList 1 Uind20)
    trianMapAreaI = M.fromList $ zip [ Append (Lind3 a) $ Append (Lind3 b) $ Append (Lind3 c) $ singletonInd $ Lind3 d | a <- [0..2], b <- [a+1..3], c <- [a..2], d <- [c+1..3], not $ a == c && b > d ] $ map (singletonInd . Uind20) [0..]
    
    trianMapAreaJ :: M.Map (IndList 4 Uind3) (IndList 1 Lind20)
    trianMapAreaJ = M.fromList $ zip [ Append (Uind3 a) $ Append (Uind3 b) $ Append (Uind3 c) $ singletonInd $ Uind3 d | a <- [0..2], b <- [a+1..3], c <- [a..2], d <- [c+1..3], not $ a == c && b > d ] $ map (singletonInd . Lind20) [0..]
    
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

    interI2 :: M.Map (IndList 2 Lind3) (IndList 1 Uind9)  -> Tensor8 0 0 0 0 1 0 0 2 Rational
    interI2 trian2 = fromListT8 $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [ (Empty, Empty, Empty, Empty, (singletonInd $ Uind9 a), Empty, Empty, (Append (Lind3 b) $ singletonInd $ Lind3 c)) | a <- [0..9], b <- [0..3], c <- [0..3]]
                f (_, _, _, _, ind1, _, _, ind2)
                    | ind1 == ((M.!) trian2 $ sortInd ind2 ) = 1 
                    | otherwise = 0 

    aSymI2 :: M.Map (IndList 2 Lind3) (IndList 1 Uind9)  -> Tensor8 0 0 0 0 1 0 0 2 Rational
    aSymI2 trian2 = fromListT8 $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [ (Empty, Empty, Empty, Empty, (singletonInd $ Uind9 a), Empty, Empty, (Append (Lind3 b) $ singletonInd $ Lind3 c)) | a <- [0..9], b <- [0..3], c <- [0..3]]
                f (_, _, _, _, ind1, _, _, ind2@(Append x (Append y Empty)))
                    | x == y = 0
                    | ind1 == (M.!) trian2 sortI = sign
                    | otherwise = 0 
                  where sortI = sortInd ind2
                        sign = if sortI == ind2 then 1 else -1

    interJ2 :: M.Map (IndList 2 Uind3) (IndList 1 Lind9)  -> Tensor8 0 0 0 0 0 1 2 0 Rational
    interJ2 trian2 = fromListT8 $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [ (Empty, Empty, Empty, Empty, Empty, (singletonInd $ Lind9 a), (Append (Uind3 b) $ singletonInd $ Uind3 c), Empty) | a <- [0..9], b <- [0..3], c <- [0..3]]
                f (_, _, _, _, _, ind1, ind2, _)
                    | ind1 == ((M.!) trian2 $ sortInd ind2 ) = jMult2 ind2  
                    | otherwise = 0 

    interI3 :: M.Map (IndList 3 Lind3) (IndList 1 Uind19) -> Tensor8 0 0 1 0 0 0 0 3 Rational
    interI3 trian3 = fromListT8 $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [(Empty, Empty, (singletonInd $ Uind19 a), Empty, Empty, Empty, Empty, (Append (Lind3 b) $ Append (Lind3 c) $ singletonInd $ Lind3 d)) | a <- [0..19], b <- [0..3], c <- [0..3], d <- [0..3]]
                f (_, _, ind1, _, _, _, _, ind2)
                    | ind1 == ((M.!) trian3 $ sortInd ind2) = 1 
                    | otherwise = 0 

    interJ3 :: M.Map (IndList 3 Uind3) (IndList 1 Lind19) -> Tensor8 0 0 0 1 0 0 3 0 Rational
    interJ3 trian3 = fromListT8 $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [ (Empty, Empty, Empty, (singletonInd $ Lind19 a), Empty, Empty, (Append (Uind3 b) $ Append (Uind3 c) $ singletonInd $ Uind3 d), Empty) | a <- [0..19], b <- [0..3], c <- [0..3], d <- [0..3]]
                f (_, _, _, ind1, _, _, ind2, _)
                    | ind1 == ((M.!) trian3 $ sortInd ind2) = jMult3 ind2 
                    | otherwise = 0 

    interIArea :: M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> Tensor8 1 0 0 0 0 0 0 4  Rational
    interIArea trianArea = fromListT8 $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [ ((singletonInd $ Uind20 a), Empty, Empty, Empty, Empty, Empty, Empty, (Append (Lind3 b) $ Append (Lind3 c) $ Append (Lind3 d) $ singletonInd $ Lind3 e)) | a <- [0..20], b <- [0..3], c <- [0..3], d <- [0..3], e <- [0..3], not (b == c || d == e)]
                f (ind1, _, _, _, _, _, _, ind2)
                    | ind1 == ((M.!) trianArea indArea) = s
                    | otherwise = 0
                        where
                            (indArea, s) = canonicalizeArea ind2 

    interJArea :: M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 0 1 0 0 0 0 4 0 Rational
    interJArea trianArea = fromListT8 $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [  (Empty, (singletonInd $ Lind20 a), Empty, Empty, Empty, Empty, (Append (Uind3 b) $ Append (Uind3 c) $ Append (Uind3 d) $ singletonInd $ Uind3 e), Empty) | a <- [0..20], b <- [0..3], c <- [0..3], d <- [0..3], e <- [0..3], not (b == c || d == e)]
                f (_, ind1, _, _, _, _, ind2, _)
                    | ind1 == ((M.!) trianArea indArea) = s * (jMultArea indArea)
                    | otherwise = 0
                        where
                            (indArea, s) = canonicalizeArea ind2 

    interMetric :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> Tensor8 0 0 0 0 1 1 1 1 Rational
    interMetric trian2I trian2J = actOnScalar ((*) (-2)) $ tensorContr3 (0,0) prod
            where
                t1 = interI2 trian2I 
                t2 = interJ2 trian2J 
                prod = tensorProd8 t1 t2 

    interArea :: M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 1 1 0 0 0 0 1 1 Rational
    interArea trianAreaI trianAreaJ =  actOnScalar ((*) (-4)) $ tensorContr3 (1,1) $ tensorContr3 (2,2) $ tensorContr3 (3,3) prod
            where
                t1 = interIArea trianAreaI 
                t2 = interJArea trianAreaJ 
                prod = tensorProd8 t1 t2 

    interEqn2 :: M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 1 1 0 0 0 0 2 2 Rational
    interEqn2 trianAreaI trianAreaJ = tensorSub8 int1 int2
            where
                intArea = interArea trianAreaI trianAreaJ
                int1 = tensorProd8 intArea delta3
                int2 = tensorProd8 (tensorTransL3 (0,1) $ tensorProd8 delta3 delta3 ) delta20

    interEqn3 :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 1 1 0 0 1 1 1 1 Rational
    interEqn3 trian2I trian2J trianAreaI trianAreaJ = intTotal
            where
                int1 = tensorProd8 (interArea trianAreaI trianAreaJ) delta9
                int2 = tensorProd8 (interMetric trian2I trian2J) delta20
                intTotal = tensorAdd8 int1 int2

    interEqn4 :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 1 1 0 0 0 1 3 1 Rational
    interEqn4 map1Metric map2Metric map1Area map2Area = tensorSub8 block1 block2 
            where
                intArea = interArea map1Area map2Area
                sym = interJ2 map2Metric 
                block1' = tensorProd8 sym intArea
                block1 = tensorAdd8 block1' $ tensorTransU3 (1,2) block1' 
                block2 = tensorProd8 delta20 $ tensorProd8 delta3 sym

    interIntCond ::  M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 2 1 0 0 0 0 0 4 Rational
    interIntCond map1Metric map2Metric map1Area map2Area = tensorSAdd8 tens tensTrans  
            where
                intA = inetrArea map1Area map2Area 
                intI = interIArea map1Area 
                tens' = tensorProd8 intA intI 
                tens = tensorContr3 (0,1) tens' 
                tensTrans = tensorTransL3 (0,1) tens 

    flatArea :: Tensor8 0 1 0 0 0 0 0 0 Rational
    flatArea = fromListT8 $ map (\(i,v) -> ( (Empty, (singletonInd $ Lind20 i), Empty, Empty, Empty, Empty, Empty, Empty), v)) [(0,-1),(5,-1),(6,-1),(9,1),(11,-1),(12,-1),(15,1),(18,1),(20,1)]

    eta :: Tensor8 0 0 0 0 0 0 0 2 Rational
    eta =  fromListT8 l 
                where
                    l = map (\(x,y,z) -> ((Empty,Empty,Empty,Empty,Empty,Empty,Empty,Append (Lind3 x) $ Append (Lind3 y) Empty),z)) [(0,0,-1),(1,1,1),(2,2,1),(3,3,1)]

    invEta :: Tensor8 0 0 0 0 0 0 2 0 Rational 
    invEta =  fromListT8 l 
                where
                    l = map (\(x,y,z) -> ((Empty,Empty,Empty,Empty,Empty,Empty,Append (Uind3 x) $ Append (Uind3 y) Empty,Empty),z)) [(0,0,-1),(1,1,1),(2,2,1),(3,3,1)]

    flatInter :: M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 0 1 0 0 0 0 1 1 Rational
    flatInter trianAreaI trianAreaJ = tensorContr20 (0,1) prod
            where
                intArea = interArea trianAreaI trianAreaJ 
                prod = tensorProd8 intArea flatArea 

    intAIB :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 1 2 0 0 1 1 2 2 Rational
    intAIB map1Metric map2Metric map1Area map2Area = tensorSub8 tens tensTrans  
            where
                intArea = interArea map1Area map2Area
                intMetric = interMetric map1Metric map2Metric
                flatIntA = flatInter map1Area map2Area 
                int3 = interEqn3 map1Metric map2Metric map1Area map2Area
                block1 = tensorProd8 delta20 $ tensorProd8 delta20 $ tensorProd8 delta9 delta3 
                block2 = tensorProd8 intArea $ tensorProd8 delta20 delta9
                block3 = tensorProd8 delta20 int3 
                totalBlock = tensorAdd8 block1 $ tensorAdd8 block2 block3 
                tens = tensorContr20 (0,2) $ tensorProd8 totalBlock flatIntA 
                tensTrans = tensorTransU3 (0,1) $ tensorTransL3 (0,1) tens 

    triangleMap3P' :: Int -> M.Map [Int] Int
    triangleMap3P' i = M.fromList $ zip j k
                    where
                        j = [ [a,b,c] | a <- [1..i], b <- [a..i], c <- [b..i] ]
                        k = [1..]

    ansatzAIBJCK' :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 3 3 0 0 4 3 0 0 Rational
    ansatzAIBJCK' map1Metric map2Metric map1Area map2Area = totalBlock3
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        int3 = interEqn3 map1Metric map2Metric map1Area map2Area
                        antiSym = aSymI2 map1Metric
                        aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                        int3Contr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 int3 aSym
                        block1 = tensorProd8 int3Contr $ tensorProd8 delta20   $ tensorProd8 delta20   $ tensorProd8 delta9 delta9
                        block2 = tensorTransU20 (0,2) $ tensorTransU9 (0,3) block1 
                        block3 = tensorTransU20 (0,1) $ tensorTransU9 (0,2) block1 
                        totalBlock1 = tensorAdd8 block1   $ tensorAdd8 block2 block3 
                        totalBlock2 = tensorTransL20 (0,2)   $ tensorTransL9 (0,2) totalBlock1
                        totalBlock3 = tensorTransL20 (0,1)   $ tensorTransL9 (0,1) totalBlock1
                        totalBlock4 = tensorTransL20 (1,2)  $ tensorTransL9 (1,2) totalBlock1
                        totalBlock5 = tensorTransL20 (1,2)  $ tensorTransL9 (1,2) totalBlock3
                        totalBlock6 = tensorTransL20 (0,2)  $ tensorTransL9 (0,2) totalBlock3
                        tens = tensorAdd8 totalBlock1 $ tensorAdd8 totalBlock2 $ tensorAdd8 totalBlock3 $ tensorAdd8 totalBlock4 $ tensorAdd8 totalBlock5 totalBlock6

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

    ansatzA :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 1 0 0 0 0 0 0 0 VarMap -> Tensor8 1 0 0 0 1 0 0 0 VarMap
    ansatzA map1Metric map2Metric map1Area map2Area ansatzTens = block1 
            where
                intA = interArea map1Area map2Area
                antiSym = aSymI2 map1Metric
                aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                intAContr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 intA aSym
                block1 = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens intAContr 

    ansatzAI :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 1 0 0 0 1 0 0 0 VarMap -> Tensor8 1 0 0 0 2 0 0 0 VarMap
    ansatzAI map1Metric map2Metric map1Area map2Area ansatzTens = block1
            where
                int3 = interEqn3 map1Metric map2Metric map1Area map2Area
                antiSym = aSymI2 map1Metric
                aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                int3Contr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 int3 aSym
                block1 = tensorContrWith9 (0,0) (I.unionWith (+)) $ tensorContrWith20 (0,0) (I.unionWith (+)) $ tensorProdWith8 (\m s -> multVarsMap s m) ansatzTens int3Contr 

    ansatzAB :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 2 0 0 0 0 0 0 0 VarMap -> Tensor8 2 0 0 0 1 0 0 0 VarMap
    ansatzAB map1Metric map2Metric map1Area map2Area ansatzTens = tensorAddWith8 (addVarsMap) block1 block2
            where
                intA = interArea map1Area map2Area
                antiSym = aSymI2 map1Metric
                aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                intAContr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 intA aSym
                block1 = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens intAContr 
                block2 = tensorTransWithU20 (0,1) addVarsMap block1 
    
    ansatzAaBb :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 2 0 0 0 0 0 2 0 VarMap -> Tensor8 2 0 0 0 1 0 2 0 VarMap
    ansatzAaBb map1Metric map2Metric map1Area map2Area ansatzTens = tensorAddWith8 (addVarsMap) block1 block2
            where
                int2 = interEqn2 map1Area map2Area
                antiSym = aSymI2 map1Metric
                aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                int2Contr = tensorContr3 (1,0) $ tensorContr3 (0,2) $ tensorProd8 int2 aSym
                block1 = tensorContrWith3 (0,0) addVarsMap $ tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens int2Contr 
                block2 = tensorTransWithU3 (0,1) addVarsMap $ tensorTransWithU20 (0,1) addVarsMap block1 

    ansatzAIB :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 2 0 0 0 1 0 0 0 VarMap -> Tensor8 2 0 0 0 2 0 0 0 VarMap
    ansatzAIB map1Metric map2Metric map1Area map2Area ansatzTens = tensorAddWith8 (addVarsMap) block1 block2 
            where
                int3 = interEqn3 map1Metric map2Metric map1Area map2Area
                intArea = interArea map1Area map2Area
                antiSym = aSymI2 map1Metric
                aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                intAContr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 intArea aSym
                int3Contr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 int3 aSym
                block1 = tensorTransWithU20 (0,1) addVarsMap $ tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens intAContr 
                block2 = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens int3Contr 


    ansatzAIBJ :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 2 0 0 0 2 0 0 0 VarMap -> Tensor8 2 0 0 0 3 0 0 0 VarMap
    ansatzAIBJ map1Metric map2Metric map1Area map2Area ansatzTens = tensorAddWith8 (addVarsMap) block1 block2
            where
                int3 = interEqn3 map1Metric map2Metric map1Area map2Area
                antiSym = aSymI2 map1Metric
                aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                int3Contr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 int3 aSym
                block1 = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens int3Contr 
                block2 = tensorTransWithU9 (0,1) addVarsMap $ tensorTransWithU20 (0,1) addVarsMap block1 

    ansatzABC :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 3 0 0 0 0 0 0 0 VarMap -> Tensor8 3 0 0 0 1 0 0 0 VarMap
    ansatzABC map1Metric map2Metric map1Area map2Area ansatzTens = tensorAddWith8 addVarsMap block1 $ tensorAddWith8 addVarsMap block2 block3 
            where
                intA = interArea map1Area map2Area
                antiSym = aSymI2 map1Metric
                aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                intAContr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 intA aSym
                block1 = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens intAContr 
                block2 = tensorTransWithU20 (1,2) addVarsMap block1 
                block3 = tensorTransWithU20 (0,2) addVarsMap block1

    ansatzABbCc :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 3 0 0 0 0 0 2 0 VarMap -> Tensor8 3 0 0 0 1 0 2 0 VarMap
    ansatzABbCc map1Metric map2Metric map1Area map2Area ansatzTens = tensorAddWith8 addVarsMap block1 $ tensorAddWith8 addVarsMap block2 block3 
            where
                intA = interArea map1Area map2Area
                int2 = interEqn2 map1Area map2Area
                antiSym = aSymI2 map1Metric
                aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                intAContr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 intA aSym
                int2Contr = tensorContr3 (1,0) $ tensorContr3 (0,2) $ tensorProd8 int2 aSym
                block1 = tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 multVarsMap intAContr ansatzTens
                block2 = tensorContrWith3 (0,0) addVarsMap $ tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens int2Contr 
                block3 = tensorTransWithU3 (0,1) addVarsMap $ tensorTransWithU20 (1,2) addVarsMap block2 

    ansatzABCI :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 3 0 0 0 1 0 0 0 VarMap -> Tensor8 3 0 0 0 2 0 0 0 VarMap
    ansatzABCI map1Metric map2Metric map1Area map2Area ansatzTens = tensorAddWith8 addVarsMap block1 $ tensorAddWith8 addVarsMap block2 block3 
            where
                intA = interArea map1Area map2Area
                int3 = interEqn3 map1Metric map2Metric map1Area map2Area
                antiSym = aSymI2 map1Metric
                aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                intAContr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 intA aSym
                int3Contr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 int3 aSym
                block1 = tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 multVarsMap intAContr ansatzTens
                block2 = tensorTransWithU20 (0,1) addVarsMap block1
                block3 = tensorTransWithU9 (0,1) addVarsMap $ tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (2,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens int3Contr 

    ansatzApBqCI :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 3 0 0 0 1 0 2 0 VarMap -> Tensor8 3 0 0 0 2 0 2 0 VarMap
    ansatzApBqCI map1Metric map2Metric map1Area map2Area ansatzTens = tensorAddWith8 addVarsMap block1 $ tensorAddWith8 addVarsMap block2 block3 
            where
                int2 = interEqn2 map1Area map2Area
                int3 = interEqn3 map1Metric map2Metric map1Area map2Area
                antiSym = aSymI2 map1Metric
                aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                int2Contr = tensorContr3 (1,0) $ tensorContr3 (0,2) $ tensorProd8 int2 aSym
                int3Contr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 int3 aSym
                block1 = tensorTransWithU9 (0,1) addVarsMap $ tensorContrWith3 (1,0) addVarsMap $ tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 multVarsMap int2Contr ansatzTens
                block2 = tensorTransWithU3 (0,1) addVarsMap $ tensorTransWithU20 (0,1) addVarsMap block1
                block3 = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (2,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens int3Contr 

    ansatzABICJ :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 3 0 0 0 2 0 0 0 VarMap -> Tensor8 3 0 0 0 3 0 0 0 VarMap
    ansatzABICJ map1Metric map2Metric map1Area map2Area ansatzTens = tensorAddWith8 addVarsMap block1 $ tensorAddWith8 addVarsMap block2 block3 
            where
                intA = interArea map1Area map2Area
                int3 = interEqn3 map1Metric map2Metric map1Area map2Area
                antiSym = aSymI2 map1Metric
                aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                int3Contr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 int3 aSym
                intAContr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 intA aSym
                block1 = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens int3Contr 
                block2 = tensorTransWithU9 (0,1) addVarsMap $ tensorTransWithU20 (1,2) addVarsMap block1 
                block3 = tensorTransWithU9 (1,2) addVarsMap $ tensorTransWithU9 (0,1) addVarsMap $ tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 multVarsMap intAContr ansatzTens

    ansatzAIBJCK :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 3 0 0 0 3 0 0 0 VarMap -> Tensor8 3 0 0 0 4 0 0 0 VarMap
    ansatzAIBJCK map1Metric map2Metric map1Area map2Area ansatzTens = tensorAddWith8 addVarsMap block1 $ tensorAddWith8 addVarsMap block2 block3 
            where
                int3 = interEqn3 map1Metric map2Metric map1Area map2Area
                antiSym = aSymI2 map1Metric
                aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                int3Contr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 int3 aSym
                block1 = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens int3Contr 
                block2 = tensorTransWithU9 (1,2) addVarsMap $ tensorTransWithU20 (1,2) addVarsMap block1 
                block3 = tensorTransWithU9 (0,2) addVarsMap $ tensorTransWithU20 (0,2) addVarsMap block1

    --order 4

    ansatzABCDJ :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 4 0 0 0 1 0 0 0 VarMap -> Tensor8 4 0 0 0 2 0 0 0 VarMap
    ansatzABCDJ map1Metric map2Metric map1Area map2Area ansatzTens = tensorAddWith8 addVarsMap block1 $ tensorAddWith8 addVarsMap block2 $ tensorAddWith8 addVarsMap block3 block4  
            where
                intA = interArea map1Area map2Area
                int3 = interEqn3 map1Metric map2Metric map1Area map2Area 
                antiSym = aSymI2 map1Metric
                aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                intAContr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 intA aSym
                int3Contr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 int3 aSym
                block1 = tensorTransWithU20 (2,3) addVarsMap $ tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens intAContr 
                block2 = tensorTransWithU20 (1,2) addVarsMap block1 
                block3 = tensorTransWithU20 (0,2) addVarsMap block1
                block4 = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (3,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens int3Contr 


    ansatzABCcDd :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 4 0 0 0 0 0 2 0 VarMap -> Tensor8 4 0 0 0 1 0 2 0 VarMap
    ansatzABCcDd map1Metric map2Metric map1Area map2Area ansatzTens = tensorAddWith8 addVarsMap block1 $ tensorAddWith8 addVarsMap block2 $ tensorAddWith8 addVarsMap block3 block4 
            where
                intA = interArea map1Area map2Area
                int2 = interEqn2 map1Area map2Area
                antiSym = aSymI2 map1Metric
                aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                intAContr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 intA aSym
                int2Contr = tensorContr3 (1,0) $ tensorContr3 (0,2) $ tensorProd8 int2 aSym
                block1 = tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 multVarsMap intAContr ansatzTens
                block2 = tensorTransWithU20 (0,1) addVarsMap block1
                block3 = tensorContrWith3 (0,0) addVarsMap $ tensorContrWith20 (2,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens int2Contr 
                block4 = tensorTransWithU3 (0,1) addVarsMap $ tensorTransWithU20 (2,3) addVarsMap block3 
    
    --the equations in tensorial form

    --start with the first subgraph (#of total derivatives = 2)

    --no Prolongation 

    eqn3 :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 1 0 0 0 1 0 0 0 VarMap -> Tensor8 0 0 0 0 0 0 3 1 VarMap
    eqn3 map1Metric map2Metric map1Area map2Area ansatzTens6 = total
            where
                flatInt = flatInter map1Area map2Area 
                intJ = interJ2 map2Metric 
                flatInt1 = tensorProd8 intJ flatInt 
                flatInt2 = tensorTransU3 (0,1) flatInt1
                flatInt3 = tensorTransU3 (1,2) flatInt1
                flatInt4 = tensorTransU3 (0,2) flatInt1 
                flatInt5 = tensorTransU3 (0,1) $ tensorTransU3 (1,2) flatInt1
                flatInt6 = tensorTransU3 (0,1) $ tensorTransU3 (0,2) flatInt1 
                tens = tensorAdd8 flatInt1 $ tensorAdd8 flatInt2 $ tensorAdd8 flatInt3 $ tensorAdd8 flatInt4 $ tensorAdd8 flatInt5 flatInt6
                total = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens6 tens  

    --1 prolongation
    
    eqn1AI ::  M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 1 0 0 0 1 0 0 0 VarMap -> Tensor8 2 0 0 0 1 0 0 0 VarMap -> Tensor8 1 0 0 0 1 0 1 1 VarMap
    --eqn1AI map1Metric map2Metric map1Area map2Area ansatzTens6 ansatzTens10 = tensorAddWith8 addVarsMap block0 $ tensorAddWith8 addVarsMap block1 block2 
    eqn1AI map1Metric map2Metric map1Area map2Area ansatzTens6 ansatzTens10 = tensorAddWith8 addVarsMap block1 block2 
          where
                flatInt = flatInter map1Area map2Area 
                int3 = interEqn3 map1Metric map2Metric map1Area map2Area 
                --block0 = tensorProdWith8 (flip multVarsMap) ansatzTens6 delta3 
                block1 = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens10 flatInt
                block2 = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens6 int3 


    eqn3A :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 1 0 0 0 1 0 0 0 VarMap ->  Tensor8 2 0 0 0 1 0 0 0 VarMap -> Tensor8 1 0 0 0 0 0 3 1 VarMap
    eqn3A map1Metric map2Metric map1Area map2Area ansatzTens6 ansatzTens10 = tensorAddWith8 addVarsMap total1 total2
            where
                flatInt = flatInter map1Area map2Area 
                intA = interArea map1Area map2Area
                intJ = interJ2 map2Metric 
                flatInt1 = tensorProd8 intJ flatInt 
                flatInt2 = tensorTransU3 (0,1) flatInt1
                flatInt3 = tensorTransU3 (1,2) flatInt1
                flatInt4 = tensorTransU3 (0,2) flatInt1 
                flatInt5 = tensorTransU3 (0,1) $ tensorTransU3 (1,2) flatInt1
                flatInt6 = tensorTransU3 (0,1) $ tensorTransU3 (0,2) flatInt1 
                intA1 = tensorProd8 intJ intA 
                intA2 = tensorTransU3 (0,1) intA1
                intA3 = tensorTransU3 (1,2) intA1
                intA4 = tensorTransU3 (0,2) intA1 
                intA5 = tensorTransU3 (0,1) $ tensorTransU3 (1,2) intA1
                intA6 = tensorTransU3 (0,1) $ tensorTransU3 (0,2) intA1 
                tens1 = tensorAdd8 flatInt1 $ tensorAdd8 flatInt2 $ tensorAdd8 flatInt3 $ tensorAdd8 flatInt4 $ tensorAdd8 flatInt5 flatInt6
                tens2 = tensorAdd8 intA1 $ tensorAdd8 intA2 $ tensorAdd8 intA3 $ tensorAdd8 intA4 $ tensorAdd8 intA5 intA6
                total1 = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens6 tens2 
                total2 = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens10 tens1   

    eqn2Aa :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 1 0 0 0 1 0 0 0 VarMap -> Tensor8 2 0 0 0 0 0 2 0 VarMap -> Tensor8 1 0 0 0 0 0 3 1 VarMap
    eqn2Aa map1Metric map2Metric map1Area map2Area ansatz6 ansatz10 = tensorAddWith8 addVarsMap tens1 tens2 
            where
                int4 = interEqn4 map1Metric map2Metric map1Area map2Area 
                flatInt = flatInter map1Area map2Area 
                block1 = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatz10 flatInt
                block1Trans = tensorTransWithU3 (0,2) addVarsMap block1
                tens1 = tensorAddWith8 addVarsMap block1 block1Trans 
                block2 = tensorContrWith9 (0,0) addVarsMap  $ tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatz6 int4
                block2' = tensorTransWithU3 (0,1) addVarsMap block2 
                block2Trans = tensorTransWithU3 (0,2) addVarsMap block2'
                tens2 = tensorAddWith8 addVarsMap block2' block2Trans 

    --2 prolongations

    eqn1ABI :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 2 0 0 0 1 0 0 0 VarMap -> Tensor8 3 0 0 0 1 0 0 0 VarMap -> Tensor8 2 0 0 0 1 0 1 1 VarMap
    eqn1ABI map1Metric map2Metric map1Area map2Area ansatz10 ansatz14 = tensorAddWith8 addVarsMap block1 $ tensorAddWith8 addVarsMap block2 block3 
            where
                flatInt = flatInter map1Area map2Area 
                intA = interArea map1Area map2Area 
                int3 = interEqn3 map1Metric map2Metric map1Area map2Area 
                block1 = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatz14 flatInt
                block2 = tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 multVarsMap intA ansatz10 
                block3 = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatz10 int3

    eqn3AB :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 2 0 0 0 1 0 0 0 VarMap ->  Tensor8 3 0 0 0 1 0 0 0 VarMap -> Tensor8 2 0 0 0 0 0 3 1 VarMap
    eqn3AB map1Metric map2Metric map1Area map2Area ansatzTens10 ansatzTens14 = tensorAddWith8 addVarsMap total1 $ tensorAddWith8 addVarsMap total2 total3
            where
                flatInt = flatInter map1Area map2Area 
                intA = interArea map1Area map2Area
                intJ = interJ2 map2Metric 
                flatInt1 = tensorProd8 intJ flatInt 
                flatInt2 = tensorTransU3 (0,1) flatInt1
                flatInt3 = tensorTransU3 (1,2) flatInt1
                flatInt4 = tensorTransU3 (0,2) flatInt1 
                flatInt5 = tensorTransU3 (0,1) $ tensorTransU3 (1,2) flatInt1
                flatInt6 = tensorTransU3 (0,1) $ tensorTransU3 (0,2) flatInt1 
                intA1 = tensorProd8 intJ intA 
                intA2 = tensorTransU3 (0,1) intA1
                intA3 = tensorTransU3 (1,2) intA1
                intA4 = tensorTransU3 (0,2) intA1 
                intA5 = tensorTransU3 (0,1) $ tensorTransU3 (1,2) intA1
                intA6 = tensorTransU3 (0,1) $ tensorTransU3 (0,2) intA1 
                tens1 = tensorAdd8 flatInt1 $ tensorAdd8 flatInt2 $ tensorAdd8 flatInt3 $ tensorAdd8 flatInt4 $ tensorAdd8 flatInt5 flatInt6
                tens2 = tensorAdd8 intA1 $ tensorAdd8 intA2 $ tensorAdd8 intA3 $ tensorAdd8 intA4 $ tensorAdd8 intA5 intA6
                total1 = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens10 tens2 
                total2 = tensorTransWithU20 (0,1) addVarsMap total1
                total3 = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (2,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens14 tens1

    
    eqn2ABb :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 2 0 0 0 0 0 2 0 VarMap -> Tensor8 2 0 0 0 1 0 0 0 VarMap -> Tensor8 3 0 0 0 0 0 2 0 VarMap -> Tensor8 2 0 0 0 0 0 3 1 VarMap
    eqn2ABb map1Metric map2Metric map1Area map2Area ansatz10_1 ansatz10_2 ansatz14 = tensorAddWith8 addVarsMap tens1 $ tensorAddWith8 addVarsMap tens2 tens3
            where
                int4 = interEqn4 map1Metric map2Metric map1Area map2Area 
                intA = interArea map1Area map2Area 
                flatInt = flatInter map1Area map2Area 
                block1 = tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatz14 flatInt
                block1Trans = tensorTransWithU3 (0,2) addVarsMap block1
                tens1 = tensorAddWith8 addVarsMap block1 block1Trans 
                block2 = tensorContrWith9 (0,0) addVarsMap  $ tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatz10_2 int4
                block2' = tensorTransWithU3 (0,1) addVarsMap block2 
                block2Trans = tensorTransWithU3 (0,2) addVarsMap block2'
                tens2 = tensorAddWith8 addVarsMap block2' block2Trans 
                block3 = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatz10_1 intA
                block3Trans = tensorTransWithU3 (0,2) addVarsMap block3
                tens3 = tensorTransWithU20 (0,1) addVarsMap $ tensorAddWith8 addVarsMap block3 block3Trans 

    eqn1AaBb :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 2 0 0 0 0 0 2 0 VarMap -> Tensor8 3 0 0 0 0 0 2 0 VarMap -> Tensor8 2 0 0 0 0 0 3 1 VarMap
    eqn1AaBb map1Metric map2Metric map1Area map2Area ansatz10_1 ansatz14 = tensorAddWith8 addVarsMap block1 $ tensorAddWith8 addVarsMap block2 block3
            where
                int2 = interEqn2 map1Area map2Area
                flatInt = flatInter map1Area map2Area 
                block1 = tensorTransWithU3 (1,2) addVarsMap $ tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatz14 flatInt 
                block2 = tensorContrWith20 (0,0) addVarsMap $ tensorContrWith3 (0,1) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatz10_1 int2 
                block3 = tensorTransWithU20 (0,1) addVarsMap $ tensorTransWithU3 (0,2) addVarsMap block2 

    --3 prolongations 

    eqn1ABbCc :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 3 0 0 0 0 0 2 0 VarMap -> Tensor8 4 0 0 0 0 0 2 0 VarMap -> Tensor8 3 0 0 0 0 0 3 1 VarMap
    eqn1ABbCc map1Metric map2Metric map1Area map2Area ansatz14 ansatz18 = tensorAddWith8 addVarsMap block0 $ tensorAddWith8 addVarsMap block1 $ tensorAddWith8 addVarsMap block2 block3
        where
            int2 = interEqn2 map1Area map2Area
            flatInt = flatInter map1Area map2Area 
            intA = interArea map1Area map2Area
            block0 = tensorTransWithU3 (1,2) addVarsMap $ tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatz18 flatInt 
            block1 = tensorTransWithU3 (0,1) addVarsMap $ tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 multVarsMap intA ansatz14 
            block2 = tensorContrWith20 (1,0) addVarsMap $ tensorContrWith3 (0,1) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatz14 int2 
            block3 = tensorTransWithU20 (1,2) addVarsMap $ tensorTransWithU3 (0,2) addVarsMap block2 


    eqn1ABCI :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 3 0 0 0 1 0 0 0 VarMap -> Tensor8 4 0 0 0 1 0 0 0 VarMap -> Tensor8 3 0 0 0 1 0 1 1 VarMap
    eqn1ABCI map1Metric map2Metric map1Area map2Area ansatz14 ansatz18 = tensorAddWith8 addVarsMap block1 $ tensorAddWith8 addVarsMap block2 $ tensorAddWith8 addVarsMap block3 block4 
            where
                flatInt = flatInter map1Area map2Area 
                intA = interArea map1Area map2Area 
                int3 = interEqn3 map1Metric map2Metric map1Area map2Area 
                block1 = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatz18 flatInt
                block2 = tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 multVarsMap intA ansatz14
                block3 = tensorTransWithU20 (0,1) addVarsMap block2 
                block4 = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (2,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatz14 int3


    eqn2ABCc :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 3 0 0 0 0 0 2 0 VarMap -> Tensor8 3 0 0 0 1 0 0 0 VarMap -> Tensor8 4 0 0 0 0 0 2 0 VarMap -> Tensor8 3 0 0 0 0 0 3 1 VarMap
    eqn2ABCc map1Metric map2Metric map1Area map2Area ansatz14_1 ansatz14_2 ansatz18 = tensorAddWith8 addVarsMap tens1 $ tensorAddWith8 addVarsMap tens2 $ tensorAddWith8 addVarsMap tens3 tens4 
            where
                int4 = interEqn4 map1Metric map2Metric map1Area map2Area 
                intA = interArea map1Area map2Area 
                flatInt = flatInter map1Area map2Area 
                block1 = tensorContrWith20 (2,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatz18 flatInt
                block1Trans = tensorTransWithU3 (0,2) addVarsMap block1
                tens1 = tensorAddWith8 addVarsMap block1 block1Trans 
                block2 = tensorContrWith9 (0,0) addVarsMap  $ tensorContrWith20 (2,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatz14_2 int4
                block2' = tensorTransWithU3 (0,1) addVarsMap block2 
                block2Trans = tensorTransWithU3 (0,2) addVarsMap block2'
                tens2 = tensorAddWith8 addVarsMap block2' block2Trans 
                block3 = tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatz14_1 intA
                block3Trans = tensorTransWithU3 (0,2) addVarsMap block3
                tens3 = tensorTransWithU20 (1,2) addVarsMap $ tensorAddWith8 addVarsMap block3 block3Trans 
                tens4 = tensorTransWithU20 (0,1) addVarsMap tens3

    eqn3ABC :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 3 0 0 0 1 0 0 0 VarMap ->  Tensor8 4 0 0 0 1 0 0 0 VarMap -> Tensor8 3 0 0 0 0 0 3 1 VarMap
    eqn3ABC map1Metric map2Metric map1Area map2Area ansatzTens14 ansatzTens18 = tensorAddWith8 addVarsMap total1 $ tensorAddWith8 addVarsMap total2 $ tensorAddWith8 addVarsMap total3 total4
            where
                flatInt = flatInter map1Area map2Area 
                intA = interArea map1Area map2Area
                intJ = interJ2 map2Metric 
                flatInt1 = tensorProd8 intJ flatInt 
                flatInt2 = tensorTransU3 (0,1) flatInt1
                flatInt3 = tensorTransU3 (1,2) flatInt1
                flatInt4 = tensorTransU3 (0,2) flatInt1 
                flatInt5 = tensorTransU3 (0,1) $ tensorTransU3 (1,2) flatInt1
                flatInt6 = tensorTransU3 (0,1) $ tensorTransU3 (0,2) flatInt1 
                intA1 = tensorProd8 intJ intA 
                intA2 = tensorTransU3 (0,1) intA1
                intA3 = tensorTransU3 (1,2) intA1
                intA4 = tensorTransU3 (0,2) intA1 
                intA5 = tensorTransU3 (0,1) $ tensorTransU3 (1,2) intA1
                intA6 = tensorTransU3 (0,1) $ tensorTransU3 (0,2) intA1 
                tens1 = tensorAdd8 flatInt1 $ tensorAdd8 flatInt2 $ tensorAdd8 flatInt3 $ tensorAdd8 flatInt4 $ tensorAdd8 flatInt5 flatInt6
                tens2 = tensorAdd8 intA1 $ tensorAdd8 intA2 $ tensorAdd8 intA3 $ tensorAdd8 intA4 $ tensorAdd8 intA5 intA6
                total1 = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (2,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens14 tens2 
                total2 = tensorTransWithU20 (0,2) addVarsMap total1
                total3 = tensorTransWithU20 (1,2) addVarsMap total1
                total4 = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (3,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens18 tens1

    --second subgraph (#of total derivatives = 0)

    eqn1 ::  M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 1 0 0 0 0 0 0 0 VarMap -> Tensor8 0 0 0 0 0 0 1 1 VarMap
    eqn1 map1Metric map2Metric map1Area map2Area ansatzTens4 = tens
            where
                intFlat = flatInter map1Area map2Area 
                tens = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens4 intFlat  

    eqn1A ::  M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 1 0 0 0 0 0 0 0 VarMap -> Tensor8 2 0 0 0 0 0 0 0 VarMap -> Tensor8 1 0 0 0 0 0 1 1 VarMap
    eqn1A map1Metric map2Metric map1Area map2Area ansatzTens4 ansatzTens8 = tensorAddWith8 addVarsMap tens1 tens2 
            where
                intFlat = flatInter map1Area map2Area 
                intArea = interArea map1Area map2Area 
                tens1 = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens4 intArea 
                tens2 = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens8 intFlat 

    --third subgraph (#of total derivatives = 4)

    eqn3AI :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 2 0 0 0 2 0 0 0 VarMap -> Tensor8 1 0 0 0 1 0 3 1 VarMap
    eqn3AI map1Metric map2Metric map1Area map2Area ansatzTens12 = total
            where
                flatInt = flatInter map1Area map2Area 
                intJ = interJ2 map2Metric 
                flatInt1 = tensorProd8 intJ flatInt 
                flatInt2 = tensorTransU3 (0,1) flatInt1
                flatInt3 = tensorTransU3 (1,2) flatInt1
                flatInt4 = tensorTransU3 (0,2) flatInt1 
                flatInt5 = tensorTransU3 (0,1) $ tensorTransU3 (1,2) flatInt1
                flatInt6 = tensorTransU3 (0,1) $ tensorTransU3 (0,2) flatInt1 
                tens = tensorAdd8 flatInt1 $ tensorAdd8 flatInt2 $ tensorAdd8 flatInt3 $ tensorAdd8 flatInt4 $ tensorAdd8 flatInt5 flatInt6
                total = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens12 tens  


    -----------------------------------

    ansatzAB2 :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 2 0 0 0 0 0 0 0 VarMap -> Tensor8 2 0 0 0 1 0 0 0 VarMap
    ansatzAB2 map1Metric map2Metric map1Area map2Area ansatzTens = block1 
            where
                intA = interArea map1Area map2Area
                antiSym = aSymI2 map1Metric
                aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                intAContr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 intA aSym
                block1 = tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens intAContr 
    
    ansatzAIB2_1 :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 2 0 0 0 1 0 0 0 VarMap -> Tensor8 2 0 0 0 2 0 0 0 VarMap
    ansatzAIB2_1 map1Metric map2Metric map1Area map2Area ansatzTens = block1 
            where
                intArea = interArea map1Area map2Area
                antiSym = aSymI2 map1Metric
                aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                intAContr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 intArea aSym
                block1 = tensorTransWithU20 (0,1) addVarsMap $ tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens intAContr 

    ansatzAIB2_2 :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 2 0 0 0 1 0 0 0 VarMap -> Tensor8 2 0 0 0 2 0 0 0 VarMap
    ansatzAIB2_2 map1Metric map2Metric map1Area map2Area ansatzTens = block2 
            where
                int3 = interEqn3 map1Metric map2Metric map1Area map2Area
                antiSym = aSymI2 map1Metric
                aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                int3Contr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 int3 aSym
                block2 = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (1,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens int3Contr 

    ansatzAIBJ2 :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 2 0 0 0 2 0 0 0 VarMap -> Tensor8 2 0 0 0 3 0 0 0 VarMap
    ansatzAIBJ2 map1Metric map2Metric map1Area map2Area ansatzTens = block1
            where
                int3 = interEqn3 map1Metric map2Metric map1Area map2Area
                antiSym = aSymI2 map1Metric
                aSym = tensorContr3 (1,1) $ tensorProd8 invEta antiSym
                int3Contr = tensorContr3 (0,0) $ tensorContr3 (0,1) $ tensorProd8 int3 aSym
                block1 = tensorContrWith9 (0,0) addVarsMap $ tensorContrWith20 (0,0) addVarsMap $ tensorProdWith8 (flip multVarsMap) ansatzTens int3Contr 
