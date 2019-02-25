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




module TensorTreeNumeric4 (
    toListT8, toListShow8, intAIB, interMetric, interArea, interEqn2, interEqn3, trianMapAreaI, trianMapAreaJ, trianMapI2, trianMapJ2,
    interI2, interJ2, aSymI2, interIArea, interJArea,
    delta20, delta19, delta9, delta3, tensorContr20, tensorContr19, tensorContr9, tensorContr3, tensorProd8, 
    tensorTransU20, tensorTransL20, tensorTransU19, tensorTransL19, tensorTransU9, tensorTransL9, tensorTransU3, tensorTransL3, tensorSub8
    
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

    data IndList n a where
        Empty :: IndList 0 a 
        Append :: a -> IndList n a -> IndList (n+1) a 

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

    instance Functor (Tensor n k) where 
        fmap f (Scalar x) = Scalar (f x)
        fmap f (Tensor m) = Tensor (M.map (fmap f) m)
        fmap f ZeroTensor = ZeroTensor 

    deriving instance (Show a, Show k) => Show (Tensor n k a)

    deriving instance (Eq a, Eq k) => Eq (Tensor n k a)

    isZeroTensor :: (Eq a, Eq k) => Tensor n k a -> Bool
    isZeroTensor (Tensor m) =  m == M.empty 
    isZeroTensor ZeroTensor = True 
    isZeroTensor x = False 

    filterTens :: (Eq a, Eq k) => (a -> Bool) -> Tensor n k a -> Tensor n k a 
    filterTens isZero (Scalar x) 
            | isZero x = ZeroTensor 
            | otherwise = Scalar x
    filterTens isZero (Tensor m) = Tensor $ M.filter (not.isZeroTensor) $ M.map (filterTens isZero) m
    filterTens isZero ZeroTensor = ZeroTensor


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

    insertOrAdd :: (Ord k, Eq k, Eq v) => (v -> Bool) -> (v -> v -> v) -> (IndList n k, v) -> Tensor n k v -> Tensor n k v 
    insertOrAdd isZero addF (Empty, a) (Scalar b) = let newVal = addF a b in if isZero newVal then ZeroTensor else Scalar newVal
    insertOrAdd isZero addF (Append x xs, a) (Tensor m) = Tensor $ M.filter (not.isZeroTensor) $ M.insertWith (\_ o -> insertOrAdd isZero addF (xs, a) o) x indTens m 
                where
                    indTens = mkTens (xs, a)
    insertOrAdd isZero addF inds ZeroTensor = mkTens inds

    tensorAdd :: (Ord k, Eq v) => (v -> Bool) -> (v -> v -> v) -> Tensor n k v -> Tensor n k v -> Tensor n k v 
    tensorAdd isZero addF (Scalar a) (Scalar b) = let newVal = addF a b in if isZero newVal then ZeroTensor else Scalar newVal
    tensorAdd isZero addF (Tensor m1) (Tensor m2) = Tensor $ M.filter (not.isZeroTensor) $ M.unionWith (tensorAdd isZero addF) m1 m2
    tensorAdd isZero addF t ZeroTensor = t
    tensorAdd isZero addF ZeroTensor t = t

    --tensorProduct: append the second tensor to the right of the first one 

    tensorProd :: (Ord k, Show k, Show a, Show b) => (a -> b -> c) -> Tensor n k a -> Tensor m k b -> Tensor (n+m) k c 
    tensorProd prodF (Scalar x) (Scalar y) = Scalar (prodF x y)
    tensorProd prodF (Scalar x) t2 = fmap (prodF x) t2 
    tensorProd prodF (Tensor m) t2 = Tensor $ M.map (\t1 -> tensorProd prodF t1 t2) m 
    tensorProd prodF x y = error $ "called with" ++ show x ++ show y

    --could be improved if not the whole tensor but only the part necessary is converted to a list

    tensorTrans :: (Ord k, Eq v) => (v -> Bool) -> (v -> v -> v) -> (Int,Int) -> Tensor n k v -> Tensor n k v
    tensorTrans isZero addF (0, j) t = foldr (insertOrAdd isZero addF) ZeroTensor l
                    where 
                        l = (map (\(x,y) -> (swapHead j x, y)) $ toListT t)
    tensorTrans isZero addF (i, j) (Tensor m) = Tensor $ M.map (tensorTrans isZero addF (i-1, j-1)) m 


    tensorContr :: (Ord k, Ord k', Eq k, Eq v) => (v -> Bool) -> (k -> Int) -> (k' -> Int) -> (v -> v -> v) -> (Int, Int) -> Tensor n k (Tensor m k' v) -> Tensor (n-1) k (Tensor (m-1) k' v)
    tensorContr isZero g f addF (0,j) t = filterTens isZeroTensor $ foldr (insertOrAdd (isZeroTensor) (tensorAdd isZero addF)) ZeroTensor tensList
            where
                l = map (\(x,y) -> (x, toListT y)) $ toListT t
                l2 = map (\(x,y) -> (tailInd x,(mapMaybe (removeContractionInd g f j (headInd x)) y))) l
                l3 = filter (\(_,y) -> length y >= 1) l2 
                tensList = map (\(x,y) -> (x, foldr (insertOrAdd isZero addF) ZeroTensor y)) l3
    tensorContr isZero g f addF (i,j) (Tensor m) = Tensor $ M.map (tensorContr isZero g f addF (i-1,j)) m


    data Uind20 =  Uind20 {indValU20 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show)
    data Lind20 =  Lind20 {indValL20 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show)
    data Uind19 =  Uind19 {indValU19 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show)
    data Lind19 =  Lind19 {indValL19 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show)
    data Uind9 =  Uind9 {indValU9 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show)
    data Lind9 =  Lind9 {indValL9 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show)
    data Uind3 =  Uind3 {indValU3 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show)
    data Lind3 =  Lind3 {indValL3 :: {-# UNPACK #-} !Int} deriving (Ord, Eq, Show)


    type Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 = Tensor n1 Uind20 (Tensor n2 Lind20 (Tensor n3 Uind19 (Tensor n4 Lind19 (Tensor n5 Uind9 (Tensor n6 Lind9 (Tensor n7 Uind3 (Tensor n8 Lind3 Rational)))))))
    
    actOnScalar :: (Rational -> Rational) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 
    actOnScalar f = fmap (fmap (fmap (fmap (fmap (fmap (fmap (fmap f)))))))

    actOnL3 :: (Tensor n8 Lind3 Rational -> Tensor m8 Lind3 Rational) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 n6 n7 m8 
    actOnL3 f = (filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap f)))))))))))))

    actOnU3 :: (Tensor n7 Uind3 (Tensor n8 Lind3 Rational) -> Tensor m7 Uind3 (Tensor m8 Lind3 Rational)) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 n6 m7 m8 
    actOnU3 f =  (filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap f)))))))))))

    actOnL9 :: (Tensor n6 Lind9 (Tensor n7 Uind3 (Tensor n8 Lind3 Rational)) -> Tensor m6 Lind9 (Tensor m7 Uind3 (Tensor m8 Lind3 Rational))) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 m6 m7 m8 
    actOnL9 f = (filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap f)))))))))

    actOnU9 :: (Tensor n5 Uind9 (Tensor n6 Lind9 (Tensor n7 Uind3 (Tensor n8 Lind3 Rational))) -> Tensor m5 Uind9 (Tensor m6 Lind9 (Tensor m7 Uind3 (Tensor m8 Lind3 Rational)))) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 m5 m6 m7 m8 
    actOnU9 f = (filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap f)))))))

    actOnL19 :: (Tensor n4 Lind19 (Tensor n5 Uind9 (Tensor n6 Lind9 (Tensor n7 Uind3 (Tensor n8 Lind3 Rational)))) -> Tensor m4 Lind19 (Tensor m5 Uind9 (Tensor m6 Lind9 (Tensor m7 Uind3 (Tensor m8 Lind3 Rational))))) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 m4 m5 m6 m7 m8 
    actOnL19 f = (filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap f)))))

    actOnU19 :: (Tensor n3 Uind19 (Tensor n4 Lind19 (Tensor n5 Uind9 (Tensor n6 Lind9 (Tensor n7 Uind3 (Tensor n8 Lind3 Rational))))) -> Tensor m3 Uind19 (Tensor m4 Lind19 (Tensor m5 Uind9 (Tensor m6 Lind9 (Tensor m7 Uind3 (Tensor m8 Lind3 Rational)))))) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 m3 m4 m5 m6 m7 m8 
    actOnU19 f = (filterTens isZeroTensor). (fmap ((filterTens isZeroTensor). (fmap f)))

    actOnL20 :: (Tensor n2 Lind20 (Tensor n3 Uind19 (Tensor n4 Lind19 (Tensor n5 Uind9 (Tensor n6 Lind9 (Tensor n7 Uind3 (Tensor n8 Lind3 Rational)))))) -> Tensor m2 Lind20 (Tensor m3 Uind19 (Tensor m4 Lind19 (Tensor m5 Uind9 (Tensor m6 Lind9 (Tensor m7 Uind3 (Tensor m8 Lind3 Rational))))))) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 m2 m3 m4 m5 m6 m7 m8 
    actOnL20 f = (filterTens isZeroTensor). (fmap f)

    actOnU20 :: (Tensor n1 Uind20 (Tensor n2 Lind20 (Tensor n3 Uind19 (Tensor n4 Lind19 (Tensor n5 Uind9 (Tensor n6 Lind9 (Tensor n7 Uind3 (Tensor n8 Lind3 Rational))))))) -> Tensor m1 Uind20 (Tensor m2 Lind20 (Tensor m3 Uind19 (Tensor m4 Lind19 (Tensor m5 Uind9 (Tensor m6 Lind9 (Tensor m7 Uind3 (Tensor m8 Lind3 Rational)))))))) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 m1 m2 m3 m4 m5 m6 m7 m8 
    actOnU20 f = f

    

    tensorAdd8 :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 
    tensorAdd8 = tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd ((==) 0) (+))))))))
    
    tensorSub8 :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 
    tensorSub8 t1 t2 = tensorAdd8 t1 $ actOnScalar ((*) (-1)) t2 

    tensorSMult :: Rational -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 
    tensorSMult s t = actOnScalar ((*) s) t

    tensorTransU20 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 
    tensorTransU20 inds = actOnU20 (tensorTrans (isZeroTensor) (\t1 t2 -> tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd ((==) 0) (+))))))) t1 t2) inds)

    tensorTransL20 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 
    tensorTransL20 inds = actOnL20 (tensorTrans (isZeroTensor) (\t1 t2 -> tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd ((==) 0) (+)))))) t1 t2) inds)

    tensorTransU19 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 
    tensorTransU19 inds = actOnU19 (tensorTrans (isZeroTensor) (\t1 t2 -> tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd ((==) 0) (+))))) t1 t2) inds)

    tensorTransL19 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 
    tensorTransL19 inds = actOnL19 (tensorTrans (isZeroTensor) (\t1 t2 -> tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd ((==) 0) (+)))) t1 t2) inds)

    tensorTransU9 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 
    tensorTransU9 inds = actOnU9 (tensorTrans (isZeroTensor) (\t1 t2 -> tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd ((==) 0) (+))) t1 t2) inds)

    tensorTransL9 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 
    tensorTransL9 inds = actOnL9 (tensorTrans (isZeroTensor) (\t1 t2 -> tensorAdd (isZeroTensor) (tensorAdd ((==) 0) (+)) t1 t2) inds)

    tensorTransU3 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 
    tensorTransU3 inds = actOnU3 (tensorTrans (isZeroTensor) (\t1 t2 -> tensorAdd ((==) 0) (+) t1 t2) inds)

    tensorTransL3 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 
    tensorTransL3 inds = actOnL3 (tensorTrans ((==) 0) (+) inds)

    

    tensorContr20 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 (n1-1) (n2-1) n3 n4 n5 n6 n7 n8 
    tensorContr20 inds = actOnU20 (tensorContr (isZeroTensor) indValU20 indValL20 addF inds)
                where
                    addF = tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd ((==) 0) (+))))))

    tensorContr19 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 (n3-1) (n4-1) n5 n6 n7 n8 
    tensorContr19 inds = actOnU19 (tensorContr (isZeroTensor) indValU19 indValL19 addF inds)
                where
                    addF = \t1 t2 -> tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd (isZeroTensor) (tensorAdd ((==) 0) (+)))) t1 t2

    tensorContr9 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 (n5-1) (n6-1) n7 n8 
    tensorContr9 inds = actOnU9 (tensorContr (isZeroTensor) indValU9 indValL9 addF inds)
                where
                    addF = \t1 t2 -> tensorAdd (isZeroTensor) (tensorAdd ((==) 0) (+)) t1 t2
        
    tensorContr3 :: (Int,Int) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 n6 (n7-1) (n8-1) 
    tensorContr3 inds = actOnU3 (tensorContr ((==) 0) indValU3 indValL3 (+) inds)
                


    tensorProd8 :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 m1 m2 m3 m4 m5 m6 m7 m8 -> Tensor8 (n1+m1) (n2+m2) (n3+m3) (n4+m4) (n5+m5) (n6+m6) (n7+m7) (n8+m8)
    tensorProd8 = tensorProd (tensorProd (tensorProd (tensorProd (tensorProd (tensorProd (tensorProd (tensorProd (*))))))))

    type IndTuple n1 n2 n3 n4 n5 n6 n7 n8 = (IndList n1 Uind20, IndList n2 Lind20 , IndList n3 Uind19, IndList n4 Lind19, IndList n5 Uind9, IndList n6 Lind9, IndList n7 Uind3, IndList n8 Lind3)

    mkTens8 :: (IndTuple n1 n2 n3 n4 n5 n6 n7 n8, Rational) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 
    mkTens8 ((i1,i2,i3,i4,i5,i6,i7,i8),s) = mkTens (i1, mkTens (i2, mkTens (i3, mkTens (i4, mkTens (i5, mkTens (i6, mkTens (i7, mkTens (i8, s))))))))

    insertOrAdd8 :: (IndTuple n1 n2 n3 n4 n5 n6 n7 n8, Rational) -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8
    insertOrAdd8 inds t = tensorAdd8 (mkTens8 inds) t

    fromListT8 :: [(IndTuple n1 n2 n3 n4 n5 n6 n7 n8, Rational)] -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 
    fromListT8 l = foldr insertOrAdd8 (mkTens8 $ head p) q 
        where
            (p,q) = splitAt 1 l

    toListT8 :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> [(IndTuple n1 n2 n3 n4 n5 n6 n7 n8, Rational)]
    toListT8 t = concat $ map (\(x,y) -> appendT7 x $ toListT y ) $ concat $ map (\(x,y) -> appendT6 x $ toListT y ) $ concat $ map (\(x,y) -> appendT5 x $ toListT y ) $ concat $ map (\(x,y) -> appendT4 x $ toListT y ) $ concat $ map (\(x,y) -> appendT3 x $ toListT y ) $ concat $ map (\(x,y) -> appendT2 x $ toListT y ) $ concat $ map (\(x,y) -> appendT1 x $ toListT y ) $ toListT t
            where
                appendT1 = \i l -> map (\(x,y) -> ((i,x),y)) l
                appendT2 = \(i1,i2) l -> map (\(x,y) -> ((i1,i2,x),y)) l
                appendT3 = \(i1,i2,i3) l -> map (\(x,y) -> ((i1,i2,i3,x),y)) l
                appendT4 = \(i1,i2,i3,i4) l -> map (\(x,y) -> ((i1,i2,i3,i4,x),y)) l
                appendT5 = \(i1,i2,i3,i4,i5) l -> map (\(x,y) -> ((i1,i2,i3,i4,i5,x),y)) l
                appendT6 = \(i1,i2,i3,i4,i5,i6) l -> map (\(x,y) -> ((i1,i2,i3,i4,i5,i6,x),y)) l
                appendT7 = \(i1,i2,i3,i4,i5,i6,i7) l -> map (\(x,y) -> ((i1,i2,i3,i4,i5,i6,i7,x),y)) l

    toListShow8 :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 -> [([Int],Rational)]
    toListShow8 t = map (\(x,y) -> (showInd x, y)) l
            where
                l = toListT8 t 
                showInd (i1,i2,i3,i4,i5,i6,i7,i8) = (map indValU20 $ toList i1) ++ (map indValL20 $ toList i2) ++ (map indValU19 $ toList i3) ++ (map indValL19 $ toList i4) ++ (map indValU9 $ toList i5) ++ (map indValL9 $ toList i6) ++ (map indValU3 $ toList i7) ++ (map indValL3 $ toList i8) 
    
    --now the basic tensors

    delta20 :: Tensor8 1 1 0 0 0 0 0 0
    delta20 = fromListT8 $ zip [(singletonInd (Uind20 i),singletonInd (Lind20 i), Empty, Empty, Empty, Empty, Empty, Empty) | i <- [0..20]] (repeat 1)

    delta19 :: Tensor8 0 0 1 1 0 0 0 0
    delta19 = fromListT8 $ zip [(Empty, Empty, singletonInd (Uind19 i),singletonInd (Lind19 i), Empty, Empty, Empty, Empty) | i <- [0..19]] (repeat 1)

    delta9 :: Tensor8 0 0 0 0 1 1 0 0
    delta9 = fromListT8 $ zip [(Empty, Empty, Empty, Empty, singletonInd (Uind9 i),singletonInd (Lind9 i), Empty, Empty) | i <- [0..9]] (repeat 1)

    delta3 :: Tensor8 0 0 0 0 0 0 1 1
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

    interI2 :: M.Map (IndList 2 Lind3) (IndList 1 Uind9)  -> Tensor8 0 0 0 0 1 0 0 2 
    interI2 trian2 = fromListT8 $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [ (Empty, Empty, Empty, Empty, (singletonInd $ Uind9 a), Empty, Empty, (Append (Lind3 b) $ singletonInd $ Lind3 c)) | a <- [0..9], b <- [0..3], c <- [0..3]]
                f (_, _, _, _, ind1, _, _, ind2)
                    | ind1 == ((M.!) trian2 $ sortInd ind2 ) = 1 
                    | otherwise = 0 

    aSymI2 :: M.Map (IndList 2 Lind3) (IndList 1 Uind9)  -> Tensor8 0 0 0 0 1 0 0 2 
    aSymI2 trian2 = fromListT8 $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [ (Empty, Empty, Empty, Empty, (singletonInd $ Uind9 a), Empty, Empty, (Append (Lind3 b) $ singletonInd $ Lind3 c)) | a <- [0..9], b <- [0..3], c <- [0..3]]
                f (_, _, _, _, ind1, _, _, ind2@(Append x (Append y Empty)))
                    | x == y = 0
                    | ind1 == (M.!) trian2 sortI = sign
                    | otherwise = 0 
                  where sortI = sortInd ind2
                        sign = if sortI == ind2 then 1 else -1

    interJ2 :: M.Map (IndList 2 Uind3) (IndList 1 Lind9)  -> Tensor8 0 0 0 0 0 1 2 0 
    interJ2 trian2 = fromListT8 $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [ (Empty, Empty, Empty, Empty, Empty, (singletonInd $ Lind9 a), (Append (Uind3 b) $ singletonInd $ Uind3 c), Empty) | a <- [0..9], b <- [0..3], c <- [0..3]]
                f (_, _, _, _, _, ind1, ind2, _)
                    | ind1 == ((M.!) trian2 $ sortInd ind2 ) = jMult2 ind2  
                    | otherwise = 0 

    interI3 :: M.Map (IndList 3 Lind3) (IndList 1 Uind19) -> Tensor8 0 0 1 0 0 0 0 3 
    interI3 trian3 = fromListT8 $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [(Empty, Empty, (singletonInd $ Uind19 a), Empty, Empty, Empty, Empty, (Append (Lind3 b) $ Append (Lind3 c) $ singletonInd $ Lind3 d)) | a <- [0..19], b <- [0..3], c <- [0..3], d <- [0..3]]
                f (_, _, ind1, _, _, _, _, ind2)
                    | ind1 == ((M.!) trian3 $ sortInd ind2) = 1 
                    | otherwise = 0 

    interJ3 :: M.Map (IndList 3 Uind3) (IndList 1 Lind19) -> Tensor8 0 0 0 1 0 0 3 0 
    interJ3 trian3 = fromListT8 $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [ (Empty, Empty, Empty, (singletonInd $ Lind19 a), Empty, Empty, (Append (Uind3 b) $ Append (Uind3 c) $ singletonInd $ Uind3 d), Empty) | a <- [0..19], b <- [0..3], c <- [0..3], d <- [0..3]]
                f (_, _, _, ind1, _, _, ind2, _)
                    | ind1 == ((M.!) trian3 $ sortInd ind2) = jMult3 ind2 
                    | otherwise = 0 

    interIArea :: M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> Tensor8 1 0 0 0 0 0 0 4  
    interIArea trianArea = fromListT8 $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [ ((singletonInd $ Uind20 a), Empty, Empty, Empty, Empty, Empty, Empty, (Append (Lind3 b) $ Append (Lind3 c) $ Append (Lind3 d) $ singletonInd $ Lind3 e)) | a <- [0..20], b <- [0..3], c <- [0..3], d <- [0..3], e <- [0..3], not (b == c || d == e)]
                f (ind1, _, _, _, _, _, _, ind2)
                    | ind1 == ((M.!) trianArea indArea) = s
                    | otherwise = 0
                        where
                            (indArea, s) = canonicalizeArea ind2 

    interJArea :: M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 0 1 0 0 0 0 4 0  
    interJArea trianArea = fromListT8 $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [  (Empty, (singletonInd $ Lind20 a), Empty, Empty, Empty, Empty, (Append (Uind3 b) $ Append (Uind3 c) $ Append (Uind3 d) $ singletonInd $ Uind3 e), Empty) | a <- [0..20], b <- [0..3], c <- [0..3], d <- [0..3], e <- [0..3], not (b == c || d == e)]
                f (_, ind1, _, _, _, _, ind2, _)
                    | ind1 == ((M.!) trianArea indArea) = s * (jMultArea indArea)
                    | otherwise = 0
                        where
                            (indArea, s) = canonicalizeArea ind2 

    interMetric :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> Tensor8 0 0 0 0 1 1 1 1 
    interMetric trian2I trian2J = actOnScalar ((*) (-2)) $ tensorContr3 (0,0) prod
            where
                t1 = interI2 trian2I 
                t2 = interJ2 trian2J 
                prod = tensorProd8 t1 t2 

    interArea :: M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 1 1 0 0 0 0 1 1 
    interArea trianAreaI trianAreaJ =  actOnScalar ((*) (-4)) $ tensorContr3 (1,1) $ tensorContr3 (2,2) $ tensorContr3 (3,3) prod
            where
                t1 = interIArea trianAreaI 
                t2 = interJArea trianAreaJ 
                prod = tensorProd8 t1 t2 

    interEqn2 :: M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 1 1 0 0 0 0 2 2 
    interEqn2 trianAreaI trianAreaJ = tensorSub8 int1 int2
            where
                intArea = interArea trianAreaI trianAreaJ
                int1 = tensorProd8 intArea delta3
                int2 = tensorProd8 (tensorTransL3 (0,1) $ tensorProd8 delta3 delta3 ) delta20

    interEqn3 :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 1 1 0 0 1 1 1 1 
    interEqn3 trian2I trian2J trianAreaI trianAreaJ = intTotal
            where
                int1 = tensorProd8 (interArea trianAreaI trianAreaJ) delta9
                int2 = tensorProd8 (interMetric trian2I trian2J) delta20
                intTotal = tensorAdd8 int1 int2

    flatArea :: Tensor8 0 1 0 0 0 0 0 0 
    flatArea = fromListT8 $ map (\(i,v) -> ( (Empty, (singletonInd $ Lind20 i), Empty, Empty, Empty, Empty, Empty, Empty), v)) [(0,-1),(5,-1),(6,-1),(9,1),(11,-1),(12,-1),(15,1),(18,1),(20,1)]

    eta :: Tensor8 0 0 0 0 0 0 0 2  
    eta =  fromListT8 l 
                where
                    l = map (\(x,y,z) -> ((Empty,Empty,Empty,Empty,Empty,Empty,Empty,Append (Lind3 x) $ Append (Lind3 y) Empty),z)) [(0,0,-1),(1,1,1),(2,2,1),(3,3,1)]

    invEta :: Tensor8 0 0 0 0 0 0 2 0  
    invEta =  fromListT8 l 
                where
                    l = map (\(x,y,z) -> ((Empty,Empty,Empty,Empty,Empty,Empty,Append (Uind3 x) $ Append (Uind3 y) Empty,Empty),z)) [(0,0,-1),(1,1,1),(2,2,1),(3,3,1)]

    flatInter :: M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 0 1 0 0 0 0 1 1 
    flatInter trianAreaI trianAreaJ = tensorContr20 (0,1) prod
            where
                intArea = interArea trianAreaI trianAreaJ 
                prod = tensorProd8 intArea flatArea 

    intAIB :: M.Map (IndList 2 Lind3) (IndList 1 Uind9) -> M.Map (IndList 2 Uind3) (IndList 1 Lind9) -> M.Map (IndList 4 Lind3) (IndList 1 Uind20) -> M.Map (IndList 4 Uind3) (IndList 1 Lind20) -> Tensor8 1 2 0 0 1 1 2 2 
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
