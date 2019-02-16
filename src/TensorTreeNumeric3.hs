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




module TensorTreeNumeric3 (
    trianMapI2, trianMapJ2, trianMapAreaI, trianMapAreaJ, Tensor(..), IndList(..), 
    Uind_20, Lind_20, Uind_19, Lind_19, Uind_9, Lind_9, Uind_3, Lind_3, toListT, toListShow, toListShowIndex, fromListT, interEqn3, tensorProd, delta20, delta9, delta3, delta19, tensorAdd,
    tensorSub, tensorContr20, tensorContr19, tensorContr9, tensorContr3, aSymI2,
    tensorTransU20, tensorTransL20, tensorTransL3, interArea, flatInter, tensorTransU3, tensorTransL9,
    interI2, interJ2, interI3, interJ3, interIArea, interJArea, interMetric, interEqn2 , isValid,
    swapHead, toListInd, printTensorTree, invEta, toListU20, toListL20, toListU19, toListL19, toListU9, toListL9, toListU3, toListL3,
    insertOrAddU20, insertOrAddL20, insertOrAddU19, insertOrAddL19, insertOrAddU9, insertOrAddL9, insertOrAddU3, insertOrAddL3,
    tensorSMult, mkTens, tensorSize, intAIB, tensorBalanceFactor, balanceTensor, tensorBalanceFactorLimit, ansatzAaBb, ansatzAIBC
    
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
    import BinaryTree
    --import Data.Singletons

    data Uind_3 = U0_3 | U1_3 | U2_3 | U3_3 deriving (Enum, Eq, Ord, Show, Read)

    data Lind_3 = L0_3 | L1_3 | L2_3 | L3_3 deriving (Enum, Eq, Ord, Show, Read)

    --symmetric 2nd derivative indices have values from 0 to 9

    data Uind_9 = U0_9 | U1_9 | U2_9 | U3_9 | U4_9 | U5_9 | U6_9 | U7_9 | U8_9 | U9_9 deriving (Enum, Eq, Ord, Show, Read)

    data Lind_9 = L0_9 | L1_9 | L2_9 | L3_9 | L4_9 | L5_9 | L6_9 | L7_9 | L8_9 | L9_9 deriving (Enum, Eq, Ord, Show, Read)

    --symmetric 3rd derivative indices have values from 0 to 10

    data Uind_19 = U0_19 | U1_19 | U2_19 | U3_19 | U4_19 | U5_19 | U6_19 | U7_19 | U8_19 | U9_19 | U10_19 | U11_19 | U12_19 | U13_19 | U14_19 | U15_19 | U16_19 | U17_19 | U18_19 | U19_19 deriving (Enum, Eq, Ord, Show, Read)

    data Lind_19 = L0_19 | L1_19 | L2_19 | L3_19 | L4_19 | L5_19 | L6_19 | L7_19 | L8_19 | L9_19 | L10_19 | L11_19 | L12_19 | L13_19 | L14_19 | L15_19 | L16_19 | L17_19 | L18_19 | L19_19 deriving (Enum, Eq, Ord, Show, Read)

    --AreaMetric DOF indices have values form 0 to 20

    data Uind_20 = U0_20 | U1_20 | U2_20 | U3_20 | U4_20 | U5_20 | U6_20 | U7_20 | U8_20 | U9_20 | U10_20 | U11_20 | U12_20 | U13_20 | U14_20 | U15_20 | U16_20 | U17_20 | U18_20 | U19_20 | U20_20 deriving (Enum, Eq, Ord, Show, Read)

    data Lind_20 = L0_20 | L1_20 | L2_20 | L3_20 | L4_20 | L5_20 | L6_20 | L7_20 | L8_20 | L9_20 | L10_20 | L11_20 | L12_20 | L13_20 | L14_20 | L15_20 | L16_20 | L17_20 | L18_20 | L19_20 | L20_20 deriving (Enum, Eq, Ord, Show, Read)

    data IndList n a where
        Empty :: IndList 0 a 
        Append :: a -> IndList (n-1) a -> IndList n a 

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

    removeContractionInd :: (Enum a, Enum b) => Int -> a -> (IndList n b, c) -> Maybe ((IndList (n-1) b),c)
    removeContractionInd 0 ind1 ((Append x xs), t)
                | fromEnum ind1 == fromEnum x = Just $ (xs,t) 
                | otherwise = Nothing 
    removeContractionInd i ind1 ((Append x xs),t) = fmap (\(m,n) -> (Append x m, n)) $ removeContractionInd (i-1) ind1 (xs,t)

                

    
    --data Index n1 n2 n3 n4 n5 n6 n7 n8 = Index (IndList n1 Uind_20) (IndList n2 Lind_20) (IndList n3 Uind_19) (IndList n4 Lind_19) (IndList n5 Uind_9) (IndList n6 Lind_9) (IndList n7 Uind_3) (IndList n8 Lind_3) deriving (Eq, Ord, Show)

    data Tensor n1 n2 n3 n4 n5 n6 n7 n8 a where
        Scalar :: a -> Tensor 0 0 0 0 0 0 0 0 a 
        TensorU20 :: BiTree Uind_20 (Tensor (n1-1) n2 n3 n4 n5 n6 n7 n8 a) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a 
        TensorL20 :: BiTree Lind_20 (Tensor 0 (n2-1) n3 n4 n5 n6 n7 n8 a) -> Tensor 0 n2 n3 n4 n5 n6 n7 n8 a 
        TensorU19 :: BiTree Uind_19 (Tensor 0 0 (n3-1) n4 n5 n6 n7 n8 a) -> Tensor 0 0 n3 n4 n5 n6 n7 n8 a 
        TensorL19 :: BiTree Lind_19 (Tensor 0 0 0 (n4-1) n5 n6 n7 n8 a) -> Tensor 0 0 0 n4 n5 n6 n7 n8 a 
        TensorU9 ::  BiTree Uind_9  (Tensor 0 0 0 0 (n5-1) n6 n7 n8 a) -> Tensor 0 0 0 0 n5 n6 n7 n8 a 
        TensorL9 ::  BiTree Lind_9  (Tensor 0 0 0 0 0 (n6-1) n7 n8 a) -> Tensor 0 0 0 0 0 n6 n7 n8 a 
        TensorU3 ::  BiTree Uind_3  (Tensor 0 0 0 0 0 0 (n7-1) n8 a) -> Tensor 0 0 0 0 0 0 n7 n8 a 
        TensorL3 ::  BiTree Lind_3  (Tensor 0 0 0 0 0 0 0 (n8-1) a) -> Tensor 0 0 0 0 0 0 0 n8 a 

    instance Functor (Tensor n1 n2 n3 n4 n5 n6 n7 n8) where
        fmap f (Scalar s) = Scalar (f s)
        fmap f (TensorU20 t) = TensorU20 $ fmap (fmap f) t 
        fmap f (TensorL20 t) = TensorL20 $ fmap (fmap f) t  
        fmap f (TensorU19 t) = TensorU19 $ fmap (fmap f) t   
        fmap f (TensorL19 t) = TensorL19 $ fmap (fmap f) t  
        fmap f (TensorU9 t) = TensorU9 $ fmap (fmap f) t 
        fmap f (TensorL9 t) = TensorL9 $ fmap (fmap f) t  
        fmap f (TensorU3 t) = TensorU3 $ fmap (fmap f) t   
        fmap f (TensorL3 t) = TensorL3 $ fmap (fmap f) t   

    isValid :: Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Bool
    isValid (Scalar x) = True 
    isValid (TensorU20 l) = isValidTree l && foldr (\ x y -> isValid x && y) True l 
    isValid (TensorL20 l) = isValidTree l && foldr (\ x y -> isValid x && y)  True l
    isValid (TensorU19 l) = isValidTree l && foldr (\ x y -> isValid x && y)  True l
    isValid (TensorL19 l) = isValidTree l && foldr (\ x y -> isValid x && y)  True l
    isValid (TensorU9 l) = isValidTree l && foldr (\ x y -> isValid x && y)  True l
    isValid (TensorL9 l) = isValidTree l && foldr (\ x y -> isValid x && y)  True l
    isValid (TensorU3 l) = isValidTree l && foldr (\ x y -> isValid x && y)  True l
    isValid (TensorL3 l) = isValidTree l && foldr (\ x y -> isValid x && y)  True l

    tensorSize :: Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Int
    tensorSize (Scalar _) = 1
    tensorSize (TensorU20 l) = (foldr (\x y -> (tensorSize x) + y) 0 l)   
    tensorSize (TensorL20 l) = (foldr (\x y -> (tensorSize x) + y) 0 l)   
    tensorSize (TensorU19 l) = (foldr (\x y -> (tensorSize x) + y) 0 l)   
    tensorSize (TensorL19 l) = (foldr (\x y -> (tensorSize x) + y) 0 l)   
    tensorSize (TensorU9 l) = (foldr (\x y -> (tensorSize x) + y) 0 l)   
    tensorSize (TensorL9 l) = (foldr (\x y -> (tensorSize x) + y) 0 l)   
    tensorSize (TensorU3 l) = (foldr (\x y -> (tensorSize x) + y) 0 l)   
    tensorSize (TensorL3 l) = (foldr (\x y -> (tensorSize x) + y) 0 l)   

    tensorBalanceFactor :: Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Rational 
    tensorBalanceFactor (Scalar _) = 0
    tensorBalanceFactor (TensorU20 l) = balanceFactor l + foldr (\x y -> (tensorBalanceFactor x) + y) 0 l
    tensorBalanceFactor (TensorL20 l) = balanceFactor l + foldr (\x y -> (tensorBalanceFactor x) + y) 0 l 
    tensorBalanceFactor (TensorU19 l) = balanceFactor l + foldr (\x y -> (tensorBalanceFactor x) + y) 0 l
    tensorBalanceFactor (TensorL19 l) = balanceFactor l + foldr (\x y -> (tensorBalanceFactor x) + y) 0 l 
    tensorBalanceFactor (TensorU9 l) = balanceFactor l + foldr (\x y -> (tensorBalanceFactor x) + y) 0 l 
    tensorBalanceFactor (TensorL9 l) = balanceFactor l + foldr (\x y -> (tensorBalanceFactor x) + y) 0 l
    tensorBalanceFactor (TensorU3 l) = balanceFactor l + foldr (\x y -> (tensorBalanceFactor x) + y) 0 l 
    tensorBalanceFactor (TensorL3 l) = balanceFactor l + foldr (\x y -> (tensorBalanceFactor x) + y) 0 l 

    tensorBalanceFactorLimit :: Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Rational 
    tensorBalanceFactorLimit (Scalar _) = 0
    tensorBalanceFactorLimit (TensorU20 l) = 1 + foldr (\x y -> (tensorBalanceFactorLimit x) + y) 0 l
    tensorBalanceFactorLimit (TensorL20 l) = 1 + foldr (\x y -> (tensorBalanceFactorLimit x) + y) 0 l
    tensorBalanceFactorLimit (TensorU19 l) = 1 + foldr (\x y -> (tensorBalanceFactorLimit x) + y) 0 l
    tensorBalanceFactorLimit (TensorL19 l) = 1 + foldr (\x y -> (tensorBalanceFactorLimit x) + y) 0 l
    tensorBalanceFactorLimit (TensorU9 l) = 1 + foldr (\x y -> (tensorBalanceFactorLimit x) + y) 0 l
    tensorBalanceFactorLimit (TensorL9 l) = 1 + foldr (\x y -> (tensorBalanceFactorLimit x) + y) 0 l
    tensorBalanceFactorLimit (TensorU3 l) = 1 + foldr (\x y -> (tensorBalanceFactorLimit x) + y) 0 l
    tensorBalanceFactorLimit (TensorL3 l) = 1 + foldr (\x y -> (tensorBalanceFactorLimit x) + y) 0 l


    balanceTensor :: (Ord a) => Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    balanceTensor (Scalar x) = Scalar x
    balanceTensor (TensorU20 l) = TensorU20 (balanceTree $ fmap balanceTensor l)
    balanceTensor (TensorL20 l) = TensorL20 (balanceTree $ fmap balanceTensor l)
    balanceTensor (TensorU19 l) = TensorU19 (balanceTree $ fmap balanceTensor l)
    balanceTensor (TensorL19 l) = TensorL19 (balanceTree $ fmap balanceTensor l)
    balanceTensor (TensorU9 l) = TensorU9 (balanceTree $ fmap balanceTensor l)
    balanceTensor (TensorL9 l) = TensorL9 (balanceTree $ fmap balanceTensor l)
    balanceTensor (TensorU3 l) = TensorU3 (balanceTree $ fmap balanceTensor l)
    balanceTensor (TensorL3 l) = TensorL3 (balanceTree $ fmap balanceTensor l)

    deriving instance (Show a) => Show (Tensor n1 n2 n3 n4 n5 n6 n7 n8 a)

    getTensorListU20 :: Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> BiTree Uind_20 (Tensor (n1-1) n2 n3 n4 n5 n6 n7 n8 a)
    getTensorListU20 (TensorU20 x) = x

    getTensorListL20 :: Tensor 0 n2 n3 n4 n5 n6 n7 n8 a -> BiTree Lind_20 (Tensor 0 (n2-1) n3 n4 n5 n6 n7 n8 a)
    getTensorListL20 (TensorL20 x) = x

    getTensorListU19 :: Tensor 0 0 n3 n4 n5 n6 n7 n8 a -> BiTree Uind_19 (Tensor 0 0 (n3-1) n4 n5 n6 n7 n8 a)
    getTensorListU19 (TensorU19 x) = x

    getTensorListL19 :: Tensor 0 0 0 n4 n5 n6 n7 n8 a -> BiTree Lind_19 (Tensor 0 0 0 (n4-1) n5 n6 n7 n8 a)
    getTensorListL19 (TensorL19 x) = x

    getTensorListU9 :: Tensor 0 0 0 0 n5 n6 n7 n8 a -> BiTree Uind_9 (Tensor 0 0 0 0 (n5-1) n6 n7 n8 a)
    getTensorListU9 (TensorU9 x) = x

    getTensorListL9 :: Tensor 0 0 0 0 0 n6 n7 n8 a -> BiTree Lind_9 (Tensor 0 0 0 0 0 (n6-1) n7 n8 a)
    getTensorListL9 (TensorL9 x) = x

    getTensorListU3 :: Tensor 0 0 0 0 0 0 n7 n8 a -> BiTree Uind_3 (Tensor 0 0 0 0 0 0 (n7-1) n8 a)
    getTensorListU3 (TensorU3 x) = x

    getTensorListL3 :: Tensor 0 0 0 0 0 0 0 n8 a -> BiTree Lind_3 (Tensor 0 0 0 0 0 0 0 (n8-1) a)
    getTensorListL3 (TensorL3 x) = x

    isNotZeroTensorU20 :: (Num a, Eq a, Show a) => Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Maybe (Tensor n1 n2 n3 n4 n5 n6 n7 n8 a)
    isNotZeroTensorU20 (Scalar x) 
                | x == 0 = Nothing
                | otherwise = Just (Scalar x)
    isNotZeroTensorU20 (TensorU20 EmptyTree) = Nothing
    isNotZeroTensorU20 (TensorL20 EmptyTree) = Nothing
    isNotZeroTensorU20 (TensorU19 EmptyTree) = Nothing
    isNotZeroTensorU20 (TensorL19 EmptyTree) = Nothing
    isNotZeroTensorU20 (TensorU9 EmptyTree) = Nothing
    isNotZeroTensorU20 (TensorL9 EmptyTree) = Nothing
    isNotZeroTensorU20 (TensorU3 EmptyTree) = Nothing
    isNotZeroTensorU20 (TensorL3 EmptyTree) = Nothing
    isNotZeroTensorU20 t = Just t

    isNotZeroTensorL20 :: (Num a, Eq a, Show a) => Tensor 0 n2 n3 n4 n5 n6 n7 n8 a -> Maybe (Tensor 0 n2 n3 n4 n5 n6 n7 n8 a)
    isNotZeroTensorL20 (Scalar x) 
                | x == 0 = Nothing
                | otherwise = Just (Scalar x)
    isNotZeroTensorL20 (TensorL20 EmptyTree) = Nothing
    isNotZeroTensorL20 (TensorU19 EmptyTree) = Nothing
    isNotZeroTensorL20 (TensorL19 EmptyTree) = Nothing
    isNotZeroTensorL20 (TensorU9 EmptyTree) = Nothing
    isNotZeroTensorL20 (TensorL9 EmptyTree) = Nothing
    isNotZeroTensorL20 (TensorU3 EmptyTree) = Nothing
    isNotZeroTensorL20 (TensorL3 EmptyTree) = Nothing
    isNotZeroTensorL20 t = Just t

    isNotZeroTensorU19 :: (Num a, Eq a, Show a) => Tensor 0 0 n3 n4 n5 n6 n7 n8 a -> Maybe (Tensor 0 0 n3 n4 n5 n6 n7 n8 a)
    isNotZeroTensorU19 (Scalar x) 
                | x == 0 = Nothing
                | otherwise = Just (Scalar x)
    isNotZeroTensorU19 (TensorU19 EmptyTree) = Nothing
    isNotZeroTensorU19 (TensorL19 EmptyTree) = Nothing
    isNotZeroTensorU19 (TensorU9 EmptyTree) = Nothing
    isNotZeroTensorU19 (TensorL9 EmptyTree) = Nothing
    isNotZeroTensorU19 (TensorU3 EmptyTree) = Nothing
    isNotZeroTensorU19 (TensorL3 EmptyTree) = Nothing
    isNotZeroTensorU19 t = Just t

    isNotZeroTensorL19 :: (Num a, Eq a, Show a) => Tensor 0 0 0 n4 n5 n6 n7 n8 a -> Maybe (Tensor 0 0 0 n4 n5 n6 n7 n8 a)
    isNotZeroTensorL19 (Scalar x) 
                | x == 0 = Nothing
                | otherwise = Just (Scalar x)
    isNotZeroTensorL19 (TensorL19 EmptyTree) = Nothing
    isNotZeroTensorL19 (TensorU9 EmptyTree) = Nothing
    isNotZeroTensorL19 (TensorL9 EmptyTree) = Nothing
    isNotZeroTensorL19 (TensorU3 EmptyTree) = Nothing
    isNotZeroTensorL19 (TensorL3 EmptyTree) = Nothing
    isNotZeroTensorL19 t = Just t

    isNotZeroTensorU9 :: (Num a, Eq a, Show a) => Tensor 0 0 0 0 n5 n6 n7 n8 a -> Maybe (Tensor 0 0 0 0 n5 n6 n7 n8 a)
    isNotZeroTensorU9 (Scalar x) 
                | x == 0 = Nothing
                | otherwise = Just (Scalar x)
    isNotZeroTensorU9 (TensorU9 EmptyTree) = Nothing
    isNotZeroTensorU9 (TensorL9 EmptyTree) = Nothing
    isNotZeroTensorU9 (TensorU3 EmptyTree) = Nothing
    isNotZeroTensorU9 (TensorL3 EmptyTree) = Nothing
    isNotZeroTensorU9 t = Just t

    isNotZeroTensorL9 :: (Num a, Eq a, Show a) => Tensor 0 0 0 0 0 n6 n7 n8 a -> Maybe (Tensor 0 0 0 0 0 n6 n7 n8 a)
    isNotZeroTensorL9 (Scalar x) 
                | x == 0 = Nothing
                | otherwise = Just (Scalar x)
    isNotZeroTensorL9 (TensorL9 EmptyTree) = Nothing
    isNotZeroTensorL9 (TensorU3 EmptyTree) = Nothing
    isNotZeroTensorL9 (TensorL3 EmptyTree) = Nothing
    isNotZeroTensorL9 t = Just t

    isNotZeroTensorU3 :: (Num a, Eq a, Show a) => Tensor 0 0 0 0 0 0 n7 n8 a -> Maybe (Tensor 0 0 0 0 0 0 n7 n8 a)
    isNotZeroTensorU3 (Scalar x) 
                | x == 0 = Nothing
                | otherwise = Just (Scalar x)
    isNotZeroTensorU3 (TensorU3 EmptyTree) = Nothing
    isNotZeroTensorU3 (TensorL3 EmptyTree) = Nothing
    isNotZeroTensorU3 t = Just t

    isNotZeroTensorL3 :: (Num a, Eq a, Show a) => Tensor 0 0 0 0 0 0 0 n8 a -> Maybe (Tensor 0 0 0 0 0 0 0 n8 a)
    isNotZeroTensorL3 (Scalar x) 
                | x == 0 = Nothing
                | otherwise = Just (Scalar x)
    isNotZeroTensorL3 (TensorL3 EmptyTree) = Nothing
    isNotZeroTensorL3 t = Just t

    --maybe it is better to first map and then convert toAscList
    
    toListU20 :: Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> [(IndList n1 Uind_20, Tensor 0 n2 n3 n4 n5 n6 n7 n8 a)]
    toListU20 (TensorU20 l) = concat $ map (\(i,t) -> appendF i $ toListU20 t) $ toAscList l
            where
                appendF = \i l2 -> map (\(l,val) -> (Append i l ,val)) l2
    toListU20 (TensorL20 l) = [(Empty, TensorL20 l)]
    toListU20 (TensorU19 l) = [(Empty, TensorU19 l)]
    toListU20 (TensorL19 l) = [(Empty, TensorL19 l)]
    toListU20 (TensorU9 l) = [(Empty, TensorU9 l)]
    toListU20 (TensorL9 l) = [(Empty, TensorL9 l)]
    toListU20 (TensorU3 l) = [(Empty, TensorU3 l)]
    toListU20 (TensorL3 l) = [(Empty, TensorL3 l)]
    toListU20 (Scalar l) = [(Empty, Scalar l)]
            
    toListL20 :: Tensor 0 n2 n3 n4 n5 n6 n7 n8 a -> [(IndList n2 Lind_20, Tensor 0 0 n3 n4 n5 n6 n7 n8 a)]
    toListL20 (TensorL20 l) = concat $ map (\(i,t) -> appendF i $ toListL20 t) $ toAscList l
            where
                appendF = \i l2 -> map (\(l,val) -> (Append i l ,val)) l2
    toListL20 (TensorU19 l) = [(Empty, TensorU19 l)]
    toListL20 (TensorL19 l) = [(Empty, TensorL19 l)]
    toListL20 (TensorU9 l) = [(Empty, TensorU9 l)]
    toListL20 (TensorL9 l) = [(Empty, TensorL9 l)]
    toListL20 (TensorU3 l) = [(Empty, TensorU3 l)]
    toListL20 (TensorL3 l) = [(Empty, TensorL3 l)]
    toListL20 (Scalar l) = [(Empty, Scalar l)]

    toListU19 :: Tensor 0 0 n3 n4 n5 n6 n7 n8 a -> [(IndList n3 Uind_19, Tensor 0 0 0 n4 n5 n6 n7 n8 a)]
    toListU19 (TensorU19 l) = concat $ map (\(i,t) -> appendF i $ toListU19 t) $ toAscList l
            where
                appendF = \i l2 -> map (\(l,val) -> (Append i l ,val)) l2
    toListU19 (TensorL19 l) = [(Empty, TensorL19 l)]
    toListU19 (TensorU9 l) = [(Empty, TensorU9 l)]
    toListU19 (TensorL9 l) = [(Empty, TensorL9 l)]
    toListU19 (TensorU3 l) = [(Empty, TensorU3 l)]
    toListU19 (TensorL3 l) = [(Empty, TensorL3 l)]
    toListU19 (Scalar l) = [(Empty, Scalar l)]

    toListL19 :: Tensor 0 0 0 n4 n5 n6 n7 n8 a -> [(IndList n4 Lind_19, Tensor 0 0 0 0 n5 n6 n7 n8 a)]
    toListL19 (TensorL19 l) = concat $ map (\(i,t) -> appendF i $ toListL19 t) $ toAscList l
            where
                appendF = \i l2 -> map (\(l,val) -> (Append i l ,val)) l2
    toListL19 (TensorU9 l) = [(Empty, TensorU9 l)]
    toListL19 (TensorL9 l) = [(Empty, TensorL9 l)]
    toListL19 (TensorU3 l) = [(Empty, TensorU3 l)]
    toListL19 (TensorL3 l) = [(Empty, TensorL3 l)]
    toListL19 (Scalar l) = [(Empty, Scalar l)]

    toListU9 :: Tensor 0 0 0 0 n5 n6 n7 n8 a -> [(IndList n5 Uind_9, Tensor 0 0 0 0 0 n6 n7 n8 a)]
    toListU9 (TensorU9 l) = concat $ map (\(i,t) -> appendF i $ toListU9 t) $ toAscList l
            where
                appendF = \i l2 -> map (\(l,val) -> (Append i l ,val)) l2
    toListU9 (TensorL9 l) = [(Empty, TensorL9 l)]
    toListU9 (TensorU3 l) = [(Empty, TensorU3 l)]
    toListU9 (TensorL3 l) = [(Empty, TensorL3 l)]
    toListU9 (Scalar l) = [(Empty, Scalar l)]

    toListL9 :: Tensor 0 0 0 0 0 n6 n7 n8 a -> [(IndList n6 Lind_9, Tensor 0 0 0 0 0 0 n7 n8 a)]
    toListL9 (TensorL9 l) = concat $ map (\(i,t) -> appendF i $ toListL9 t) $ toAscList l
            where
                appendF = \i l2 -> map (\(l,val) -> (Append i l ,val)) l2
    toListL9 (TensorU3 l) = [(Empty, TensorU3 l)]
    toListL9 (TensorL3 l) = [(Empty, TensorL3 l)]
    toListL9 (Scalar l) = [(Empty, Scalar l)]

    toListU3 :: Tensor 0 0 0 0 0 0 n7 n8 a -> [(IndList n7 Uind_3, Tensor 0 0 0 0 0 0 0 n8 a)]
    toListU3 (TensorU3 l) = concat $ map (\(i,t) -> appendF i $ toListU3 t) $ toAscList l
            where
                appendF = \i l2 -> map (\(l,val) -> (Append i l ,val)) l2
    toListU3 (TensorL3 l) = [(Empty, TensorL3 l)]
    toListU3 (Scalar l) = [(Empty, Scalar l)]

    toListL3 :: Tensor 0 0 0 0 0 0 0 n8 a -> [(IndList n8 Lind_3, Tensor 0 0 0 0 0 0 0 0 a)]
    toListL3 (TensorL3 l) = concat $ map (\(i,t) -> appendF i $ toListL3 t) $ toAscList l
            where
                appendF = \i l2 -> map (\(l,val) -> (Append i l ,val)) l2
    toListL3 (Scalar l) = [(Empty, Scalar l)]

    type Index n1 n2 n3 n4 n5 n6 n7 n8 =
        (IndList n1 Uind_20, IndList n2 Lind_20, IndList n3 Uind_19, IndList n4 Lind_19, IndList n5 Uind_9, IndList n6 Lind_9, IndList n7 Uind_3, IndList n8 Lind_3) 

    toListT :: Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> [(Index n1 n2 n3 n4 n5 n6 n7 n8, a)]
    toListT t = map (\(x,Scalar y) -> (x,y) ) $ concat $ map (\(x,y) -> appendT7 x $ toListL3 y ) $ concat $ map (\(x,y) -> appendT6 x $ toListU3 y ) $ concat $ map (\(x,y) -> appendT5 x $ toListL9 y ) $ concat $ map (\(x,y) -> appendT4 x $ toListU9 y ) $ concat $ map (\(x,y) -> appendT3 x $ toListL19 y ) $ concat $ map (\(x,y) -> appendT2 x $ toListU19 y ) $ concat $ map (\(x,y) -> appendT1 x $ toListL20 y ) $ toListU20 t
            where
                appendT1 = \i l -> map (\(x,y) -> ((i,x),y)) l
                appendT2 = \(i1,i2) l -> map (\(x,y) -> ((i1,i2,x),y)) l
                appendT3 = \(i1,i2,i3) l -> map (\(x,y) -> ((i1,i2,i3,x),y)) l
                appendT4 = \(i1,i2,i3,i4) l -> map (\(x,y) -> ((i1,i2,i3,i4,x),y)) l
                appendT5 = \(i1,i2,i3,i4,i5) l -> map (\(x,y) -> ((i1,i2,i3,i4,i5,x),y)) l
                appendT6 = \(i1,i2,i3,i4,i5,i6) l -> map (\(x,y) -> ((i1,i2,i3,i4,i5,i6,x),y)) l
                appendT7 = \(i1,i2,i3,i4,i5,i6,i7) l -> map (\(x,y) -> ((i1,i2,i3,i4,i5,i6,i7,x),y)) l

    toListShow :: Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> [([Int],a)]
    toListShow t = map (\(x,y) -> (showInd x, y)) l
            where
                l = toListT t 
                showInd (i1,i2,i3,i4,i5,i6,i7,i8) = (map fromEnum $ toList i1) ++ (map fromEnum $ toList i2) ++ (map fromEnum $ toList i3) ++ (map fromEnum $ toList i4) ++ (map fromEnum $ toList i5) ++ (map fromEnum $ toList i6) ++ (map fromEnum $ toList i7) ++ (map fromEnum $ toList i8) 

    toListShowIndex :: Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> [(([Int],[Int],[Int],[Int],[Int],[Int],[Int],[Int]),a)]
    toListShowIndex t = map (\(x,y) -> (showInd x, y)) l
            where
                l = toListT t 
                showInd (i1,i2,i3,i4,i5,i6,i7,i8) = ((map fromEnum $ toList i1), (map fromEnum $ toList i2), (map fromEnum $ toList i3), (map fromEnum $ toList i4), (map fromEnum $ toList i5), (map fromEnum $ toList i6), (map fromEnum $ toList i7), (map fromEnum $ toList i8)) 


    fromListT :: (Num a, Eq a, Show a) => [(Index n1 n2 n3 n4 n5 n6 n7 n8, a)] -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    fromListT (x:[]) = mkTens x 
    fromListT (x:xs) = foldr insertOrAdd (mkTens x) xs

    
    tensorAddU20 :: (Num a, Eq a, Show a) => Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorAddU20 (TensorU20 EmptyTree) t = t
    tensorAddU20 t (TensorU20 EmptyTree) = t
    tensorAddU20 (TensorU20 t1) (TensorU20 t2) = TensorU20 $ unionTreeWithMaybe (\x y -> isNotZeroTensorU20 $ tensorAddU20 x y) t1 t2 
    tensorAddU20 (TensorL20 t1) (TensorL20 t2) = tensorAddL20 (TensorL20 t1) (TensorL20 t2)
    tensorAddU20 (TensorU19 t1) (TensorU19 t2) = tensorAddU19 (TensorU19 t1) (TensorU19 t2)
    tensorAddU20 (TensorL19 t1) (TensorL19 t2) = tensorAddL19 (TensorL19 t1) (TensorL19 t2)
    tensorAddU20 (TensorU9 t1) (TensorU9 t2) = tensorAddU9 (TensorU9 t1) (TensorU9 t2)
    tensorAddU20 (TensorL9 t1) (TensorL9 t2) = tensorAddL9 (TensorL9 t1) (TensorL9 t2)
    tensorAddU20 (TensorU3 t1) (TensorU3 t2) = tensorAddU3 (TensorU3 t1) (TensorU3 t2)
    tensorAddU20 (TensorL3 t1) (TensorL3 t2) = tensorAddL3 (TensorL3 t1) (TensorL3 t2)
    tensorAddU20 (Scalar s1) (Scalar s2) = Scalar (s1+s2)

    tensorAddL20 :: (Num a, Eq a, Show a) => Tensor 0 n2 n3 n4 n5 n6 n7 n8 a -> Tensor 0 n2 n3 n4 n5 n6 n7 n8 a -> Tensor 0 n2 n3 n4 n5 n6 n7 n8 a
    tensorAddL20 (TensorL20 EmptyTree) t = t
    tensorAddL20 t (TensorL20 EmptyTree) = t
    tensorAddL20 (TensorL20 t1) (TensorL20 t2) = TensorL20 $ unionTreeWithMaybe (\x y -> isNotZeroTensorL20 $ tensorAddL20 x y) t1 t2 
    tensorAddL20 (TensorU19 t1) (TensorU19 t2) = tensorAddU19 (TensorU19 t1) (TensorU19 t2)
    tensorAddL20 (TensorL19 t1) (TensorL19 t2) = tensorAddL19 (TensorL19 t1) (TensorL19 t2)
    tensorAddL20 (TensorU9 t1) (TensorU9 t2) = tensorAddU9 (TensorU9 t1) (TensorU9 t2)
    tensorAddL20 (TensorL9 t1) (TensorL9 t2) = tensorAddL9 (TensorL9 t1) (TensorL9 t2)
    tensorAddL20 (TensorU3 t1) (TensorU3 t2) = tensorAddU3 (TensorU3 t1) (TensorU3 t2)
    tensorAddL20 (TensorL3 t1) (TensorL3 t2) = tensorAddL3 (TensorL3 t1) (TensorL3 t2)
    tensorAddL20 (Scalar s1) (Scalar s2) = Scalar (s1+s2)

    tensorAddU19 :: (Num a, Eq a, Show a) => Tensor 0 0 n3 n4 n5 n6 n7 n8 a -> Tensor 0 0 n3 n4 n5 n6 n7 n8 a -> Tensor 0 0 n3 n4 n5 n6 n7 n8 a
    tensorAddU19 (TensorU19 EmptyTree) t = t
    tensorAddU19 t (TensorU19 EmptyTree) = t
    tensorAddU19 (TensorU19 t1) (TensorU19 t2) = TensorU19 $ unionTreeWithMaybe (\x y -> isNotZeroTensorU19 $ tensorAddU19 x y) t1 t2 
    tensorAddU19 (TensorL19 t1) (TensorL19 t2) = tensorAddL19 (TensorL19 t1) (TensorL19 t2)
    tensorAddU19 (TensorU9 t1) (TensorU9 t2) = tensorAddU9 (TensorU9 t1) (TensorU9 t2)
    tensorAddU19 (TensorL9 t1) (TensorL9 t2) = tensorAddL9 (TensorL9 t1) (TensorL9 t2)
    tensorAddU19 (TensorU3 t1) (TensorU3 t2) = tensorAddU3 (TensorU3 t1) (TensorU3 t2)
    tensorAddU19 (TensorL3 t1) (TensorL3 t2) = tensorAddL3 (TensorL3 t1) (TensorL3 t2)
    tensorAddU19 (Scalar s1) (Scalar s2) = Scalar (s1+s2)

    tensorAddL19 :: (Num a, Eq a, Show a) => Tensor 0 0 0 n4 n5 n6 n7 n8 a -> Tensor 0 0 0 n4 n5 n6 n7 n8 a -> Tensor 0 0 0 n4 n5 n6 n7 n8 a
    tensorAddL19 (TensorL19 EmptyTree) t = t
    tensorAddL19 t (TensorL19 EmptyTree) = t
    tensorAddL19 (TensorL19 t1) (TensorL19 t2) = TensorL19 $ unionTreeWithMaybe (\x y -> isNotZeroTensorL19 $ tensorAddL19 x y) t1 t2 
    tensorAddL19 (TensorU9 t1) (TensorU9 t2) = tensorAddU9 (TensorU9 t1) (TensorU9 t2)
    tensorAddL19 (TensorL9 t1) (TensorL9 t2) = tensorAddL9 (TensorL9 t1) (TensorL9 t2)
    tensorAddL19 (TensorU3 t1) (TensorU3 t2) = tensorAddU3 (TensorU3 t1) (TensorU3 t2)
    tensorAddL19 (TensorL3 t1) (TensorL3 t2) = tensorAddL3 (TensorL3 t1) (TensorL3 t2)
    tensorAddL19 (Scalar s1) (Scalar s2) = Scalar (s1+s2)

    tensorAddU9 :: (Num a, Eq a, Show a) => Tensor 0 0 0 0 n5 n6 n7 n8 a -> Tensor 0 0 0 0 n5 n6 n7 n8 a -> Tensor 0 0 0 0 n5 n6 n7 n8 a
    tensorAddU9 (TensorU9 EmptyTree) t = t
    tensorAddU9 t (TensorU9 EmptyTree) = t
    tensorAddU9 (TensorU9 t1) (TensorU9 t2) = TensorU9 $ unionTreeWithMaybe (\x y -> isNotZeroTensorU9 $ tensorAddU9 x y) t1 t2 
    tensorAddU9 (TensorL9 t1) (TensorL9 t2) = tensorAddL9 (TensorL9 t1) (TensorL9 t2)
    tensorAddU9 (TensorU3 t1) (TensorU3 t2) = tensorAddU3 (TensorU3 t1) (TensorU3 t2)
    tensorAddU9 (TensorL3 t1) (TensorL3 t2) = tensorAddL3 (TensorL3 t1) (TensorL3 t2)
    tensorAddU9 (Scalar s1) (Scalar s2) = Scalar (s1+s2)

    tensorAddL9 :: (Num a, Eq a, Show a) => Tensor 0 0 0 0 0 n6 n7 n8 a -> Tensor 0 0 0 0 0 n6 n7 n8 a -> Tensor 0 0 0 0 0 n6 n7 n8 a
    tensorAddL9 (TensorL9 EmptyTree) t = t
    tensorAddL9 t (TensorL9 EmptyTree) = t
    tensorAddL9 (TensorL9 t1) (TensorL9 t2) = TensorL9 $ unionTreeWithMaybe (\x y -> isNotZeroTensorL9 $ tensorAddL9 x y) t1 t2 
    tensorAddL9 (TensorU3 t1) (TensorU3 t2) = tensorAddU3 (TensorU3 t1) (TensorU3 t2)
    tensorAddL9 (TensorL3 t1) (TensorL3 t2) = tensorAddL3 (TensorL3 t1) (TensorL3 t2)
    tensorAddL9 (Scalar s1) (Scalar s2) = Scalar (s1+s2)

    tensorAddU3 :: (Num a, Eq a, Show a) => Tensor 0 0 0 0 0 0 n7 n8 a -> Tensor 0 0 0 0 0 0 n7 n8 a -> Tensor 0 0 0 0 0 0 n7 n8 a
    tensorAddU3 (TensorU3 EmptyTree) t = t
    tensorAddU3 t (TensorU3 EmptyTree) = t
    tensorAddU3 (TensorU3 t1) (TensorU3 t2) = TensorU3 $ unionTreeWithMaybe (\x y -> isNotZeroTensorU3 $ tensorAddU3 x y) t1 t2 
    tensorAddU3 (TensorL3 t1) (TensorL3 t2) = tensorAddL3 (TensorL3 t1) (TensorL3 t2)
    tensorAddU3 (Scalar s1) (Scalar s2) = Scalar (s1+s2)

    tensorAddL3 :: (Num a, Eq a, Show a) => Tensor 0 0 0 0 0 0 0 n8 a -> Tensor 0 0 0 0 0 0 0 n8 a -> Tensor 0 0 0 0 0 0 0 n8 a
    tensorAddL3 (TensorL3 EmptyTree) t = t
    tensorAddL3 t (TensorL3 EmptyTree) = t
    tensorAddL3 (TensorL3 t1) (TensorL3 t2) = TensorL3 $ unionTreeWithMaybe (\x y -> isNotZeroTensorL3 $ tensorAddL3 x y) t1 t2 
    tensorAddL3 (Scalar s1) (Scalar s2) = Scalar (s1+s2)

    tensorAdd :: (Num a, Eq a, Show a) => Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorAdd (TensorU20 t1) (TensorU20 t2) =  tensorAddU20 (TensorU20 t1) (TensorU20 t2)
    tensorAdd (TensorL20 t1) (TensorL20 t2) =  tensorAddL20 (TensorL20 t1) (TensorL20 t2)
    tensorAdd (TensorU19 t1) (TensorU19 t2) =  tensorAddU19 (TensorU19 t1) (TensorU19 t2)
    tensorAdd (TensorL19 t1) (TensorL19 t2) =  tensorAddL19 (TensorL19 t1) (TensorL19 t2)
    tensorAdd (TensorU9 t1) (TensorU9 t2) =  tensorAddU9 (TensorU9 t1) (TensorU9 t2)
    tensorAdd (TensorL9 t1) (TensorL9 t2) =  tensorAddL9 (TensorL9 t1) (TensorL9 t2)
    tensorAdd (TensorU3 t1) (TensorU3 t2) =  tensorAddU3 (TensorU3 t1) (TensorU3 t2)
    tensorAdd (TensorL3 t1) (TensorL3 t2) =  tensorAddL3 (TensorL3 t1) (TensorL3 t2)
    tensorAdd (Scalar s1) (Scalar s2) = Scalar (s1+s2)

    tensorSmultU20 :: (Num a) => a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorSmultU20 s (TensorU20 t) = TensorU20 $ fmap (tensorSmultU20 s) t
    tensorSmultU20 s (TensorL20 t) = tensorSmultL20 s (TensorL20 t)
    tensorSmultU20 s (TensorU19 t) = tensorSmultU19 s (TensorU19 t)
    tensorSmultU20 s (TensorL19 t) = tensorSmultL19 s (TensorL19 t)
    tensorSmultU20 s (TensorU9 t) = tensorSmultU9 s (TensorU9 t)
    tensorSmultU20 s (TensorL9 t) = tensorSmultL9 s (TensorL9 t)
    tensorSmultU20 s (TensorU3 t) = tensorSmultU3 s (TensorU3 t)
    tensorSmultU20 s (TensorL3 t) = tensorSmultL3 s (TensorL3 t)
    tensorSmultU20 s (Scalar t) = Scalar (s*t)

    tensorSmultL20 :: (Num a) => a -> Tensor 0 n2 n3 n4 n5 n6 n7 n8 a -> Tensor 0 n2 n3 n4 n5 n6 n7 n8 a
    tensorSmultL20 s (TensorL20 t) = TensorL20 $ fmap (tensorSmultL20 s) t
    tensorSmultL20 s (TensorU19 t) = tensorSmultU19 s (TensorU19 t)
    tensorSmultL20 s (TensorL19 t) = tensorSmultL19 s (TensorL19 t)
    tensorSmultL20 s (TensorU9 t) = tensorSmultU9 s (TensorU9 t)
    tensorSmultL20 s (TensorL9 t) = tensorSmultL9 s (TensorL9 t)
    tensorSmultL20 s (TensorU3 t) = tensorSmultU3 s (TensorU3 t)
    tensorSmultL20 s (TensorL3 t) = tensorSmultL3 s (TensorL3 t)
    tensorSmultL20 s (Scalar t) = Scalar (s*t)

    tensorSmultU19 :: (Num a) => a -> Tensor 0 0 n3 n4 n5 n6 n7 n8 a -> Tensor 0 0 n3 n4 n5 n6 n7 n8 a
    tensorSmultU19 s (TensorU19 t) = TensorU19 $ fmap (tensorSmultU19 s) t
    tensorSmultU19 s (TensorL19 t) = tensorSmultL19 s (TensorL19 t)
    tensorSmultU19 s (TensorU9 t) = tensorSmultU9 s (TensorU9 t)
    tensorSmultU19 s (TensorL9 t) = tensorSmultL9 s (TensorL9 t)
    tensorSmultU19 s (TensorU3 t) = tensorSmultU3 s (TensorU3 t)
    tensorSmultU19 s (TensorL3 t) = tensorSmultL3 s (TensorL3 t)
    tensorSmultU19 s (Scalar t) = Scalar (s*t)

    tensorSmultL19 :: (Num a) => a -> Tensor 0 0 0 n4 n5 n6 n7 n8 a -> Tensor 0 0 0 n4 n5 n6 n7 n8 a
    tensorSmultL19 s (TensorL19 t) = TensorL19 $ fmap (tensorSmultL19 s) t
    tensorSmultL19 s (TensorU9 t) = tensorSmultU9 s (TensorU9 t)
    tensorSmultL19 s (TensorL9 t) = tensorSmultL9 s (TensorL9 t)
    tensorSmultL19 s (TensorU3 t) = tensorSmultU3 s (TensorU3 t)
    tensorSmultL19 s (TensorL3 t) = tensorSmultL3 s (TensorL3 t)
    tensorSmultL19 s (Scalar t) = Scalar (s*t)

    tensorSmultU9 :: (Num a) => a -> Tensor 0 0 0 0 n5 n6 n7 n8 a -> Tensor 0 0 0 0 n5 n6 n7 n8 a
    tensorSmultU9 s (TensorU9 t) = TensorU9 $ fmap (tensorSmultU9 s) t
    tensorSmultU9 s (TensorL9 t) = tensorSmultL9 s (TensorL9 t)
    tensorSmultU9 s (TensorU3 t) = tensorSmultU3 s (TensorU3 t)
    tensorSmultU9 s (TensorL3 t) = tensorSmultL3 s (TensorL3 t)
    tensorSmultU9 s (Scalar t) = Scalar (s*t)

    tensorSmultL9 :: (Num a) => a -> Tensor 0 0 0 0 0 n6 n7 n8 a -> Tensor 0 0 0 0 0 n6 n7 n8 a
    tensorSmultL9 s (TensorL9 t) = TensorL9 $ fmap (tensorSmultL9 s) t
    tensorSmultL9 s (TensorU3 t) = tensorSmultU3 s (TensorU3 t)
    tensorSmultL9 s (TensorL3 t) = tensorSmultL3 s (TensorL3 t)
    tensorSmultL9 s (Scalar t) = Scalar (s*t)

    tensorSmultU3 :: (Num a) => a -> Tensor 0 0 0 0 0 0 n7 n8 a -> Tensor 0 0 0 0 0 0 n7 n8 a
    tensorSmultU3 s (TensorU3 t) = TensorU3 $ fmap (tensorSmultU3 s) t
    tensorSmultU3 s (TensorL3 t) = tensorSmultL3 s (TensorL3 t)
    tensorSmultU3 s (Scalar t) = Scalar (s*t)

    tensorSmultL3 :: (Num a) => a -> Tensor 0 0 0 0 0 0 0 n8 a -> Tensor 0 0 0 0 0 0 0 n8 a
    tensorSmultL3 s (TensorL3 t) = TensorL3 $ fmap (tensorSmultL3 s) t
    tensorSmultL3 s (Scalar t) = Scalar (s*t)

    tensorSMult :: (Num a) => a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorSMult s (TensorU20 t) = tensorSmultU20 s (TensorU20 t)
    tensorSMult s (TensorL20 t) = tensorSmultL20 s (TensorL20 t)
    tensorSMult s (TensorU19 t) = tensorSmultU19 s (TensorU19 t)
    tensorSMult s (TensorL19 t) = tensorSmultL19 s (TensorL19 t)
    tensorSMult s (TensorU9 t) = tensorSmultU9 s (TensorU9 t)
    tensorSMult s (TensorL9 t) = tensorSmultL9 s (TensorL9 t)
    tensorSMult s (TensorU3 t) = tensorSmultU3 s (TensorU3 t)
    tensorSMult s (TensorL3 t) = tensorSmultL3 s (TensorL3 t)
    tensorSMult s (Scalar t) = Scalar (s*t)

    tensorSub :: (Num a, Eq a, Show a) => Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorSub t1 t2 = tensorAdd t1 $ tensorSMult (-1) t2 

    mkTensU20 :: (IndList n1 Uind_20, Tensor 0 n2 n3 n4 n5 n6 n7 n8 a) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    mkTensU20 (Empty, t) = t
    mkTensU20 (Append x xs, t) = TensorU20 $ Leaf x (mkTensU20 (xs, t))

    insertOrAddU20 :: (Num a, Eq a, Show a) => (IndList n1 Uind_20, Tensor 0 n2 n3 n4 n5 n6 n7 n8 a) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a  -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    insertOrAddU20 ind (TensorU20 EmptyTree) = mkTensU20 ind 
    insertOrAddU20 (Append l ls, t1) (TensorU20 t2)  = TensorU20 $ insertTreeWith (\x y -> tensorAdd x y) l (mkTensU20 (ls, t1)) t2
    insertOrAddU20 (Empty, t) t' = tensorAdd t t'

    mkTensL20 :: (IndList n2 Lind_20, Tensor 0 0 n3 n4 n5 n6 n7 n8 a) -> Tensor 0 n2 n3 n4 n5 n6 n7 n8 a
    mkTensL20 (Empty, t) = t
    mkTensL20 (Append x xs, t) = TensorL20 $ Leaf x (mkTensL20 (xs, t) )

    insertOrAddL20 :: (Num a, Eq a, Show a) => (IndList n2 Lind_20, Tensor 0 0 n3 n4 n5 n6 n7 n8 a) -> Tensor 0 n2 n3 n4 n5 n6 n7 n8 a  -> Tensor 0 n2 n3 n4 n5 n6 n7 n8 a
    insertOrAddL20 ind (TensorL20 EmptyTree) = mkTensL20 ind 
    insertOrAddL20 (Append l ls, t1) (TensorL20 t2) =  TensorL20 $ insertTreeWith (\ x y -> tensorAddL20 x y) l (mkTensL20 (ls, t1)) t2
    insertOrAddL20 (Empty, t) t' = tensorAdd t t'

    mkTensU19 :: (IndList n3 Uind_19, Tensor 0 0 0 n4 n5 n6 n7 n8 a) -> Tensor 0 0 n3 n4 n5 n6 n7 n8 a
    mkTensU19 (Empty, t) = t
    mkTensU19 (Append x xs, t) = TensorU19 $ Leaf x (mkTensU19 (xs, t) )

    insertOrAddU19 :: (Num a, Eq a, Show a) => (IndList n3 Uind_19, Tensor 0 0 0 n4 n5 n6 n7 n8 a) -> Tensor 0 0 n3 n4 n5 n6 n7 n8 a  -> Tensor 0 0 n3 n4 n5 n6 n7 n8 a
    insertOrAddU19 ind (TensorU19 EmptyTree) = mkTensU19 ind 
    insertOrAddU19 (Append l ls, t1) (TensorU19 t2) =  TensorU19 $ insertTreeWith (\x y -> tensorAddU19 x y) l (mkTensU19 (ls, t1)) t2
    insertOrAddU19 (Empty, t) t' = tensorAdd t t'

    mkTensL19 :: (IndList n4 Lind_19, Tensor 0 0 0 0 n5 n6 n7 n8 a) -> Tensor 0 0 0 n4 n5 n6 n7 n8 a
    mkTensL19 (Empty, t) = t
    mkTensL19 (Append x xs, t) = TensorL19 $ Leaf x (mkTensL19 (xs, t) )

    insertOrAddL19 :: (Num a, Eq a, Show a) => (IndList n4 Lind_19, Tensor 0 0 0 0 n5 n6 n7 n8 a) -> Tensor 0 0 0 n4 n5 n6 n7 n8 a  -> Tensor 0 0 0 n4 n5 n6 n7 n8 a
    insertOrAddL19 ind (TensorL19 EmptyTree) = mkTensL19 ind 
    insertOrAddL19 (Append l ls, t1) (TensorL19 t2) =  TensorL19 $ insertTreeWith (\x y -> tensorAddL19 x y) l (mkTensL19 (ls, t1)) t2
    insertOrAddL19 (Empty, t) t' = tensorAdd t t'
   

    mkTensU9 :: (IndList n5 Uind_9, Tensor 0 0 0 0 0 n6 n7 n8 a) -> Tensor 0 0 0 0 n5 n6 n7 n8 a
    mkTensU9 (Empty, t) = t
    mkTensU9 (Append x xs, t) = TensorU9 $ Leaf x (mkTensU9 (xs, t) )

    insertOrAddU9 :: (Num a, Eq a, Show a) => (IndList n5 Uind_9, Tensor 0 0 0 0 0 n6 n7 n8 a) -> Tensor 0 0 0 0 n5 n6 n7 n8 a  -> Tensor 0 0 0 0 n5 n6 n7 n8 a
    insertOrAddU9 ind (TensorU9 EmptyTree) = mkTensU9 ind 
    insertOrAddU9 (Append l ls, t1) (TensorU9 t2) =  TensorU9 $ insertTreeWith (\x y -> tensorAddU9 x y) l (mkTensU9 (ls, t1)) t2
    insertOrAddU9 (Empty, t) t' = tensorAdd t t'
    
    mkTensL9 :: (IndList n6 Lind_9, Tensor 0 0 0 0 0 0 n7 n8 a) -> Tensor 0 0 0 0 0 n6 n7 n8 a
    mkTensL9 (Empty, t) = t
    mkTensL9 (Append x xs, t) = TensorL9 $ Leaf x (mkTensL9 (xs, t) )

    insertOrAddL9 :: (Num a, Eq a, Show a) => (IndList n6 Lind_9, Tensor 0 0 0 0 0 0 n7 n8 a) -> Tensor 0 0 0 0 0 n6 n7 n8 a  -> Tensor 0 0 0 0 0 n6 n7 n8 a
    insertOrAddL9 ind (TensorL9 EmptyTree) = mkTensL9 ind 
    insertOrAddL9 (Append l ls, t1) (TensorL9 t2) =  TensorL9 $ insertTreeWith (\x y -> tensorAddL9 x y) l (mkTensL9 (ls, t1)) t2
    insertOrAddL9 (Empty, t) t' = tensorAdd t t'
    
    mkTensU3 :: (IndList n7 Uind_3, Tensor 0 0 0 0 0 0 0 n8 a) -> Tensor 0 0 0 0 0 0 n7 n8 a
    mkTensU3 (Empty, t) = t
    mkTensU3 (Append x xs, t) = TensorU3 $ Leaf x (mkTensU3 (xs, t) )

    insertOrAddU3 :: (Num a, Eq a, Show a) => (IndList n7 Uind_3, Tensor 0 0 0 0 0 0 0 n8 a) -> Tensor 0 0 0 0 0 0 n7 n8 a  -> Tensor 0 0 0 0 0 0 n7 n8 a
    insertOrAddU3 ind (TensorU3 EmptyTree) = mkTensU3 ind 
    insertOrAddU3 (Append l ls, t1) (TensorU3 t2) =  TensorU3 $ insertTreeWith (\x y -> tensorAddU3 x y) l (mkTensU3 (ls, t1)) t2
    insertOrAddU3 (Empty, t) t' = tensorAdd t t'

    mkTensL3 :: (IndList n8 Lind_3, Tensor 0 0 0 0 0 0 0 0 a) -> Tensor 0 0 0 0 0 0 0 n8 a
    mkTensL3 (Empty, t) = t
    mkTensL3 (Append x xs, t) = TensorL3 $ Leaf x (mkTensL3 (xs, t) )

    insertOrAddL3 :: (Num a, Eq a, Show a) => (IndList n8 Lind_3, Tensor 0 0 0 0 0 0 0 0 a) -> Tensor 0 0 0 0 0 0 0 n8 a  -> Tensor 0 0 0 0 0 0 0 n8 a
    insertOrAddL3 ind (TensorL3 EmptyTree) = mkTensL3 ind 
    insertOrAddL3 (Append l ls, t1) (TensorL3 t2) =  TensorL3 $ insertTreeWith (\x y -> tensorAddL3 x y) l (mkTensL3 (ls, t1)) t2
    insertOrAddL3 (Empty, t) t' = tensorAdd t t'


    mkTens :: (Index n1 n2 n3 n4 n5 n6 n7 n8, a) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    mkTens ((i1,i2,i3,i4,i5,i6,i7,i8),s) = mkTensU20 (i1, mkTensL20 (i2, mkTensU19 (i3, mkTensL19 (i4, mkTensU9 (i5, mkTensL9 (i6, mkTensU3 (i7, mkTensL3 (i8, (Scalar s)))))))))

    insertOrAdd :: (Num a, Eq a, Show a) => (Index n1 n2 n3 n4 n5 n6 n7 n8, a) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a 
    insertOrAdd ((i1,i2,i3,i4,i5,i6,i7,i8),s) (TensorU20 t) = insertOrAddU20 (i1, mkTens ((Empty,i2,i3,i4,i5,i6,i7,i8), s) ) (TensorU20 t)
    insertOrAdd ((Empty,i2,i3,i4,i5,i6,i7,i8),s) (TensorL20 t) = insertOrAddL20 (i2, mkTens ((Empty,Empty,i3,i4,i5,i6,i7,i8), s) ) (TensorL20 t)
    insertOrAdd ((Empty,Empty,i3,i4,i5,i6,i7,i8),s) (TensorU19 t) = insertOrAddU19 (i3, mkTens ((Empty,Empty,Empty,i4,i5,i6,i7,i8), s) ) (TensorU19 t)
    insertOrAdd ((Empty,Empty,Empty,i4,i5,i6,i7,i8),s) (TensorL19 t) = insertOrAddL19 (i4, mkTens ((Empty,Empty,Empty,Empty,i5,i6,i7,i8), s) ) (TensorL19 t)
    insertOrAdd ((Empty,Empty,Empty,Empty,i5,i6,i7,i8),s) (TensorU9 t) = insertOrAddU9 (i5, mkTens ((Empty,Empty,Empty,Empty,Empty,i6,i7,i8), s) ) (TensorU9 t)
    insertOrAdd ((Empty,Empty,Empty,Empty,Empty,i6,i7,i8),s) (TensorL9 t) = insertOrAddL9 (i6, mkTens ((Empty,Empty,Empty,Empty,Empty,Empty,i7,i8), s) ) (TensorL9 t)
    insertOrAdd ((Empty,Empty,Empty,Empty,Empty,Empty,i7,i8),s) (TensorU3 t) = insertOrAddU3 (i7, mkTens ((Empty,Empty,Empty,Empty,Empty,Empty,Empty,i8), s) ) (TensorU3 t)
    insertOrAdd ((Empty,Empty,Empty,Empty,Empty,Empty,Empty,i8),s) (TensorL3 t) = insertOrAddL3 (i8, mkTens ((Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty), s) ) (TensorL3 t)
    insertOrAdd ((Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty),s) (Scalar t) = Scalar (s + t)

    mapToL20 :: (Tensor 0 m2 m3 m4 m5 m6 m7 m8 a -> Tensor 0 n2 n3 n4 n5 n6 n7 n8 a) -> Tensor m1 m2 m3 m4 m5 m6 m7 m8 a -> Tensor m1 n2 n3 n4 n5 n6 n7 n8 a
    mapToL20 f (TensorU20 t) = TensorU20 $ fmap (mapToL20 f) t
    mapToL20 f (TensorL20 t) = f $ TensorL20 t
    mapToL20 f (TensorU19 t) = f $ TensorU19 t
    mapToL20 f (TensorL19 t) = f $ TensorL19 t
    mapToL20 f (TensorU9 t) = f $ TensorU9 t
    mapToL20 f (TensorL9 t) = f $ TensorL9 t
    mapToL20 f (TensorU3 t) = f $ TensorU3 t
    mapToL20 f (TensorL3 t) = f $ TensorL3 t
    mapToL20 f (Scalar t) = f $ Scalar t

    mapToU19 :: (Tensor 0 0 m3 m4 m5 m6 m7 m8 a -> Tensor 0 0 n3 n4 n5 n6 n7 n8 a) -> Tensor m1 m2 m3 m4 m5 m6 m7 m8 a -> Tensor m1 m2 n3 n4 n5 n6 n7 n8 a
    mapToU19 f (TensorU20 t) = TensorU20 $ fmap (mapToU19 f) t
    mapToU19 f (TensorL20 t) = TensorL20 $ fmap (mapToU19 f) t
    mapToU19 f (TensorU19 t) = f $ TensorU19 t
    mapToU19 f (TensorL19 t) = f $ TensorL19 t
    mapToU19 f (TensorU9 t) = f $ TensorU9 t
    mapToU19 f (TensorL9 t) = f $ TensorL9 t
    mapToU19 f (TensorU3 t) = f $ TensorU3 t
    mapToU19 f (TensorL3 t) = f $ TensorL3 t
    mapToU19 f (Scalar t) = f $ Scalar t

    mapToL19 :: (Tensor 0 0 0 m4 m5 m6 m7 m8 a -> Tensor 0 0 0 n4 n5 n6 n7 n8 a) -> Tensor m1 m2 m3 m4 m5 m6 m7 m8 a -> Tensor m1 m2 m3 n4 n5 n6 n7 n8 a
    mapToL19 f (TensorU20 t) = TensorU20 $ fmap (mapToL19 f) t
    mapToL19 f (TensorL20 t) = TensorL20 $ fmap (mapToL19 f) t
    mapToL19 f (TensorU19 t) = TensorU19 $ fmap (mapToL19 f) t
    mapToL19 f (TensorL19 t) = f $ TensorL19 t
    mapToL19 f (TensorU9 t) = f $ TensorU9 t
    mapToL19 f (TensorL9 t) = f $ TensorL9 t
    mapToL19 f (TensorU3 t) = f $ TensorU3 t
    mapToL19 f (TensorL3 t) = f $ TensorL3 t
    mapToL19 f (Scalar t) = f $ Scalar t

    mapToU9 :: (Tensor 0 0 0 0 m5 m6 m7 m8 a -> Tensor 0 0 0 0 n5 n6 n7 n8 a) -> Tensor m1 m2 m3 m4 m5 m6 m7 m8 a -> Tensor m1 m2 m3 m4 n5 n6 n7 n8 a
    mapToU9 f (TensorU20 t) = TensorU20 $ fmap (mapToU9 f) t
    mapToU9 f (TensorL20 t) = TensorL20 $ fmap (mapToU9 f) t
    mapToU9 f (TensorU19 t) = TensorU19 $ fmap (mapToU9 f) t
    mapToU9 f (TensorL19 t) = TensorL19 $ fmap (mapToU9 f) t
    mapToU9 f (TensorU9 t) = f $ TensorU9 t
    mapToU9 f (TensorL9 t) = f $ TensorL9 t
    mapToU9 f (TensorU3 t) = f $ TensorU3 t
    mapToU9 f (TensorL3 t) = f $ TensorL3 t
    mapToU9 f (Scalar t) = f $ Scalar t

    mapToL9 :: (Tensor 0 0 0 0 0 m6 m7 m8 a -> Tensor 0 0 0 0 0 n6 n7 n8 a) -> Tensor m1 m2 m3 m4 m5 m6 m7 m8 a -> Tensor m1 m2 m3 m4 m5 n6 n7 n8 a
    mapToL9 f (TensorU20 t) = TensorU20 $ fmap (mapToL9 f) t
    mapToL9 f (TensorL20 t) = TensorL20 $ fmap (mapToL9 f) t
    mapToL9 f (TensorU19 t) = TensorU19 $ fmap (mapToL9 f) t
    mapToL9 f (TensorL19 t) = TensorL19 $ fmap (mapToL9 f) t
    mapToL9 f (TensorU9 t) = TensorU9 $ fmap (mapToL9 f) t
    mapToL9 f (TensorL9 t) = f $ TensorL9 t
    mapToL9 f (TensorU3 t) = f $ TensorU3 t
    mapToL9 f (TensorL3 t) = f $ TensorL3 t
    mapToL9 f (Scalar t) = f $ Scalar t

    mapToU3 :: (Tensor 0 0 0 0 0 0 m7 m8 a -> Tensor 0 0 0 0 0 0 n7 n8 a) -> Tensor m1 m2 m3 m4 m5 m6 m7 m8 a -> Tensor m1 m2 m3 m4 m5 m6 n7 n8 a
    mapToU3 f (TensorU20 t) = TensorU20 $ fmap (mapToU3 f) t
    mapToU3 f (TensorL20 t) = TensorL20 $ fmap (mapToU3 f) t
    mapToU3 f (TensorU19 t) = TensorU19 $ fmap (mapToU3 f) t
    mapToU3 f (TensorL19 t) = TensorL19 $ fmap (mapToU3 f) t
    mapToU3 f (TensorU9 t) = TensorU9 $ fmap (mapToU3 f) t
    mapToU3 f (TensorL9 t) = TensorL9 $ fmap (mapToU3 f) t
    mapToU3 f (TensorU3 t) = f $ TensorU3 t
    mapToU3 f (TensorL3 t) = f $ TensorL3 t
    mapToU3 f (Scalar t) = f $ Scalar t

    mapToL3 :: (Tensor 0 0 0 0 0 0 0 m8 a -> Tensor 0 0 0 0 0 0 0 n8 a) -> Tensor m1 m2 m3 m4 m5 m6 m7 m8 a -> Tensor m1 m2 m3 m4 m5 m6 m7 n8 a
    mapToL3 f (TensorU20 t) = TensorU20 $ fmap (mapToL3 f) t
    mapToL3 f (TensorL20 t) = TensorL20 $ fmap (mapToL3 f) t
    mapToL3 f (TensorU19 t) = TensorU19 $ fmap (mapToL3 f) t
    mapToL3 f (TensorL19 t) = TensorL19 $ fmap (mapToL3 f) t
    mapToL3 f (TensorU9 t) = TensorU9 $ fmap (mapToL3 f) t
    mapToL3 f (TensorL9 t) = TensorL9 $ fmap (mapToL3 f) t
    mapToL3 f (TensorU3 t) = TensorU3 $ fmap (mapToL3 f) t
    mapToL3 f (TensorL3 t) = f $ TensorL3 t
    mapToL3 f (Scalar t) = f $ Scalar t

    tensorTransposeU20 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorTransposeU20 (0,j) (TensorU20 t) = foldr insertOrAddU20 (TensorU20 EmptyTree) $ map (\(x,y) -> (swapHead j x, y)) $ toListU20 $ TensorU20 t
    tensorTransposeU20 (i,j) (TensorU20 t) = TensorU20 $ fmap (tensorTransposeU20 (i-1,j-1)) t

    tensorTransposeL20 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor 0 n2 n3 n4 n5 n6 n7 n8 a -> Tensor 0 n2 n3 n4 n5 n6 n7 n8 a
    tensorTransposeL20 (0,j) (TensorL20 t) = foldr insertOrAddL20 (TensorL20 EmptyTree) $ map (\(x,y) -> (swapHead j x, y)) $ toListL20 $ TensorL20 t
    tensorTransposeL20 (i,j) (TensorL20 t) = TensorL20 $ fmap (tensorTransposeL20 (i-1,j-1)) t

    tensorTransposeU19 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor 0 0 n3 n4 n5 n6 n7 n8 a -> Tensor 0 0 n3 n4 n5 n6 n7 n8 a
    tensorTransposeU19 (0,j) (TensorU19 t) = foldr insertOrAddU19 (TensorU19 EmptyTree) $ map (\(x,y) -> (swapHead j x, y)) $ toListU19 $ TensorU19 t
    tensorTransposeU19 (i,j) (TensorU19 t) = TensorU19 $ fmap (tensorTransposeU19 (i-1,j-1)) t

    tensorTransposeL19 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor 0 0 0 n4 n5 n6 n7 n8 a -> Tensor 0 0 0 n4 n5 n6 n7 n8 a
    tensorTransposeL19 (0,j) (TensorL19 t) = foldr insertOrAddL19 (TensorL19 EmptyTree) $ map (\(x,y) -> (swapHead j x, y)) $ toListL19 $ TensorL19 t
    tensorTransposeL19 (i,j) (TensorL19 t) = TensorL19 $ fmap (tensorTransposeL19 (i-1,j-1)) t

    tensorTransposeU9 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor 0 0 0 0 n5 n6 n7 n8 a -> Tensor 0 0 0 0 n5 n6 n7 n8 a
    tensorTransposeU9 (0,j) (TensorU9 t) = foldr insertOrAddU9 (TensorU9 EmptyTree) $ map (\(x,y) -> (swapHead j x, y)) $ toListU9 $ TensorU9 t
    tensorTransposeU9 (i,j) (TensorU9 t) = TensorU9 $ fmap (tensorTransposeU9 (i-1,j-1)) t

    tensorTransposeL9 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor 0 0 0 0 0 n6 n7 n8 a -> Tensor 0 0 0 0 0 n6 n7 n8 a
    tensorTransposeL9 (0,j) (TensorL9 t) = foldr insertOrAddL9 (TensorL9 EmptyTree) $ map (\(x,y) -> (swapHead j x, y)) $ toListL9 $ TensorL9 t
    tensorTransposeL9 (i,j) (TensorL9 t) = TensorL9 $ fmap (tensorTransposeL9 (i-1,j-1)) t

    tensorTransposeU3 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor 0 0 0 0 0 0 n7 n8 a -> Tensor 0 0 0 0 0 0 n7 n8 a
    tensorTransposeU3 (0,j) (TensorU3 t) = foldr insertOrAddU3 (TensorU3 EmptyTree) $ map (\(x,y) -> (swapHead j x, y)) $ toListU3 $ TensorU3 t
    tensorTransposeU3 (i,j) (TensorU3 t) = TensorU3 $ fmap (tensorTransposeU3 (i-1,j-1)) t

    tensorTransposeL3 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor 0 0 0 0 0 0 0 n8 a -> Tensor 0 0 0 0 0 0 0 n8 a
    tensorTransposeL3 (0,j) (TensorL3 t) = foldr insertOrAddL3 (TensorL3 EmptyTree) $ map (\(x,y) -> (swapHead j x, y)) $ toListL3 $ TensorL3 t
    tensorTransposeL3 (i,j) (TensorL3 t) = TensorL3 $ fmap (tensorTransposeL3 (i-1,j-1)) t

    tensorTransU20 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorTransU20 = tensorTransposeU20

    tensorTransL20 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorTransL20 inds = mapToL20 (tensorTransposeL20 inds)

    tensorTransU19 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorTransU19 inds = mapToU19 (tensorTransposeU19 inds)

    tensorTransL19 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorTransL19 inds = mapToL19 (tensorTransposeL19 inds)

    tensorTransU9 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorTransU9 inds = mapToU9 (tensorTransposeU9 inds)

    tensorTransL9 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorTransL9 inds = mapToL9 (tensorTransposeL9 inds)

    tensorTransU3 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorTransU3 inds = mapToU3 (tensorTransposeU3 inds)

    tensorTransL3 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorTransL3 inds = mapToL3 (tensorTransposeL3 inds)


    tensorContract20 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor (n1-1) (n2-1) n3 n4 n5 n6 n7 n8 a
    tensorContract20 (0,j) t = foldr insertOrAddU20 (TensorU20 EmptyTree) tensList 
                    where
                        l = map (\(x,y) -> (x, toListL20 y)) $ toListU20 t
                        l2 = map (\(x,y) -> (tailInd x,(mapMaybe (removeContractionInd j (headInd x)) y))) l
                        l3 = filter (\(_,y) -> length y >= 1) l2 
                        tensList = map (\(x,y) -> (x, foldr insertOrAddL20 (TensorL20 EmptyTree) y)) l3
    tensorContract20 (i,j) (TensorU20 t) = TensorU20 $ fmap (tensorContract20 (i-1,j)) t

    tensorContract19 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor 0 0 n3 n4 n5 n6 n7 n8 a -> Tensor 0 0 (n3-1) (n4-1) n5 n6 n7 n8 a
    tensorContract19 (0,j) t = foldr insertOrAddU19 (TensorU19 EmptyTree) tensList 
                    where
                        l = map (\(x,y) -> (x, toListL19 y)) $ toListU19 t
                        l2 = map (\(x,y) -> (tailInd x,(mapMaybe (removeContractionInd j (headInd x)) y))) l
                        l3 = filter (\(_,y) -> length y >= 1) l2 
                        tensList = map (\(x,y) -> (x, foldr insertOrAddL19 (TensorL19 EmptyTree) y)) l3
    tensorContract19 (i,j) (TensorU19 t) = TensorU19 $ fmap (tensorContract19 (i-1,j)) t

    tensorContract9 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor 0 0 0 0 n5 n6 n7 n8 a -> Tensor 0 0 0 0 (n5-1) (n6-1) n7 n8 a
    tensorContract9 (0,j) t = foldr insertOrAddU9 (TensorU9 EmptyTree) tensList 
                    where
                        l = map (\(x,y) -> (x, toListL9 y)) $ toListU9 t
                        l2 = map (\(x,y) -> (tailInd x,(mapMaybe (removeContractionInd j (headInd x)) y))) l
                        l3 = filter (\(_,y) -> length y >= 1) l2 
                        tensList = map (\(x,y) -> (x, foldr insertOrAddL9 (TensorL9 EmptyTree) y)) l3
    tensorContract9 (i,j) (TensorU9 t) = TensorU9 $ fmap (tensorContract9 (i-1,j)) t

    tensorContract3 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor 0 0 0 0 0 0 n7 n8 a -> Tensor 0 0 0 0 0 0 (n7-1) (n8-1) a
    tensorContract3 (0,j) t = foldr insertOrAddU3 (TensorU3 EmptyTree) tensList 
                    where
                        l = map (\(x,y) -> (x, toListL3 y)) $ toListU3 t
                        l2 = map (\(x,y) -> (tailInd x,(mapMaybe (removeContractionInd j (headInd x)) y))) l
                        l3 = filter (\(_,y) -> length y >= 1) l2 
                        tensList = map (\(x,y) -> (x, foldr insertOrAddL3 (TensorL3 EmptyTree) y)) l3
    tensorContract3 (i,j) (TensorU3 t) = TensorU3 $ fmap (tensorContract3 (i-1,j)) t

    tensorContr20 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor (n1-1) (n2-1) n3 n4 n5 n6 n7 n8 a
    tensorContr20 inds = tensorContract20 inds 

    tensorContr19 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 (n3-1) (n4-1) n5 n6 n7 n8 a
    tensorContr19 inds = mapToU19 (tensorContract19 inds) 
 
    tensorContr9 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 (n5-1) (n6-1) n7 n8 a
    tensorContr9 inds = mapToU9 (tensorContract9 inds) 

    tensorContr3 :: (Num a, Eq a, Show a) => (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 (n7-1) (n8-1) a
    tensorContr3 inds = mapToU3 (tensorContract3 inds) 


    tensorProdU20 :: (Num a, Eq a, Show a) => Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor m1 m2 m3 m4 m5 m6 m7 m8 a -> Tensor (n1+m1) (n2+m2) (n3+m3) (n4+m4) (n5+m5) (n6+m6) (n7+m7) (n8+m8) a
    tensorProdU20 t1 (Scalar s) = tensorSMult s t1
    tensorProdU20 (Scalar s) t2 = tensorSMult s t2
    tensorProdU20 (TensorU20 t1) t2 = TensorU20 $ fmap (\v -> tensorProdU20 v t2) t1
    tensorProdU20 t1 (TensorU20 t2) = TensorU20 $ fmap (\v -> tensorProdU20 t1 v) t2
    tensorProdU20 (TensorL20 t1) (TensorL20 t2) = tensorProdL20 (TensorL20 t1) (TensorL20 t2) 
    tensorProdU20 (TensorL20 t1) (TensorU19 t2) = tensorProdL20 (TensorL20 t1) (TensorU19 t2) 
    tensorProdU20 (TensorL20 t1) (TensorL19 t2) = tensorProdL20 (TensorL20 t1) (TensorL19 t2) 
    tensorProdU20 (TensorL20 t1) (TensorU9 t2) = tensorProdL20 (TensorL20 t1) (TensorU9 t2) 
    tensorProdU20 (TensorL20 t1) (TensorL9 t2) = tensorProdL20 (TensorL20 t1) (TensorL9 t2) 
    tensorProdU20 (TensorL20 t1) (TensorU3 t2) = tensorProdL20 (TensorL20 t1) (TensorU3 t2) 
    tensorProdU20 (TensorL20 t1) (TensorL3 t2) = tensorProdL20 (TensorL20 t1) (TensorL3 t2) 
    tensorProdU20 (TensorU19 t1) (TensorL20 t2) = tensorProdL20 (TensorU19 t1) (TensorL20 t2) 
    tensorProdU20 (TensorL19 t1) (TensorL20 t2) = tensorProdL20 (TensorL19 t1) (TensorL20 t2) 
    tensorProdU20 (TensorU9 t1) (TensorL20 t2) = tensorProdL20 (TensorU9 t1) (TensorL20 t2) 
    tensorProdU20 (TensorL9 t1) (TensorL20 t2) = tensorProdL20 (TensorL9 t1) (TensorL20 t2) 
    tensorProdU20 (TensorU3 t1) (TensorL20 t2) = tensorProdL20 (TensorU3 t1) (TensorL20 t2) 
    tensorProdU20 (TensorL3 t1) (TensorL20 t2) = tensorProdL20 (TensorL3 t1) (TensorL20 t2) 
    tensorProdU20 (TensorU19 t1) (TensorU19 t2) = tensorProdU19 (TensorU19 t1) (TensorU19 t2) 
    tensorProdU20 (TensorU19 t1) (TensorL19 t2) = tensorProdU19 (TensorU19 t1) (TensorL19 t2) 
    tensorProdU20 (TensorU19 t1) (TensorU9 t2) = tensorProdU19 (TensorU19 t1) (TensorU9 t2) 
    tensorProdU20 (TensorU19 t1) (TensorL9 t2) = tensorProdU19 (TensorU19 t1) (TensorL9 t2) 
    tensorProdU20 (TensorU19 t1) (TensorU3 t2) = tensorProdU19 (TensorU19 t1) (TensorU3 t2) 
    tensorProdU20 (TensorU19 t1) (TensorL3 t2) = tensorProdU19 (TensorU19 t1) (TensorL3 t2) 
    tensorProdU20 (TensorL19 t1) (TensorU19 t2) = tensorProdU19 (TensorL19 t1) (TensorU19 t2) 
    tensorProdU20 (TensorU9 t1) (TensorU19 t2) = tensorProdU19 (TensorU9 t1) (TensorU19 t2) 
    tensorProdU20 (TensorL9 t1) (TensorU19 t2) = tensorProdU19 (TensorL9 t1) (TensorU19 t2) 
    tensorProdU20 (TensorU3 t1) (TensorU19 t2) = tensorProdU19 (TensorU3 t1) (TensorU19 t2) 
    tensorProdU20 (TensorL3 t1) (TensorU19 t2) = tensorProdU19 (TensorL3 t1) (TensorU19 t2) 
    tensorProdU20 (TensorL19 t1) (TensorL19 t2) = tensorProdL19 (TensorL19 t1) (TensorL19 t2)
    tensorProdU20 (TensorL19 t1) (TensorU9 t2) = tensorProdL19 (TensorL19 t1) (TensorU9 t2) 
    tensorProdU20 (TensorL19 t1) (TensorL9 t2) = tensorProdL19 (TensorL19 t1) (TensorL9 t2) 
    tensorProdU20 (TensorL19 t1) (TensorU3 t2) = tensorProdL19 (TensorL19 t1) (TensorU3 t2) 
    tensorProdU20 (TensorL19 t1) (TensorL3 t2) = tensorProdL19 (TensorL19 t1) (TensorL3 t2) 
    tensorProdU20 (TensorU9 t1) (TensorL19 t2) = tensorProdL19 (TensorU9 t1) (TensorL19 t2) 
    tensorProdU20 (TensorL9 t1) (TensorL19 t2) = tensorProdL19 (TensorL9 t1) (TensorL19 t2) 
    tensorProdU20 (TensorU3 t1) (TensorL19 t2) = tensorProdL19 (TensorU3 t1) (TensorL19 t2) 
    tensorProdU20 (TensorL3 t1) (TensorL19 t2) = tensorProdL19 (TensorL3 t1) (TensorL19 t2) 
    tensorProdU20 (TensorU9 t1) (TensorU9 t2) = tensorProdU9 (TensorU9 t1) (TensorU9 t2) 
    tensorProdU20 (TensorU9 t1) (TensorL9 t2) = tensorProdU9 (TensorU9 t1) (TensorL9 t2) 
    tensorProdU20 (TensorU9 t1) (TensorU3 t2) = tensorProdU9 (TensorU9 t1) (TensorU3 t2) 
    tensorProdU20 (TensorU9 t1) (TensorL3 t2) = tensorProdU9 (TensorU9 t1) (TensorL3 t2) 
    tensorProdU20 (TensorL9 t1) (TensorU9 t2) = tensorProdU9 (TensorL9 t1) (TensorU9 t2) 
    tensorProdU20 (TensorU3 t1) (TensorU9 t2) = tensorProdU9 (TensorU3 t1) (TensorU9 t2) 
    tensorProdU20 (TensorL3 t1) (TensorU9 t2) = tensorProdU9 (TensorL3 t1) (TensorU9 t2) 
    tensorProdU20 (TensorL9 t1) (TensorL9 t2) = tensorProdL9 (TensorL9 t1) (TensorL9 t2) 
    tensorProdU20 (TensorL9 t1) (TensorU3 t2) = tensorProdL9 (TensorL9 t1) (TensorU3 t2) 
    tensorProdU20 (TensorL9 t1) (TensorL3 t2) = tensorProdL9 (TensorL9 t1) (TensorL3 t2) 
    tensorProdU20 (TensorU3 t1) (TensorL9 t2) = tensorProdL9 (TensorU3 t1) (TensorL9 t2) 
    tensorProdU20 (TensorL3 t1) (TensorL9 t2) = tensorProdL9 (TensorL3 t1) (TensorL9 t2) 
    tensorProdU20 (TensorU3 t1) (TensorU3 t2) = tensorProdU3 (TensorU3 t1) (TensorU3 t2)
    tensorProdU20 (TensorU3 t1) (TensorL3 t2) = tensorProdU3 (TensorU3 t1) (TensorL3 t2) 
    tensorProdU20 (TensorL3 t1) (TensorU3 t2) = tensorProdU3 (TensorL3 t1) (TensorU3 t2) 
    tensorProdU20 (TensorL3 t1) (TensorL3 t2) = tensorProdL3 (TensorL3 t1) (TensorL3 t2) 


    tensorProdL20 :: (Num a, Eq a, Show a) => Tensor 0 n2 n3 n4 n5 n6 n7 n8 a -> Tensor 0 m2 m3 m4 m5 m6 m7 m8 a -> Tensor 0 (n2+m2) (n3+m3) (n4+m4) (n5+m5) (n6+m6) (n7+m7) (n8+m8) a
    tensorProdL20 t1 (Scalar s) = tensorSMult s t1
    tensorProdL20 (Scalar s) t2 = tensorSMult s t2
    tensorProdL20 (TensorL20 t1) t2 = TensorL20 $ fmap (\v -> tensorProdL20 v t2) t1
    tensorProdL20 t1 (TensorL20 t2) = TensorL20 $ fmap (\v -> tensorProdL20 t1 v) t2
    tensorProdL20 (TensorU19 t1) (TensorU19 t2) = tensorProdU19 (TensorU19 t1) (TensorU19 t2) 
    tensorProdL20 (TensorU19 t1) (TensorL19 t2) = tensorProdU19 (TensorU19 t1) (TensorL19 t2) 
    tensorProdL20 (TensorU19 t1) (TensorU9 t2) = tensorProdU19 (TensorU19 t1) (TensorU9 t2) 
    tensorProdL20 (TensorU19 t1) (TensorL9 t2) = tensorProdU19 (TensorU19 t1) (TensorL9 t2) 
    tensorProdL20 (TensorU19 t1) (TensorU3 t2) = tensorProdU19 (TensorU19 t1) (TensorU3 t2) 
    tensorProdL20 (TensorU19 t1) (TensorL3 t2) = tensorProdU19 (TensorU19 t1) (TensorL3 t2) 
    tensorProdL20 (TensorL19 t1) (TensorU19 t2) = tensorProdU19 (TensorL19 t1) (TensorU19 t2) 
    tensorProdL20 (TensorU9 t1) (TensorU19 t2) = tensorProdU19 (TensorU9 t1) (TensorU19 t2) 
    tensorProdL20 (TensorL9 t1) (TensorU19 t2) = tensorProdU19 (TensorL9 t1) (TensorU19 t2) 
    tensorProdL20 (TensorU3 t1) (TensorU19 t2) = tensorProdU19 (TensorU3 t1) (TensorU19 t2) 
    tensorProdL20 (TensorL3 t1) (TensorU19 t2) = tensorProdU19 (TensorL3 t1) (TensorU19 t2) 
    tensorProdL20 (TensorL19 t1) (TensorL19 t2) = tensorProdL19 (TensorL19 t1) (TensorL19 t2)
    tensorProdL20 (TensorL19 t1) (TensorU9 t2) = tensorProdL19 (TensorL19 t1) (TensorU9 t2) 
    tensorProdL20 (TensorL19 t1) (TensorL9 t2) = tensorProdL19 (TensorL19 t1) (TensorL9 t2) 
    tensorProdL20 (TensorL19 t1) (TensorU3 t2) = tensorProdL19 (TensorL19 t1) (TensorU3 t2) 
    tensorProdL20 (TensorL19 t1) (TensorL3 t2) = tensorProdL19 (TensorL19 t1) (TensorL3 t2) 
    tensorProdL20 (TensorU9 t1) (TensorL19 t2) = tensorProdL19 (TensorU9 t1) (TensorL19 t2) 
    tensorProdL20 (TensorL9 t1) (TensorL19 t2) = tensorProdL19 (TensorL9 t1) (TensorL19 t2) 
    tensorProdL20 (TensorU3 t1) (TensorL19 t2) = tensorProdL19 (TensorU3 t1) (TensorL19 t2) 
    tensorProdL20 (TensorL3 t1) (TensorL19 t2) = tensorProdL19 (TensorL3 t1) (TensorL19 t2) 
    tensorProdL20 (TensorU9 t1) (TensorU9 t2) = tensorProdU9 (TensorU9 t1) (TensorU9 t2) 
    tensorProdL20 (TensorU9 t1) (TensorL9 t2) = tensorProdU9 (TensorU9 t1) (TensorL9 t2) 
    tensorProdL20 (TensorU9 t1) (TensorU3 t2) = tensorProdU9 (TensorU9 t1) (TensorU3 t2) 
    tensorProdL20 (TensorU9 t1) (TensorL3 t2) = tensorProdU9 (TensorU9 t1) (TensorL3 t2) 
    tensorProdL20 (TensorL9 t1) (TensorU9 t2) = tensorProdU9 (TensorL9 t1) (TensorU9 t2) 
    tensorProdL20 (TensorU3 t1) (TensorU9 t2) = tensorProdU9 (TensorU3 t1) (TensorU9 t2) 
    tensorProdL20 (TensorL3 t1) (TensorU9 t2) = tensorProdU9 (TensorL3 t1) (TensorU9 t2) 
    tensorProdL20 (TensorL9 t1) (TensorL9 t2) = tensorProdL9 (TensorL9 t1) (TensorL9 t2) 
    tensorProdL20 (TensorL9 t1) (TensorU3 t2) = tensorProdL9 (TensorL9 t1) (TensorU3 t2) 
    tensorProdL20 (TensorL9 t1) (TensorL3 t2) = tensorProdL9 (TensorL9 t1) (TensorL3 t2) 
    tensorProdL20 (TensorU3 t1) (TensorL9 t2) = tensorProdL9 (TensorU3 t1) (TensorL9 t2) 
    tensorProdL20 (TensorL3 t1) (TensorL9 t2) = tensorProdL9 (TensorL3 t1) (TensorL9 t2) 
    tensorProdL20 (TensorU3 t1) (TensorU3 t2) = tensorProdU3 (TensorU3 t1) (TensorU3 t2)
    tensorProdL20 (TensorU3 t1) (TensorL3 t2) = tensorProdU3 (TensorU3 t1) (TensorL3 t2) 
    tensorProdL20 (TensorL3 t1) (TensorU3 t2) = tensorProdU3 (TensorL3 t1) (TensorU3 t2) 
    tensorProdL20 (TensorL3 t1) (TensorL3 t2) = tensorProdL3 (TensorL3 t1) (TensorL3 t2) 


    tensorProdU19 :: (Num a, Eq a, Show a) => Tensor 0 0 n3 n4 n5 n6 n7 n8 a -> Tensor 0 0 m3 m4 m5 m6 m7 m8 a -> Tensor 0 0 (n3+m3) (n4+m4) (n5+m5) (n6+m6) (n7+m7) (n8+m8) a
    tensorProdU19 t1 (Scalar s) = tensorSMult s t1
    tensorProdU19 (Scalar s) t2 = tensorSMult s t2
    tensorProdU19 (TensorU19 t1) t2 = TensorU19 $ fmap (\v -> tensorProdU19 v t2) t1
    tensorProdU19 t1 (TensorU19 t2) = TensorU19 $ fmap (\v -> tensorProdU19 t1 v) t2
    tensorProdU19 (TensorL19 t1) (TensorL19 t2) = tensorProdL19 (TensorL19 t1) (TensorL19 t2)
    tensorProdU19 (TensorL19 t1) (TensorU9 t2) = tensorProdL19 (TensorL19 t1) (TensorU9 t2) 
    tensorProdU19 (TensorL19 t1) (TensorL9 t2) = tensorProdL19 (TensorL19 t1) (TensorL9 t2) 
    tensorProdU19 (TensorL19 t1) (TensorU3 t2) = tensorProdL19 (TensorL19 t1) (TensorU3 t2) 
    tensorProdU19 (TensorL19 t1) (TensorL3 t2) = tensorProdL19 (TensorL19 t1) (TensorL3 t2) 
    tensorProdU19 (TensorU9 t1) (TensorL19 t2) = tensorProdL19 (TensorU9 t1) (TensorL19 t2) 
    tensorProdU19 (TensorL9 t1) (TensorL19 t2) = tensorProdL19 (TensorL9 t1) (TensorL19 t2) 
    tensorProdU19 (TensorU3 t1) (TensorL19 t2) = tensorProdL19 (TensorU3 t1) (TensorL19 t2) 
    tensorProdU19 (TensorL3 t1) (TensorL19 t2) = tensorProdL19 (TensorL3 t1) (TensorL19 t2) 
    tensorProdU19 (TensorU9 t1) (TensorU9 t2) = tensorProdU9 (TensorU9 t1) (TensorU9 t2) 
    tensorProdU19 (TensorU9 t1) (TensorL9 t2) = tensorProdU9 (TensorU9 t1) (TensorL9 t2) 
    tensorProdU19 (TensorU9 t1) (TensorU3 t2) = tensorProdU9 (TensorU9 t1) (TensorU3 t2) 
    tensorProdU19 (TensorU9 t1) (TensorL3 t2) = tensorProdU9 (TensorU9 t1) (TensorL3 t2) 
    tensorProdU19 (TensorL9 t1) (TensorU9 t2) = tensorProdU9 (TensorL9 t1) (TensorU9 t2) 
    tensorProdU19 (TensorU3 t1) (TensorU9 t2) = tensorProdU9 (TensorU3 t1) (TensorU9 t2) 
    tensorProdU19 (TensorL3 t1) (TensorU9 t2) = tensorProdU9 (TensorL3 t1) (TensorU9 t2) 
    tensorProdU19 (TensorL9 t1) (TensorL9 t2) = tensorProdL9 (TensorL9 t1) (TensorL9 t2) 
    tensorProdU19 (TensorL9 t1) (TensorU3 t2) = tensorProdL9 (TensorL9 t1) (TensorU3 t2) 
    tensorProdU19 (TensorL9 t1) (TensorL3 t2) = tensorProdL9 (TensorL9 t1) (TensorL3 t2) 
    tensorProdU19 (TensorU3 t1) (TensorL9 t2) = tensorProdL9 (TensorU3 t1) (TensorL9 t2) 
    tensorProdU19 (TensorL3 t1) (TensorL9 t2) = tensorProdL9 (TensorL3 t1) (TensorL9 t2) 
    tensorProdU19 (TensorU3 t1) (TensorU3 t2) = tensorProdU3 (TensorU3 t1) (TensorU3 t2)
    tensorProdU19 (TensorU3 t1) (TensorL3 t2) = tensorProdU3 (TensorU3 t1) (TensorL3 t2) 
    tensorProdU19 (TensorL3 t1) (TensorU3 t2) = tensorProdU3 (TensorL3 t1) (TensorU3 t2) 
    tensorProdU19 (TensorL3 t1) (TensorL3 t2) = tensorProdL3 (TensorL3 t1) (TensorL3 t2)
 
    
    tensorProdL19 :: (Num a, Eq a, Show a) => Tensor 0 0 0 n4 n5 n6 n7 n8 a -> Tensor 0 0 0 m4 m5 m6 m7 m8 a -> Tensor 0 0 0 (n4+m4) (n5+m5) (n6+m6) (n7+m7) (n8+m8) a
    tensorProdL19 t1 (Scalar s) = tensorSMult s t1
    tensorProdL19 (Scalar s) t2 = tensorSMult s t2
    tensorProdL19 (TensorL19 t1) t2 = TensorL19 $ fmap (\v -> tensorProdL19 v t2) t1
    tensorProdL19 t1 (TensorL19 t2) = TensorL19 $ fmap (\v -> tensorProdL19 t1 v) t2
    tensorProdL19 (TensorU9 t1) (TensorU9 t2) = tensorProdU9 (TensorU9 t1) (TensorU9 t2) 
    tensorProdL19 (TensorU9 t1) (TensorL9 t2) = tensorProdU9 (TensorU9 t1) (TensorL9 t2) 
    tensorProdL19 (TensorU9 t1) (TensorU3 t2) = tensorProdU9 (TensorU9 t1) (TensorU3 t2) 
    tensorProdL19 (TensorU9 t1) (TensorL3 t2) = tensorProdU9 (TensorU9 t1) (TensorL3 t2) 
    tensorProdL19 (TensorL9 t1) (TensorU9 t2) = tensorProdU9 (TensorL9 t1) (TensorU9 t2) 
    tensorProdL19 (TensorU3 t1) (TensorU9 t2) = tensorProdU9 (TensorU3 t1) (TensorU9 t2) 
    tensorProdL19 (TensorL3 t1) (TensorU9 t2) = tensorProdU9 (TensorL3 t1) (TensorU9 t2) 
    tensorProdL19 (TensorL9 t1) (TensorL9 t2) = tensorProdL9 (TensorL9 t1) (TensorL9 t2) 
    tensorProdL19 (TensorL9 t1) (TensorU3 t2) = tensorProdL9 (TensorL9 t1) (TensorU3 t2) 
    tensorProdL19 (TensorL9 t1) (TensorL3 t2) = tensorProdL9 (TensorL9 t1) (TensorL3 t2) 
    tensorProdL19 (TensorU3 t1) (TensorL9 t2) = tensorProdL9 (TensorU3 t1) (TensorL9 t2) 
    tensorProdL19 (TensorL3 t1) (TensorL9 t2) = tensorProdL9 (TensorL3 t1) (TensorL9 t2) 
    tensorProdL19 (TensorU3 t1) (TensorU3 t2) = tensorProdU3 (TensorU3 t1) (TensorU3 t2)
    tensorProdL19 (TensorU3 t1) (TensorL3 t2) = tensorProdU3 (TensorU3 t1) (TensorL3 t2) 
    tensorProdL19 (TensorL3 t1) (TensorU3 t2) = tensorProdU3 (TensorL3 t1) (TensorU3 t2) 
    tensorProdL19 (TensorL3 t1) (TensorL3 t2) = tensorProdL3 (TensorL3 t1) (TensorL3 t2) 


    tensorProdU9 :: (Num a, Eq a, Show a) => Tensor 0 0 0 0 n5 n6 n7 n8 a -> Tensor 0 0 0 0 m5 m6 m7 m8 a -> Tensor 0 0 0 0 (n5+m5) (n6+m6) (n7+m7) (n8+m8) a
    tensorProdU9 t1 (Scalar s) = tensorSMult s t1
    tensorProdU9 (Scalar s) t2 = tensorSMult s t2
    tensorProdU9 (TensorU9 t1) t2 = TensorU9 $ fmap (\v -> tensorProdU9 v t2) t1
    tensorProdU9 t1 (TensorU9 t2) = TensorU9 $ fmap (\v -> tensorProdU9 t1 v) t2
    tensorProdU9 (TensorL9 t1) (TensorL9 t2) = tensorProdL9 (TensorL9 t1) (TensorL9 t2) 
    tensorProdU9 (TensorL9 t1) (TensorU3 t2) = tensorProdL9 (TensorL9 t1) (TensorU3 t2) 
    tensorProdU9 (TensorL9 t1) (TensorL3 t2) = tensorProdL9 (TensorL9 t1) (TensorL3 t2) 
    tensorProdU9 (TensorU3 t1) (TensorL9 t2) = tensorProdL9 (TensorU3 t1) (TensorL9 t2) 
    tensorProdU9 (TensorL3 t1) (TensorL9 t2) = tensorProdL9 (TensorL3 t1) (TensorL9 t2) 
    tensorProdU9 (TensorU3 t1) (TensorU3 t2) = tensorProdU3 (TensorU3 t1) (TensorU3 t2)
    tensorProdU9 (TensorU3 t1) (TensorL3 t2) = tensorProdU3 (TensorU3 t1) (TensorL3 t2) 
    tensorProdU9 (TensorL3 t1) (TensorU3 t2) = tensorProdU3 (TensorL3 t1) (TensorU3 t2) 
    tensorProdU9 (TensorL3 t1) (TensorL3 t2) = tensorProdL3 (TensorL3 t1) (TensorL3 t2) 


    tensorProdL9 :: (Num a, Eq a, Show a) => Tensor 0 0 0 0 0 n6 n7 n8 a -> Tensor 0 0 0 0 0 m6 m7 m8 a -> Tensor 0 0 0 0 0 (n6+m6) (n7+m7) (n8+m8) a
    tensorProdL9 t1 (Scalar s) = tensorSMult s t1
    tensorProdL9 (Scalar s) t2 = tensorSMult s t2
    tensorProdL9 (TensorL9 t1) t2 = TensorL9 $ fmap (\v -> tensorProdL9 v t2) t1
    tensorProdL9 t1 (TensorL9 t2) = TensorL9 $ fmap (\v -> tensorProdL9 t1 v) t2
    tensorProdL9 (TensorU3 t1) (TensorU3 t2) = tensorProdU3 (TensorU3 t1) (TensorU3 t2)
    tensorProdL9 (TensorU3 t1) (TensorL3 t2) = tensorProdU3 (TensorU3 t1) (TensorL3 t2) 
    tensorProdL9 (TensorL3 t1) (TensorU3 t2) = tensorProdU3 (TensorL3 t1) (TensorU3 t2) 
    tensorProdL9 (TensorL3 t1) (TensorL3 t2) = tensorProdL3 (TensorL3 t1) (TensorL3 t2) 


    tensorProdU3 :: (Num a, Eq a, Show a) => Tensor 0 0 0 0 0 0 n7 n8 a -> Tensor 0 0 0 0 0 0 m7 m8 a -> Tensor 0 0 0 0 0 0 (n7+m7) (n8+m8) a
    tensorProdU3 t1 (Scalar s) = tensorSMult s t1
    tensorProdU3 (Scalar s) t2 = tensorSMult s t2
    tensorProdU3 (TensorU3 t1) t2 = TensorU3 $ fmap (\v -> tensorProdU3 v t2) t1
    tensorProdU3 t1 (TensorU3 t2) = TensorU3 $ fmap (\v -> tensorProdU3 t1 v) t2
    tensorProdU3 (TensorL3 t1) (TensorL3 t2) = tensorProdL3 (TensorL3 t1) (TensorL3 t2) 


    tensorProdL3 :: (Num a, Eq a, Show a) => Tensor 0 0 0 0 0 0 0 n8 a -> Tensor 0 0 0 0 0 0 0 m8 a -> Tensor 0 0 0 0 0 0 0 (n8+m8) a
    tensorProdL3 (TensorL3 t1) (TensorL3 t2) = TensorL3 $ fmap (\v -> tensorProdL3 v (TensorL3 t2)) t1
    tensorProdL3 t1 (Scalar s) = tensorSMult s t1
    tensorProdL3 (Scalar s) t2 = tensorSMult s t2


    tensorProd :: (Num a, Eq a, Show a) => Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor m1 m2 m3 m4 m5 m6 m7 m8 a -> Tensor (n1+m1) (n2+m2) (n3+m3) (n4+m4) (n5+m5) (n6+m6) (n7+m7) (n8+m8) a
    tensorProd = tensorProdU20

    data Forest = Forest [(String, Forest)]| LeafT String   deriving (Show, Eq)

    forestMap :: Forest -> [(String, Forest)]
    forestMap (Forest m) = m

    mapForest :: (String -> String) -> Forest -> Forest 
    mapForest f (LeafT x) = LeafT $ f x
    mapForest f (Forest m) = Forest $ map (\(x,y) -> (f x, mapForest f y)) m  

    tensorToForest :: (Show a) => Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Forest 
    tensorToForest (Scalar x) = LeafT $ show x 
    tensorToForest (TensorU20 t) = Forest $ map (\(i,v) -> (show $ fromEnum i, tensorToForest v)) $ toAscList t
    tensorToForest (TensorL20 t) = Forest $ map (\(i,v) -> (show $ fromEnum i, tensorToForest v)) $ toAscList t
    tensorToForest (TensorU19 t) = Forest $ map (\(i,v) -> (show $ fromEnum i, tensorToForest v)) $ toAscList t
    tensorToForest (TensorL19 t) = Forest $ map (\(i,v) -> (show $ fromEnum i, tensorToForest v)) $ toAscList t
    tensorToForest (TensorU9 t) = Forest $ map (\(i,v) -> (show $ fromEnum i, tensorToForest v)) $ toAscList t
    tensorToForest (TensorL9 t) = Forest $ map (\(i,v) -> (show $ fromEnum i, tensorToForest v)) $ toAscList t
    tensorToForest (TensorU3 t) = Forest $ map (\(i,v) -> (show $ fromEnum i, tensorToForest v)) $ toAscList t
    tensorToForest (TensorL3 t) = Forest $ map (\(i,v) -> (show $ fromEnum i, tensorToForest v)) $ toAscList t

    shiftForest :: Forest -> Forest
    shiftForest (LeafT var) = LeafT var 
    shiftForest (Forest m) = Forest $ map (\(x,y) -> (x,shiftForest y))  shiftedForestMap
            where
                mapElems f (Forest m) =  Forest $ map (\(x,y) -> (f x,y)) m
                mapElems f (LeafT var) = LeafT (f var)
                shiftedForestMap = map (\(f1,f2) ->(f1, mapElems (\x -> "   " ++ x) f2)) m

    printForest :: Forest -> [String]
    printForest (LeafT var) = [var] 
    printForest (Forest m) = map (init.unlines) subForests
            where
                shiftedForest = shiftForest (Forest m)
                pairs = forestMap shiftedForest
                subForests = map (\(k,v) -> k : (printForest v)) pairs

    printTensorTree :: (Show a) => Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> String 
    printTensorTree t = unlines $ printForest forest 
            where
                forest = mapForest (\x -> "+----" ++ x) $ shiftForest $ tensorToForest t 


    --now the basic tensors

    delta20 :: Tensor 1 1 0 0 0 0 0 0 Rational 
    delta20 = balanceTensor $ fromListT delta20List
            where
                delta20List = [ (((singletonInd (toEnum x)), (singletonInd (toEnum x)), Empty, Empty, Empty, Empty, Empty, Empty), 1) | x <- [0..20] ]

    delta19 :: Tensor 0 0 1 1 0 0 0 0 Rational 
    delta19 = balanceTensor $ fromListT delta19List
            where
                delta19List = [ ((Empty, Empty, (singletonInd (toEnum x)), (singletonInd (toEnum x)), Empty, Empty, Empty, Empty), 1) | x <- [0..19] ]

    delta9 :: Tensor 0 0 0 0 1 1 0 0 Rational 
    delta9 = balanceTensor $ fromListT delta9List
            where
                delta9List = [ ((Empty, Empty, Empty, Empty, (singletonInd (toEnum x)), (singletonInd (toEnum x)), Empty, Empty), 1) | x <- [0..9] ]

    delta3 :: Tensor 0 0 0 0 0 0 1 1 Rational 
    delta3 = balanceTensor $ fromListT delta3List
            where
                delta3List = [ ((Empty, Empty, Empty, Empty, Empty, Empty, (singletonInd (toEnum x)), (singletonInd (toEnum x))), 1) | x <- [0..3] ]

    trianMapI2 :: M.Map (IndList 2 Lind_3) (IndList 1 Uind_9) 
    trianMapI2 = M.fromList $ zip [ Append a $ singletonInd b | a <- [toEnum 0..toEnum 3], b <- [a..toEnum 3] ] $ map singletonInd [toEnum 0..]

    trianMapJ2 :: M.Map (IndList 2 Uind_3) (IndList 1 Lind_9) 
    trianMapJ2 = M.fromList $ zip [ Append a $ singletonInd b | a <- [toEnum 0..toEnum 3], b <- [a..toEnum 3] ] $ map singletonInd [toEnum 0..]

    trianMapI3 :: M.Map (IndList 3 Lind_3) (IndList 1 Uind_19) 
    trianMapI3 = M.fromList $ zip [ Append a $ Append b $ singletonInd c | a <- [toEnum 0..toEnum 3], b <- [a..toEnum 3], c <- [b..toEnum 3] ] $ map singletonInd [toEnum 0..]

    trianMapJ3 :: M.Map (IndList 3 Uind_3) (IndList 1 Lind_19) 
    trianMapJ3 = M.fromList $ zip [ Append a $ Append b $ singletonInd c | a <- [toEnum 0..toEnum 3], b <- [a..toEnum 3], c <- [b..toEnum 3] ] $ map singletonInd [toEnum 0..]

    trianMapAreaI :: M.Map (IndList 4 Lind_3) (IndList 1 Uind_20)
    trianMapAreaI = M.fromList $ zip [ Append a $ Append b $ Append c $ singletonInd d | a <- [toEnum 0..toEnum 2], b <- [succ a..toEnum 3], c <- [a..toEnum 2], d <- [succ c..toEnum 3], not $ a == c && b > d ] $ map singletonInd [toEnum 0..]
    
    trianMapAreaJ :: M.Map (IndList 4 Uind_3) (IndList 1 Lind_20)
    trianMapAreaJ = M.fromList $ zip [ Append a $ Append b $ Append c $ singletonInd d | a <- [toEnum 0..toEnum 2], b <- [succ a..toEnum 3], c <- [a..toEnum 2], d <- [succ c..toEnum 3], not $ a == c && b > d ] $ map singletonInd [toEnum 0..]
    
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

    interI2 :: M.Map (IndList 2 Lind_3) (IndList 1 Uind_9)  -> Tensor 0 0 0 0 1 0 0 2 Rational
    interI2 trian2 = balanceTensor $ fromListT $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [ (Empty, Empty, Empty, Empty, (singletonInd a), Empty, Empty, (Append b $ singletonInd c)) | a <- [toEnum 0..toEnum 9], b <- [toEnum 0..toEnum 3], c <- [toEnum 0..toEnum 3]]
                f (_, _, _, _, ind1, _, _, ind2)
                    | ind1 == ((M.!) trian2 $ sortInd ind2 ) = 1 
                    | otherwise = 0 

    aSymI2 :: M.Map (IndList 2 Lind_3) (IndList 1 Uind_9)  -> Tensor 0 0 0 0 1 0 0 2 Rational
    aSymI2 trian2 = balanceTensor $ fromListT $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [ (Empty, Empty, Empty, Empty, (singletonInd a), Empty, Empty, (Append b $ singletonInd c)) | a <- [toEnum 0..toEnum 9], b <- [toEnum 0..toEnum 3], c <- [toEnum 0..toEnum 3]]
                f (_, _, _, _, ind1, _, _, ind2@(Append x (Append y Empty)))
                    | x == y = 0
                    | ind1 == (M.!) trian2 sortI = sign
                    | otherwise = 0 
                  where sortI = sortInd ind2
                        sign = if sortI == ind2 then 1 else -1

    interJ2 :: M.Map (IndList 2 Uind_3) (IndList 1 Lind_9)  -> Tensor 0 0 0 0 0 1 2 0 Rational
    interJ2 trian2 = balanceTensor $ fromListT $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [ (Empty, Empty, Empty, Empty, Empty, (singletonInd a), (Append b $ singletonInd c), Empty) | a <- [toEnum 0..toEnum 9], b <- [toEnum 0..toEnum 3], c <- [toEnum 0..toEnum 3]]
                f (_, _, _, _, _, ind1, ind2, _)
                    | ind1 == ((M.!) trian2 $ sortInd ind2 ) = jMult2 ind2  
                    | otherwise = 0 

    interI3 :: M.Map (IndList 3 Lind_3) (IndList 1 Uind_19) -> Tensor 0 0 1 0 0 0 0 3 Rational
    interI3 trian3 = balanceTensor $ fromListT $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [(Empty, Empty, (singletonInd a), Empty, Empty, Empty, Empty, (Append b $ Append c $ singletonInd d)) | a <- [toEnum 0..toEnum 19], b <- [toEnum 0..toEnum 3], c <- [toEnum 0..toEnum 3], d <- [toEnum 0..toEnum 3]]
                f (_, _, ind1, _, _, _, _, ind2)
                    | ind1 == ((M.!) trian3 $ sortInd ind2) = 1 
                    | otherwise = 0 

    interJ3 :: M.Map (IndList 3 Uind_3) (IndList 1 Lind_19) -> Tensor 0 0 0 1 0 0 3 0 Rational
    interJ3 trian3 = balanceTensor $ fromListT $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [ (Empty, Empty, Empty, (singletonInd a), Empty, Empty, (Append b $ Append c $ singletonInd d), Empty) | a <- [toEnum 0..toEnum 19], b <- [toEnum 0..toEnum 3], c <- [toEnum 0..toEnum 3], d <- [toEnum 0..toEnum 3]]
                f (_, _, _, ind1, _, _, ind2, _)
                    | ind1 == ((M.!) trian3 $ sortInd ind2) = jMult3 ind2 
                    | otherwise = 0 

    interIArea :: M.Map (IndList 4 Lind_3) (IndList 1 Uind_20) -> Tensor 1 0 0 0 0 0 0 4 Rational 
    interIArea trianArea = balanceTensor $ fromListT $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = filter (\(_, _, _, _, _, _, _, x) -> (not $ isZeroArea x)) [ ((singletonInd a), Empty, Empty, Empty, Empty, Empty, Empty, (Append b $ Append c $ Append d $ singletonInd e)) | a <- [toEnum 0..toEnum 20], b <- [toEnum 0..toEnum 3], c <- [toEnum 0..toEnum 3], d <- [toEnum 0..toEnum 3], e <- [toEnum 0..toEnum 3]]
                f (ind1, _, _, _, _, _, _, ind2)
                    | ind1 == ((M.!) trianArea indArea) = s
                    | otherwise = 0
                        where
                            (indArea, s) = canonicalizeArea ind2 

    interJArea :: M.Map (IndList 4 Uind_3) (IndList 1 Lind_20) -> Tensor 0 1 0 0 0 0 4 0 Rational 
    interJArea trianArea = balanceTensor $ fromListT $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = filter (\(_, _, _, _, _, _, x, _) -> (not $ isZeroArea x)) [  (Empty, (singletonInd a), Empty, Empty, Empty, Empty, (Append b $ Append c $ Append d $ singletonInd e), Empty) | a <- [toEnum 0..toEnum 20], b <- [toEnum 0..toEnum 3], c <- [toEnum 0..toEnum 3], d <- [toEnum 0..toEnum 3], e <- [toEnum 0..toEnum 3]]
                f (_, ind1, _, _, _, _, ind2, _)
                    | ind1 == ((M.!) trianArea indArea) = s * (jMultArea indArea)
                    | otherwise = 0
                        where
                            (indArea, s) = canonicalizeArea ind2 

    interMetric :: M.Map (IndList 2 Lind_3) (IndList 1 Uind_9) -> M.Map (IndList 2 Uind_3) (IndList 1 Lind_9) -> Tensor 0 0 0 0 1 1 1 1 Rational
    interMetric trian2I trian2J = fmap ((*) (-2)) $ tensorContr3 (0,0) prod
            where
                t1 = interI2 trian2I 
                t2 = interJ2 trian2J 
                prod = tensorProd t1 t2 

    interArea :: M.Map (IndList 4 Lind_3) (IndList 1 Uind_20) -> M.Map (IndList 4 Uind_3) (IndList 1 Lind_20) -> Tensor 1 1 0 0 0 0 1 1 Rational
    interArea trianAreaI trianAreaJ = balanceTensor $ fmap ((*) (-4)) $ tensorContr3 (1,1) $ tensorContr3 (2,2) $ tensorContr3 (3,3) prod
            where
                t1 = interIArea trianAreaI 
                t2 = interJArea trianAreaJ 
                prod = tensorProd t1 t2 

    interEqn2 :: M.Map (IndList 4 Lind_3) (IndList 1 Uind_20) -> M.Map (IndList 4 Uind_3) (IndList 1 Lind_20) -> Tensor 1 1 0 0 0 0 2 2 Rational
    interEqn2 trianAreaI trianAreaJ = balanceTensor $ tensorSub int1 int2
            where
                intArea = interArea trianAreaI trianAreaJ
                int1 = tensorProd intArea delta3
                int2 = tensorProd (tensorTransL3 (0,1) $ tensorProd delta3 delta3 ) delta20

    interEqn3 :: M.Map (IndList 2 Lind_3) (IndList 1 Uind_9) -> M.Map (IndList 2 Uind_3) (IndList 1 Lind_9) -> M.Map (IndList 4 Lind_3) (IndList 1 Uind_20) -> M.Map (IndList 4 Uind_3) (IndList 1 Lind_20) -> Tensor 1 1 0 0 1 1 1 1 Rational
    interEqn3 trian2I trian2J trianAreaI trianAreaJ = balanceTensor $ intTotal
            where
                int1 = tensorProd (interArea trianAreaI trianAreaJ) delta9
                int2 = tensorProd (interMetric trian2I trian2J) delta20
                intTotal = tensorAdd int1 int2

    flatArea :: Tensor 0 1 0 0 0 0 0 0 Rational 
    flatArea = balanceTensor $ fromListT $ map (\(i,v) -> ( (Empty, (singletonInd $ toEnum i), Empty, Empty, Empty, Empty, Empty, Empty), v)) [(0,-1),(5,-1),(6,-1),(9,1),(11,-1),(12,-1),(15,1),(18,1),(20,1)]

    eta :: Tensor 0 0 0 0 0 0 0 2 Rational 
    eta = balanceTensor $ fromListT l 
                where
                    l = map (\(x,y,z) -> ((Empty,Empty,Empty,Empty,Empty,Empty,Empty,Append (toEnum x) $ Append (toEnum y) Empty),z)) [(0,0,1),(1,1,-1),(2,2,-1),(3,3,-1)]

    invEta :: Tensor 0 0 0 0 0 0 2 0 Rational 
    invEta = balanceTensor $ fromListT l 
                where
                    l = map (\(x,y,z) -> ((Empty,Empty,Empty,Empty,Empty,Empty,Append (toEnum x) $ Append (toEnum y) Empty,Empty),z)) [(0,0,1),(1,1,-1),(2,2,-1),(3,3,-1)]

    flatInter :: M.Map (IndList 4 Lind_3) (IndList 1 Uind_20) -> M.Map (IndList 4 Uind_3) (IndList 1 Lind_20) -> Tensor 0 1 0 0 0 0 1 1 Rational
    flatInter trianAreaI trianAreaJ = balanceTensor $ tensorContr20 (0,1) prod
            where
                intArea = interArea trianAreaI trianAreaJ 
                prod = tensorProd intArea flatArea 

    intAIB :: M.Map (IndList 2 Lind_3) (IndList 1 Uind_9) -> M.Map (IndList 2 Uind_3) (IndList 1 Lind_9) -> M.Map (IndList 4 Lind_3) (IndList 1 Uind_20) -> M.Map (IndList 4 Uind_3) (IndList 1 Lind_20) -> Tensor 1 2 0 0 1 1 2 2 Rational
    intAIB map1Metric map2Metric map1Area map2Area = balanceTensor $ tensorSub tens tensTrans  
            where
                intArea = interArea map1Area map2Area
                intMetric = interMetric map1Metric map2Metric
                flatIntA = flatInter map1Area map2Area 
                int3 = interEqn3 map1Metric map2Metric map1Area map2Area
                block1 = tensorProd delta20 $ tensorProd delta20 $ tensorProd delta9 delta3 
                block2 = tensorProd intArea $ tensorProd delta20 delta9
                block3 = tensorProd delta20 int3 
                totalBlock = tensorAdd block1 $ tensorAdd block2 block3 
                tens = tensorContr20 (0,2) $ tensorProd totalBlock flatIntA 
                tensTrans = tensorTransU3 (0,1) $ tensorTransL3 (0,1) tens 

    ansatzAaBb :: M.Map (IndList 2 Lind_3) (IndList 1 Uind_9) -> M.Map (IndList 2 Uind_3) (IndList 1 Lind_9) -> M.Map (IndList 4 Lind_3) (IndList 1 Uind_20) -> M.Map (IndList 4 Uind_3) (IndList 1 Lind_20) -> Tensor 2 2 0 0 1 0 2 2 Rational 
    ansatzAaBb map1Metric map2Metric map1Area map2Area = tensorContr3 (2,0) $ tensorContr3 (0,3) prod
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        int2 = interEqn2 map1Area map2Area  
                        antiSym = aSymI2 map1Metric
                        block1 = tensorProd int2 $ tensorProd delta20 delta3
                        block2 = tensorTransU3 (1,2) $ tensorTransU20 (0,1) block1
                        totalBlock1 = tensorAdd block1 block2
                        totalBlockTrans = tensorTransL3 (1,2) $ tensorTransL20 (0,1) totalBlock1
                        tens = tensorAdd totalBlock1 totalBlockTrans
                        totalBlock2 = tensorContr3 (1,1) $ tensorProd  invEta antiSym
                        prod = tensorProd tens totalBlock2

    ansatzAIBC ::  M.Map (IndList 2 Lind_3) (IndList 1 Uind_9) -> M.Map (IndList 2 Uind_3) (IndList 1 Lind_9) -> M.Map (IndList 4 Lind_3) (IndList 1 Uind_20) -> M.Map (IndList 4 Uind_3) (IndList 1 Lind_20) -> Tensor 3 3 0 0 2 1 0 0 Rational 
    ansatzAIBC map1Metric map2Metric map1Area map2Area = tensorContr3 (0,0) $ tensorContr3 (0,1) prod
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        int3 = interEqn3 map1Metric map2Metric map1Area map2Area
                        antiSym = aSymI2 map1Metric
                        block1 = tensorProd int3 $ tensorProd delta20 delta20 
                        block2 = tensorProd delta20 $ tensorProd delta9 $ tensorProd intArea delta20
                        block3 = tensorTransU20 (1,2) block2
                        totalBlock1 = tensorAdd block1 $ tensorAdd block2 block3 
                        totalBlockTrans = tensorTransL20 (1,2) totalBlock1
                        tens = tensorAdd totalBlock1 totalBlockTrans
                        totalBlock2 = tensorContr3 (1,1) $ tensorProd invEta antiSym
                        prod = tensorProd tens totalBlock2