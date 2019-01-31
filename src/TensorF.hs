--pushes type stuff to kind stuff (prefixed with ')
{-# LANGUAGE DataKinds #-}
--matching on type constructors
{-# LANGUAGE GADTs #-}
--kind signature
{-# LANGUAGE KindSignatures #-}
--type family definitions
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
--infix type plus and mult
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TensorF (
    tensorSMult, tensorAdd, tensorSub, tensorProd, tensorTranspose, tensorContract_20, tensorContract_19, tensorContract_9, tensorContract_3,
    evalTensor, evalFullTensor, mkTensorFromMap, tensor2Map, delta_20, delta_19, delta_9, delta_3, triangleMap2, triangleMap3, inter_I2Map, inter_J2Map, inter_I3Map, inter_J3Map,
    inter_IAreaMap, inter_JAreaMap, triangleMapArea, interMetric, interArea, interEqn1_2, interEqn1_3, epsilonMap, flatAreaSTMap, flatAreaMap, flatInterMap,
    intAIB

) where 

    import Index
    import Numeric.Natural 
    import GHC.TypeNats
    import Data.Proxy
    import Control.Monad
    import qualified Data.Map as M
    import Data.Maybe 
    import qualified Data.Sequence as S
    import Data.Foldable
    import Data.List

    data Tensor (n1::Nat) (n2::Nat) (n3::Nat) (n4::Nat) (n5::Nat) (n6::Nat) (n7::Nat) (n8::Nat) a =
        Tensor ((Index n1 n2 n3 n4 n5 n6 n7 n8) -> a)

    instance Functor (Tensor n1 n2 n3 n4 n5 n6 n7 n8) where
        fmap f (Tensor g) = Tensor (f.g)
    
    tensorSMult :: Num a => a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a 
    tensorSMult a = fmap ( (*) a)

    tensorAdd :: Num a => Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorAdd (Tensor f) (Tensor g) = Tensor $ liftM2 (+) f g

    tensorSub :: Num a => Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorSub t1 t2 = tensorAdd t1 (tensorSMult (-1) t2)

    type Rank = (Int,Int,Int,Int,Int,Int,Int,Int)

    getRank :: forall n1 n2 n3 n4 n5 n6 n7 n8 a. (KnownNat n1, KnownNat n2, KnownNat n3, KnownNat n4, KnownNat n5, KnownNat n6, KnownNat n7, KnownNat n8) =>
        Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Rank
    getRank t = (i1,i2,i3,i4,i5,i6,i7,i8)
                where 
                    i1 = fromIntegral $ natVal (Proxy @n1)
                    i2 = fromIntegral $ natVal (Proxy @n2)
                    i3 = fromIntegral $ natVal (Proxy @n3)
                    i4 = fromIntegral $ natVal (Proxy @n4)
                    i5 = fromIntegral $ natVal (Proxy @n5)
                    i6 = fromIntegral $ natVal (Proxy @n6)
                    i7 = fromIntegral $ natVal (Proxy @n7)
                    i8 = fromIntegral $ natVal (Proxy @n8)

    tensorProd :: (Num a, Eq a, KnownNat n1, KnownNat n2, KnownNat n3, KnownNat n4, KnownNat n5, KnownNat n6, KnownNat n7, KnownNat n8,
        KnownNat m1, KnownNat m2, KnownNat m3, KnownNat m4, KnownNat m5, KnownNat m6, KnownNat m7, KnownNat m8) =>
        Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor m1 m2 m3 m4 m5 m6 m7 m8 a -> 
        Tensor (n1+m1) (n2+m2) (n3+m3) (n4+m4) (n5+m5) (n6+m6) (n7+m7) (n8+m8) a
    tensorProd (Tensor f) (Tensor g) = Tensor h 
                where
                    rank = getRank (Tensor f)
                    times 0 y = 0
                    times x 0 = 0
                    times x y = x*y
                    h = liftM2 times (f.fst.(splitIndex rank)) (g.snd.(splitIndex rank))

    tensorTranspose :: Int -> (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorTranspose i pair (Tensor f) = Tensor $ f.(swapPosIndex i pair) 


    tensorContract_20 :: (Num a, KnownNat n1, KnownNat n2) => (Int,Int) -> Tensor (n1+1) (n2+1) n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorContract_20 inds (Tensor f) = Tensor g
            where
                l = contractionIndexList_20 inds 
                g = \i -> foldl (\x y -> x + (f y)) 0 $ l i

    tensorContract_19 :: (Num a, KnownNat n3, KnownNat n4) => (Int,Int) -> Tensor n1 n2 (n3+1) (n4+1) n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorContract_19 inds (Tensor f) = Tensor g
            where
                l = contractionIndexList_19 inds 
                g = \i -> foldl (\x y -> x + (f y)) 0 $ l i

    tensorContract_9 :: (Num a, KnownNat n5, KnownNat n6) => (Int,Int) -> Tensor n1 n2 n3 n4 (n5+1) (n6+1) n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorContract_9 inds (Tensor f) = Tensor g
            where
                l = contractionIndexList_9 inds 
                g = \i -> foldl (\x y -> x + (f y)) 0 $ l i

    tensorContract_3 :: (Num a, KnownNat n7, KnownNat n8) => (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 (n7+1) (n8+1) a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorContract_3 inds (Tensor f) = Tensor g
            where
                l = contractionIndexList_3 inds 
                g = \i -> foldl (\x y -> x + (f y)) 0 $ l i

    --eval the first ind of the Tensor

    evalTensor :: Index n1 n2 n3 n4 n5 n6 n7 n8 -> Tensor (n1+m1) (n2+m2) (n3+m3) (n4+m4) (n5+m5) (n6+m6) (n7+m7) (n8+m8) a -> Tensor m1 m2 m3 m4 m5 m6 m7 m8 a
    evalTensor ind (Tensor f) = Tensor g 
                    where
                        g = f.(combineIndex ind)

    evalFullTensor :: (KnownNat n1, KnownNat n2, KnownNat n3, KnownNat n4, KnownNat n5, KnownNat n6, KnownNat n7, KnownNat n8, Num a, Eq a) =>
        Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> [a]
    evalFullTensor (Tensor f) =   map f indList 
                where
                    indList = tensorIndList $ getRank (Tensor f)

    mkTensorFromMap :: (Num a) => M.Map (Index n1 n2 n3 n4 n5 n6 n7 n8) a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a 
    mkTensorFromMap m = Tensor f
                    where
                        f = (fromMaybe 0).((M.!?) m)

    tensorIndList :: (KnownNat n1, KnownNat n2, KnownNat n3, KnownNat n4, KnownNat n5, KnownNat n6, KnownNat n7, KnownNat n8) =>
        Rank -> [Index n1 n2 n3 n4 n5 n6 n7 n8] 
    tensorIndList (r1,r2,r3,r4,r5,r6,r7,r8) =  map (\(x1,x2,x3,x4,x5,x6,x7,x8) -> ((mkInd x1), (mkInd x2), (mkInd x3), (mkInd x4), (mkInd x5), (mkInd x6), (mkInd x7), (mkInd x8))) list
            where 
                list = [ (y1,y2,y3,y4,y5,y6,y7,y8) | y1 <- (getRangeList r1 20), y2 <- (getRangeList r2 20), y3 <- (getRangeList r3 19), y4 <- (getRangeList r4 19),
                 y5 <- (getRangeList r5 9), y6 <- (getRangeList r6 9), y7 <- (getRangeList r7 3), y8 <- (getRangeList r8 3)]

    tensor2Map :: (KnownNat n1, KnownNat n2, KnownNat n3, KnownNat n4, KnownNat n5, KnownNat n6, KnownNat n7, KnownNat n8, Num a, Eq a) =>
         Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> M.Map (Index n1 n2 n3 n4 n5 n6 n7 n8) a
    tensor2Map (Tensor f) = M.filter ( /= 0) $ M.fromList (zip indList valueList)
            where 
                indList = tensorIndList $ getRank (Tensor f) 
                valueList = map f indList

    --now some basic tensors

    delta_3F :: Index 0 0 0 0 0 0 1 1 -> Rational
    delta_3F (_,_,_,_,_,_,a,b) 
            | fromEnum (getValInd a  0) == fromEnum ( getValInd b 0) = 1
            | otherwise = 0

    delta_3 :: Tensor 0 0 0 0 0 0 1 1 Rational
    delta_3 =  Tensor delta_3F

    delta_9F :: Index 0 0 0 0 1 1 0 0 -> Rational
    delta_9F (_,_,_,_,a,b,_,_) 
            | fromEnum (getValInd a  0) == fromEnum ( getValInd b 0) = 1
            | otherwise = 0

    delta_9 :: Tensor 0 0 0 0 1 1 0 0 Rational
    delta_9 = Tensor delta_9F

    delta_19F :: Index 0 0 1 1 0 0 0 0 -> Rational
    delta_19F (_,_,a,b,_,_,_,_) 
            | fromEnum (getValInd a  0) == fromEnum ( getValInd b 0) = 1
            | otherwise = 0

    delta_19 :: Tensor 0 0 1 1 0 0 0 0 Rational
    delta_19 = Tensor delta_19F

    delta_20F :: Index 1 1 0 0 0 0 0 0 -> Rational
    delta_20F (a,b,_,_,_,_,_,_) 
            | fromEnum (getValInd a  0) == fromEnum ( getValInd b 0) = 1
            | otherwise = 0

    delta_20 :: Tensor 1 1 0 0 0 0 0 0 Rational
    delta_20 = Tensor delta_20F

    getLastSeq :: S.Seq a -> a
    getLastSeq (S.Empty) = error "empty seq has no last elem"
    getLastSeq ((S.:|>) xs x) = x
    

    symIndList :: Enum a => Int -> Int -> [S.Seq a]
    symIndList n j 
            | n <= toEnum 0 = error "wrong number of indices"
            | n == 1 = [ S.singleton a | a <- [toEnum 0.. toEnum j] ]
            | otherwise = [ (S.|>) a b | a <- (symIndList (n-1) j), b <- [(getLastSeq a)..toEnum j] ] 


    triangleMap2 :: (Enum a, Enum b, Ord a) =>  M.Map (S.Seq a) b
    triangleMap2 = M.fromList $ zip (symIndList 2 3) [toEnum 0..]

    triangleMap3 :: (Enum a, Enum b, Ord a) =>  M.Map (S.Seq a) b
    triangleMap3 = M.fromList $ zip (symIndList 3 3) [toEnum 0..]

    interF_I2 :: M.Map (Linds_3 2) Uind_9 -> Index 0 0 0 0 1 0 0 2 -> Rational
    interF_I2 map1 (_,_,_,_,x,_,_,y) 
                | indI == xVal = 1
                | otherwise = 0
                 where 
                    indI = (M.!) map1 $ sortInd y
                    xVal = getValInd x 0

    jMult2 :: Eq a => Ind 2 a -> Rational
    jMult2 ind 
                | i == j = 1
                | otherwise = 1/2
                 where 
                    i = getValInd ind 0
                    j = getValInd ind 1

    interF_J2 :: M.Map (Uinds_3 2) Lind_9 -> Index 0 0 0 0 0 1 2 0 -> Rational
    interF_J2 map1 (_,_,_,_,_,x,y,_) 
                | indI == xVal = mult
                | otherwise = 0
                 where 
                    indI = (M.!) map1 $ sortInd y
                    xVal = getValInd x 0
                    mult = jMult2 y 

            
    symF_I2 :: M.Map (Linds_3 2) Uind_9 -> Index 0 0 0 0 1 0 0 2 -> Rational
    symF_I2 map1 (_,_,_,_,x,_,_,y) 
                | indI == xVal = mult
                | otherwise = 0
                 where 
                    indI = (M.!) map1 $ sortInd y
                    xVal = getValInd x 0
                    mult = jMult2 y 

    aSymF_I2 :: M.Map (Linds_3 2) Uind_9 -> Index 0 0 0 0 1 0 0 2 -> Rational
    aSymF_I2 map1 (_,_,_,_,x,_,_,y) 
                | indI == xVal = sign 
                | otherwise = 0
                 where 
                    indI = (M.!) map1 $ sortInd y
                    xVal = getValInd x 0
                    sign = fromIntegral $ indSign2 y 


    interF_I3 :: M.Map (Linds_3 3) Uind_19 -> Index 0 0 1 0 0 0 0 3 -> Rational
    interF_I3 map1 (_,_,x,_,_,_,_,y) 
                | indI == xVal = 1
                | otherwise = 0
                 where 
                    indI = (M.!) map1 $ sortInd y
                    xVal = getValInd x 0

    jMult3 :: Eq a => Ind 3 a -> Rational
    jMult3 ind 
                | i == j && j == k = 1
                | i == j || j == k || i == k = 1/3
                | otherwise = 1/6
                 where 
                    i = getValInd ind 0
                    j = getValInd ind 1
                    k = getValInd ind 2

    interF_J3 :: M.Map (Uinds_3 3) Lind_19 -> Index 0 0 0 1 0 0 3 0 -> Rational
    interF_J3 map1 (_,_,_,x,_,_,y,_) 
                | indI == xVal = mult
                | otherwise = 0
                 where 
                    indI = (M.!) map1 $ sortInd y
                    xVal = getValInd x 0
                    mult = jMult3 y 
            
    symF_I3 :: M.Map (Linds_3 3) Uind_19 -> Index 0 0 1 0 0 0 0 3 -> Rational
    symF_I3 map1 (_,_,x,_,_,_,_,y) 
                | indI == xVal = mult
                | otherwise = 0
                 where 
                    indI = (M.!) map1 $ sortInd y
                    xVal = getValInd x 0
                    mult = jMult3 y 

    inter_I2Map :: M.Map (Linds_3 2) Uind_9 -> M.Map (Index 0 0 0 0 1 0 0 2) Rational
    inter_I2Map trian = M.filter (0/=) $ M.fromList $ zip inds vals 
            where
                f = interF_I2 trian 
                inds = tensorIndList (0,0,0,0,1,0,0,2)
                vals = map f inds  

    inter_J2Map :: M.Map (Uinds_3 2) Lind_9 -> M.Map (Index 0 0 0 0 0 1 2 0) Rational
    inter_J2Map trian = M.filter (0/=) $ M.fromList $ zip inds vals 
            where
                f = interF_J2 trian 
                inds = tensorIndList (0,0,0,0,0,1,2,0)
                vals = map f inds  

    sym_I2Map :: M.Map (Linds_3 2) Uind_9 -> M.Map (Index 0 0 0 0 1 0 0 2) Rational
    sym_I2Map trian = M.filter (0/=) $ M.fromList $ zip inds vals 
            where
                f = symF_I2 trian 
                inds = tensorIndList (0,0,0,0,1,0,0,2)
                vals = map f inds  

    aSym_I2Map :: M.Map (Linds_3 2) Uind_9 -> M.Map (Index 0 0 0 0 1 0 0 2) Rational
    aSym_I2Map trian = M.filter (0/=) $ M.fromList $ zip inds vals 
            where
                f = aSymF_I2 trian 
                inds = tensorIndList (0,0,0,0,1,0,0,2)
                vals = map f inds  
        
    inter_I3Map :: M.Map (Linds_3 3) Uind_19 -> M.Map (Index 0 0 1 0 0 0 0 3) Rational
    inter_I3Map trian = M.filter (0/=) $ M.fromList $ zip inds vals 
            where
                f = interF_I3 trian 
                inds = tensorIndList (0,0,1,0,0,0,0,3)
                vals = map f inds  

    inter_J3Map :: M.Map (Uinds_3 3) Lind_19 -> M.Map (Index 0 0 0 1 0 0 3 0) Rational
    inter_J3Map trian = M.filter (0/=) $ M.fromList $ zip inds vals 
            where
                f = interF_J3 trian 
                inds = tensorIndList (0,0,0,1,0,0,3,0)
                vals = map f inds  

    sym_I3Map :: M.Map (Linds_3 3) Uind_19 -> M.Map (Index 0 0 1 0 0 0 0 3) Rational
    sym_I3Map trian = M.filter (0/=) $ M.fromList $ zip inds vals 
            where
                f = symF_I3 trian 
                inds = tensorIndList (0,0,1,0,0,0,0,3)
                vals = map f inds  

    areaDofList :: (Enum a, Eq a, Ord a) => [S.Seq a]
    areaDofList = [ S.fromList [a,b,c,d] | a <- [toEnum 0..toEnum 2], b <- [succ a .. toEnum 3], c <- [a..toEnum 2], d <- [succ c.. toEnum 3], not $ a == c && b > d  ]

    triangleMapArea :: (Enum a, Enum b, Ord a) =>  M.Map (S.Seq a) b
    triangleMapArea = M.fromList $ zip (areaDofList) [toEnum 0..]

    isZeroArea :: (Eq a, Ord a, Enum a) => Ind 4 a -> Bool
    isZeroArea ind 
                | a == b || c == d = True
                | otherwise = False 
                 where 
                    a = getValInd ind 0
                    b = getValInd ind 1
                    c = getValInd ind 2
                    d = getValInd ind 3

    jMultArea :: Eq a => Ind 4 a -> Rational
    jMultArea ind 
                | a == c && b == d = 1/4
                | otherwise = 1/8
                 where 
                    a = getValInd ind 0
                    b = getValInd ind 1
                    c = getValInd ind 2
                    d = getValInd ind 3

    areaSign :: (Ord a, Enum a, Eq a) => Ind 4 a -> Rational
    areaSign s
        | xor (a<b) (c<d) = -1
        | otherwise = 1
        where
            xor True False = True
            xor False True = True
            xor True True = False
            xor False False = False
            [a,b,c,d] = ind2List s 

    canonicalizeArea :: (Ord a, Enum a, Eq a) => Ind 4 a -> (Ind 4 a,Rational)
    canonicalizeArea s = (newS, sign)
        where
            sNew = getSeq s
            [s1,s2] = sort $ map S.sort $ toList $ S.chunksOf 2 sNew
            sign = areaSign s
            newS = mkInd $ (S.><) s1 s2

    interF_IArea :: M.Map (Linds_3 4) Uind_20 -> Index 1 0 0 0 0 0 0 4 -> Rational
    interF_IArea map1 (x,_,_,_,_,_,_,y) 
                | isZeroArea y = 0
                | indI == xVal = snd sortY
                | otherwise = 0
                 where 
                    sortY = canonicalizeArea y
                    indI = (M.!) map1 $ fst sortY
                    xVal = getValInd x 0


    symF_IArea :: M.Map (Linds_3 4) Uind_20 -> Index 1 0 0 0 0 0 0 4 -> Rational
    symF_IArea map1 (x,_,_,_,_,_,_,y) 
                | isZeroArea y = 0
                | indI == xVal = snd sortY * (jMultArea (fst sortY))
                | otherwise = 0
                 where 
                    sortY = canonicalizeArea y
                    indI = (M.!) map1 $ fst sortY
                    xVal = getValInd x 0

    interF_JArea :: M.Map (Uinds_3 4) Lind_20 -> Index 0 1 0 0 0 0 4 0 -> Rational
    interF_JArea map1 (_,x,_,_,_,_,y,_) 
                | isZeroArea y = 0
                | indI == xVal = snd sortY * (jMultArea (fst sortY))
                | otherwise = 0
                 where 
                    sortY = canonicalizeArea y
                    indI = (M.!) map1 $ fst sortY
                    xVal = getValInd x 0

    inter_IAreaMap :: M.Map (Linds_3 4) Uind_20 -> M.Map (Index 1 0 0 0 0 0 0 4) Rational
    inter_IAreaMap trian = M.filter (0/=) $ M.fromList $ zip inds vals 
                where
                 f = interF_IArea trian 
                 inds = tensorIndList (1,0,0,0,0,0,0,4)
                 vals = map f inds  

    inter_JAreaMap :: M.Map (Uinds_3 4) Lind_20 -> M.Map (Index 0 1 0 0 0 0 4 0) Rational
    inter_JAreaMap trian = M.filter (0/=) $ M.fromList $ zip inds vals 
                where
                 f = interF_JArea trian 
                 inds = tensorIndList (0,1,0,0,0,0,4,0)
                 vals = map f inds  

    sym_IAreaMap :: M.Map (Linds_3 4) Uind_20 -> M.Map (Index 1 0 0 0 0 0 0 4) Rational
    sym_IAreaMap trian = M.filter (0/=) $ M.fromList $ zip inds vals 
                where
                 f = symF_IArea trian 
                 inds = tensorIndList (1,0,0,0,0,0,0,4)
                 vals = map f inds  
            
    interMetric ::  M.Map (Index 0 0 0 0 1 0 0 2) Rational ->  M.Map (Index 0 0 0 0 0 1 2 0) Rational  -> M.Map (Index 0 0 0 0 1 1 1 1) Rational 
    interMetric iMap jMap = tensor2Map $ tensorSMult (-2) $ tensorContract_3 (0,0) prod 
            where 
                i = mkTensorFromMap iMap
                j = mkTensorFromMap jMap
                prod = tensorProd i j

    interArea ::  M.Map (Index 1 0 0 0 0 0 0 4) Rational ->  M.Map (Index 0 1 0 0 0 0 4 0) Rational  -> M.Map (Index 1 1 0 0 0 0 1 1) Rational 
    interArea iMap jMap = tensor2Map $ tensorSMult (-4) $ tensorContract_3 (1,1) $ tensorContract_3 (2,2) $ tensorContract_3 (3,3) prod 
        where 
            i = mkTensorFromMap iMap
            j = mkTensorFromMap jMap
            prod = tensorProd i j 

    interEqn1_2 :: M.Map (Index 1 1 0 0 0 0 1 1) Rational  -> M.Map (Index 1 1 0 0 0 0 2 2) Rational 
    interEqn1_2 intArea = tensor2Map intTotal
                        where
                                intAreaTens = mkTensorFromMap intArea
                                int1 = tensorProd intAreaTens delta_3 
                                int2 = tensorProd (tensorTranspose 8 (0,1) $ tensorProd delta_3 delta_3 ) delta_20
                                intTotal = tensorSub int1 int2 

    interEqn1_3 ::  M.Map (Index 0 0 0 0 1 1 1 1) Rational  -> M.Map (Index 1 1 0 0 0 0 1 1) Rational -> M.Map (Index 1 1 0 0 1 1 1 1) Rational
    interEqn1_3 intMetric intArea = tensor2Map intTotal 
                        where
                                intMetricTens = mkTensorFromMap intMetric
                                intAreaTens = mkTensorFromMap intArea
                                int1 = tensorProd intAreaTens delta_9
                                int2 = tensorProd intMetricTens delta_20
                                intTotal = tensorAdd int1 int2

    eta_F :: Index 0 0 0 0 0 0 0 2 -> Rational
    eta_F (_,_,_,_,_,_,_,a) 
                | x == y && x == 0 = 1
                | x == y = -1
                | otherwise = 0
                 where 
                         x = fromEnum $ getValInd a 0
                         y = fromEnum $ getValInd a 1

    invEta_F :: Index 0 0 0 0 0 0 2 0 -> Rational
    invEta_F (_,_,_,_,_,_,a,_) 
                | x == y && x == 0 = 1
                | x == y = -1
                | otherwise = 0
                 where 
                         x = fromEnum $ getValInd a 0
                         y = fromEnum $ getValInd a 1

    eta :: Tensor 0 0 0 0 0 0 0 2 Rational
    eta = Tensor eta_F

    invEta :: Tensor 0 0 0 0 0 0 2 0 Rational
    invEta = Tensor invEta_F

    etaAbs :: M.Map (Index 0 0 0 0 0 1 2 0) Rational -> M.Map (Index 0 0 0 0 0 1 0 0) Rational
    etaAbs intJ2 = tensor2Map $ tensorContract_3 (0,0) $ tensorContract_3 (1,1) t1
                where 
                        t1 = tensorProd eta (mkTensorFromMap intJ2)

    invEtaAbs :: M.Map (Index 0 0 0 0 1 0 0 2) Rational -> M.Map (Index 0 0 0 0 1 0 0 0) Rational
    invEtaAbs intI2 = tensor2Map $ tensorContract_3 (0,0) $ tensorContract_3 (1,1) t1
                where 
                        t1 = tensorProd invEta (mkTensorFromMap intI2)


    permSignN :: Ord a => [a] -> Int
    permSignN [] = 0
    permSignN [a] = 0
    permSignN (x:xs) = (permSignN xs)  + (length $ filter (>x) xs)
    
    permSign :: Ord a => [a] -> Int
    permSign l = (-1)^(permSignN l)

    
    epsilon_F :: Index 0 0 0 0 0 0 0 4 -> Rational
    epsilon_F (_,_,_,_,_,_,_,x)
                | a == b || a == c || a == d || b == c || b == d || c == d = 0
                | otherwise = fromIntegral $ permSign [a,b,c,d]
                 where
                        a = fromEnum $ getValInd x 0
                        b = fromEnum $ getValInd x 1
                        c = fromEnum $ getValInd x 2
                        d = fromEnum $ getValInd x 3

    epsilonMap :: M.Map (Index 0 0 0 0 0 0 0 4) Rational
    epsilonMap = M.filter (0/=) $ M.fromList $ zip inds vals 
                where
                    inds = tensorIndList (0,0,0,0,0,0,0,4)
                    vals = map epsilon_F inds  

    
    flatAreaSTMap :: M.Map (Index 0 0 0 0 0 0 0 4) Rational -> M.Map (Index 0 0 0 0 0 0 0 4) Rational
    flatAreaSTMap eps = tensor2Map $ tensorSub (tensorSub etaProd1 etaProd2) $ mkTensorFromMap eps 
                where
                        etaProd = tensorProd eta eta
                        etaProd1 = tensorTranspose 8 (1,2) etaProd
                        etaProd2 = tensorTranspose 8 (1,3) $ tensorTranspose 8 (2,3) etaProd
    

    flatAreaMap :: M.Map (Index 0 1 0 0 0 0 4 0) Rational -> M.Map (Index 0 0 0 0 0 0 0 4) Rational -> M.Map (Index 0 1 0 0 0 0 0 0) Rational
    flatAreaMap intJMap flatMap = tensor2Map $ tensorContract_3 (0,0) $ tensorContract_3 (1,1) $ tensorContract_3 (2,2) $ tensorContract_3 (3,3) prod
                where
                        intJ = mkTensorFromMap intJMap
                        flatAreaST = mkTensorFromMap flatMap
                        prod = tensorProd flatAreaST intJ 

    flatInterMap :: M.Map (Index 0 1 0 0 0 0 0 0) Rational -> M.Map (Index 1 1 0 0 0 0 1 1) Rational -> M.Map (Index 0 1 0 0 0 0 1 1) Rational
    flatInterMap flatA intArea = tensor2Map $ tensorContract_20 (0,1) prod 
                where
                    flatATens = mkTensorFromMap flatA
                    intAreaTens = mkTensorFromMap intArea 
                    prod = tensorProd intAreaTens flatATens 

    --test the first int cond

    intAIB :: M.Map (Index 1 1 0 0 0 0 1 1) Rational -> M.Map (Index 0 0 0 0 1 1 1 1) Rational -> M.Map (Index 0 1 0 0 0 0 1 1) Rational -> M.Map (Index 1 1 0 0 1 1 1 1) Rational -> Tensor 1 2 0 0 1 1 2 2 Rational 
    intAIB intAreaMap intMetricMap flatIntMap int3Map = tensorSub tens tensTrans  
            where
                intArea = mkTensorFromMap intAreaMap 
                intMetric = mkTensorFromMap intMetricMap 
                flatInt = mkTensorFromMap flatIntMap 
                int3 = mkTensorFromMap int3Map
                block1 = tensorProd delta_20 $ tensorProd delta_20 $ tensorProd delta_9 delta_3 
                block2 = tensorProd intArea $ tensorProd delta_20 delta_9
                block3 = tensorProd delta_20 int3 
                totalBlock = tensorAdd block1 $ tensorAdd block2 block3 
                tens = tensorContract_20 (0,2) $ tensorProd totalBlock flatInt 
                tensTrans = tensorTranspose 7 (0,1) $ tensorTranspose 8 (0,1) tens 