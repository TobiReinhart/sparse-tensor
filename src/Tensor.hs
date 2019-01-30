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

module Tensor (
    Tensor(..), mkTensorfromList, mkTensorfromF, getVal, tensorProductWith, tensorProductNumeric, tensorContractWith_3, tensorContractWith_9, tensorContractWith_19,
    tensorContractWith_20, tensorSMult, tensorAdd, tensorSub, symTensor, aSymTensor, blockSymTensor, cyclicSymTensor, tensorTranspose,
    tensorIndList, mkTensorfromFZeros, evalFullTensor, evalTensorVals, unsafeGetVal, tensorProductWith2, tensorProductNumeric2, tensorProductNew
) where

    import Index
    import qualified Data.Sequence as S
    import Numeric.Natural 
    import GHC.TypeNats
    import Data.Proxy
    import Data.Maybe
    import qualified Data.Map.Strict as M


    --start by defining the tensor data type (try using Map instead of functions)

    data Tensor (n1::Nat) (n2::Nat) (n3::Nat) (n4::Nat) (n5::Nat) (n6::Nat) (n7::Nat) (n8::Nat) a =
        Tensor (M.Map (Index n1 n2 n3 n4 n5 n6 n7 n8) a) deriving Show

    instance Functor (Tensor n1 n2 n3 n4 n5 n6 n7 n8) where
        fmap f (Tensor tMap) = Tensor (M.map f tMap)

    --later on the primitive tensors, i.e ivarsTensors and various kinds of deltas are most easily constructed from functions

    --higher rank tensors can be built from these by tensor products and contractions

    mkTensorfromList :: [((Index n1 n2 n3 n4 n5 n6 n7 n8), a)] -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    mkTensorfromList l =  Tensor $ M.fromList l 

    --for constructing tensors from functions we need a function that takes a tensor and constructs (from its rank the list of all possible indices of the tensor)

    type Rank = (Int, Int, Int, Int, Int, Int, Int, Int)

    tensorIndList :: (KnownNat n1, KnownNat n2, KnownNat n3, KnownNat n4, KnownNat n5, KnownNat n6, KnownNat n7, KnownNat n8) =>
        Rank -> [Index n1 n2 n3 n4 n5 n6 n7 n8] 
    tensorIndList (r1,r2,r3,r4,r5,r6,r7,r8) =  map (\(x1,x2,x3,x4,x5,x6,x7,x8) -> ((mkInd x1), (mkInd x2), (mkInd x3), (mkInd x4), (mkInd x5), (mkInd x6), (mkInd x7), (mkInd x8))) list
            where 
                list = [ (y1,y2,y3,y4,y5,y6,y7,y8) | y1 <- (getRangeList r1 20), y2 <- (getRangeList r2 20), y3 <- (getRangeList r3 19), y4 <- (getRangeList r4 19),
                 y5 <- (getRangeList r5 9), y6 <- (getRangeList r6 9), y7 <- (getRangeList r7 3), y8 <- (getRangeList r8 3)]

    --we need to use at least S.Seq for this and for getRangeList
    --this function works by producing a list of all possible indices for a given rank (as Ints) and then translating it to Inds
    --if this is to slow we need to directly construct the Inds 

    mkTensorfromF :: (KnownNat n1, KnownNat n2, KnownNat n3, KnownNat n4, KnownNat n5, KnownNat n6, KnownNat n7, KnownNat n8, Num a, Eq a) =>
        Rank -> ((Index n1 n2 n3 n4 n5 n6 n7 n8) -> a) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    mkTensorfromF rank f = Tensor $ M.filter ( /= 0) $ M.fromList (zip indList valueList)
            where 
                indList = tensorIndList rank 
                valueList = map f indList

    mkTensorfromFZeros :: (KnownNat n1, KnownNat n2, KnownNat n3, KnownNat n4, KnownNat n5, KnownNat n6, KnownNat n7, KnownNat n8) =>
        Rank -> ((Index n1 n2 n3 n4 n5 n6 n7 n8) -> a) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    mkTensorfromFZeros rank f = Tensor $ M.fromList (zip indList valueList)
            where 
                indList = tensorIndList rank 
                valueList = map f indList    
                
    --mkTensorfromF stores only non zero values, mkTensorfromFZeros stores all values

    getVal :: Num a => Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Index n1 n2 n3 n4 n5 n6 n7 n8 -> a
    getVal (Tensor map1) ind 
            | M.member ind map1 = (M.!) map1 ind
            | otherwise = 0  

    unsafeGetVal :: Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Index n1 n2 n3 n4 n5 n6 n7 n8 -> a
    unsafeGetVal (Tensor map1) ind = (M.!) map1 ind

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

    --now start with the tensor algebra functions

    tensorSMult :: Num a => a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a 
    tensorSMult a = fmap ( (*) a)

    tensorAdd :: Num a => Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorAdd (Tensor map1) (Tensor map2) = Tensor $ M.unionWith (+) map1 map2 
    
    tensorSub :: Num a => Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorSub t1 t2 = tensorAdd t1 (tensorSMult (-1) t2)

    tensorTranspose :: Int -> (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorTranspose i pair (Tensor map1) = Tensor $ M.mapKeys (swapPosIndex i pair) map1 

    tensorBlockTranspose :: Int -> ([Int],[Int]) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorBlockTranspose i pair (Tensor map1) = Tensor $ M.mapKeys (swapBlockPosIndex i pair) map1 

    --symmetrizer

    symTensor :: (Fractional a) =>
         Int -> (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a 
    symTensor i pair t = tensorSMult (1/2) $ tensorAdd t $ tensorTranspose i pair t

    aSymTensor :: (Fractional a) =>
        Int -> (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a 
    aSymTensor i pair t = tensorSMult (1/2) $ tensorSub t $ tensorTranspose i pair t

    blockSymTensor :: (Fractional a) =>
         Int -> ([Int],[Int]) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a 
    blockSymTensor i pair t = tensorSMult (1/2) $ tensorAdd t $ tensorBlockTranspose i pair t

    --for the cyclic symmetrization we need some extra stuff

    factorial :: (Num a, Eq a) => a -> a 
    factorial 0 = 1
    factorial n = n * factorial (n-1)

    cyclicSymTensor :: (Fractional a) =>
         Int -> [Int] -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a 
    cyclicSymTensor i list (Tensor map1) = tensorSMult (1/fac) (Tensor map2)
                    where
                        fac = fromIntegral $ factorial $ length list 
                        cIndsF = cyclicSwapIndex i list 
                        g = \k a -> (foldl (+) 0  (map ((M.!) map1)  (cIndsF k)) )
                        map2 = M.mapWithKey g map1  

    --now tensor products and constractions

    

    tensorProductFNew :: (Num a ,Eq a) => ((Index n1 n2 n3 n4 n5 n6 n7 n8), a) -> ((Index m1 m2 m3 m4 m5 m6 m7 m8), a) -> ((Index (n1+m1) (n2+m2) (n3+m3) (n4+m4) (n5+m5) (n6+m6) (n7+m7) (n8+m8)), a)
    tensorProductFNew (ind1,val1) (ind2,val2) =  (newInd, newVal)
                    where 
                        newVal = val1 * val2
                        newInd = combineIndex ind1 ind2

    tensorProductNew :: (Num a, Eq a) => Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor m1 m2 m3 m4 m5 m6 m7 m8 a -> 
        Tensor (n1+m1) (n2+m2) (n3+m3) (n4+m4) (n5+m5) (n6+m6) (n7+m7) (n8+m8) a
    tensorProductNew (Tensor map1) (Tensor map2) = Tensor $ M.fromList newList
                    where 
                        l1 = M.toAscList $ M.filter (/= 0) map1 
                        l2 = M.toAscList $ M.filter (/= 0) map2 
                        newList = map (\x -> tensorProductFNew x ) l1 <*> l2  

    

    tensorProductF :: (a -> b -> c) ->  M.Map (Index n1 n2 n3 n4 n5 n6 n7 n8) a -> M.Map (Index m1 m2 m3 m4 m5 m6 m7 m8) b -> Index m1 m2 m3 m4 m5 m6 m7 m8 
        -> M.Map (Index (n1+m1) (n2+m2) (n3+m3) (n4+m4) (n5+m5) (n6+m6) (n7+m7) (n8+m8)) c
    tensorProductF f map1 map2 index =  map3
                    where 
                        val = (M.!) map2 index 
                        map3 = M.map (\y -> f y val) $ M.mapKeys (\x -> combineIndex x index) map1


    tensorProductFNumeric :: (Num a, Eq a) => M.Map (Index n1 n2 n3 n4 n5 n6 n7 n8) a -> M.Map (Index m1 m2 m3 m4 m5 m6 m7 m8) a -> Index m1 m2 m3 m4 m5 m6 m7 m8 
        -> M.Map (Index (n1+m1) (n2+m2) (n3+m3) (n4+m4) (n5+m5) (n6+m6) (n7+m7) (n8+m8)) a
    tensorProductFNumeric map1 map2 index = map3
                    where 
                        val = (M.!) map2 index 
                        map3 = M.map (\y -> (*) y val) $ M.mapKeys (\x -> combineIndex x index) map1

    
    tensorProductNumeric2 :: (Num a, Eq a) => Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor m1 m2 m3 m4 m5 m6 m7 m8 a -> 
        Tensor (n1+m1) (n2+m2) (n3+m3) (n4+m4) (n5+m5) (n6+m6) (n7+m7) (n8+m8) a
    tensorProductNumeric2 (Tensor map1) (Tensor map2) = Tensor newMap
                    where 
                        map1New = M.filter (/=0) map1
                        map2New = M.filter (/=0) map2
                        indList = M.keys map2New 
                        mapList = map (tensorProductFNumeric map1New map2New) indList
                        newMap = M.unions mapList

    tensorProductWith2 :: (a -> b -> c) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor m1 m2 m3 m4 m5 m6 m7 m8 b -> 
        Tensor (n1+m1) (n2+m2) (n3+m3) (n4+m4) (n5+m5) (n6+m6) (n7+m7) (n8+m8) c
    tensorProductWith2 f (Tensor map1) (Tensor map2) = Tensor newMap
                    where 
                        indList = M.keys map2
                        mapList = map (tensorProductF f map1 map2) indList
                        newMap = M.unions mapList

    --seems like the old versions ...2 are faster

    tensorProductWith :: (a -> b -> c) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor m1 m2 m3 m4 m5 m6 m7 m8 b -> 
        Tensor (n1+m1) (n2+m2) (n3+m3) (n4+m4) (n5+m5) (n6+m6) (n7+m7) (n8+m8) c
    tensorProductWith f (Tensor map1) (Tensor map2) = Tensor newMap
                    where
                        pairs1 = M.assocs map1 
                        pairs2 = M.assocs map2
                        combineF = \(a,b) (c,d) -> (combineIndex a c, f b d)
                        newMap = M.fromAscList $ combineF <$> pairs1 <*> pairs2

    tensorProductNumeric :: (Num a, Eq a) => Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> Tensor m1 m2 m3 m4 m5 m6 m7 m8 a -> 
        Tensor (n1+m1) (n2+m2) (n3+m3) (n4+m4) (n5+m5) (n6+m6) (n7+m7) (n8+m8) a
    tensorProductNumeric (Tensor map1) (Tensor map2) = Tensor newMap
                    where 
                        pairs1 = M.assocs $ M.filter (/=0) map1 
                        pairs2 = M.assocs $ M.filter (/=0) map2
                        combineF = \(a,b) (c,d) -> (combineIndex a c, (*) b d)
                        newMap = M.fromAscList $ combineF <$> pairs1 <*> pairs2

    --see if this implementation of the tensor product is fast enough ??

    --the problem is that we need to change both keys and values completely (maybe working with lists intermediatly is better ?)

    --now the contraction of a given Tensor -> add folds of these functions?

    tensorContractWith_20 :: (KnownNat n1, KnownNat n2) =>
        (Int,Int) -> (a -> a -> a) -> Tensor (n1+1) (n2+1) n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorContractWith_20 pair f (Tensor map1) = Tensor map2 
                    where 
                        mapFilt = M.filterWithKey (\k _ -> isContractionIndex 1 pair k) map1 
                        map2 = M.mapKeysWith f (delContractionIndex_20 pair) mapFilt

    tensorContractWith_19 :: (KnownNat n3, KnownNat n4) =>
        (Int,Int) -> (a -> a -> a) -> Tensor n1 n2 (n3+1) (n4+1) n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorContractWith_19 pair f (Tensor map1) = Tensor map2 
                    where 
                        mapFilt = M.filterWithKey (\k _ -> isContractionIndex 2 pair k) map1 
                        map2 = M.mapKeysWith f (delContractionIndex_19 pair) mapFilt
                    
    tensorContractWith_9 :: (KnownNat n5, KnownNat n6) =>
        (Int,Int) -> (a -> a -> a) -> Tensor n1 n2 n3 n4 (n5+1) (n6+1) n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorContractWith_9 pair f (Tensor map1) = Tensor map2 
                    where 
                        mapFilt = M.filterWithKey (\k _ -> isContractionIndex 3 pair k) map1 
                        map2 = M.mapKeysWith f (delContractionIndex_9 pair) mapFilt
                    
    tensorContractWith_3 :: (KnownNat n7, KnownNat n8) =>
        (Int,Int) -> (a -> a -> a) -> Tensor n1 n2 n3 n4 n5 n6 (n7+1) (n8+1) a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorContractWith_3 pair f (Tensor map1) = Tensor map2 
                    where 
                        mapFilt = M.filterWithKey (\k _ -> isContractionIndex 4 pair k) map1 
                        map2 = M.mapKeysWith f (delContractionIndex_3 pair) mapFilt

    --for evaluating tensors we can use functions that evaluate the tensors for one specific index (i.e replacing one index with a number)
    --the result ist then a tensor of lower rank

    --it is however not suggested to use these functions if we are interested in all values stored in a tensor as it is usually slow (a lot of inds must be created peice by piece)

    --eval first indices at pos i with value j

    evalTensor_20_1 :: KnownNat n1 => Tensor (n1+1) n2 n3 n4 n5 n6 n7 n8 a -> Int -> Int -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    evalTensor_20_1 (Tensor map1) i j = Tensor map3
                    where 
                        checkKey = \(a,b,c,d,e,f,g,h) _ -> checkInd a i $ toEnum j 
                        redKey = \(a,b,c,d,e,f,g,h) -> (delInd i a,b,c,d,e,f,g,h)
                        map2 = M.filterWithKey (checkKey) map1 
                        map3 = M.mapKeys redKey map2 

    evalTensor_20_2 :: KnownNat n2 => Tensor n1 (n2+1) n3 n4 n5 n6 n7 n8 a -> Int -> Int -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    evalTensor_20_2 (Tensor map1) i j = Tensor map3
                    where 
                        checkKey = \(a,b,c,d,e,f,g,h) _ -> checkInd b i $ toEnum j 
                        redKey = \(a,b,c,d,e,f,g,h) -> (a,delInd i b,c,d,e,f,g,h)
                        map2 = M.filterWithKey (checkKey) map1 
                        map3 = M.mapKeys redKey map2
                        
    evalTensor_19_1 :: KnownNat n3 => Tensor n1 n2 (n3+1) n4 n5 n6 n7 n8 a -> Int -> Int -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    evalTensor_19_1 (Tensor map1) i j = Tensor map3
                    where 
                        checkKey = \(a,b,c,d,e,f,g,h) _ -> checkInd c i $ toEnum j 
                        redKey = \(a,b,c,d,e,f,g,h) -> (a,b,delInd i c,d,e,f,g,h)
                        map2 = M.filterWithKey (checkKey) map1 
                        map3 = M.mapKeys redKey map2

    evalTensor_19_2 :: KnownNat n4 => Tensor n1 n2 n3 (n4+1) n5 n6 n7 n8 a -> Int -> Int -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    evalTensor_19_2 (Tensor map1) i j = Tensor map3
                    where 
                        checkKey = \(a,b,c,d,e,f,g,h) _ -> checkInd d i $ toEnum j 
                        redKey = \(a,b,c,d,e,f,g,h) -> (a,b,c,delInd i d,e,f,g,h)
                        map2 = M.filterWithKey (checkKey) map1 
                        map3 = M.mapKeys redKey map2

    evalTensor_9_1 :: KnownNat n5 => Tensor n1 n2 n3 n4 (n5+1) n6 n7 n8 a -> Int -> Int -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    evalTensor_9_1 (Tensor map1) i j = Tensor map3
                    where 
                        checkKey = \(a,b,c,d,e,f,g,h) _ -> checkInd e i $ toEnum j 
                        redKey = \(a,b,c,d,e,f,g,h) -> (a,b,c,d,delInd i e,f,g,h)
                        map2 = M.filterWithKey (checkKey) map1 
                        map3 = M.mapKeys redKey map2

    evalTensor_9_2 :: KnownNat n6 => Tensor n1 n2 n3 n4 n5 (n6+1) n7 n8 a -> Int -> Int -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    evalTensor_9_2 (Tensor map1) i j = Tensor map3
                    where 
                        checkKey = \(a,b,c,d,e,f,g,h) _ -> checkInd f i $ toEnum j 
                        redKey = \(a,b,c,d,e,f,g,h) -> (a,b,c,d,e,delInd i f,g,h)
                        map2 = M.filterWithKey (checkKey) map1 
                        map3 = M.mapKeys redKey map2

    evalTensor_3_1 :: KnownNat n7 => Tensor n1 n2 n3 n4 n5 n6 (n7+1) n8 a -> Int -> Int -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    evalTensor_3_1 (Tensor map1) i j = Tensor map3
                    where 
                        checkKey = \(a,b,c,d,e,f,g,h) _ -> checkInd g i $ toEnum j 
                        redKey = \(a,b,c,d,e,f,g,h) -> (a,b,c,d,e,f,delInd i g,h)
                        map2 = M.filterWithKey (checkKey) map1 
                        map3 = M.mapKeys redKey map2

    evalTensor_3_2 :: KnownNat n8 => Tensor n1 n2 n3 n4 n5 n6 n7 (n8+1) a -> Int -> Int -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    evalTensor_3_2 (Tensor map1) i j = Tensor map3
                    where 
                        checkKey = \(a,b,c,d,e,f,g,h) _ -> checkInd h i $ toEnum j 
                        redKey = \(a,b,c,d,e,f,g,h) -> (a,b,c,d,e,f,g,delInd i h)
                        map2 = M.filterWithKey (checkKey) map1 
                        map3 = M.mapKeys redKey map2

    --full evaluation of a tensor can be achieved by simply extracting all the values in the map

    evalFullTensor :: Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> [(Index n1 n2 n3 n4 n5 n6 n7 n8,a)]
    evalFullTensor (Tensor m) = M.assocs m  

    evalTensorVals :: Tensor n1 n2 n3 n4 n5 n6 n7 n8 a -> [a]
    evalTensorVals (Tensor m) = M.elems m

    --this is all information we need to extract from the tensor

    --the only thing missing is mapping the keys (Index n1 .. n8) to sparse matrix indices (Int,Int) -> in BasicTensor

   
    

   



       
    

