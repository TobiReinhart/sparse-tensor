--this modlue defines some of the needed primitive tnesors and the constructor functions for the 6 blocks of the diffeo equations
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

module BasicTensors (
    triangleMap2, triangleMap3, interI_2, interJ_2, symI_2, interI_3, interJ_3, symI_3, areaDofList

) where

    import Index
    import Tensor
    import Ivar 
    import qualified Data.Sequence as S
    import Numeric.Natural 
    import GHC.TypeNats
    import Data.Proxy
    import Data.Maybe
    import qualified Data.Map.Strict as M

    --define some basic tensors

    delta_3F :: Index 0 0 0 0 0 0 1 1 -> Rational
    delta_3F (_,_,_,_,_,_,a,b) 
            | fromEnum (getValInd a  0) == fromEnum ( getValInd b 0) = 1
            | otherwise = 0

    delta_3 :: Tensor 0 0 0 0 0 0 1 1 Rational
    delta_3 = mkTensorfromF (0,0,0,0,0,0,1,1) delta_3F

    delta_9F :: Index 0 0 0 0 1 1 0 0 -> Rational
    delta_9F (_,_,_,_,a,b,_,_) 
            | fromEnum (getValInd a  0) == fromEnum ( getValInd b 0) = 1
            | otherwise = 0

    delta_9 :: Tensor 0 0 0 0 1 1 0 0 Rational
    delta_9 = mkTensorfromF (0,0,0,0,1,1,0,0) delta_9F

    delta_19F :: Index 0 0 1 1 0 0 0 0 -> Rational
    delta_19F (_,_,a,b,_,_,_,_) 
            | fromEnum (getValInd a  0) == fromEnum ( getValInd b 0) = 1
            | otherwise = 0

    delta_19 :: Tensor 0 0 1 1 0 0 0 0 Rational
    delta_19 = mkTensorfromF (0,0,1,1,0,0,0,0) delta_19F

    delta_20F :: Index 1 1 0 0 0 0 0 0 -> Rational
    delta_20F (a,b,_,_,_,_,_,_) 
            | fromEnum (getValInd a  0) == fromEnum ( getValInd b 0) = 1
            | otherwise = 0

    delta_20 :: Tensor 1 1 0 0 0 0 0 0 Rational
    delta_20 = mkTensorfromF (1,1,0,0,0,0,0,0) delta_20F

    --the next step is defining the intertwiners

    getLastSeq :: S.Seq a -> a
    getLastSeq (S.Empty) = error "empty seq has no last elem"
    getLastSeq ((S.:|>) xs x) = x
    

    symIndList :: Enum a => Int -> Int -> [S.Seq a]
    symIndList n j 
            | n <= toEnum 0 = error "wrong number of indices"
            | n == 1 = [ S.singleton a | a <- [toEnum 0.. toEnum j] ]
            | otherwise = [ (S.|>) a b | a <- (symIndList (n-1) j), b <- [(getLastSeq a)..toEnum j] ] 

    --first define the functions for building the intertwiners as maps -> must be adapted in the datatype s.t. [a] is the right Ind data type
    --probably already in symIndList

    triangleMap2 :: (Enum a, Enum b, Ord a) =>  M.Map (S.Seq a) b
    triangleMap2 = M.fromList $ zip (symIndList 2 3) [toEnum 1..]

    triangleMap3 :: (Enum a, Enum b, Ord a) =>  M.Map (S.Seq a) b
    triangleMap3 = M.fromList $ zip (symIndList 3 3) [toEnum 1..]

    --construct from these functions the functions for the intertwiners

    --or implement the intertwiners in some other way ??  -> maybe directly as map or from a list ??

    --maybe directly construct the tensor from a list ?

    --we test the following and if it is too slow change it !!

    interF_I2 :: M.Map (Linds_3 2) Uind_9 -> Index 0 0 0 0 1 0 0 2 -> Rational
    interF_I2 map1 (_,_,_,_,x,_,_,y) 
                | indI == xVal = 1
                | otherwise = 0
                 where 
                    indI = (M.!) map1 $ sortInd y
                    xVal = getValInd x 0

    --for the J intertwiners we need factors

    jMult2 :: Eq a => Ind 2 a -> Rational
    jMult2 ind 
                | i == j = 1
                | otherwise = 2
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

    --and define the symmetrizer (for equations without fractions)                
            
    symF_I2 :: M.Map (Linds_3 2) Uind_9 -> Index 0 0 0 0 1 0 0 2 -> Rational
    symF_I2 map1 (_,_,_,_,x,_,_,y) 
                | indI == xVal = mult
                | otherwise = 0
                 where 
                    indI = (M.!) map1 $ sortInd y
                    xVal = getValInd x 0
                    mult = jMult2 y 


    --now the 3 intertwiners

    interF_I3 :: M.Map (Linds_3 3) Uind_19 -> Index 0 0 1 0 0 0 0 3 -> Rational
    interF_I3 map1 (_,_,x,_,_,_,_,y) 
                | indI == xVal = 1
                | otherwise = 0
                 where 
                    indI = (M.!) map1 $ sortInd y
                    xVal = getValInd x 0

    --for the J intertwiners we need factors

    jMult3 :: Eq a => Ind 3 a -> Rational
    jMult3 ind 
                | i == j && j == k = 1
                | i == j || j == k || i == k = 3
                | otherwise = 6
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

    --and define the symmetrizer (for equations without fractions)                
            
    symF_I3 :: M.Map (Linds_3 3) Uind_19 -> Index 0 0 1 0 0 0 0 3 -> Rational
    symF_I3 map1 (_,_,x,_,_,_,_,y) 
                | indI == xVal = mult
                | otherwise = 0
                 where 
                    indI = (M.!) map1 $ sortInd y
                    xVal = getValInd x 0
                    mult = jMult3 y 

    --now define the tensors

    interI_2 :: M.Map (Linds_3 2) Uind_9 -> Tensor 0 0 0 0 1 0 0 2 Rational
    interI_2 map1 = mkTensorfromF (0,0,0,0,1,0,0,2) (interF_I2 map1) 

    interJ_2 :: M.Map (Uinds_3 2) Lind_9 -> Tensor 0 0 0 0 0 1 2 0 Rational
    interJ_2 map1 = mkTensorfromF (0,0,0,0,0,1,2,0) (interF_J2 map1) 

    symI_2 :: M.Map (Linds_3 2) Uind_9 -> Tensor 0 0 0 0 1 0 0 2 Rational
    symI_2 map1 = mkTensorfromF (0,0,0,0,1,0,0,2) (symF_I2 map1) 

    interI_3 :: M.Map (Linds_3 3) Uind_19 -> Tensor 0 0 1 0 0 0 0 3 Rational
    interI_3 map1 = mkTensorfromF (0,0,1,0,0,0,0,3) (interF_I3 map1) 

    interJ_3 :: M.Map (Uinds_3 3) Lind_19 -> Tensor 0 0 0 1 0 0 3 0 Rational
    interJ_3 map1 = mkTensorfromF (0,0,0,1,0,0,3,0) (interF_J3 map1) 

    symI_3 :: M.Map (Linds_3 3) Uind_19 -> Tensor 0 0 1 0 0 0 0 3 Rational
    symI_3 map1 = mkTensorfromF (0,0,1,0,0,0,0,3) (symF_I3 map1) 

    --the only simple tensor that is missing is the area metric intertwiner

    --we try it  this way if it is fast enough

    areaDofList :: (Enum a, Eq a, Ord a) => [S.Seq a]
    areaDofList = [ S.fromList [a,b,c,d] | a <- [toEnum 0..toEnum 2], b <- [succ a .. toEnum 3], c <- [a..toEnum 2], d <- [succ c.. toEnum 3], not $ a == c && b > d  ]

    triangleMapArea :: (Enum a, Enum b, Ord a) =>  M.Map (S.Seq a) b
    triangleMapArea = M.fromList $ zip (areaDofList) [toEnum 1..]

    jMultArea :: Eq a => Ind 4 a -> Rational
    jMultArea ind 
                | a == c && b == d = 4
                | otherwise = 8
                 where 
                    a = getValInd ind 0
                    b = getValInd ind 1
                    c = getValInd ind 2
                    d = getValInd ind 3

    --de need to canonicalize the are indices

    isZeroArea :: (Eq a, Ord a, Enum a) => Ind 4 a -> Bool
    isZeroArea ind 
                | a == b || c == d = True
                | otherwise = False 
                 where 
                    a = getValInd ind 0
                    b = getValInd ind 1
                    c = getValInd ind 2
                    d = getValInd ind 3

    blockSortArea :: (Eq a, Ord a, Enum a) => Ind 4 a -> Ind 4 a
    blockSortArea ind
                | a < c || (a == c && b <= d) = ind
                | otherwise = mkInd $ S.fromList [c,d,a,b] 
                 where 
                    a = getValInd ind 0
                    b = getValInd ind 1
                    c = getValInd ind 2
                    d = getValInd ind 3

    aPairSortArea :: (Eq a, Ord a, Enum a) => Ind 4 a -> (Ind 4 a, Rational)
    aPairSortArea ind
                | a < b && c < d = (ind, 1)
                | a < b && c > d = (mkInd $ S.fromList [a,b,d,c], -1)
                | a > b && c < d = (mkInd $ S.fromList [b,a,c,d], -1)
                | otherwise = (mkInd $ S.fromList [b,a,d,c], 1)
                 where 
                    a = getValInd ind 0
                    b = getValInd ind 1
                    c = getValInd ind 2
                    d = getValInd ind 3


    canonicalizeArea :: (Eq a, Ord a, Enum a) => Ind 4 a -> (Ind 4 a,Rational)
    canonicalizeArea = aPairSortArea.blockSortArea

    --now define the intertwiner functions for the area metric

    interF_IArea :: M.Map (Linds_3 4) Uind_20 -> Index 1 0 0 0 0 0 0 4 -> Rational
    interF_IArea map1 (x,_,_,_,_,_,_,y) 
                | isZeroArea y = 0
                | indI == xVal = snd sortY
                | otherwise = 0
                 where 
                    sortY = canonicalizeArea y
                    indI = (M.!) map1 $ fst sortY
                    xVal = getValInd x 0

    interF_JArea :: M.Map (Uinds_3 4) Lind_20 -> Index 0 1 0 0 0 0 4 0 -> Rational
    interF_JArea map1 (_,x,_,_,_,_,y,_) 
                | isZeroArea y = 0
                | indI == xVal = snd sortY * (jMultArea y)
                | otherwise = 0
                 where 
                    sortY = canonicalizeArea y
                    indI = (M.!) map1 $ fst sortY
                    xVal = getValInd x 0

    --now deifne the tensors

    interI_Area :: M.Map (Linds_3 4) Uind_20 -> Tensor 1 0 0 0 0 0 0 4 Rational
    interI_Area map1 = mkTensorfromF (1,0,0,0,0,0,0,4) (interF_IArea map1) 

    interJ_Area :: M.Map (Uinds_3 4) Lind_20 -> Tensor 0 1 0 0 0 0 4 0 Rational
    interJ_Area map1 = mkTensorfromF (0,1,0,0,0,0,4,0) (interF_JArea map1) 

    --the last step is defining the metric and area metric intertwiner, they booth need the appropriate maps

    interMetric ::  M.Map (Linds_3 2) Uind_9 ->  M.Map (Uinds_3 2) Lind_9  -> Tensor 0 0 0 0 1 1 1 1 Rational 
    interMetric iMap jMap = tensorSMult (-2) $ tensorContractWith_3 (0,0) (+) prod 
            where 
                i = interI_2 iMap
                j = interJ_2 jMap
                prod = tensorProductWith (*) i j 

    interArea ::  M.Map (Linds_3 4) Uind_20 ->  M.Map (Uinds_3 4) Lind_20  -> Tensor 1 1 0 0 0 0 1 1 Rational 
    interArea iMap jMap = tensorSMult (-4) $ tensorContractWith_3 (1,1) (+) 
        $ tensorContractWith_3 (2,2) (+) $ tensorContractWith_3 (3,3) (+) prod 
            where 
                i = interI_Area iMap
                j = interJ_Area jMap
                prod = tensorProductWith (*) i j 

    --we also need the ivar Tensors (careful where we start with Enums!!)

    ivar1F :: Index 0 1 0 0 0 0 0 0 -> Ivar Rational 
    ivar1F (_,a,_,_,_,_,_,_) = number2Ivar $ 1 + (fromEnum $ getValInd a 0)

    ivar2F :: Index 0 1 0 0 0 0 0 1 -> Ivar Rational 
    ivar2F (_,a,_,_,_,_,_,b) = number2Ivar $ (21+1) + (fromEnum $ getValInd a 0)*4 + (fromEnum $ getValInd b 0)
    
    ivar3F :: Index 0 1 0 0 0 1 0 0 -> Ivar Rational 
    ivar3F (_,a,_,_,_,b,_,_) = number2Ivar $ (21*5+1) + (fromEnum $ getValInd a 0)*10 + (fromEnum $ getValInd b 0)

    --define the tensors

    ivar1 :: Tensor 0 1 0 0 0 0 0 0 (Ivar Rational)
    ivar1 = mkTensorfromF (0,1,0,0,0,0,0,0) ivar1F

    ivar2 :: Tensor 0 1 0 0 0 0 0 1 (Ivar Rational)
    ivar2 = mkTensorfromF (0,1,0,0,0,0,0,1) ivar2F

    ivar3 :: Tensor 0 1 0 0 0 1 0 0 (Ivar Rational)
    ivar3 = mkTensorfromF (0,1,0,0,0,1,0,0) ivar3F



    --these are all tensors we need

    --the last step is constructionf the equivariance equations

                