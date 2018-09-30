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

) where

    import Index
    import Tensor
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

    symIndList :: Enum a => Int -> Int -> [[a]]
    symIndList n j 
            | n <= toEnum 0 = error "wrong number of indices"
            | n == 1 = [ [a] | a <- [toEnum 0.. toEnum j] ]
            | otherwise = [ a ++ [b] | a <- (symIndList (n-1) j), b <- [(last a)..toEnum j] ] 

    --first define the functions for building the intertwiners as maps

    triangleMap2 :: (Enum a, Enum b, Ord a) =>  M.Map [a] b
    triangleMap2 = M.fromList $ zip (symIndList 2 3) [toEnum 1..]

    triangleMap3 :: (Enum a, Enum b, Ord a) =>  M.Map [a] b
    triangleMap3 = M.fromList $ zip (symIndList 3 3) [toEnum 1..]

    --construct from these functions the functions for the intertwiners

    --or implement the intertwiners in some other way ?? 

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






    --these functions are specified for are metric (21 dofs)

    index2Sparse1 :: Index 0 1 0 0 0 0 1 1 -> (Int,Int) 
    index2Sparse1 (_, x2, _, _, _, _, x7, x8) = ((m-1)*4+n,a)
                             where 
                                 a = 1 + (fromEnum $ getValInd x2 0)
                                 m = 1 + (fromEnum $ getValInd x7 0)
                                 n = 1 + (fromEnum $ getValInd x8 0)
 
    --the V_Ai i index always comes after the delta_mn n index (order of indices is important !)

    --check this later on when we have extracted the equations
 
    index2Sparse2 :: Index 0 1 0 0 0 0 1 2 -> (Int,Int) 
    index2Sparse2 (_, x2, _, _, _, _, x7, x8) = ((m-1)*4+n,21+(a-1)*4+i)
                             where 
                                 a = 1 + (fromEnum $ getValInd x2 0)
                                 i = 1 + (fromEnum $ getValInd x8 1)
                                 m = 1 + (fromEnum $ getValInd x7 0)
                                 n = 1 + (fromEnum $ getValInd x8 0)
 
    index2Sparse3 :: Index 0 1 0 0 0 1 1 1 -> (Int,Int) 
    index2Sparse3 (_, x2, _, _, _, x6, x7, x8) = ((m-1)*4+n,21*5+(a-1)*10+i)
                             where 
                                 a = 1 + (fromEnum $ getValInd x2 0)
                                 i = 1 + (fromEnum $ getValInd x6 0)
                                 m = 1 + (fromEnum $ getValInd x7 0)
                                 n = 1 + (fromEnum $ getValInd x8 0)
 
    index2Sparse4 :: Index 0 1 0 0 1 0 0 2 -> (Int,Int) 
    index2Sparse4 (_, x2, _, _, x5, _, _, x8) = ((j-1)*4+n,21+(a-1)*4+i)
                             where 
                                 a = 1 + (fromEnum $ getValInd x2 0)
                                 j = 1 + (fromEnum $ getValInd x5 0)
                                 i = 1 + (fromEnum $ getValInd x8 1)
                                 n = 1 + (fromEnum $ getValInd x8 0)

    index2Sparse5 :: Index 0 1 0 0 1 1 0 1 -> (Int,Int) 
    index2Sparse5 (_, x2, _, _, x5, x6, _, x8) = ((j-1)*4+n,21*5+(a-1)*10+i)
                             where 
                                 a = 1 + (fromEnum $ getValInd x2 0)
                                 j = 1 + (fromEnum $ getValInd x5 0)
                                 i = 1 + (fromEnum $ getValInd x6 1)
                                 n = 1 + (fromEnum $ getValInd x8 0)

    index2Sparse6 :: Index 0 1 1 0 0 1 0 1 -> (Int,Int) 
    index2Sparse6 (_, x2, x3, _, _, x6, _, x8) = ((j-1)*4+n,21*5+(a-1)*10+i)
                             where 
                                 a = 1 + (fromEnum $ getValInd x2 0)
                                 j = 1 + (fromEnum $ getValInd x3 0)
                                 i = 1 + (fromEnum $ getValInd x6 1)
                                 n = 1 + (fromEnum $ getValInd x8 0)
 