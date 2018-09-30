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
 