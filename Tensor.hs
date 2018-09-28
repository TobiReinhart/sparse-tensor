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
        Tensor (M.Map (Index n1 n2 n3 n4 n5 n6 n7 n8) a)

    instance Functor (Tensor n1 n2 n3 n4 n5 n6 n7 n8) where
        fmap f (Tensor tMap) = Tensor (M.map f tMap)

    --later on the primitive tensors, i.e ivarsTensors and various kinds of deltas are most easily constructed from functions

    --higher rank tensors can be built from these by tensor products and contractions

    mkTensorfromList :: (KnownNat n1, KnownNat n2, KnownNat n3, KnownNat n4, KnownNat n5, KnownNat n6, KnownNat n7, KnownNat n8, Num a) =>
        [((Index n1 n2 n3 n4 n5 n6 n7 n8), a)] -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    mkTensorfromList l =  Tensor $ M.fromList l 

    --for constructing tensors from functions we need a function that takes a tensor and constructs (from its rank the list of all possible indices of the tensor)

    type Rank = (Int, Int, Int, Int, Int, Int, Int, Int)

    tensorIndList :: (KnownNat n1, KnownNat n2, KnownNat n3, KnownNat n4, KnownNat n5, KnownNat n6, KnownNat n7, KnownNat n8) =>
        Rank -> [Index n1 n2 n3 n4 n5 n6 n7 n8] 
    tensorIndList (r1,r2,r3,r4,r5,r6,r7,r8) =  map (\(x1,x2,x3,x4,x5,x6,x7,x8) -> indexList x1 x2 x3 x4 x5 x6 x7 x8) list
            where 
                list = [ (y1,y2,y3,y4,y5,y6,y7,y8) | y1 <- (getRangeList r1 20), y2 <- (getRangeList r2 20), y3 <- (getRangeList r3 19), y4 <- (getRangeList r4 19),
                 y5 <- (getRangeList r5 9), y6 <- (getRangeList r6 9), y7 <- (getRangeList r7 3), y8 <- (getRangeList r8 3)]

    --this function works by producing a list of all possible indices for a given rank (as Ints) and then translating it to Inds
    --if this is to slow we need to directly construct the Inds 

    mkTensorfromF :: (KnownNat n1, KnownNat n2, KnownNat n3, KnownNat n4, KnownNat n5, KnownNat n6, KnownNat n7, KnownNat n8, Num a) =>
        Rank -> ((Index n1 n2 n3 n4 n5 n6 n7 n8) -> a) -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    mkTensorfromF rank f = Tensor $ M.fromList (zip indList valueList)
            where 
                indList = tensorIndList rank 
                valueList = map f indList

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


