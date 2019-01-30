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
    import Numeric.Natural 
    import GHC.TypeNats
    import Data.Proxy
    import Control.Monad

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
                    times x y = x*y
                    h = liftM2 times (f.fst.(splitIndex rank)) (g.snd.(splitIndex rank)) 


    tensorContract_20 :: (Num a, KnownNat n1, KnownNat n2) => (Int,Int) -> Tensor (n1+1) (n2+1) n3 n4 n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorContract_20 inds (Tensor f) = Tensor g
            where
                l = contractionIndexList_20 inds 
                g = \inds -> foldl (\x y -> x + (f y)) 0 $ l inds

    tensorContract_19 :: (Num a, KnownNat n3, KnownNat n4) => (Int,Int) -> Tensor n1 n2 (n3+1) (n4+1) n5 n6 n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorContract_19 inds (Tensor f) = Tensor g
            where
                l = contractionIndexList_19 inds 
                g = \inds -> foldl (\x y -> x + (f y)) 0 $ l inds

    tensorContract_9 :: (Num a, KnownNat n5, KnownNat n6) => (Int,Int) -> Tensor n1 n2 n3 n4 (n5+1) (n6+1) n7 n8 a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorContract_9 inds (Tensor f) = Tensor g
            where
                l = contractionIndexList_9 inds 
                g = \inds -> foldl (\x y -> x + (f y)) 0 $ l inds

    tensorContract_3 :: (Num a, KnownNat n7, KnownNat n8) => (Int,Int) -> Tensor n1 n2 n3 n4 n5 n6 (n7+1) (n8+1) a -> Tensor n1 n2 n3 n4 n5 n6 n7 n8 a
    tensorContract_3 inds (Tensor f) = Tensor g
            where
                l = contractionIndexList_3 inds 
                g = \inds -> foldl (\x y -> x + (f y)) 0 $ l inds
