{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Math.Tensor.Examples.Gravity.Schwarzschild

(schwarzschild, schwarzschild', christoffel, ricci, einstein)

where

import Math.Tensor
import Math.Tensor.Examples.Gravity

import Data.Ratio ((%))

import Control.Applicative

import Numeric.AD.Rank1.Forward

import GHC.TypeLits

newtype CFun a b = CFun (a -> b)

instance Functor (CFun a) where
    fmap f (CFun g) = CFun $ f . g

instance Applicative (CFun a) where
    pure = CFun . const
    (CFun f) <*> (CFun g) = CFun $ \x -> f x (g x)

instance Num b => Num (CFun a b) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = CFun . const . fromInteger

instance Num b => TAdd (CFun a b) where
    addS = (+)
    negateS = negate
    scaleZero = const False

instance Num b => Prod (CFun a b) (CFun a b) where
    type TProd (CFun a b) (CFun a b) = CFun a b
    prod = (*)

instance Num b => Prod (SField b) (CFun a b) where
    type TProd (SField b) (CFun a b) = CFun a b
    prod (SField s) (CFun f) = CFun $ (*s) . f

schwarzschild :: Floating a => a -> STTens 0 2 (CFun [a] a)
schwarzschild rs = fromListT2
    [
      ((Empty, Ind3 0 `Append` singletonInd (Ind3 0)), CFun $ \(_:r:_) -> r' r ),
      ((Empty, Ind3 1 `Append` singletonInd (Ind3 1)), CFun $ \(_:r:_) -> -1/(r' r)),
      ((Empty, Ind3 2 `Append` singletonInd (Ind3 2)), CFun $ \(_:r:_) -> -(r^2)),
      ((Empty, Ind3 3 `Append` singletonInd (Ind3 3)), CFun $ \(_:r:theta:_) -> -(r*sin theta)^2)
    ]
  where
    r' r = 1 - rs / r

schwarzschild' :: Floating a => a -> STTens 2 0 (CFun [a] a)
schwarzschild' rs = fromListT2
    [
      ((Ind3 0 `Append` singletonInd (Ind3 0), Empty), CFun $ \(_:r:_) -> 1/(r' r)),
      ((Ind3 1 `Append` singletonInd (Ind3 1), Empty), CFun $ \(_:r:_) -> - r' r),
      ((Ind3 2 `Append` singletonInd (Ind3 2), Empty), CFun $ \(_:r:_) -> -1/(r^2)),
      ((Ind3 3 `Append` singletonInd (Ind3 3), Empty), CFun $ \(_:r:theta:_) -> -1/(r*sin theta)^2)
    ]
  where
    r' r = 1 - rs / r

half :: Fractional a => SField a
half = SField $ 1/2

christoffel :: forall a.Floating a => STTens 1 2 (CFun [a] a)
christoffel = gamma
    where
        g = schwarzschild 1
        g' = schwarzschild' 1 :: STTens 2 0 (CFun [a] a)
        del_g = partial g :: STTens 0 3 (CFun [a] a)
        g'_del_g = g' &* del_g
        t1 = contrATens1 (0, 0) g'_del_g
        t2 = contrATens1 (0, 1) g'_del_g
        t3 = tensorTrans2 (0, 1) t2
        s = t2 &+ (t3 &- t1)
        h = half :: SField a
        gamma = h &. s

ricci :: forall a.Floating a => STTens 0 2 (CFun [a] a)
ricci = (term1 &- term2) &+ (term3 &- term4)
    where
        gamma1 = christoffel
        gamma2 = christoffel
        del_gamma = partial gamma1 :: STTens 1 3 (CFun [a] a)
        gamma_gamma = contrATens1 (1,1) $ gamma2 &* gamma2 :: STTens 1 3 (CFun [a] a)
        term1 = contrATens1 (0,0) del_gamma
        term2 = contrATens1 (0,1) del_gamma
        term3 = contrATens1 (0,0) gamma_gamma
        term4 = contrATens1 (0,1) gamma_gamma

einstein :: forall a.Floating a => STTens 0 2 (CFun [a] a)
einstein = r_ab &- (h &. r &* g)
    where
        r_ab = ricci :: STTens 0 2 (CFun [a] a)
        g = schwarzschild 1 :: STTens 0 2 (CFun [a] a)
        g' = schwarzschild' 1 :: STTens 2 0 (CFun [a] a)
        r = contrATens1 (0,0) $ contrATens1 (1,1) $ g' &* r_ab
        h = half :: SField a

myGrad :: Num a => [Int] -> ([Forward a] -> Forward a) -> [(Int, [a] -> a)]
myGrad is f = map (\i -> (i, (!!i) . g)) is
    where
        g = grad f

-- | partial derivative of a tensor with spacetime indices

partial :: Num a => STTens n1 n2 (CFun [Forward a] (Forward a)) -> STTens n1 (n2+1) (CFun [a] a)
partial tens = tens'
    where
        tList = toListT2 tens
        grads = map (\(is, (CFun v)) -> (is, myGrad [0..3] v)) tList
        tList' = concatMap (\((i1, i2), gs) -> map (\(ig, g) -> ((i1, (Ind3 ig `Append` i2)), CFun g)) gs) grads
        tens' = fromListT2 tList'
