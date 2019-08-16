-----------------------------------------------------------------------------
-- |
-- Module      :  Math.Tensor.Examples.Gravity.Schwarzschild
-- Copyright   :  (c) 2019 Tobias Reinhart and Nils Alex
-- License     :  MIT
-- Maintainer  :  tobi.reinhart@fau.de, nils.alex@fau.de
--
--
-- This module provides the metric, inverse metric, Christoffel symbol, Ricci tensor and Einstein tensor for the Schwarzschild spacetime as an
-- example for tensor sections and partial derivatives thereof.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.Tensor.Examples.Gravity.Schwarzschild (
schwarzschild,
schwarzschild',
christoffel,
ricci,
einstein
)
where

import Math.Tensor

import Numeric.AD.Internal.Forward (Forward(..))

-- | Schwarzschild metric \( g = (1-\frac{r_\text{s}}{r})\,\mathrm dt\otimes\mathrm dt - \frac{1}{1-\frac{r_\text{s}}{r}}\,\mathrm dr\otimes \mathrm dr - r^2\,\mathrm d\theta\otimes \mathrm d\theta - r^2\sin^2\theta\,\mathrm d\phi\otimes \mathrm d\phi \).

schwarzschild :: Floating a => a -> STTens 0 2 (CFun [a] a)
schwarzschild rs = fromListT2
    [
      ((Empty, Ind3 0 `Append` singletonInd (Ind3 0)), CFun $ \(_:r:_) -> r' r ),
      ((Empty, Ind3 1 `Append` singletonInd (Ind3 1)), CFun $ \(_:r:_) -> -1/r' r),
      ((Empty, Ind3 2 `Append` singletonInd (Ind3 2)), CFun $ \(_:r:_) -> -(r^2)),
      ((Empty, Ind3 3 `Append` singletonInd (Ind3 3)), CFun $ \(_:r:theta:_) -> -(r*sin theta)^2)
    ]
  where
    r' r = 1 - rs / r


-- | Inverse Schwarzschild metric \( g = \frac{1}{1-\frac{r_\text{s}}{r}}\,\partial_t \otimes \partial_t - (1-\frac{r_\text{s}}{r})\,\partial_r \otimes \partial_r - \frac{1}{r^2}\,\partial_\theta \otimes \partial_\theta - \frac{1}{r^2\sin^2\theta}\,\partial_\phi \otimes \partial_\phi \).

schwarzschild' :: Floating a => a -> STTens 2 0 (CFun [a] a)
schwarzschild' rs = fromListT2
    [
      ((Ind3 0 `Append` singletonInd (Ind3 0), Empty), CFun $ \(_:r:_) -> 1/r' r),
      ((Ind3 1 `Append` singletonInd (Ind3 1), Empty), CFun $ \(_:r:_) -> - r' r),
      ((Ind3 2 `Append` singletonInd (Ind3 2), Empty), CFun $ \(_:r:_) -> -1/(r^2)),
      ((Ind3 3 `Append` singletonInd (Ind3 3), Empty), CFun $ \(_:r:theta:_) -> -1/(r*sin theta)^2)
    ]
  where
    r' r = 1 - rs / r

half :: Fractional a => SField a
half = SField $ 1/2

-- | Christoffel symbol of the Schwarzschild metric.

christoffel :: forall a.Floating a => a -> STTens 1 2 (CFun [a] a)
christoffel rs = gamma
    where
        g = schwarzschild (Lift rs)
        g' = schwarzschild' rs :: STTens 2 0 (CFun [a] a)
        del_g = partial g :: STTens 0 3 (CFun [a] a)
        g'_del_g = g' &* del_g
        t1 = contrATens1 (0, 0) g'_del_g
        t2 = contrATens1 (0, 1) g'_del_g
        t3 = tensorTrans2 (0, 1) t2
        s = t2 &+ (t3 &- t1)
        h = half :: SField a
        gamma = h &. s

-- | Ricci tensor of the Schwarzschild metric.

ricci :: forall a.Floating a => a -> STTens 0 2 (CFun [a] a)
ricci rs = (term1 &- term2) &+ (term3 &- term4)
    where
        gamma1 = christoffel (Lift rs)
        gamma2 = christoffel rs
        del_gamma = partial gamma1 :: STTens 1 3 (CFun [a] a)
        gamma_gamma = contrATens1 (1,1) $ gamma2 &* gamma2 :: STTens 1 3 (CFun [a] a)
        term1 = contrATens1 (0,0) del_gamma
        term2 = contrATens1 (0,1) del_gamma
        term3 = contrATens1 (0,0) gamma_gamma
        term4 = contrATens1 (0,1) gamma_gamma

-- | Einstein tensor of the Schwarzschild metric.
--   The component functions evaluate to zero:
--
-- >>> let g = einstein 2
-- >>> g `evalSec` [1.1, 2.4, 1.7, 2.2]
-- ZeroTensor

einstein :: forall a.Floating a => a -> STTens 0 2 (CFun [a] a)
einstein rs = r_ab &- (h &. r &* g)
    where
        r_ab = ricci rs :: STTens 0 2 (CFun [a] a)
        g = schwarzschild rs :: STTens 0 2 (CFun [a] a)
        g' = schwarzschild' rs :: STTens 2 0 (CFun [a] a)
        r = contrATens1 (0,0) $ contrATens1 (1,1) $ g' &* r_ab
        h = half :: SField a
