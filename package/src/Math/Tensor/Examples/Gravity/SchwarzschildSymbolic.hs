-----------------------------------------------------------------------------
-- |
-- Module      :  Math.Tensor.Examples.Gravity.SchwarzschildSymbolic
-- Copyright   :  (c) 2019 Tobias Reinhart and Nils Alex
-- License     :  MIT
-- Maintainer  :  tobi.reinhart@fau.de, nils.alex@fau.de
--
--
-- This module provides the Schwarzschild metric as an example for a tensor with symbolic values
-- as well as functions to calculate Christoffel symbols, Ricci tensors and Einstein tensors
-- from metric tensors with symbolic values.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}

module Math.Tensor.Examples.Gravity.SchwarzschildSymbolic (
schwarzschildS,
schwarzschildS',
christoffelS,
ricciS,
einsteinS
)
where

import Math.Tensor

-- | Schwarzschild metric \( g = (1-\frac{r_\text{s}}{r})\,\mathrm dt\otimes\mathrm dt - \frac{1}{1-\frac{r_\text{s}}{r}}\,\mathrm dr\otimes \mathrm dr - r^2\,\mathrm d\theta\otimes \mathrm d\theta - r^2\sin^2\theta\,\mathrm d\phi\otimes \mathrm d\phi \).

schwarzschildS :: STTens 0 2 SSymbolic
schwarzschildS = fromListT2
    [
      ((Empty, Ind3 0 `Append` singletonInd (Ind3 0)), SSymbolic "1 - rs / r" ),
      ((Empty, Ind3 1 `Append` singletonInd (Ind3 1)), SSymbolic "-1 / (1 - rs / r)"),
      ((Empty, Ind3 2 `Append` singletonInd (Ind3 2)), SSymbolic "-r^2"),
      ((Empty, Ind3 3 `Append` singletonInd (Ind3 3)), SSymbolic "-r^2*sin(theta)^2")
    ]

-- | Inverse Schwarzschild metric \( g = \frac{1}{1-\frac{r_\text{s}}{r}}\,\partial_t \otimes \partial_t - (1-\frac{r_\text{s}}{r})\,\partial_r \otimes \partial_r - \frac{1}{r^2}\,\partial_\theta \otimes \partial_\theta - \frac{1}{r^2\sin^2\theta}\,\partial_\phi \otimes \partial_\phi \).

schwarzschildS' :: STTens 2 0 SSymbolic
schwarzschildS' = fromListT2
    [
      ((Ind3 0 `Append` singletonInd (Ind3 0), Empty), SSymbolic "1/(1 - rs/r)"),
      ((Ind3 1 `Append` singletonInd (Ind3 1), Empty), SSymbolic "-(1-rs/r)"),
      ((Ind3 2 `Append` singletonInd (Ind3 2), Empty), SSymbolic "-1/r^2"),
      ((Ind3 3 `Append` singletonInd (Ind3 3), Empty), SSymbolic "-1/(r^2*sin(theta)^2)")
    ]

half :: SField Rational
half = SField (1/2)

-- | Christoffel symbols of any symbolic metric.

christoffelS :: STTens 0 2 SSymbolic -> STTens 2 0 SSymbolic -> STTens 1 2 SSymbolic
christoffelS g g' = gamma
    where
        del_g = partialSymbolic ["t", "r", "theta", "phi"] g
        g'_del_g = g' &* del_g
        t1 = contrATens1 (0, 0) g'_del_g
        t2 = contrATens1 (0, 1) g'_del_g
        t3 = tensorTrans2 (0, 1) t2
        s = t2 &+ (t3 &- t1)
        gamma = half &. s

-- | Ricci tensor of any symbolic metric.

ricciS :: STTens 0 2 SSymbolic -> STTens 2 0 SSymbolic -> STTens 0 2 SSymbolic
ricciS g g' = (term1 &- term2) &+ (term3 &- term4)
    where
        gamma = christoffelS g g'
        del_gamma = partialSymbolic ["t", "r", "theta", "phi"] gamma
        gamma_gamma = contrATens1 (1,1) $ gamma &* gamma
        term1 = contrATens1 (0,0) del_gamma
        term2 = contrATens1 (0,1) del_gamma
        term3 = contrATens1 (0,0) gamma_gamma
        term4 = contrATens1 (0,1) gamma_gamma

-- | Einstein tensor of any symbolic metric.
--   The components evaluate to zero:
--
-- >>> let g  = schwarzschildS
-- >>> let g' = schwarzschildS'
-- >>> let e  = einsteinS g g'
-- >>> print e
-- ZeroTensor -- modulo symbolic simplification, which is not implemented yet.

einsteinS :: STTens 0 2 SSymbolic -> STTens 2 0 SSymbolic -> STTens 0 2 SSymbolic
einsteinS g g' = r_ab &- (half &. r &* g)
    where
        r_ab = ricciS g g'
        r = contrATens1 (0,0) $ contrATens1 (1,1) $ g' &* r_ab
