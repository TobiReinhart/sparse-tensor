{-# LANGUAGE DataKinds #-}

module Intertwiners (inverseDerivativeInt)
where

import TensorTreeNumeric4
    (Tensor, Tensor8,
     toListShow8,
     tensorProd8, tensorContr3, tensorSMult,
     interIArea, interJArea, interJAreaInv, interIAreaInv,
     trianMapAreaI, trianMapAreaJ)

import Data.Ratio ((%))

iI :: Tensor8 1 0 0 0 0 0 0 4 Rational
iI = interIArea trianMapAreaI

iJ :: Tensor8 0 1 0 0 0 0 4 0 Rational
iJ = interJArea trianMapAreaJ

iIInv :: Tensor8 0 1 0 0 0 0 4 0 Rational
iIInv = interIAreaInv trianMapAreaJ

iJInv :: Tensor8 1 0 0 0 0 0 0 4 Rational
iJInv = interJAreaInv trianMapAreaI

inverseDerivativeInt :: Tensor8 2 2 0 0 0 0 0 0 Rational
inverseDerivativeInt = result
    where
        product = tensorProd8 iJInv $
                  tensorProd8 iIInv $
                  tensorProd8 iIInv iI
        contracted = tensorContr3 (0,0) $
                     tensorContr3 (1,1) $
                     tensorContr3 (2,2) $
                     tensorContr3 (3,3) $
                     tensorContr3 (2,4) $
                     tensorContr3 (3,5) $
                     tensorContr3 (6,6) $
                     tensorContr3 (7,7) product
        result = tensorSMult ((-1)%4) contracted
