{-# LANGUAGE DataKinds #-}

module Intertwiners (inverseDerivativeInt, intCondInt)
where

import TensorTreeNumeric4
    (Tensor, Tensor8,
     delta3,
     toListShow8,
     tensorProd8, tensorContr3, tensorSMult, tensorTransU3,
     tensorSub8, tensorAdd8,
     interArea,
     interIArea, interJArea, interJAreaInv, interIAreaInv,
     trianMapAreaI, trianMapAreaJ)

import Data.Ratio ((%))

int :: Tensor8 1 1 0 0 0 0 1 1 Rational
int = interArea trianMapAreaI trianMapAreaJ

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

intCondInt' :: Tensor8 0 1 0 0 0 0 5 1 Rational
intCondInt' = result
    where
        block1'  = tensorTransU3 (3,4) $ tensorProd8 iIInv delta3
        block1'' = tensorTransU3 (2,3) block1'
        block1   = tensorSub8 block1' block1''
        block2  = tensorTransU3 (0,2) $ tensorTransU3 (1,3) block1
        result  = tensorSub8 block1 block2

intCondInt :: Tensor8 1 2 0 0 0 0 4 0 Rational
intCondInt = result
    where
        product = tensorProd8 int intCondInt' 
        contracted = tensorContr3 (4,0) $ tensorContr3 (0,1) product
        result = contracted
