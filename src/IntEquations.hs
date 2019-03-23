{-# LANGUAGE DataKinds #-}

module IntEquations (intEquation, ansatz8Solved, cyclic, invAreaDerivativeFlat) where

import Intertwiners (inverseDerivativeInt, intCondInt)

import TensorTreeNumeric4 (Tensor, Tensor8, VarMap, multVarsMap,
                           addVarsMap,
                           tensorProd8, tensorContr20,
                           tensorSMult, tensorProdWith8,
                           tensorContrWith20, tensorTransWithU20,
                           tensorAddWith8, flatAreaInvNoEps,
                           flatAreaNoEps, tensorAdd8, tensorTransU3,
                           interIAreaInv, trianMapAreaJ, trianMapAreaI,
                           interJAreaInv, tensorContr3)
import PerturbationTree2_2 (generic8Ansatz,
                            areaEvalMap8Inds, filterList8, symList8,
                            epsMap, mkAnsatzTensor, trianMapArea, trianMapDerivative)

import qualified Data.Map.Strict as M (empty)

import Data.Ratio ((%))

mult :: VarMap -> Rational -> VarMap
mult = flip multVarsMap

invAreaDerivativeFlat :: Tensor8 2 0 0 0 0 0 0 0 Rational
invAreaDerivativeFlat = contracted
    where
        product    = tensorProd8 inverseDerivativeInt $
                     tensorProd8 flatAreaInvNoEps flatAreaInvNoEps
        contracted = tensorContr20 (2,0) $ tensorContr20 (3,1) product

iIInv :: Tensor8 0 1 0 0 0 0 4 0 Rational
iIInv = interIAreaInv trianMapAreaJ

iJInv :: Tensor8 1 0 0 0 0 0 0 4 Rational
iJInv = interJAreaInv trianMapAreaI

cyclic :: Tensor8 2 0 0 0 0 0 0 0 Rational ->
          Tensor8 2 0 0 0 0 0 0 0 Rational
cyclic tens = result
    where
        product  = tensorProd8 iIInv tens
        block1   = tensorContr20 (0,0) product
        block2   = tensorSMult (-1) $ tensorTransU3 (2,3) block1
        block3   = tensorSMult (-1) $ tensorTransU3 (1,3) block1
        block4   = tensorSMult (-1) $ tensorTransU3 (1,2) block1
        block5   = tensorTransU3 (1,2) $ tensorTransU3 (2,3) block1
        block6   = tensorTransU3 (1,2) $ tensorTransU3 (1,3) block1
        anti'    = tensorSMult ((-1)%6) $
                   tensorAdd8 block1 $
                   tensorAdd8 block2 $
                   tensorAdd8 block3 $
                   tensorAdd8 block4 $
                   tensorAdd8 block5 block6
        anti     = tensorContr3 (0,0) $
                   tensorContr3 (1,1) $
                   tensorContr3 (2,2) $
                   tensorContr3 (3,3) $
                   tensorProd8 iJInv anti'
        result   = tensorAdd8 tens anti

ansatz8Solved :: Tensor8 2 0 0 0 0 0 0 0 VarMap
ansatz8Solved = mkAnsatzTensor 8 filterList8 symList8 1 epsMap eval
    where
        eval       = areaEvalMap8Inds trianMapArea trianMapDerivative

intEquation :: Tensor8 2 0 0 0 0 0 4 0 VarMap
intEquation = result
    where
        ansatz   = generic8Ansatz
        flatInt1 = tensorContr20 (1,1) $
                   tensorProd8 intCondInt flatAreaInvNoEps
        flatInt2 = tensorSMult (-1) $
                   tensorContr20 (0,1) $
                   tensorContr20 (0,2) $
                   tensorProd8 intCondInt $
                   tensorProd8 flatAreaNoEps $ cyclic invAreaDerivativeFlat
        block1'  = tensorProdWith8 mult ansatz flatInt1
        block1   = tensorContrWith20 (0,0) addVarsMap block1'
        block2'  = tensorProdWith8 mult ansatz flatInt2
        block2'' = tensorContrWith20 (0,0) addVarsMap block2'
        block2   = tensorTransWithU20 (0,1) addVarsMap block2''
        result   = tensorAddWith8 addVarsMap block1 block2
