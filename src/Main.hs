{-# LANGUAGE DataKinds #-}

module Main (
 main
) where

import TensorTreeNumeric4_2
import PerturbationTree2_3
import qualified Data.Eigen.Matrix as Mat 
import Data.List
import qualified Data.Eigen.LA as Sol 
import qualified Data.Eigen.SparseMatrix as Sparse

import qualified Numeric.LinearAlgebra.Data as HMat
import qualified Numeric.LinearAlgebra as HLin 
 

import Data.Ratio


main = do 

       let evalM14_1 = areaList14_1Inds 

       let (etaAns14_1, epsAns14_1,_) = mkAnsatzTensor 14 filterList14_1 symList14_1 evalM14_1 

       let etaVars = getForestLabels etaAns14_1 

       let epsVars = getForestLabelsEpsilon epsAns14_1 

       print etaVars

       print epsVars

       print $ (length etaVars, length epsVars)

    