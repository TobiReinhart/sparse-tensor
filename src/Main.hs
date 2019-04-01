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

import qualified Data.IntMap.Strict as I
 

import Data.Ratio


main = do 

       let (etaAns14_1, epsAns14_1, tens) = mkAnsatzTensor 14 filterList14_1 symPairs14_1 areaBlocks14_1 symList14_1 areaList14_1IndsEta areaList14_1IndsEps

       print $ getForestLabels etaAns14_1

       print $ getForestLabelsEpsilon epsAns14_1 



       

      



    