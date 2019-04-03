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

       --let (etaAns20, epsAns20, tens) = mkAnsatzTensor 20 filterList20 symPairs20 areaBlocks20 symList20 areaList20IndsEta areaList20IndsEps

       --print $ getForestLabels etaAns20

       --print $ getForestLabelsEpsilon epsAns20

       let (a,b,_) = mkAnsatzTensorAlg 8 filterList8 symPairs8 areaBlocks8 symList8 areaList8IndsEta areaList8IndsEps

       print $ length $ getForestLabels a
       print $ length $ getForestLabelsEpsilon b
       --print $ length $ getForestLabels a'
       --print $ length $ getForestLabelsEpsilon b'

       
       
       


      

       

      



    