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

       let etaL = map (\(x,y,z) -> x) areaList12IndsEta

       let epsL = map (\(x,y,z) -> x) areaList12IndsEps

       let (_, _, etaMat, epsMat) = getFullForest [1..12] filterList12 symPairs12 areaBlocks12 symList12 etaL epsL

       let (_, _, etaMat2, epsMat2) = getFullForestSym [1..12] filterList12 symPairs12 areaBlocks12 symList12 etaL epsL

       print $ Sparse.transpose epsMat

       print $ Sparse.transpose epsMat2

       print $ Sparse.transpose etaMat

       print $ Sparse.transpose etaMat2

       
       


      

       

      



    