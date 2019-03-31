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

        let evalMaps = map (\(x,y,z) -> x) areaList14_1Inds

        let (_,testAnsatz) = getEtaForest [1..14] filterList14_1 symList14_1 evalMaps  

        print $ Sparse.toList testAnsatz 

    