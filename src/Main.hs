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

       let eta14_2Inds = map (\(a,b,c) -> a) areaList14_2IndsEta

       let eps14_2Inds = map (\(a,b,c) -> a) areaList14_2IndsEps

       let (_,_,a,b) = getFullForest [1..14] filterList14_2 symPairs14_2 areaBlocks14_2 symList14_2 eta14_2Inds eps14_2Inds

       let l = Sparse.toDenseList a

       let l' =  map (\x -> (fromIntegral $ length $ filter (/= 0) x) / (fromIntegral $ length x)) l

       let l2 = Sparse.toDenseList b

       let l2' =  map (\x -> (fromIntegral $ length $ filter (/= 0) x) / (fromIntegral $ length x)) l2

       print $ (sum l2')/ (fromIntegral $ length l2')

       
       


      

       

      



    