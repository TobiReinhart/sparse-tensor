{-# LANGUAGE DataKinds #-}

module Main (
 main
) where

import PerturbationTree2_3
import TensorTreeNumeric4_2
import FlatTensorEquations
import GenericTensorEquations
import BasicTensors4_2

import System.IO

import qualified Data.ByteString.Lazy as BS
import Codec.Compression.GZip
import Data.Serialize

import Data.Maybe
import Data.List


import qualified Data.Eigen.Matrix as Mat 
import qualified Data.Eigen.SparseMatrix as Sparse
import qualified Data.Eigen.LA as Sol
import Data.Either

import qualified Data.IntMap as I 

import Data.Ratio


main = do 

    met <- randMetric

    axon <- randAxon 

    let metG = genericMetric  

    let area' = (contrATens2 (0,0) $ contrATens2 (1,1) $ interMetricArea &* met &* met) &+ axon

    let areaG = (contrATens2 (0,0) $ contrATens2 (1,1) $ interMetricArea &* metG &* metG) 
    
    let ans4 = generic4Ansatz 

    let ans8 = generic8Ansatz
    
    let eqn1 = contrATens1 (0,0) $ contrATens1 (1,1) $ ans4 &* interArea &* area' 

    let intCond1 = aSymATens6 (0,1) $ contrATens1 (0,0) $ contrATens3 (0,2) $ contrATens2 (0,0) $ ans4 &* interArea &* interI2 &* met 

    let intCond1Test = aSymATens6 (0,1) $ contrATens3 (0,2) $ contrATens2 (0,0) $ contrATens1 (0,1) $ interArea &* area' &* interI2 &* met

    let total1 = eqn1 &> (singletonTList intCond1)

    let l1 = map (map truncate) $ Mat.toList $ Sparse.toMatrix $ toEMatrix6 (singletonTList eqn1)
    
    print l1

    print " "

    let l2 = map (map truncate) $ Mat.toList $ Sparse.toMatrix $ toEMatrix6 (singletonTList intCond1)
    
    print l2


    