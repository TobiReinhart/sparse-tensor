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

    let t1 = symATens3 (1,2) $ aSymATens6 (0,1) $ contrATens3 (0,2) $ contrATens1 (0,1) $ interArea &* interMetricArea &* interI2 

    let t2 = aSymATens6 (0,1) $ contrATens3 (0,2) $ contrATens1 (0,1) $ interArea &* interI2 &* interMetricArea

    print $ removeZeros6 $ t1 &+ t2 