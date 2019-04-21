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


import qualified Data.Eigen.Matrix as Mat 
import qualified Data.Eigen.SparseMatrix as Sparse
import qualified Data.Eigen.LA as Sol
import Data.Either

import qualified Data.IntMap as I 

import Data.Ratio


main = do 

    met <- randMetric 

    let intProd = contrATens2 (0,0) $ interMetricArea &* met

    let l = toListShow6 intProd 

    let l' = map (\([a,b],v) -> (a,b,fromRational v)) l :: [(Int,Int,Double)]

    let m = Sparse.toMatrix $ Sparse.fromList 21 10 l'

    print $ Sol.rank Sol.FullPivLU m 