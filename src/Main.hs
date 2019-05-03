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
    
    let (_,_,ans4) = mkAnsatzTensorEig 4 filterList4 symList4 areaList4IndsEta areaList4IndsEps 

    let (_,_,ans8) = mkAnsatzTensorEig 8 filterList8 symList8 areaList8IndsEta areaList8IndsEps 

    let (_,_,ans16) = mkAnsatzTensorFast 16 filterList16 symList16 areaList16IndsEta areaList16IndsEps 


    let eq = contrATens1 (0,0) $ ans4 &* flatInter

    let interASym = aSymATens6 (0,1) $ contrATens3 (0,1)  $ interArea &* eta 

    let test = aSymATens1 (0,1) $ contrATens1 (0,0) $ ans8 &* interASym

    let eqn = contrATens1 (0,0) $ ans8 &* flatInter

    let ans16Test = ansatzABCD ans16

    print $ toListShowVar6 ans16Test 