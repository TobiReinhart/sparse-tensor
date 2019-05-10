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

    --mass term ansätze
    
    let (_,_,ans8') = mkAnsatzTensorEig 8 filterList8 symList8 areaList8IndsEta areaList8IndsEps 

    let (_,_,ans12') = mkAnsatzTensorFast 12 filterList12 symList12 areaList12IndsEta areaList12IndsEps 

    --kinetic term ansätze 

    let (_,_,ans10') = mkAnsatzTensorFast 10 filterList10Rom symList10Rom areaList10IndsEtaRom areaList10IndsEpsRom 

    let (_,_,ans14') = mkAnsatzTensorFast 14 filterList14Rom symList14Rom areaList14IndsEtaRom areaList14IndsEpsRom
    
    let r8 = tensorRank' ans8' 

    let r10 = tensorRank' ans10'

    let r12 = tensorRank' ans12'

    let r14 = tensorRank' ans14'
    
    let ans14 = ans14' 

    let ans12 = ans12' 

    let ans10 = shiftLabels6 r14 ans10'

    let ans8 = shiftLabels6 r12 ans8' 

    let linMassEqn = linMass ans8

    let linKinEqn = linKin ans10 

    let quadMassEqn = quadMass ans12 ans8 
    
    let quadKinEqn1 = quadKin1 ans14 ans10 
    
    let quadKinEqn2 = quadKin2 ans14 ans10

    let quadKinEqn3 = quadKin3 ans14 ans10 

    let totalLinMass = singletonTList linMassEqn 

    let totalQuadMass = linMassEqn &> (singletonTList quadMassEqn)

    let totalLinKin = singletonTList linKinEqn 

    let totalQuadKin = linKinEqn &> quadKinEqn1 &> quadKinEqn2 &> (singletonTList quadKinEqn3)
    
    let matLinKin = toMatList6 totalLinKin
    
    let matQuadKin = toMatList6 totalQuadKin

    let linSym = linSymbol ans10

    let quadSym = quadSymbol ans14 

    print 1