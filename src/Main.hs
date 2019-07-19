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

    --metric term ansätze

    let ans0 = fromListT6' [(([],[],[],[],[],[]),AnsVar $I.fromList [(1,1)] )] :: ATens 0 0 0 0 0 0 (AnsVar Rational)

    let (_,_,ans2) = mkAnsatzTensorEig 2 metricfilterList2 metricsymList2 metricList2IndsEta metricList2IndsEps  
    
    let (_,_,ans4_1) = mkAnsatzTensorEig 4 metricfilterList4_1 metricsymList4_1 metricList4_1IndsEta metricList4_1IndsEps  

    let (_,_,ans4_2) = mkAnsatzTensorEig 4 metricfilterList4_2 metricsymList4_2 metricList4_2IndsEta metricList4_2IndsEps  

    let (_,_,ans6_1) = mkAnsatzTensorEig 6 metricfilterList6_1 metricsymList6_1 metricList6_1IndsEta metricList6_1IndsEps  

    let (_,_,ans6_2) = mkAnsatzTensorEig 6 metricfilterList6_2 metricsymList6_2 metricList6_2IndsEta metricList6_2IndsEps  

    let (_,_,ans6_3) = mkAnsatzTensorEig 6 metricfilterList6_3 metricsymList6_3 metricList6_3IndsEta metricList6_3IndsEps  

    let (_,_,ans8_1) = mkAnsatzTensorEig 8 metricfilterList8_1 metricsymList8_1 metricList8_1IndsEta metricList8_1IndsEps  

    let (_,_,ans8_2) = mkAnsatzTensorEig 8 metricfilterList8_2 metricsymList8_2 metricList8_2IndsEta metricList8_2IndsEps  

    let r0 = tensorRank' ans0

    let r2 = tensorRank' ans2 

    let r4_1 = tensorRank' ans4_1 

    let r4_2 = tensorRank' ans4_2
    
    let r6_1 = tensorRank' ans6_1
    
    let r6_2 = tensorRank' ans6_2
    
    let r6_3 = tensorRank' ans6_3
    
    let r8_1 = tensorRank' ans8_1
    
    let r8_2 = tensorRank' ans8_2

    let ans8_2' = ans8_2

    let ans8_1' = shiftLabels6 r8_2 ans8_1 
    
    let ans6_3' = shiftLabels6 (r8_2 + r8_1) ans6_3

    let ans6_2' = shiftLabels6 (r8_2 + r8_1+ r6_3) ans6_2

    let ans6_1' = shiftLabels6 (r8_2 + r8_1+ r6_3+ r6_2) ans6_1

    let ans4_2' = shiftLabels6 (r8_2 + r8_1+ r6_3+ r6_2+ r6_1) ans4_2

    let ans4_1' = shiftLabels6 (r8_2 + r8_1+ r6_3+ r6_2+ r6_1+ r4_2) ans4_1

    let ans2' = shiftLabels6 (r8_2 + r8_1+ r6_3+ r6_2+ r6_1+ r4_2 +r4_1) ans2

    let ans0' = shiftLabels6 (r8_2 + r8_1+ r6_3+ r6_2+ r6_1+ r4_2 +r4_1 +r2) ans0

    let eqn1 = eqn1Met ans0' ans2' 

    let eqn3 = eqn3Met ans4_1' 

    let eqn1A = eqn1AMet ans2' ans4_2' 

    let eqn1AB = eqn1ABMet ans4_2' ans6_3'

    let eqn1AI = eqn1AIMet ans4_1' ans6_2' 

    let eqn2Aa = eqn2AaMet ans4_1' ans6_1' 

    let eqn3A = eqn3AMet ans4_1' ans6_2'

    let eqn1ABI = eqn1ABIMet ans6_2' ans8_2'

    let eqn3AB = eqn3ABMet ans6_2' ans8_2' 

    let eqn2ABb = eqn2ABbMet ans6_1' ans6_2' ans8_1' 

    let eqn1AaBb = eqn1AaBbMet ans6_1' ans8_1' 
    
    let tList = eqn1AaBb &> eqn2ABb &> eqn3AB &> eqn1ABI &> eqn3A &> eqn2Aa &> eqn1AI &> eqn1AB &> eqn1A &> eqn3 &> (singletonTList eqn1) 

    print $ tensorRank tList 

    print $ r0 + r2 + r4_1 + r4_2 + r6_1 + r6_2 + r6_3 + r8_1 + r8_2 