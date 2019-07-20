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

    let (eta2,eps2,ans2) = mkAnsatzTensorEig 2 metricfilterList2 metricsymList2 metricList2IndsEta metricList2IndsEps  
    
    let (eta4_1,eps4_1,ans4_1) = mkAnsatzTensorEig 4 metricfilterList4_1 metricsymList4_1 metricList4_1IndsEta metricList4_1IndsEps 
    
    let (eta4_1Fast,eps4_1Fast,ans4_1Fast) = mkAnsatzTensorFast 4 metricfilterList4_1 metricsymList4_1 metricList4_1IndsEta metricList4_1IndsEps  

    let (eta4_2,eps4_2,ans4_2) = mkAnsatzTensorEig 4 metricfilterList4_2 metricsymList4_2 metricList4_2IndsEta metricList4_2IndsEps  

    let (eta6_1,eps6_1,ans6_1) = mkAnsatzTensorEig 6 metricfilterList6_1 metricsymList6_1 metricList6_1IndsEta metricList6_1IndsEps  

    let (eta6_2,eps6_2,ans6_2) = mkAnsatzTensorEig 6 metricfilterList6_2 metricsymList6_2 metricList6_2IndsEta metricList6_2IndsEps  

    let (eta6_3,eps6_3,ans6_3) = mkAnsatzTensorEig 6 metricfilterList6_3 metricsymList6_3 metricList6_3IndsEta metricList6_3IndsEps  

    let (eta8_1,eps8_1,ans8_1) = mkAnsatzTensorEig 8 metricfilterList8_1 metricsymList8_1 metricList8_1IndsEta metricList8_1IndsEps  

    let (eta8_2,eps8_2,ans8_2) = mkAnsatzTensorEig 8 metricfilterList8_2 metricsymList8_2 metricList8_2IndsEta metricList8_2IndsEps  

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

    let mList = eqn1AB &> eqn1A &> (singletonTList eqn1)

    let kList = eqn1AaBb &> eqn2ABb &> eqn3AB &> eqn1ABI &> eqn3A &> eqn2Aa &> eqn1AI &> (singletonTList eqn3) 

    let kList' = eqn3A &> eqn2Aa &> eqn1AI &> (singletonTList eqn3) 

    let l = toMatList6 tList 

    let showfrac x = if denominator x == 1 then show (numerator x) else show (numerator x) ++ "/" ++ show (denominator x)

    let l' =  map (\(x,y) -> show x ++ "=" ++ showfrac y ++ "," ) l 

    let lMass = toMatList6 mList

    let lMass' =  map (\(x,y) -> show x ++ "=" ++ showfrac y ++ "," ) lMass
    
    let lKin = toMatList6 kList'

    let lKin' =  map (\(x,y) -> show x ++ "=" ++ showfrac y ++ "," ) lKin

    --print $ tensorRank' eqn3

    --print $ toListShowVar6 eqn3 

    let l3 = toMatList6 $ singletonTList eqn3 

    let l3' =  map (\(x,y) -> show x ++ "=" ++ showfrac y ++ "," ) l3 

    --putStr $ unlines l3'

    let l4 =  toMatList6' (eqn3Met ans4_1)

    let l4' = map (\x -> tail $ concat $ map (\(a,b) -> "+" ++ show (numerator b) ++ "*x[" ++ show a ++ "]") x) l4

    --putStr $ unlines l4'

    print $ flattenForest eta4_1

    let var1 = fromListT6' [(([],[],[],[],[],[]),AnsVar $I.fromList [(1,4)] )] :: ATens 0 0 0 0 0 0 (AnsVar Rational)

    let var2 = fromListT6' [(([],[],[],[],[],[]),AnsVar $I.fromList [(2,2)] )] :: ATens 0 0 0 0 0 0 (AnsVar Rational)


    let etaProd = invEta &* invEta

    let etaTest = etaProd &* var1 &+ (tensorTrans5 (1,2) etaProd) &* var2 &+ (tensorTrans5 (1,3) $ tensorTrans5 (1,2) etaProd) &* var2

    let etaAbsTest = contrATens3  (0,0) $ contrATens3 (1,1) $ contrATens3 (2,2) $ contrATens3 (3,3) $ etaTest &* interI2 &* interI2

    print $ tensorRank' (eqn3Met ans4_1)

    --print $ toListShowVar6 (etaAbsTest &- ans4_1)

    --check this 