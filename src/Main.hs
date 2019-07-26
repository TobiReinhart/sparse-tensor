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

    let (eta4,eps4,ans4) = mkAnsatzTensorEig 4 filterList4 symList4 areaList4IndsEta areaList4IndsEps  
    
    let (eta6,eps6,ans6) = mkAnsatzTensorEig 6 filterList6 symList6 areaList6IndsEta areaList6IndsEps 
    
    let (eta8,eps8,ans8) = mkAnsatzTensorFast 8 filterList8 symList8 areaList8IndsEta areaList8IndsEps  

    let (eta10_1,eps10_1,ans10_1) = mkAnsatzTensorEig 10 filterList10_1 symList10_1 areaList10_1IndsEta areaList10_1IndsEps  

    let (eta10_2,eps10_2,ans10_2) = mkAnsatzTensorEig 10 filterList10_2 symList10_2 areaList10_2IndsEta areaList10_2IndsEps  

    let (eta12,eps12,ans12) = mkAnsatzTensorEig 12 filterList12 symList12 areaList12IndsEta areaList12IndsEps  

    let (eta14_1,eps14_1,ans14_1) = mkAnsatzTensorEig 14 filterList14_1 symList14_1 areaList14_1IndsEta areaList14_1IndsEps  

    let (eta14_2,eps14_2,ans14_2) = mkAnsatzTensorEig 14 filterList14_2 symList14_2 areaList14_2IndsEta areaList14_2IndsEps  

    let r0 = tensorRank' ans0

    let r4 = tensorRank' ans4 

    let r6 = tensorRank' ans6 

    let r8 = tensorRank' ans8
    
    let r10_1 = tensorRank' ans10_1
    
    let r10_2 = tensorRank' ans10_2
    
    let r12 = tensorRank' ans12
    
    let r14_1 = tensorRank' ans14_1
    
    let r14_2 = tensorRank' ans14_2

    let ans14_2' = ans14_2

    let ans14_1' = shiftLabels6 r14_2 ans14_1 
    
    let ans12' = shiftLabels6 (r14_2 + r14_1) ans12

    let ans10_2' = shiftLabels6 (r14_2 + r14_1+ r12) ans10_2

    let ans10_1' = shiftLabels6 (r14_2 + r14_1+ r12+ r10_2) ans10_1

    let ans8' = shiftLabels6 (r14_2 + r14_1+ r12+ r10_2+ r10_1) ans8

    let ans6' = shiftLabels6 (r14_2 + r14_1+ r12+ r10_2+ r10_1+ r8) ans6

    let ans4' = shiftLabels6 (r14_2 + r14_1+ r12+ r10_2+ r10_1+ r8 +r6) ans4

    let ans0' = shiftLabels6 (r14_2 + r14_1+ r12+ r10_2+ r10_1+ r8 +r6 + r4) ans0

    let eqn1Area = eqn1 ans0' ans4' 

    let eqn3Area = eqn3 ans6' 

    let eqn1AArea = eqn1A ans4' ans8' 

    let eqn1ABArea = eqn1AB ans8' ans12'

    let eqn1AIArea = eqn1AI ans6' ans10_2' 

    let eqn2AaArea = eqn2Aa ans6' ans10_1' 

    let eqn3AArea = eqn3A ans6' ans10_2'

    let eqn1ABIArea = eqn1ABI ans10_2' ans14_2'

    let eqn3ABArea = eqn3AB ans10_2' ans14_2' 

    let eqn2ABbArea = eqn2ABb ans10_1' ans10_2' ans14_1' 

    let eqn1AaBbArea = eqn1AaBb ans10_1' ans14_1' 
    
    let tList = eqn1AaBbArea &> eqn2ABbArea &> eqn3ABArea&> eqn1ABIArea &> eqn3AArea &> eqn2AaArea &> eqn1AIArea &> eqn1ABArea &> eqn1AArea &> eqn3Area &> (singletonTList eqn1Area) 

    let mList = eqn1ABArea &> eqn1AArea &> (singletonTList eqn1Area)

    let kList = eqn1AaBbArea &> eqn2ABbArea &> eqn3ABArea &> eqn1ABIArea &> eqn3AArea &> eqn2AaArea &> eqn1AIArea &> (singletonTList eqn3Area) 

    let l = toMatList6 tList 

    let showfrac x = if denominator x == 1 then show (numerator x) else show (numerator x) ++ "/" ++ show (denominator x)

    let l' =  map (\(x,y) -> show x ++ "=" ++ showfrac y ++ "," ) l 

    --putStr $ unlines l' 

    --print (r0,r4,r6,r8,r10_1,r10_2,r12,r14_1,r14_2)

    --print $ tensorRank tList

    --print $ tensorRank kList

    --print $ tensorRank mList



    --print $ forestEtaListLatex (relabelAnsatzForest 145 eta14_2) "abcdefghijklpq" 'v' 

    --print $ forestEpsListLatex (relabelAnsatzForestEpsilon 184 eps14_2) "abcdefghijklpq" 'v' 
    
    --print $ flattenForest eta4_2

    --print $ tensorRank' eqn3Area 

    let kList' = eqn3AArea &> eqn2AaArea &> eqn1AIArea &> (singletonTList eqn3Area) 
    
    print $tensorRank kList'