{-# LANGUAGE DataKinds #-}

import Math.Tensor
import Math.Tensor.LorentzGenerator
import Math.Tensor.Examples.Gravity
import Math.Tensor.Examples.Gravity.DiffeoSymEqns

import Data.Ratio

import qualified Data.IntMap.Strict as I

main = do

  let ans0 = fromListT6' [(([],[],[],[],[],[]),AnsVar $I.fromList [(1,1)] )] :: ATens 0 0 0 0 0 0 (AnsVarR)
  let (eta4,eps4,ans4) = mkAnsatzTensorFastAbs 4 symList4 areaList4 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 1 0 0 0 0 0 (AnsVarR))
  let (eta6,eps6,ans6) = mkAnsatzTensorFastAbs 6 symList6 areaList6 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 1 0 1 0 0 0 (AnsVarR))
  let (eta8,eps8,ans8) = mkAnsatzTensorFastAbs 8 symList8 areaList8 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 2 0 0 0 0 0 (AnsVarR))
  let (eta10_1,eps10_1,ans10_1) = mkAnsatzTensorFastAbs 10 symList10_1 areaList10_1 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 2 0 0 0 2 0 (AnsVarR))
  let (eta10_2,eps10_2,ans10_2) = mkAnsatzTensorFastAbs 10 symList10_2 areaList10_2 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 2 0 1 0 0 0 (AnsVarR))
  let (eta12,eps12,ans12) = mkAnsatzTensorFastAbs 12 symList12 areaList12 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 3 0 0 0 0 0 (AnsVarR))
  let evalL = map (\(x,_,_) -> x) areaList14_1
  let (eta14_1,eps14_1,ans14_1) = mkAnsatzTensorEig 14 symList14_1 evalL :: (AnsatzForestEta, AnsatzForestEpsilon, STTens 14 0 (AnsVarR))
  let (eta14_2,eps14_2,ans14_2) = mkAnsatzTensorFastAbs 14 symList14_2 areaList14_2 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 3 0 1 0 0 0 (AnsVarR))

  let zeroT = fromListT6' [(([],[],[],[],[],[]),AnsVar $I.fromList [(1,0)] )] :: ATens 0 0 0 0 0 0 (AnsVarR)


  putStr $ unlines $ map (\x -> "-- > " ++ x) $ lines $ drawAnsatzEpsilon eps6    

  
 

