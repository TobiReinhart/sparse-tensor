{-#LANGUAGE DataKinds#-}

import Math.Tensor
import Math.Tensor.LorentzGenerator
import Math.Tensor.Examples.Gravity
import Math.Tensor.Examples.Gravity.DiffeoSymEqns
import Math.Tensor.Examples.Gravity.Schwarzschild

import Data.Ratio

import qualified Data.IntMap.Strict as I

main = do

  let (eta4,eps4,ans4) = mkAnsatzTensorFastAbs 4 symList4 areaList4 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 1 0 0 0 0 0 AnsVarR)
  let (eta6,eps6,ans6) = mkAnsatzTensorFastAbs 6 symList6 areaList6 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 1 0 1 0 0 0 AnsVarR)
  let (eta8,eps8,ans8) = mkAnsatzTensorFastAbs 8 symList8 areaList8 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 2 0 0 0 0 0 AnsVarR)
  let (eta10_1,eps10_1,ans10_1) = mkAnsatzTensorFastAbs 10 symList10_1 areaList10_1 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 2 0 0 0 2 0 AnsVarR)
  let (_,_,ans10_1') = mkAnsatzTensorFastSym' 10 symList10_1 :: (AnsatzForestEta, AnsatzForestEpsilon, STTens 10 0 AnsVarR)
  let (eta10_2,eps10_2,ans10_2) = mkAnsatzTensorFastAbs 10 symList10_2 areaList10_2 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 2 0 1 0 0 0 AnsVarR)
  let (eta12,eps12,ans12) = mkAnsatzTensorFastAbs 12 symList12 areaList12 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 3 0 0 0 0 0 AnsVarR)
  let (eta14_1,eps14_1,ans14_1) = mkAnsatzTensorFastAbs 14 symList14_1 areaList14_1 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 3 0 0 0 2 0 AnsVarR)
  let (eta14_2,eps14_2,ans14_2) = mkAnsatzTensorFastAbs 14 symList14_2 areaList14_2 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 3 0 1 0 0 0 AnsVarR)

  let ans10_1'' = Scalar $ Scalar $ Scalar $ Scalar ans10_1' :: ATens 0 0 0 0 10 0 AnsVarR

  let ans10_1 = contrATens3 (0,0) $ contrATens3 (1,1) $ contrATens3 (2,2) $ contrATens3 (3,3) $
                contrATens3 (5,4) $ contrATens3 (6,5) $ contrATens3 (7,6) $ contrATens3 (8,7) $
                interIArea &* interIArea &* ans10_1''

  print $ tensorRank6' $ ansatzA ans4
  print $ tensorRank6' $ ansatzAI ans6
  print $ tensorRank6' $ ansatzAB ans8
  print $ tensorRank6' $ ansatzAaBb ans10_1
  print $ tensorRank6' $ ansatzABI ans10_2
  print $ tensorRank6' $ ansatzABC ans12
  print $ tensorRank6' $ ansatzABbCc ans14_1
  print $ tensorRank6' $ ansatzABCI ans14_2
