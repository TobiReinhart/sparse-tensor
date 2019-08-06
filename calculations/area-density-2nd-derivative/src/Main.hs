{-# LANGUAGE DataKinds #-}

import Math.Tensor
import Math.Tensor.LorentzGenerator
import Math.Tensor.Examples.Gravity
import Math.Tensor.Examples.Gravity.DiffeoSymEqns

import Data.Ratio

import qualified Data.IntMap.Strict as I

main = do

  let ans0 = fromListT6' [(([],[],[],[],[],[]),AnsVar $I.fromList [(1,1)] )] :: ATens 0 0 0 0 0 0 (AnsVar Rational)
  let (eta4,eps4,ans4) = mkAnsatzTensorFastAbs 4 symList4 areaList4 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 1 0 0 0 0 0 (AnsVar Rational))
  let (eta6,eps6,ans6) = mkAnsatzTensorFastAbs 6 symList6 areaList6 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 1 0 1 0 0 0 (AnsVar Rational))
  let (eta8,eps8,ans8) = mkAnsatzTensorFastAbs 8 symList8 areaList8 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 2 0 0 0 0 0 (AnsVar Rational))
  let (eta10_1,eps10_1,ans10_1) = mkAnsatzTensorFastAbs 10 symList10_1 areaList10_1 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 2 0 0 0 2 0 (AnsVar Rational))
  let (eta10_2,eps10_2,ans10_2) = mkAnsatzTensorFastAbs 10 symList10_2 areaList10_2 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 2 0 1 0 0 0 (AnsVar Rational))
  let (eta12,eps12,ans12) = mkAnsatzTensorFastAbs 12 symList12 areaList12 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 3 0 0 0 0 0 (AnsVar Rational))
  let (eta14_1,eps14_1,ans14_1) = mkAnsatzTensorEigAbs 14 symList14_1 areaList14_1 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 3 0 0 0 2 0 (AnsVar Rational))
  let (eta14_2,eps14_2,ans14_2) = mkAnsatzTensorFastAbs 14 symList14_2 areaList14_2 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 3 0 1 0 0 0 (AnsVar Rational))


  let r0 = tensorRank6' ans0

  let r4 = tensorRank6' ans4 

  let r6 = tensorRank6' ans6 

  let r8 = tensorRank6' ans8
  
  let r10_1 = tensorRank6' ans10_1
  
  let r10_2 = tensorRank6' ans10_2
  
  let r12 = tensorRank6' ans12
  
  let r14_1 = tensorRank6' ans14_1
  
  let r14_2 = tensorRank6' ans14_2

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
  
  let tList = eqn1AaBbArea &.&> eqn2ABbArea &.&> eqn3ABArea&.&> eqn1ABIArea &.&> eqn3AArea &.&> eqn2AaArea &.&> eqn1AIArea &.&> eqn1ABArea &.&> eqn1AArea &.&> eqn3Area &.&> (singletonTList6 eqn1Area :: TensList6 Ind20 Ind9 Ind3 (AnsVar Rational) ) 

  let l = toMatList6 tList 

  let showfrac x = if denominator x == 1 then show (numerator x) else show (numerator x) ++ "/" ++ show (denominator x)

  let l' =  map (\(x,y) -> show x ++ "=" ++ showfrac y ++ "," ) l 

  --putStr $ unlines l' 

  --print (r0,r4,r6,r8,r10_1,r10_2,r12,r14_1,r14_2)

  print $ tensorRank6' ans14_1

