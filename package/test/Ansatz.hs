{-# LANGUAGE DataKinds #-}

module Ansatz (ansatzTest) where

import Math.Tensor
import Math.Tensor.LorentzGenerator

import Test.Tasty
import Test.Tasty.HUnit

ansatzForestAIEpsilon :: Bool
ansatzForestAIEpsilon = actual == expected
  where
    (_,eps6,_) = mkAnsatzTensorFastAbs 6 symList6 areaList6 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 1 0 1 0 0 0 AnsVarR)
    actual = drawAnsatzEpsilon eps6
    expected = "(1,2,3,4)\n|\n`---- (5,6) * (16) * x[3]\n\n"

testCase1 = testCase "AI epsilon ansatz string" $
  assertBool "epsilon ansatz forest AI does not match expected forest" ansatzForestAIEpsilon

ansatz14_1Fast :: Bool
ansatz14_1Fast = rank == 110
  where
    evalL = map (\(x,_,_) -> x) areaList14_1
    (_,_,ans) = mkAnsatzTensorFast 14 symList14_1 evalL :: (AnsatzForestEta, AnsatzForestEpsilon, STTens 14 0 AnsVarR)
    rank = tensorRank2' ans

testCase2 = testCase "ABpCq ansatz rank" $
  assertBool "rank of ABpCq ansatz does not equal 110" ansatz14_1Fast

ansatz14_1Incr :: Bool
ansatz14_1Incr = rank == 110
  where
    evalL = map (\(x,_,_) -> x) areaList14_1
    (_,_,ans) = mkAnsatzTensorIncremental 14 symList14_1 evalL :: (AnsatzForestEta, AnsatzForestEpsilon, STTens 14 0 AnsVarR)
    rank = tensorRank2' ans

testCase3 = testCase "ABpCq ansatz (incremental) rank" $
  assertBool "rank of ABpCq ansatz (incremental) does not equal 110" ansatz14_1Incr

ansatz14_2Fast :: Bool
ansatz14_2Fast = rank == 72
  where
    (_,_,ans) = mkAnsatzTensorFastAbs 14 symList14_2 areaList14_2 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 3 0 1 0 0 0 AnsVarR)
    rank = tensorRank6' ans

testCase4 = testCase "ABCI ansatz rank" $
  assertBool "rank of ABCI ansatz does not equal 72" ansatz14_2Fast

ansatz14_2Incr :: Bool
ansatz14_2Incr = rank == 72
  where
    (_,_,ans) = mkAnsatzTensorIncrementalAbs 14 symList14_2 areaList14_2 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 3 0 1 0 0 0 AnsVarR)
    rank = tensorRank6' ans

testCase5 = testCase "ABCI ansatz (incremental) rank" $
  assertBool "rank of ABCI ansatz (incremental) does not equal 72" ansatz14_2Incr

testCase6 = testCase "" $ assertBool "" False

ansatzTest = testGroup "AnsatzTest" [testCase1, testCase2, testCase3, testCase4, testCase5]
