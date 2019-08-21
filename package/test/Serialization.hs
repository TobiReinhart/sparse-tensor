{-# LANGUAGE DataKinds #-}

module Serialization (serializationTest) where

import Math.Tensor
import Math.Tensor.LorentzGenerator

import Test.Tasty
import Test.Tasty.HUnit

ans4 :: ATens 1 0 0 0 0 0 AnsVarR
ans4 = ans4'
  where
    (_, _, ans4') = mkAnsatzTensorFastAbs 4 symList4 areaList4 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 1 0 0 0 0 0 AnsVarR)

ans4Enc = encodeTensor ans4

ans4Success :: Bool
ans4Success = case ans4Dec of
                Left _      -> False
                Right ans4' -> ans4 == ans4'
  where
    ans4Dec = decodeTensor ans4Enc :: Either String (ATens 1 0 0 0 0 0 AnsVarR)

testCase1 = testCase "encode and decode tensor (equal type)" $
  assertBool "decoded tensor does not match encoded tensor or failure while decoding" ans4Success

ans4Fail1 :: Bool
ans4Fail1 = case ans4Dec of
                Left _  -> True
                Right _ -> False
  where
    ans4Dec = decodeTensor ans4Enc :: Either String (ATens 1 0 0 0 0 0 (SField Double))

testCase2 = testCase "encode and decode tensor (wrong type)" $
  assertBool "no error while decoding tensor of wrong type" ans4Fail1

ans4Fail2 :: Bool
ans4Fail2 = case ans4Dec of
                Left _  -> True
                Right _ -> False
  where
    ans4Dec = decodeTensor ans4Enc :: Either String (STTens 0 2 AnsVarR)

testCase3 = testCase "encode and decode tensor (wrong type)" $
  assertBool "no error while decoding tensor of wrong type" ans4Fail2

ans4Fail3 :: Bool
ans4Fail3 = case ans4Dec of
                Left _  -> True
                Right _ -> False
  where
    ans4Dec = decodeTensor ans4Enc :: Either String (ATens 0 1 0 0 0 0 AnsVarR)

serializationTest = testGroup "SerializationTest" [testCase1, testCase2, testCase3]
