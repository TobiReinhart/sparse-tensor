{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module IndList (indListTest) where

import Math.Tensor (IndList(..), fromList)

import Test.Tasty
import Test.Tasty.HUnit

il1 :: Maybe (IndList 5 Int)
il1 = fromList [1,2,3,4,5]

il2 :: Maybe (IndList 7 Int)
il2 = fromList [1,2,3,4,5]

il3 :: Maybe (IndList 1 Int)
il3 = fromList [1,2,3,4,5]

testCase1 = testCase "IndListTest1" $
  assertBool
    "fromList [1,2,3,4,5] :: Maybe (IndList 5 Int) failed" $
    case il1 of
      Nothing -> False
      Just (1 `Append` (2 `Append` (3 `Append` (4 `Append` (5 `Append` Empty))))) -> True

testCase2 = testCase "IndListTest2" $
  assertBool "fromList [1,2,3,4,5] :: Maybe (IndList 5 Int) failed" $
    case il2 of
      Nothing -> True
      _       -> False

testCase3 = testCase "IndListTest3" $
  assertBool "fromList [1,2,3,4,5] :: Maybe (IndList 5 Int) failed" $
    case il2 of
      Nothing -> True
      _       -> False

indListTest = testGroup "IndListTest" [testCase1, testCase2, testCase3]
