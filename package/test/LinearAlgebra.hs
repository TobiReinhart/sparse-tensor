{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module LinearAlgebra (linearAlgebraTest) where

import Test.QuickCheck
import Test.Tasty.QuickCheck

import Test.Tasty

import System.Exit

import Numeric.LinearAlgebra (rank)
import qualified Numeric.LinearAlgebra.Data as Matrix

import Math.Tensor.Internal.LinearAlgebra (independentColumns, independentColumnsMat)

data SmallInt = S0 | S1 deriving (Show, Ord, Eq, Enum, Bounded)

toSmall :: Int -> SmallInt
toSmall 0 = S0
toSmall 1 = S1
toSmall i = error $ "cannot convert " ++ show i ++ " to SmallInt"

fromSmall :: Num a => SmallInt -> a
fromSmall S0 = 0
fromSmall S1 = 1

instance Arbitrary SmallInt where
    arbitrary = arbitraryBoundedEnum

data MatrixData a = MatrixData (Positive Int) (Positive Int) [a] deriving Show

instance Arbitrary a => Arbitrary (MatrixData a) where
    arbitrary = do
      m@(Positive m') <- arbitrary
      n@(Positive n') <- arbitrary
      xs <- vector (m'*n')
      return $ MatrixData m n xs

prop_smallValues :: MatrixData SmallInt -> Bool
prop_smallValues (MatrixData (Positive rows) (Positive cols) xs) =
    rank mat' == rank mat
  where
    mat  = (rows Matrix.>< cols) $ map fromSmall xs
    mat' = independentColumnsMat mat

prop_ints :: MatrixData Int -> Bool
prop_ints (MatrixData (Positive rows) (Positive cols) xs) =
    rank mat' == rank mat
  where
    mat  = (rows Matrix.>< cols) $ map fromIntegral xs
    mat' = independentColumnsMat mat

prop_doubles :: MatrixData Double -> Bool
prop_doubles (MatrixData (Positive rows) (Positive cols) xs) =
    rank mat' == rank mat
  where
    mat  = (rows Matrix.>< cols) xs
    mat' = independentColumnsMat mat

prop_consec :: Positive Int -> Int -> Bool
prop_consec (Positive dim') start =
    independentColumns mat == [0,1]
  where
    dim = dim' + 100
    mat = (dim Matrix.>< dim) $ map fromIntegral [start..]

testCase1 = testProperty "prop_smallValues" prop_smallValues
testCase2 = testProperty "prop_ints" prop_ints
testCase3 = testProperty "prop_doubles" prop_doubles
testCase4 = testProperty "prop_consec" prop_consec

linearAlgebraTest = testGroup "LinearAlgebraTest" [testCase1, testCase2, testCase3, testCase4]
