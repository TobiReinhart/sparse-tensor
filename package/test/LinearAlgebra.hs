{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module LinearAlgebra (props_LinearAlgebra) where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers

import System.Exit

import Numeric.LinearAlgebra (rank)
import qualified Numeric.LinearAlgebra.Data as Matrix

import Math.Tensor.Internal.LinearAlgebra (independentColumnsMat)

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

smallValues :: MatrixData SmallInt -> Bool
smallValues (MatrixData (Positive rows) (Positive cols) xs) =
    rank mat' == rank mat
  where
    mat  = (rows Matrix.>< cols) $ map fromSmall xs
    mat' = independentColumnsMat mat

ints :: MatrixData Int -> Bool
ints (MatrixData (Positive rows) (Positive cols) xs) =
    rank mat' == rank mat
  where
    mat  = (rows Matrix.>< cols) $ map fromIntegral xs
    mat' = independentColumnsMat mat

doubles :: MatrixData Double -> Bool
doubles (MatrixData (Positive rows) (Positive cols) xs) =
    rank mat' == rank mat
  where
    mat  = (rows Matrix.>< cols) xs
    mat' = independentColumnsMat mat

prop_linIndepColsSmall  = withMaxSuccess 1000 smallValues
prop_linIndepColsInt    = withMaxSuccess 1000 ints
prop_linIndepColsDouble = withMaxSuccess 1000 doubles

return []
props_LinearAlgebra = \case
                        True  -> return ()
                        False -> exitFailure
                      =<< $quickCheckAll
