{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module LinearAlgebra (props_LinearAlgebra) where

import Test.QuickCheck
import Test.QuickCheck.Modifiers

import System.Exit

import Numeric.LinearAlgebra (rank)
import qualified Numeric.LinearAlgebra.Data as Matrix

import Math.Tensor.Internal.LinearAlgebra (independentColumnsMat)

data SmallInt = S0 | S1 deriving (Show, Ord, Eq, Enum, Bounded)

fromSmall :: Num a => SmallInt -> a
fromSmall S0 = 0
fromSmall S1 = 1

instance Arbitrary SmallInt where
    arbitrary = elements [S0, S1]

smallValues :: Positive Int -> Positive Int -> InfiniteList SmallInt -> Bool
smallValues (Positive rows) (Positive cols) (InfiniteList xs _) =
    rank mat' == rank mat
  where
    mat  = (rows Matrix.>< cols) $ take (rows * cols) $ map fromSmall xs
    mat' = independentColumnsMat mat

ints :: Positive Int -> Positive Int -> InfiniteList Int -> Bool
ints (Positive rows) (Positive cols) (InfiniteList xs _) =
    rank mat' == rank mat
  where
    mat  = (rows Matrix.>< cols) $ take (rows * cols) $ map fromIntegral xs
    mat' = independentColumnsMat mat

doubles :: Positive Int -> Positive Int -> InfiniteList Double -> Bool
doubles (Positive rows) (Positive cols) (InfiniteList xs _) =
    rank mat' == rank mat
  where
    mat  = (rows Matrix.>< cols) $ take (rows * cols) xs
    mat' = independentColumnsMat mat

prop_linIndepColsSmall  = withMaxSuccess 1000 smallValues
prop_linIndepColsInt    = withMaxSuccess 1000 ints
prop_linIndepColsDouble = withMaxSuccess 1000 doubles

return []
props_LinearAlgebra = \case
                        True  -> return ()
                        False -> exitFailure
                      =<< $quickCheckAll
