{-# LANGUAGE TemplateHaskell #-}

module LinearAlgebra (props_LinearAlgebra) where

import Test.QuickCheck (quickCheckAll, withMaxSuccess)
import Test.QuickCheck.Modifiers

import Numeric.LinearAlgebra (rank)
import Numeric.LinearAlgebra.Data

import Math.Tensor.Internal.LinearAlgebra (independentColumnsMat)

independentColumnsRank :: Positive Int -> Positive Int -> InfiniteList Double -> Bool
independentColumnsRank (Positive rows) (Positive cols) (InfiniteList xs _) =
    rank mat' == rank mat
  where
    mat  = (rows >< cols) $ take (rows * cols) xs
    mat' = independentColumnsMat mat

prop_independentColumnsRank = withMaxSuccess 10000 independentColumnsRank

return []
props_LinearAlgebra = $quickCheckAll

