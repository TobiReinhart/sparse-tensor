-----------------------------------------------------------------------------
-- |
-- Module      :  Math.Tensor.Internal.LinearAlgebra
-- Copyright   :  (c) 2019 Tobias Reinhart and Nils Alex
-- License     :  MIT
-- Maintainer  :  tobi.reinhart@fau.de, nils.alex@fau.de
--
-- Gaussian elimination algorithm based on hmatrix.
-----------------------------------------------------------------------------
module Math.Tensor.Internal.LinearAlgebra (
-- * Gaussian Elimination
-- ** In the ST Monad
gaussianST,
-- ** Pure
gaussian,
-- * Linearly Independent Columns
-- ** Indices of Independent Colums
independentColumns,
-- ** Submatrix of Independent Columns
independentColumnsMat,
-- * Helper Functions
-- ** Pivots of Upper Triangular Matrix
pivotsU,
-- ** Next Pivot for Elimination Step
findPivotMax)

where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.Devel

import Data.List (maximumBy)

import Control.Monad
import Control.Monad.ST

pivotsU :: Matrix Double -> [Int]
pivotsU mat = go (0,0)
  where
    go (i,j)
      = case findPivot mat (i,j) of
          Nothing       -> []
          Just (i', j') -> j' : go (i'+1, j'+1)

eps :: Double -> Bool
eps = (< 1e-12) . abs

findPivot :: Matrix Double -> (Int, Int) -> Maybe (Int, Int)
findPivot mat (i, j)
    | n == j = Nothing
    | m == i = Nothing
    | otherwise = case nonZeros of
                    []          -> if n == j+1
                                   then Nothing
                                   else findPivot mat (i, j+1)
                    (pi, pj):_  -> Just (pi, pj+j)
    where
        m = rows mat
        n = cols mat
        col = mat ¿ [j]
        nonZeros = filter (\(i', _) -> i' >= i) $ find (not . eps) col

findPivotMax :: Matrix Double -> Int -> Int -> Maybe (Int, Int)
findPivotMax mat i j
    | n == j = Nothing
    | m == i = Nothing
    | otherwise = case nonZeros of
                    [] -> if n == j+1
                          then Nothing
                          else findPivotMax mat i (j+1)
                    _  -> Just (pi, pj+j)
    where
        m = rows mat
        n = cols mat
        col = mat ¿ [j]
        nonZeros = filter (\(i', _) -> i' >= i) $ find (not . eps) col
        (pi, pj) = maximumBy (\ix jx -> abs (col `atIndex` ix)
                                        `compare`
                                        abs (col `atIndex` jx))
                             nonZeros

gaussian' :: Int -> Int -> STMatrix s Double -> ST s ()
gaussian' i j mat = do
    m       <- liftSTMatrix rows mat
    n       <- liftSTMatrix cols mat
    iPivot' <- liftSTMatrix (\x -> findPivotMax x i j) mat
    case iPivot' of
        Nothing     -> return ()
        Just (r, p) -> do
                          rowOper (SWAP i r (FromCol j)) mat
                          pv <- liftSTMatrix (`atIndex` (i, p)) mat
                          mapM_ (reduce pv p) [i+1 .. m-1]
                          gaussian' (i+1) (p+1) mat
  where
    reduce pv p r = do
                      rv <- liftSTMatrix (`atIndex` (r, p)) mat
                      if eps rv
                        then return ()
                        else
                         let frac = -rv / pv
                             op   = AXPY frac i r (FromCol p)
                         in  rowOper op mat

gaussianST :: STMatrix s Double -> ST s ()
gaussianST = gaussian' 0 0

gaussian :: Matrix Double -> Matrix Double
gaussian mat = runST $ do
    m <- thawMatrix mat
    gaussianST m
    freezeMatrix m

independentColumns :: Matrix Double -> [Int]
independentColumns mat = pivotsU mat'
    where
        mat' = gaussian mat

independentColumnsMat :: Matrix Double -> Matrix Double
independentColumnsMat mat =
  case independentColumns mat of
    [] -> (rows mat >< 1) $ repeat 0
    cs -> mat ¿ cs
