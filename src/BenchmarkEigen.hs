{-# LANGUAGE DataKinds #-}

module BenchmarkEigen (
 mainBench
) where

import Criterion.Main

import Control.DeepSeq

import qualified Data.Eigen.Matrix as Mat
import qualified Data.Eigen.LA as LA

randMat :: Int -> Int -> IO (Mat.MatrixXd)
randMat = Mat.rand

rank :: Mat.MatrixXd -> Int
rank = LA.rank LA.JacobiSVD

dims :: [Int]
dims = [2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000]
--dims = [2000, 5000, 10000]

mainBench :: IO ()
mainBench = do
        mats <- sequence $ fmap (\dim -> randMat dim dim) dims
        let benchmarks = map (\(dim, mat) -> bench (show dim ++ "x" ++ show dim) $ nf rank mat) $ zip dims mats
        defaultMain benchmarks
