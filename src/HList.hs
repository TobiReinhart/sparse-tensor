{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module HList (toMatrixN, rankN, HBox (HB)) where

import TensorTreeNumeric4
import PerturbationTree2_2

import Data.List (nubBy)

import qualified Data.Eigen.SparseMatrix as Sparse

import qualified Numeric.LinearAlgebra as HLin
import qualified Numeric.LinearAlgebra.Data as HMat

class MatrixType a where
    toMat :: a -> [[(Int, Rational)]]

instance MatrixType (Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 VarMap) where
    toMat = toMatList

data HBox = forall r.MatrixType r => HB r

instance MatrixType HBox where
    toMat (HB a) = toMat a

hList :: [HBox]
hList = [HB generic4Ansatz, HB generic8Ansatz]

toMatrixN :: [HBox] -> (Int, Int, [((Int, Int), Rational)])
toMatrixN h = (n, m, l')
    where
        mat = concat $ map toMat h
        l2 = nubBy (\(a,_) (b,_) -> a == b) $ map normalize mat
        l = map (\(x,y) -> map (\(a,b) -> (a, b*y)) x) l2
        l' = concat $ zipWith (\r z -> map (\(x,y) -> ((z, x), y)) r) l [1..]
        n = length l
        m = maximum $ map fst $ concat l 

rankN :: [HBox] -> Int
rankN h = r
    where
        (n,m,mat) = toMatrixN h
        eigenFormat = map (\((i, j), v) -> (i-1, j-1, fromRational v)) mat :: [(Int, Int, Double)]
        sp = Sparse.fromList n m eigenFormat :: Sparse.SparseMatrixXd
        trans = Sparse.transpose sp
        rows = Sparse.rows sp
        cols = Sparse.cols sp
        sing = if rows < cols then sp * trans else trans * sp
        l = Sparse.toList sing
        hmat = HMat.toDense $ map (\(i,j,v) -> ((i,j),v)) l
        r = HLin.rank hmat
