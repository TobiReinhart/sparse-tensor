{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Main (
 main
) where

import TensorTreeNumeric4 (Tensor, toListShow8, Tensor8, getTensorRank, shiftVarLabels, getTensorRank2, getTensorRank3, toSparseMat, VarMap, toListShowVar, interIArea, interJArea, interJAreaInv, interIAreaInv, trianMapAreaI, trianMapAreaJ)
import PerturbationTree2_2 (mkAnsatzTensor, epsMap, symList12, filterList12, trianMapArea, trianMapDerivative, areaEvalMap12Inds)
import ScalarEquations

import Intertwiners (inverseDerivativeInt)

import qualified Data.ByteString.Lazy as BS (readFile, writeFile)
import qualified Codec.Compression.GZip as GZ (decompress, compress)
import qualified Data.Serialize as S (decodeLazy, Serialize, encodeLazy)
import Data.Either (fromRight)
import Data.Ratio (numerator, denominator)

import GHC.TypeLits

readTensor :: (KnownNat n1, KnownNat n2, KnownNat n3, KnownNat n4,
               KnownNat n5, KnownNat n6, KnownNat n7, KnownNat n8,
               S.Serialize a)
              => String -> IO (Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a)
readTensor fileName =
    do
     bs <- BS.readFile fileName
     let decomp = GZ.decompress bs
     let deserialized = S.decodeLazy decomp
     let unpacked = fromRight undefined deserialized
     return unpacked

readAnsatz0 :: IO (Ansatz0)
readAnsatz0 = readTensor "ansatz/ansatz4.dat.gz"

readAnsatz1 :: IO (Ansatz1)
readAnsatz1 = readTensor "ansatz/ansatz8.dat.gz"

readAnsatz2 :: IO (Ansatz2)
readAnsatz2 = readTensor "ansatz/ansatz12.dat.gz"

generateAnsaetze :: IO ()
generateAnsaetze =
    do
     let aList12 = areaEvalMap12Inds trianMapArea trianMapDerivative
     let ansatz12' = mkAnsatzTensor 12 filterList12 symList12 1 epsMap aList12 
     let ansatz12 = S.encodeLazy ansatz12'
     BS.writeFile "ansatz/ansatz12.dat.gz" $ GZ.compress ansatz12

writeMatrix :: String -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 VarMap -> IO ()
writeMatrix fileName tensor =
    do
     let (rows,cols,mat) = toSparseMat tensor
     let content = unlines $
                   map (\((row, col), v) -> case denominator v of
                                                1 -> "(" ++ show row ++ ", " ++ show col ++ ") = " ++ show (numerator v) ++ ","
                                                _ -> undefined)
                   mat
     writeFile fileName ("mat = Matrix(" ++ show rows ++ ", " ++ show cols ++ " {\n" ++ content ++ "});")

main :: IO ()
main =
    do
     let t = inverseDerivativeInt
     let encoded = S.encodeLazy t
     let compressed = GZ.compress encoded
     BS.writeFile "../inverseDerivativeInt.dat.gz" compressed
