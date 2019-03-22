{-# LANGUAGE DataKinds #-}

module Main (
 main
) where

import TensorTreeNumeric4 (Tensor8, VarMap, toMatrix,
                           toListShowVar, toListShow8,
                           getTensorRank, flatAreaNoEps,
                           toSparseMatRed,
                           flatAreaInvNoEps)
import IntEquations (intEquation, ansatz8Solved, cyclic, invAreaDerivativeFlat)
import PerturbationTree2_2 (generic8Ansatz)

import Data.Serialize (encodeLazy, decodeLazy)
import Codec.Compression.GZip (compress, decompress)
import qualified Data.ByteString.Lazy as BS (writeFile, readFile)

import Data.Ratio (numerator, denominator)

readTensor :: IO (Tensor8 2 0 0 0 0 0 4 0 VarMap)
readTensor = do
                compressed <- BS.readFile "intCond.dat.gz"
                let decompressed = decompress compressed
                let Right t = decodeLazy decompressed
                return t

showMaple :: (Int, Int, [((Int, Int), Rational)]) -> String
showMaple (r, c, vs) = "mat := Matrix(" ++ show r ++ ", " ++ show c ++ ", {\n" ++ content ++ "\n});"
    where
        content = unlines $ map (\((i, j), v) -> "(" ++ show i ++ ", " ++ show j ++ ") = (" ++ show (numerator v) ++ ")/" ++ show (denominator v) ++ ",") vs

prettyT :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> String
prettyT = unlines . map show . filter ((/=0) . snd) . toListShow8

main :: IO ()
main = do
        let t = intEquation
        --let serialized = encodeLazy t
        --let compressed = compress serialized
        --BS.writeFile "intCond.dat.gz" compressed
        --t <- readTensor
        print $ getTensorRank t
        {-
        let t = invAreaDerivativeFlat
        let t' = cyclic t
        let t'' = cyclic t'
        putStrLn "vanilla:"
        putStr $ prettyT t
        putStrLn "one cycle:"
        putStr $ prettyT t'
        putStrLn "two cycles:"
        putStr $ prettyT t''
        putStrLn "equal?"
        putStrLn $ if prettyT t' == prettyT t''
                 then "yes!"
                 else "no"
                 -}
