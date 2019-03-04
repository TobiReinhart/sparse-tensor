{-# LANGUAGE DataKinds #-}

module Main (
 main
) where

import TensorTreeNumeric4

import Codec.Compression.GZip
import Data.Serialize
import Data.Either
import qualified Data.ByteString.Lazy as BS

main = do

    let map1Area = trianMapAreaI
    let map2Area = trianMapAreaJ
    let map1Metric = trianMapI2
    let map2Metric = trianMapJ2

    let int = intAIB map1Metric map2Metric map1Area map2Area

    let e = encodeLazy int
    BS.writeFile "tensor_bs.dat.gz" $ compress e

    e' <- BS.readFile "tensor_bs.dat.gz"
    let d = (fromRight undefined $ decodeLazy $ decompress e') :: Tensor8 1 2 0 0 1 1 2 2 Rational

    -- compare stuff

    print $ int == d
