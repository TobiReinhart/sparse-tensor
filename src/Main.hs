{-# LANGUAGE DataKinds #-}

module Main (
 main
) where

import TensorTreeNumeric4
import PerturbationTree2_2 

import Codec.Compression.GZip
import Data.Serialize
import Data.Either
import qualified Data.ByteString.Lazy as BS

main = do

    let map1Area = trianMapAreaI
    let map2Area = trianMapAreaJ
    let map1Metric = trianMapI2
    let map2Metric = trianMapJ2

    let aList4 = areaEvalMap4Inds trianMapArea trianMapDerivative
    let aList6 = areaEvalMap6Inds trianMapArea trianMapDerivative
    let aList8 = areaEvalMap8Inds trianMapArea trianMapDerivative
    let aList10_1 = areaEvalMap10_1Inds trianMapArea trianMapDerivative
    let aList10_2 = areaEvalMap10_2Inds trianMapArea trianMapDerivative
    let aList12 = areaEvalMap12Inds trianMapArea trianMapDerivative
    let aList12_1 = areaEvalMap12_1Inds trianMapArea trianMapDerivative
    let aList14_1 = areaEvalMap14_1Inds trianMapArea trianMapDerivative
    let aList14_2 = areaEvalMap14_2Inds trianMapArea trianMapDerivative
    let aList16_1 = areaEvalMap16_1Inds trianMapArea trianMapDerivative
    let aList16_2 = areaEvalMap16_2Inds trianMapArea trianMapDerivative
    let aList18 = areaEvalMap18Inds trianMapArea trianMapDerivative


    let ansatz4' = mkAnsatzTensor 4 filterList4 symList4 1 epsMap aList4 
    let ansatz6' = mkAnsatzTensor 6 filterList6 symList6 1 epsMap aList6 
    let ansatz8' = mkAnsatzTensor 8 filterList8 symList8 1 epsMap aList8 
    let ansatz10_1' = mkAnsatzTensor 10 filterList10_1 symList10_1 1 epsMap aList10_1 
    let ansatz10_2' = mkAnsatzTensor 10 filterList10_2 symList10_2 1 epsMap aList10_2 
    let ansatz12' = mkAnsatzTensor 12 filterList12 symList12 1 epsMap aList12 
    let ansatz12_1' = mkAnsatzTensor 12 filterList12_1 symList12_1 1 epsMap aList12_1 
    let ansatz14_1' = mkAnsatzTensor 14 filterList14_1 symList14_1 1 epsMap aList14_1 
    let ansatz14_2' = mkAnsatzTensor 14 filterList14_2 symList14_2 1 epsMap aList14_2 
    let ansatz16_1' = mkAnsatzTensor 16 filterList16_1 symList16_1 1 epsMap aList16_1 
    let ansatz16_2' = mkAnsatzTensor 16 filterList16_2 symList16_2 1 epsMap aList16_2
    let ansatz18' = mkAnsatzTensor 18 filterList18 symList18 1 epsMap aList18 
 

    let ansatz4 = encodeLazy ansatz4'
    let ansatz6 = encodeLazy ansatz6'
    let ansatz8 = encodeLazy ansatz8'
    let ansatz10_1 = encodeLazy ansatz10_1'
    let ansatz10_2 = encodeLazy ansatz10_2'
    let ansatz12 = encodeLazy ansatz12'
    let ansatz12_1 = encodeLazy ansatz12_1'
    let ansatz14_1 = encodeLazy ansatz14_1'
    let ansatz14_2 = encodeLazy ansatz14_2'
    let ansatz16_1 = encodeLazy ansatz16_1'
    let ansatz16_2 = encodeLazy ansatz16_2'
    let ansatz18 = encodeLazy ansatz18'

    BS.writeFile "/cip/austausch/cgg/ansatz4.dat.gz" $ compress ansatz4
    BS.writeFile "/cip/austausch/cgg/ansatz6.dat.gz" $ compress ansatz6
    BS.writeFile "/cip/austausch/cgg/ansatz8.dat.gz" $ compress ansatz8
    BS.writeFile "/cip/austausch/cgg/ansatz10_1.dat.gz" $ compress ansatz10_1
    BS.writeFile "/cip/austausch/cgg/ansatz10_2.dat.gz" $ compress ansatz10_2
    BS.writeFile "/cip/austausch/cgg/ansatz12.dat.gz" $ compress ansatz12
    BS.writeFile "/cip/austausch/cgg/ansatz12_1.dat.gz" $ compress ansatz12_1
    BS.writeFile "/cip/austausch/cgg/ansatz14_1.dat.gz" $ compress ansatz14_1
    BS.writeFile "/cip/austausch/cgg/ansatz14_2.dat.gz" $ compress ansatz14_2
    BS.writeFile "/cip/austausch/cgg/ansatz16_1.dat.gz" $ compress ansatz16_1
    BS.writeFile "/cip/austausch/cgg/ansatz16_2.dat.gz" $ compress ansatz16_2
    --BS.writeFile "/cip/austausch/cgg/ansatz18.dat.gz" $ compress ansatz18

    --e' <- BS.readFile "tensor_bs.dat.gz"
    --let d = (fromRight undefined $ decodeLazy $ decompress e') :: Tensor8 3 0 0 0 1 0 0 0 VarMap

    -- compare stuff

