module Main (
 main
) where

import PerturbationTree2_3
import TensorTreeNumeric4_2

import System.IO

import qualified Data.ByteString.Lazy as BS

main =
    do
     hSetBuffering stdout LineBuffering
     (eta20, eps20, _) <- mkAnsatzTensorEigIO 20 filterList20 symList20 areaList20IndsEta areaList20IndsEps
     BS.writeFile "ansatz/eta20.dat.gz" $ encodeAnsatzForestEta eta20
     BS.writeFile "ansatz/eps20.dat.gz" $ encodeAnsatzForestEpsilon eps20
