module Main (
 main
) where

import PerturbationTree2_3
import TensorTreeNumeric4_2

import qualified Data.ByteString.Lazy as BS


main = do 

    let (eta14_1,eps14_1,tens14_1) = mkAnsatzTensorEig 14 filterList14_1 symList14_1 areaList14_1IndsEta areaList14_1IndsEps 

    let bsEta14_1 = encodeAnsatzForestEta eta14_1 

    let bsEps14_1 = encodeAnsatzForestEpsilon eps14_1 

    let bsTens14_1 = encodeTensor tens14_1 


    let (eta14_2,eps14_2,tens14_2) = mkAnsatzTensorEig 14 filterList14_2 symList14_2 areaList14_2IndsEta areaList14_2IndsEps 

    let bsEta14_2 = encodeAnsatzForestEta eta14_2 

    let bsEps14_2 = encodeAnsatzForestEpsilon eps14_2 

    let bsTens14_2 = encodeTensor tens14_2 


    let (eta16_1,eps16_1,tens16_1) = mkAnsatzTensorEig 16 filterList16_1 symList16_1 areaList16_1IndsEta areaList16_1IndsEps 

    let bsEta16_1 = encodeAnsatzForestEta eta16_1 

    let bsEps16_1 = encodeAnsatzForestEpsilon eps16_1 

    let bsTens16_1 = encodeTensor tens16_1 


    let (eta16_2,eps16_2,tens16_2) = mkAnsatzTensorEig 16 filterList16_2 symList16_2 areaList16_2IndsEta areaList16_2IndsEps 

    let bsEta16_2 = encodeAnsatzForestEta eta16_2 

    let bsEps16_2 = encodeAnsatzForestEpsilon eps16_2 

    let bsTens16_2 = encodeTensor tens16_2 


    
    BS.writeFile "/cip/austausch/cgg/7.4.eta14_1" bsEta14_1

    BS.writeFile "/cip/austausch/cgg/7.4.eps14_1" bsEps14_1

    BS.writeFile "/cip/austausch/cgg/7.4.tens14_1" bsTens14_1

    print "14_1 done !"



    BS.writeFile "/cip/austausch/cgg/7.4.eta14_2" bsEta14_2

    BS.writeFile "/cip/austausch/cgg/7.4.eps14_2" bsEps14_2

    BS.writeFile "/cip/austausch/cgg/7.4.tens14_2" bsTens14_2

    print "14_2 done !"




    BS.writeFile "/cip/austausch/cgg/7.4.eta16_1" bsEta16_1

    BS.writeFile "/cip/austausch/cgg/7.4.eps16_1" bsEps16_1

    BS.writeFile "/cip/austausch/cgg/7.4.tens16_1" bsTens16_1

    print "16_1 done !"




    BS.writeFile "/cip/austausch/cgg/7.4.eta16_2" bsEta16_2

    BS.writeFile "/cip/austausch/cgg/7.4.eps16_2" bsEps16_2

    BS.writeFile "/cip/austausch/cgg/7.4.tens16_2" bsTens16_2

    print "16_2 done !"
    







    
    

    
   