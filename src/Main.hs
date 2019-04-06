module Main (
 main
) where

import PerturbationTree2_3

main = do 

    let (a,b,_) = mkAnsatzTensorEig 14 filterList14_1 symList14_1 areaList14_1IndsEta areaList14_1IndsEps 
    
    print $ getForestLabels a 

    print $ getForestLabelsEpsilon b 

