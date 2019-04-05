module Main (
 main
) where

import PerturbationTree2_3

main = do 

    let (a,b,_) = mkAnsatzTensorFast 14 filterList14_2 symList14_2 areaList14_2IndsEta areaList14_2IndsEps 
    
    print $ getForestLabels a 

    print $ getForestLabelsEpsilon b 
