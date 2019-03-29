{-# LANGUAGE DataKinds #-}

module Main (
 main
) where

import TensorTreeNumeric4 
import PerturbationTree2_2 
import qualified Data.Eigen.Matrix as Mat 
import Data.List
import qualified Data.Eigen.LA as Sol 

import HList

import Data.Ratio


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
    let aList18_2 = areaEvalMap18_2Inds trianMapArea trianMapDerivative
    let aList18_3 = areaEvalMap18_3Inds trianMapArea trianMapDerivative
    let aList16 = areaEvalMap16Inds trianMapArea trianMapDerivative 
    let aList20 = areaEvalMap20Inds trianMapArea trianMapDerivative 




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
    let ansatz18_2' = mkAnsatzTensor 18 filterList18_2 symList18_2 1 epsMap aList18_2 
    let ansatz18_3' = mkAnsatzTensor 18 filterList18_3 symList18_3 1 epsMap aList18_3 
    let ansatz16' = mkAnsatzTensor 16 filterList16 symList16 1 epsMap aList16 
    let ansatz20' = mkAnsatzTensor 20 filterList20 symList20 1 epsMap aList20 

    let ansatz12'' = ansatz12' 

    let ansatz8'' = shiftVarLabels (getTensorRank ansatz12'') ansatz8' 

    let ansatz4'' = shiftVarLabels (getTensorRank ansatz12'' + getTensorRank ansatz8'') ansatz4' 


    let eqn1T = eqn1 map1Metric map2Metric map1Area map2Area ansatz4'' 

    let eqn1TG = eqn1 map1Metric map2Metric map1Area map2Area generic4Ansatz 

    let intCond1 = ansatzA map1Metric map2Metric map1Area map2Area generic4Ansatz

    let eqn1AT = eqn1A map1Metric map2Metric map1Area map2Area ansatz4'' ansatz8'' 

    let eqn1ATG = eqn1A map1Metric map2Metric map1Area map2Area generic4Ansatz generic8Ansatz 

    let intCondAB = ansatzAB map1Metric map2Metric map1Area map2Area generic8Ansatz

    let eqn1ABT = eqn1AB map1Metric map2Metric map1Area map2Area ansatz8'' ansatz12'' 
    

    --print $ getTensorRank2 eqn1T eqn1AT
    
    --print $ getTensorRank3 eqn1T eqn1AT eqn1ABT

    let symbol = toMatrix2 eqn1ATG intCondAB 

    let intCondTest = interMetricAreaTest map1Metric map2Metric map1Area map2Area ansatz4'

    let eqn1T' =  eqn1 map1Metric map2Metric map1Area map2Area ansatz4'

    --print $ getTensorRank2 intCondTest eqn1T' 

    --generic equations


    let eqn1Generic = eqn1G 2 3 5 7 11 13 map1Metric map2Metric map1Area map2Area generic4Ansatz

    let eqn2Generic = eqn2G 2 3 5 7 11 13 map1Metric map2Metric map1Area map2Area generic5Ansatz

    let eqn3Generic = eqn3G 2 3 5 7 11 13 map1Metric map2Metric map1Area map2Area generic6Ansatz

    let eqn1AGeneric = eqn1AG 2 3 4 7 11 13 map1Metric map2Metric map1Area map2Area generic4Ansatz generic8Ansatz

    let eqn1AaGeneric = eqn1AaG 2 3 4 7 11 13 map1Metric map2Metric map1Area map2Area generic5Ansatz generic9Ansatz 

    let eqn1AIGeneric = eqn1AIG 2 3 4 7 11 13 map1Metric map2Metric map1Area map2Area generic6Ansatz generic10_2Ansatz 

    let eqn2AGeneric = eqn2AG 2 3 4 7 11 13 map1Metric map2Metric map1Area map2Area generic5Ansatz generic9Ansatz 

    let eqn2AaGeneric = eqn2AaG 2 3 4 7 11 13 map1Metric map2Metric map1Area map2Area generic6Ansatz generic10_1Ansatz 

    let eqn2AIGeneric = eqn2AIG 2 3 4 7 11 13 map1Metric map2Metric map1Area map2Area generic11Ansatz 
    
    let eqn3AGeneric = eqn3AG 2 3 4 7 11 13 map1Metric map2Metric map1Area map2Area generic6Ansatz generic10_2Ansatz 

    let eqn3AaGeneric = eqn3AaG 2 3 4 7 11 13 map1Metric map2Metric map1Area map2Area generic11Ansatz  

    let eqn3AIGeneric = eqn3AIG 2 3 4 7 11 13 map1Metric map2Metric map1Area map2Area generic12Ansatz  

    print $ rankN [HB eqn1Generic, HB eqn2Generic, HB eqn3Generic,
                   HB eqn1AGeneric, HB eqn1AaGeneric, HB eqn1AIGeneric,
                   HB eqn2AGeneric, HB eqn2AaGeneric, HB eqn2AIGeneric,
                   HB eqn3AGeneric, HB eqn3AaGeneric, HB eqn3AIGeneric]
