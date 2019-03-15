--pushes type stuff to kind stuff (prefixed with ')
{-# LANGUAGE DataKinds #-}
--matching on type constructors
{-# LANGUAGE GADTs #-}
--kind signature
{-# LANGUAGE KindSignatures #-}
--type family definitions
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
--infix type plus and mult
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE LambdaCase #-}


{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver   #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

{-# OPTIONS_GHC -dcore-lint #-}

{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}
module Main (
 main
) where

import TensorTreeNumeric4
import PerturbationTree2_2 

import Codec.Compression.GZip
import Data.Serialize
import Data.Either
import qualified Data.ByteString.Lazy as BS
import qualified Data.Eigen.Matrix as Mat 
import qualified Data.Eigen.SparseMatrix as Sparse
import qualified Data.Eigen.LA as Sol 

import qualified Data.Map.Strict as M
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
    let ansatz18_2 = encodeLazy ansatz18_2'
    let ansatz18_3 = encodeLazy ansatz18_3'


    --BS.writeFile "/cip/austausch/cgg/ansatz4.dat.gz" $ compress ansatz4
    --BS.writeFile "/cip/austausch/cgg/ansatz6.dat.gz" $ compress ansatz6
    --BS.writeFile "/cip/austausch/cgg/ansatz8.dat.gz" $ compress ansatz8
    --BS.writeFile "/cip/austausch/cgg/ansatz10_1.dat.gz" $ compress ansatz10_1
    --BS.writeFile "/cip/austausch/cgg/ansatz10_2.dat.gz" $ compress ansatz10_2
    --BS.writeFile "/cip/austausch/cgg/ansatz12.dat.gz" $ compress ansatz12
    --BS.writeFile "/cip/austausch/cgg/ansatz12_1.dat.gz" $ compress ansatz12_1
    --BS.writeFile "/cip/austausch/cgg/ansatz14_1.dat.gz" $ compress ansatz14_1
    --BS.writeFile "/cip/austausch/cgg/ansatz14_2.dat.gz" $ compress ansatz14_2
    --BS.writeFile "/cip/austausch/cgg/ansatz16_1.dat.gz" $ compress ansatz16_1
    --BS.writeFile "/cip/austausch/cgg/ansatz16_2.dat.gz" $ compress ansatz16_2
    --BS.writeFile "/cip/austausch/cgg/ansatz18.dat.gz" $ compress ansatz18


    --e' <- BS.readFile "tensor_bs.dat.gz"
    --let d = (fromRight undefined $ decodeLazy $ decompress e') :: Tensor8 3 0 0 0 1 0 0 0 VarMap

    let ansatz10_2'' = shiftVarLabels 3 ansatz10_2' 

    let intCond = intCondTestnoDens map1Metric map2Metric map1Area map2Area ansatz10_2''

    let eqn1Pro = eqn1TestnoDens map1Metric map2Metric map1Area map2Area ansatz6' ansatz10_2''

    let eqn3 = eqn3Test1 map1Metric map2Metric map1Area map2Area ansatz6' 

    let eqn3Pro = eqn3Test2 map1Metric map2Metric map1Area map2Area ansatz6' ansatz10_2''

    let mat = toMatrix2 eqn1Pro intCond

    let matList = Mat.toList mat 

    let testList = [0,1] ++ replicate ((length $ head matList)-1) 0 

    let newList = matList ++ [testList]

    let newMat = Mat.fromList newList 

    --print $ Sol.rank Sol.FullPivLU newMat

    let intTest = interTest map1Metric map2Metric map1Area map2Area 

    let intTest2 = interTest2 map1Metric map2Metric map1Area map2Area 

    let ans14Test = ansatz14Test map1Metric map2Metric map1Area map2Area ansatz14_2' 

    let ans14Test2 = ansatz14Test2 map1Metric map2Metric map1Area map2Area ansatz14_2' 

    let ans14Test3 = ansatz14Test3 map1Metric map2Metric map1Area map2Area ansatz14_2' 

    let ans14Test4 = ansatz14Test4 map1Metric map2Metric map1Area map2Area ansatz14_2' 

    let ans14Test5 = ansatz14Test5 map1Metric map2Metric map1Area map2Area ansatz14_2' 

    let ans14Test6 = ansatz14Test6 map1Metric map2Metric map1Area map2Area ansatz14_2' 

    let ans14Test7 = ansatz14Test7 map1Metric map2Metric map1Area map2Area ansatz14_2' 

    let ans14Test8 = ansatz14Test8 map1Metric map2Metric map1Area map2Area ansatz14_2' 

    let ans14Test9 = ansatz14Test9 map1Metric map2Metric map1Area map2Area ansatz14_2' 

    let ans14Test10 = ansatz14Test10 map1Metric map2Metric map1Area map2Area ansatz14_2' 

    let ans14Test11 = ansatz14Test11 map1Metric map2Metric map1Area map2Area ansatz14_2' 

    let ans14Test12 = ansatz14Test12 map1Metric map2Metric map1Area map2Area ansatz14_2' 

    let ans14Test13 = ansatz14Test13 map1Metric map2Metric map1Area map2Area ansatz14_2' 

    let ans14Test14 = ansatz14Test14 map1Metric map2Metric map1Area map2Area ansatz14_2' 

    let ans10Test = ansatz10_2Test7 map1Metric map2Metric map1Area map2Area ansatz10_2'

    let ans6TestZero = ansatz6TestZero map1Metric map2Metric map1Area map2Area 

    let ans6TestZero2 = ansatz6Test map1Metric map2Metric map1Area map2Area ansatz6' 

    let ans6TestZero3 = ansatz6TestZero2 map1Metric map2Metric map1Area map2Area  

    let ans6TestZero4 = ansatz6TestZero3 map1Metric map2Metric map1Area map2Area 
    
    let ans6TestZero5' = ansatz6TestZero4 map1Metric map2Metric map1Area map2Area testTens  

    let ans6TestZero6 = ansatz6TestZero5 map1Metric map2Metric map1Area map2Area   

    let ans10Mat = ansatz10TestMat map1Metric map2Metric map1Area map2Area

    let matAI = map evalAnsatz6Test $ filter (\(x,y) -> y/=0) $ toListShow8 ans6TestZero6 

    let trian315 = triangleMap2P' 315 

    let matACJ = map (evalAnsatz10Test trian315) $ filter (\(x,y) -> y/=0) $ toListShow8 ans10Mat

    let eqn1_1 = map evalEqn1Part1 $ filter (\(x,y) -> y/=0) $ toListShow8 (eqn1Part1 map1Metric map2Metric map1Area map2Area)

    --let eqn1_2 = map (evalEqn1Part2 trian315) $ filter (\(x,y) -> y/=0) $ toListShow8 (eqn1Part2 map1Metric map2Metric map1Area map2Area)

    --let eqn1 = eqn1_1 ++ eqn1_2 


    let ans6Full' = mkAnsatzTensor 6 filterList6 symList6 1 epsMap areaEvalMap6IndsFull 

    let ans6Full = tensorContrWith3 (0,0) addVarsMap $ tensorContrWith3 (1,1) addVarsMap $ tensorContrWith3 (2,2) addVarsMap $ tensorContrWith3 (3,3) addVarsMap $ tensorContrWith3 (4,4) addVarsMap $  tensorContrWith3 (5,5) addVarsMap $ tensorProdWith8 (flip multVarsMap) ans6Full' $ tensorProd8 (interIArea map1Area) (interI2 map1Metric) 

    let ans6TestZero5 = ansatz6TestZero4 map1Metric map2Metric map1Area map2Area ans6Full  

    --print $ toListShowVar ans6TestZero5

    let ans6TestZero7 = ansatzAI map1Metric map2Metric map1Area map2Area ans6Full  

    --print $ toListShowVar ans6TestZero7

    --print $ toListShowVar ans6Full 

    let sym1 = symbol1 map1Metric map2Metric map1Area map2Area ansatz10_2' 

    let sym1Mat =  map evalSymbol1Mat $ toListShow8 $ symbol1Mat map1Metric map2Metric map1Area map2Area 

    let ans1Mat = map evalAnsatz1Mat $ toListShow8 $ ansatz1Mat map1Metric map2Metric map1Area map2Area

    let totalMat = sym1Mat ++ (map (\(a,b,c) -> (a+2100,b,c)) ans1Mat) 

    let ansatz6'' = shiftVarLabels 213 ansatz6'

    let ansatz10_2'' = shiftVarLabels 197 ansatz10_2'

    let ansatz10_1'' = shiftVarLabels 182 ansatz10_1'

    let ansatz14_1'' = shiftVarLabels 72 ansatz14_1' 

    let (m3,_,eqn3_1) = toSparseMatRed $ eqn3Test1 map1Metric map2Metric map1Area map2Area ansatz6''

    let (m4,_,eqn3_2) = toSparseMatRed $ eqn3Test2 map1Metric map2Metric map1Area map2Area ansatz6'' ansatz10_2''

    let (m5,_,eqn3_3) = toSparseMatRed $ eqn3Test3 map1Metric map2Metric map1Area map2Area ansatz10_2'' ansatz14_2'
    
    let (m,_,eqn1List) = toSparseMatRed $ eqn1Total map1Metric map2Metric map1Area map2Area ansatz6'' ansatz10_2'' 

    let (m',_,eqn2List) = toSparseMatRed $ eqn1Prolong map1Metric map2Metric map1Area map2Area ansatz10_2'' ansatz14_2' 

    let (m6,_,eqn2_1) = toSparseMatRed $ eqn2 map1Metric map2Metric map1Area map2Area ansatz6'' ansatz10_1''

    let (m7,_,eqn2_2') = toSparseMatRed $ eqn2_2 map1Metric map2Metric map1Area map2Area ansatz10_1'' ansatz10_2'' ansatz14_1''

    let (m8,_,eqn1_2') = toSparseMatRed $ eqn1_2 map1Metric map2Metric map1Area map2Area ansatz10_1'' ansatz14_1''



    let fullEqn1 = eqn1List ++ (map (\((x,y),z) -> ((x+m,y),z)) eqn2List) ++ (map (\((x,y),z) -> ((x+m+m',y),z)) eqn3_1) ++ (map (\((x,y),z) -> ((x+m+m'+m3,y),z)) eqn3_2) ++ (map (\((x,y),z) -> ((x+m+m'+m3 +m4 ,y),z)) eqn3_3)++ (map (\((x,y),z) -> ((x+m+m'+m3 +m4+m5 ,y),z)) eqn2_1) ++ (map (\((x,y),z) -> ((x+m+m'+m3 +m4+m5+m6 ,y),z)) eqn2_2')++ (map (\((x,y),z) -> ((x+m+m'+m3 +m4+m5+m6+m7 ,y),z)) eqn1_2')

    --print $ m+m'+m3+m4 +m5 +m6 +m7 +m8

    --putStr $ unlines $ map (\((i, j), v) -> "(" ++ show i ++ "," ++ show j ++ ")" ++ "=" ++  show (numerator v) ++ "/" ++ show (denominator v) ++ "," ) fullEqn1 

    BS.writeFile "/cip/austausch/cgg/ansatz18_3Ord4.dat.gz" $ compress ansatz18_3


    