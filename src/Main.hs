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

import qualified BasicTensors as B 
import qualified Tensor as T 
import qualified Index as I
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S 
import Numeric.Natural
import GHC.TypeLits
import Data.Proxy
import GHC.TypeLits.Normalise
import Data.List

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


    --print $ toListShow8 intTest 

    let trianAreaT1 = M.mapKeys I.mkInd B.triangleMapArea :: M.Map (I.Linds_3 4) I.Uind_20  
    let trianAreaT2 = M.mapKeys I.mkInd B.triangleMapArea :: M.Map (I.Uinds_3 4) I.Lind_20 

    let trianMetricT1 = M.mapKeys I.mkInd B.triangleMap2 :: M.Map (I.Linds_3 2) I.Uind_9  
    let trianMetricT2 = M.mapKeys I.mkInd B.triangleMap2 :: M.Map (I.Uinds_3 2) I.Lind_9 

    let interT = B.interArea trianAreaT1 trianAreaT2 

    let flatInterT = B.flatInter trianAreaT1 trianAreaT2

    let interProd = T.tensorContractWith_20 (0,1) (+) $ T.tensorProductNumeric interT flatInterT

    let interProd2 = T.tensorSub interProd $ T.tensorTranspose 7 (0,1) $ T.tensorTranspose 8 (0,1) interProd

    let l1 = sort $ filter (\(x,y) -> y /= 0) $ T.toListShow2 interProd
    let l2 = sort $ filter (\(x,y) -> y /= 0) $ toListShow8 intTest

    let l3 = sort $ filter (\(x,y) -> y /= 0) $ T.toListShow2 interProd2

    let l4 = sort $ filter (\(x,y) -> y /= 0) $ toListShow8 intTest2

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

    let int3' = B.interEqn1_3 trianAreaT1 trianAreaT2 trianMetricT1 trianMetricT2 

    let int3 = T.tensorContractWith_3 (1,0) (+) $ T.tensorProductNumeric int3' B.invEta 

    let int3Sym = T.tensorAdd int3 $ T.tensorTranspose 7 (0,1) int3 
    
    let int3Prod = T.tensorContractWith_20 (0,1) (+) $ T.tensorContractWith_9 (0,1) (+) $ T.tensorProductNumeric int3Sym int3Sym

    let int3Res = T.tensorSub int3Prod $ T.tensorTranspose 7 (0,2) $ T.tensorTranspose 7 (1,3) int3Prod 

    print $ T.toListShow2 int3Res








    

