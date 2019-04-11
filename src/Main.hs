{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver   #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -dcore-lint #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}


module Main (
 main
) where

import PerturbationTree2_3
import TensorTreeNumeric4_2
import FlatTensorEquations
import BasicTensors4_2
import GenericTensorEquations

import Data.List

import qualified Data.ByteString.Lazy as BS
import Codec.Compression.GZip
import Data.Serialize


import qualified Data.Eigen.Matrix as Mat 
import qualified Data.Eigen.SparseMatrix as Sparse
import qualified Data.Eigen.LA as Sol
import Data.Either

import qualified Data.IntMap as I 


main = do 

    

    --ansatz18' <- BS.readFile "/cip/austausch/cgg/7.4.tens18"
    
    --let ansatz18 = decodeTensor ansatz18' :: ATens 3 0 3 0 0 0 AnsVar

    --ansatz18_2' <- BS.readFile "/cip/austausch/cgg/7.4.tens18_2" 
    
    --let ansatz18_2 = decodeTensor ansatz18_2' :: ATens 4 0 1 0 0 0 AnsVar

    --ansatz18_3' <- BS.readFile "/cip/austausch/cgg/7.4.tens18_3" 
    
    --let ansatz18_3 = decodeTensor ansatz18_3' :: ATens 4 0 0 0 2 0 AnsVar

    --let ans18 = ansatzAIBJCK ansatz18 

    --let ans18_2 = ansatzABCDJ ansatz18_2 

    --let ans18_3 = ansatzABCcDd ansatz18_3 

    --print $ toListT6 ans18 
    
    --print $ toListShowVar6 ans18_2 

    --print $ toListShowVar6 ans18_3

    

    let (_,_,ans4') = mkAnsatzTensorFast 4 filterList4 symList4 areaList4IndsEta areaList4IndsEps 

    let (_,_,ans6') = mkAnsatzTensorFast 6 filterList6 symList6 areaList6IndsEta areaList6IndsEps 

    let (_,_,ans8') = mkAnsatzTensorFast 8 filterList8 symList8 areaList8IndsEta areaList8IndsEps

    let (_,_,ans10_1') = mkAnsatzTensorFast 10 filterList10_1 symList10_1 areaList10_1IndsEta areaList10_1IndsEps

    let (_,_,ans10_2') = mkAnsatzTensorFast 10 filterList10_2 symList10_2 areaList10_2IndsEta areaList10_2IndsEps

    let (_,_,ans12_1') = mkAnsatzTensorFast 12 filterList12_1 symList12_1 areaList12_1IndsEta areaList12_1IndsEps 

    let (r1,r2,r3,r4,r5,r6) = (tensorRank ans4', tensorRank ans6', tensorRank ans8', tensorRank ans10_1', tensorRank ans10_2', tensorRank ans12_1')

    let ans12_1 = ans12_1' 

    let ans10_2 = shiftLabels6 r6 ans10_2' 

    let ans10_1 = shiftLabels6 (r6+r5) ans10_1 

    let ans8 = shiftLabels6 (r6+r5+r4) ans8 

    let ans6 = shiftLabels6 (r6+r5+r4+r3) ans6' 

    let ans4 = shiftLabels6 (r6+r5+r4+r3+r2) ans4 

    let eqn1T = eqn1 ans4 

    let eqn3T = eqn3 ans6 

    let eqn1AT = eqn1A ZeroTensor ans8 

    let eqn1AIT = eqn1AI ZeroTensor ans10_2 

    let eqn2AaT = eqn2Aa ZeroTensor ans10_1 

    let eqn3AT = eqn3A ans6 ans10_2

    let eqn3AIT = eqn3AI ans12_1

    let ord1 = eqn1T &> (singletonTList eqn3T)

    let ord2 = eqn1AT &> eqn1AIT &> eqn2AaT &> eqn3AT &> (singletonTList eqn3AIT)
    
    let sym1 = toEMatrix6 ord1 

    let sym2 = toEMatrix6 ord2 

    print $ (r1,r2,r3,r4,r5,r6)


    