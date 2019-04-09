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
import Data.List

import qualified Data.ByteString.Lazy as BS

import qualified Data.Eigen.Matrix as Mat 
import qualified Data.Eigen.SparseMatrix as Sparse
import qualified Data.Eigen.LA as Sol

import qualified Data.IntMap as I 


main = do 

    --try the improved fast version
    
    let (eta18,eps18,tens18) = mkAnsatzTensorFast 18 filterList18 symList18 areaList18IndsEta areaList18IndsEps

    let (eta18_2,eps18_2,tens18_2) = mkAnsatzTensorFast 18 filterList18_2 symList18_2 areaList18_2IndsEta areaList18_2IndsEps

    let (eta18_3,eps18_3,tens18_3) = mkAnsatzTensorFast 18 filterList18_3 symList18_3 areaList18_3IndsEta areaList18_3IndsEps

    let (eta20,eps20,tens20) = mkAnsatzTensorFast 20 filterList20 symList20 areaList20IndsEta areaList20IndsEps

    BS.writeFile "/cip/austausch/cgg/7.4.eta18" $ encodeAnsatzForestEta eta18 

    BS.writeFile "/cip/austausch/cgg/7.4.eps18" $ encodeAnsatzForestEpsilon eps18 

    BS.writeFile "/cip/austausch/cgg/7.4.tens18" $ encodeTensor tens18 

    print "ansatz 18 done"


    BS.writeFile "/cip/austausch/cgg/7.4.eta18_2" $ encodeAnsatzForestEta eta18_2 

    BS.writeFile "/cip/austausch/cgg/7.4.eps18_2" $ encodeAnsatzForestEpsilon eps18_2 

    BS.writeFile "/cip/austausch/cgg/7.4.tens18_2" $ encodeTensor tens18_2 

    print "ansatz 18_2 done"


    BS.writeFile "/cip/austausch/cgg/7.4.eta18_3" $ encodeAnsatzForestEta eta18_3 

    BS.writeFile "/cip/austausch/cgg/7.4.eps18_3" $ encodeAnsatzForestEpsilon eps18_3 

    BS.writeFile "/cip/austausch/cgg/7.4.tens18_3" $ encodeTensor tens18_3 

    print "ansatz 18_3 done"


    BS.writeFile "/cip/austausch/cgg/7.4.eta20" $ encodeAnsatzForestEta eta20 

    BS.writeFile "/cip/austausch/cgg/7.4.eps20" $ encodeAnsatzForestEpsilon eps20 

    BS.writeFile "/cip/austausch/cgg/7.4.tens20" $ encodeTensor tens20 

    print "ansatz 20 done"







