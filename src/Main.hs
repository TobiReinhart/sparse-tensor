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

import qualified Data.Eigen.Matrix as Mat 
import qualified Data.Eigen.SparseMatrix as Sparse
import qualified Data.Eigen.LA as Sol

import qualified Data.IntMap as I 


main = do 

    let (eta20,eps20,tens20) = mkAnsatzTensorFast 20 filterList20 symList20 areaList20IndsEta areaList20IndsEps

    BS.writeFile "/cip/austausch/cgg/7.4.eta20" $ encodeAnsatzForestEta eta20 
	
	BS.writeFile "/cip/austausch/cgg/7.4.eps20" $ encodeAnsatzForestEpsilon eps20 
	
	BS.writeFile "/cip/austausch/cgg/7.4.tens20" $ encodeTensor tens20 
	
	print "ansatz 20 done"