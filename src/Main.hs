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

    area <- randArea 

    area_p <- randAreaDerivative1 

    area_I <- randAreaDerivative2

    let eqn1Tens = eqn1Generic' generic4Ansatz generic5Ansatz generic6Ansatz area area_p area_I 

    let eqn2Tens = eqn2Generic' generic5Ansatz generic6Ansatz area area_p 

    let eqn3Tens = eqn3Generic' generic6Ansatz area 

    let eqn1ATens = eqn1AGeneric' generic4Ansatz generic8Ansatz generic9Ansatz generic10_2Ansatz area area_p area_I 

    let eqn1AaTens = eqn1AaGeneric' generic5Ansatz generic9Ansatz generic10_1Ansatz generic11Ansatz area area_p area_I 

    let eqn1AITens = eqn1AIGeneric' generic6Ansatz generic10_2Ansatz generic11Ansatz generic12_1Ansatz area area_p area_I

    let eqnList = eqn1Tens &> eqn1ATens &> eqn1AaTens &> (singletonTList eqn1AITens) 

    let eqnMat = toEMatrix6 eqnList 

    print $ Sol.rank Sol.JacobiSVD $ Sparse.toMatrix eqnMat  

    