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

import Data.Ratio


main = do 

    let area = flatArea  

    let area_p = ZeroTensor 

    let area_I = ZeroTensor 

    let ans4 = generic4Ansatz 

    let ans5 = generic5Ansatz 

    let ans6 = generic6Ansatz 

    let ans8 = generic8Ansatz 

    let ans9 = generic9Ansatz 

    let ans10_1 = generic10_1Ansatz 

    let ans10_2 = generic10_2Ansatz
    
    let ans11 = generic11Ansatz
    
    let ans12_1 = generic12_1Ansatz 



    let eqn1T = eqn1Generic' ans4 ans5 ans6 area area_p area_I 

    let eqn2T = eqn2Generic' ans5 ans6 area area_p 

    let eqn3T = eqn3Generic' ans6 area 



    let eqn1AT = eqn1AGeneric' ans4 ans8 ans9 ans10_2 area area_p area_I 
    
    let eqn1AaT = eqn1AaGeneric' ans5 ans9 ans10_1 ans11 area area_p area_I 

    let eqn1AIT = eqn1AIGeneric' ans6 ans10_2 ans11 ans12_1 area area_p area_I 

    let eqn2AT = eqn2AGeneric' ans5 ans9 ans10_2 area area_p 

    let eqn2AaT = eqn2AaGeneric' ans6 ans10_1 ans11 area area_p 

    let eqn2AIT = eqn2AIGeneric' ans11 ans12_1 area area_p 

    let eqn3AT = eqn3AGeneric' ans6 ans10_2 area 

    let eqn3AaT = eqn3AaGeneric' ans11 area 

    let eqn3AIT = eqn3AIGeneric' ans12_1 area 

    let ord1 = eqn1T &> eqn2T &> (singletonTList eqn3T)

    let sym1 = toEMatrix6 ord1 

    let ord2 = eqn1AT &> eqn1AaT &> eqn1AIT &> eqn2AT &> eqn2AaT &> eqn2AIT &> eqn3AT &> eqn3AaT &> (singletonTList eqn3AIT)

    let sym2 = toEMatrix6 ord2 

    let total = toEMatrix6 $ ord1 &++ ord2 

    print $ Sol.rank Sol.JacobiSVD $ Sparse.toMatrix total 


    