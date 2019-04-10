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

    

    ansatz18' <- BS.readFile "/cip/austausch/cgg/7.4.tens18"
    
    let ansatz18 = decodeTensor ansatz18' :: ATens 3 0 3 0 0 0 AnsVar

    --ansatz18_2' <- BS.readFile "/cip/austausch/cgg/7.4.tens18_2" 
    
    --let ansatz18_2 = decodeTensor ansatz18_2' :: ATens 4 0 1 0 0 0 AnsVar

    --ansatz18_3' <- BS.readFile "/cip/austausch/cgg/7.4.tens18_3" 
    
    --let ansatz18_3 = decodeTensor ansatz18_3' :: ATens 4 0 0 0 2 0 AnsVar

    --let ans18 = ansatzAIBJCK ansatz18 

    --let ans18_2 = ansatzABCDJ ansatz18_2 

    --let ans18_3 = ansatzABCcDd ansatz18_3 

    print $ toListT6 ansatz18 
    
    --print $ toListShowVar6 ans18_2 

    --print $ toListShowVar6 ans18_3

    