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

    {-

    --first subgraph

   let (_,_,ans6') = mkAnsatzTensorFast 6 filterList6 symList6 areaList6IndsEta areaList6IndsEps 

   let (_,_,ans10_1') = mkAnsatzTensorFast 10 filterList10_1 symList10_1 areaList10_1IndsEta areaList10_1IndsEps 

   let (_,_,ans10_2') = mkAnsatzTensorFast 10 filterList10_2 symList10_2 areaList10_2IndsEta areaList10_2IndsEps 

   let (_,_,ans14_1') = mkAnsatzTensorFast 14 filterList14_1 symList14_1 areaList14_1IndsEta areaList14_1IndsEps 

   let (_,_,ans14_2') = mkAnsatzTensorFast 14 filterList14_2 symList14_2 areaList14_2IndsEta areaList14_2IndsEps 

   ans18_2BS <- BS.readFile "/cip/austausch/cgg/7.4.tens18_2" 

   ans18_3BS <- BS.readFile "/cip/austausch/cgg/7.4.tens18_3" 

   let ans18_2' = decodeTensor ans18_2BS :: ATens 4 0 1 0 0 0 AnsVar 

   let ans18_3' = decodeTensor ans18_3BS :: ATens 4 0 0 0 2 0 AnsVar 

   let (r6,r10_1,r10_2,r14_1,r14_2,r18_2,r18_3) = (tensorRank ans6', tensorRank ans10_1', tensorRank ans10_2', tensorRank ans14_1', tensorRank ans14_2', tensorRank ans18_2', tensorRank ans18_3')

   let ans18_3 = ans18_3' 

   let ans18_2 = shiftLabels6 r18_3 ans18_2'

   let ans14_2 = shiftLabels6 (r18_3 + r18_2) ans14_2'

   let ans14_1 = shiftLabels6 (r18_3 + r18_2 + r14_2) ans14_1'

   let ans10_2 = shiftLabels6 (r18_3 + r18_2 + r14_2 + r14_1) ans10_2' 

   let ans10_1 = shiftLabels6 (r18_3 + r18_2 + r14_2 + r14_1 + r10_2) ans10_1' 

   let ans6 = shiftLabels6 (r18_3 + r18_2 + r14_2 + r14_1 + r10_2 + r10_1) ans6'

   --theEquations

   --ord 0
   
   let eqnOrd0 = singletonTList $ eqn3 ans6 

   --ord 1

   let eqn1AIT = eqn1AI ans6 ans10_2 

   let eqn2AaT = eqn2Aa ans6 ans10_1 

   let eqn3AT = eqn3A ans6 ans10_2

   let eqnOrd1 = eqn1AIT &> eqn2AaT &> (singletonTList eqn3AT)

   --ord 2 

   let eqn1ABIT = eqn1ABI ans10_2 ans14_2

   let eqn3ABT = eqn3AB ans10_2 ans14_2 

   let eqn2ABbT = eqn2ABb ans10_1 ans10_2 ans14_1 
   
   let eqn1AaBbT = eqn1AaBb ans10_1 ans14_1 

   let eqnOrd2 = eqn1ABIT &> eqn1AaBbT &> eqn2ABbT &> (singletonTList eqn3ABT)

   --ord 3
   
   let eqn1ABbCcT = eqn1ABbCc ans14_1 ans18_3 

   let eqn1ABCIT = eqn1ABCI ans14_2 ans18_2 

   let eqn2ABCcT = eqn2ABCc ans14_1 ans14_2 ans18_3 

   let eqn3ABCT = eqn3ABC ans14_2 ans18_2

   let eqnOrd3 = eqn1ABbCcT &> eqn1ABCIT &> eqn2ABCcT &> (singletonTList eqn3ABCT)

   -------------------------------------------------------------
   
   let mat0 = toEMatrix6 eqnOrd0 

   let mat1 = toEMatrix6 (eqnOrd0 &++ eqnOrd1)

   let mat2 = toEMatrix6 (eqnOrd0 &++ eqnOrd1 &++ eqnOrd2)

   let mat3 = toEMatrix6 (eqnOrd0 &++ eqnOrd1 &++ eqnOrd2 &++ eqnOrd3)

   --the symbols 

   --ord 0
   
   let symOrd0 = singletonTList $ eqn3 ans6 

   --ord 1

   let sym1AIT = eqn1AI ZeroTensor ans10_2 

   let sym2AaT = eqn2Aa ZeroTensor ans10_1 

   let sym3AT = eqn3A ZeroTensor ans10_2

   let symOrd1 = sym1AIT &> sym2AaT &> (singletonTList sym3AT)

   --ord 2 

   let sym1ABIT = eqn1ABI ZeroTensor ans14_2

   let sym3ABT = eqn3AB ZeroTensor ans14_2 

   let sym2ABbT = eqn2ABb ZeroTensor ZeroTensor ans14_1 
   
   let sym1AaBbT = eqn1AaBb ZeroTensor ans14_1 

   let symOrd2 = sym1ABIT &> sym1AaBbT &> sym2ABbT &> (singletonTList sym3ABT)

   --ord 3
   
   let sym1ABbCcT = eqn1ABbCc ZeroTensor ans18_3 

   let sym1ABCIT = eqn1ABCI ZeroTensor ans18_2 
 
   let sym2ABCcT = eqn2ABCc ZeroTensor ZeroTensor ans18_3 
 
   let sym3ABCT = eqn3ABC ZeroTensor ans18_2
 
   let symOrd3 = sym1ABbCcT &> sym1ABCIT &> sym2ABCcT &> (singletonTList sym3ABCT)
 
   -------------------------------------------------------------

   let sym0 = toEMatrix6 symOrd0 

   let sym1 = toEMatrix6 symOrd1

   let sym2 = toEMatrix6 symOrd2

   let sym3 = toEMatrix6 symOrd3

   let (m0,s0) = (Sol.rank Sol.FullPivLU $ Sparse.toMatrix mat0, Sol.rank Sol.FullPivLU $ Sparse.toMatrix sym0)

   let (m1,s1) = (Sol.rank Sol.JacobiSVD $ Sparse.toMatrix mat1, Sol.rank Sol.JacobiSVD $ Sparse.toMatrix sym1)

   let (m2,s2) = (Sol.rank Sol.JacobiSVD $ Sparse.toMatrix mat2, Sol.rank Sol.JacobiSVD $ Sparse.toMatrix sym2)

   let (m3,s3) = (Sol.rank Sol.JacobiSVD $ Sparse.toMatrix mat3, Sol.rank Sol.JacobiSVD $ Sparse.toMatrix sym3)


   -}


   let (_,eps14,_) = mkAnsatzTensorEig 16 filterList16_2 symList16_2 areaList16_2IndsEta areaList16_2IndsEps

   print $ getForestLabelsEpsilon eps14



    