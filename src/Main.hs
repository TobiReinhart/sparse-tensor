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

import qualified Data.ByteString.Lazy as BS


main = do 

    tens16_1' <- BS.readFile "/cip/austausch/cgg/7.4.tens16_1"

    tens16_2' <- BS.readFile "/cip/austausch/cgg/7.4.tens16_2"

    let tens16_1 = decodeTensor tens16_1' :: ATens 3 0 1 0 2 0 AnsVar

    let tens16_2 = decodeTensor tens16_2' :: ATens 3 0 2 0 0 0 AnsVar

    let l1 = toListShowVar6 $ ansatzAaBbCI tens16_1 

    let l2 = toListShowVar6 $ ansatzABICJ tens16_2 

    print $ l1 == [] 

    print $ l2 == [] 


    