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

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver   #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -dcore-lint #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}

module SymbolicTensor where

import TensorTreeNumeric4

import GHC.TypeLits
import Data.Kind (Type)

data Label = Label

type Labels n = IndList n Label

type SymbolicTensor8 n1 n2 n3 n4 n5 n6 n7 n8 a = (Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 a, Labels (n1+n2+n3+n4+n5+n6+n7+n8))
