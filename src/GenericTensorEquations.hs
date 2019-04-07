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

module GenericTensorEquations (

) where

    import TensorTreeNumeric4_2 
    import BasicTensors4_2

    eqn1Generic :: ATens 1 0 0 0 0 0 AnsVar -> ATens 1 0 0 0 1 0 AnsVar -> ATens 1 0 1 0 0 0 AnsVar -> ATens 0 1 0 0 0 0 (AreaVar Rational) -> ATens 0 1 0 0 0 1 (AreaVar Rational) -> ATens 0 1 0 1 0 0 (AreaVar Rational) -> ATens 0 0 0 0 1 1 (AreaVar AnsVar)
    eqn1Generic ans4 ans5 ans6 area area_p area_I = block1 &+ block2 &+ block3
        where 
            block1 = contrATens1 (0,0) $ contrATens1 (1,1) $ ans4 &* interArea &* area
            block2 =  contrATens1 (0,0) $ contrATens3 (0,1) $ contrATens1 (1,1) $ contrATens3 (2,2) $ ans5 &* interEqn2 &* area_p
            block3 = contrATens1 (0,0) $ contrATens2 (0,0) $ contrATens1 (1,1) $ contrATens2 (1,1) $ ans6 &* interEqn3 &* area_I

    eqn2Generic :: ATens 1 0 0 0 1 0 AnsVar -> ATens 1 0 1 0 0 0 AnsVar -> ATens 0 1 0 0 0 0 (AreaVar Rational) -> ATens 0 1 0 0 0 1 (AreaVar Rational) -> ATens 0 0 0 0 2 1 (AreaVar AnsVar)
    eqn2Generic ans5 ans6 area area_p = block1 &+ block2 
        where 
            block1 = symATens5 (0,1) $ contrATens1 (0,0) $ contrATens1 (1,1) $ ans5 &* interArea &* area
            block2 = contrATens1 (0,0) $ contrATens2 (0,0) $ contrATens1 (1,1) $ contrATens3 (0,1) $ ans6 &* interEqn4 &* area_p 

    eqn3Generic :: ATens 1 0 1 0 0 0 AnsVar -> ATens 0 1 0 0 0 0 (AreaVar Rational) -> ATens 0 0 0 0 3 1 (AreaVar AnsVar)
    eqn3Generic ans6 area = contrATens1 (0,0) $ contrATens2 (0,0) $ contrATens1 (1,1) $ ans6 &* interEqn5 &* area 

    eqn1AGeneric :: ATens 1 0 0 0 0 0 AnsVar -> ATens 2 0 0 0 0 0 AnsVar -> ATens 2 0 0 0 1 0 AnsVar -> ATens 2 0 1 0 0 0 AnsVar -> ATens 0 1 0 0 0 0 (AreaVar Rational) -> ATens 0 1 0 0 0 1 (AreaVar Rational) -> ATens 0 1 0 1 0 0 (AreaVar Rational) -> ATens 1 0 0 0 1 1 (AreaVar AnsVar)
    eqn1AGeneric ans4 ans8 ans9 ans10_2 area area_p area_I = block1 &+ block2 &+ block3 &+ block4 
        where 
            block1 = contrATens1 (0,0) $ contrATens1 (2,1) $ ans8 &* interArea &* area
            block2 = mapTo6 (ansVarToAreaVar) $ contrATens1 (0,0) $ ans4 &* interArea
            block3 = contrATens1 (0,0) $ contrATens3 (0,1) $ contrATens1 (2,1) $ contrATens3 (2,2) $ ans9 &* interEqn2 &* area_p
            block4 = contrATens1 (0,0) $ contrATens2 (0,0) $ contrATens1 (2,1) $ contrATens2 (1,1) $ ans10_2 &* interEqn3 &* area_I

    eqn1AaGeneric :: ATens 1 0 0 0 1 0 AnsVar -> ATens 2 0 0 0 1 0 AnsVar -> ATens 2 0 0 0 2 0 AnsVar -> ATens 2 0 1 0 1 0 AnsVar -> ATens 0 1 0 0 0 0 (AreaVar Rational) -> ATens 0 1 0 0 0 1 (AreaVar Rational) -> ATens 0 1 0 1 0 0 (AreaVar Rational) -> ATens 1 0 0 0 2 1 (AreaVar AnsVar)
    eqn1AaGeneric ans5 ans9 ans10_1 ans11 area area_p area_I = block1 &+ block2 &+ block3 &+ block4 
        where 
            block1 = contrATens1 (0,0) $ contrATens1 (2,1) $ ans9 &* interArea &* area
            block2 = contrATens1 (0,0) $ contrATens3 (0,1) $ contrATens1 (2,1) $ contrATens3 (3,2) $ ans10_1 &* interEqn2 &* area_p
            block3 = mapTo6 (ansVarToAreaVar) $ contrATens1 (0,0) $ contrATens3 (0,1) $ ans5 &* interEqn2
            block4 = contrATens1 (0,0) $ contrATens2 (0,0) $ contrATens1 (2,1) $ contrATens2 (1,1) $ ans11 &* interEqn3 &* area_I

    eqn1AIGeneric :: ATens 1 0 1 0 0 0 AnsVar -> ATens 2 0 1 0 0 0 AnsVar -> ATens 2 0 1 0 1 0 AnsVar -> ATens 2 0 2 0 0 0 AnsVar -> ATens 0 1 0 0 0 0 (AreaVar Rational) -> ATens 0 1 0 0 0 1 (AreaVar Rational) -> ATens 0 1 0 1 0 0 (AreaVar Rational) -> ATens 1 0 1 0 1 1 (AreaVar AnsVar)
    eqn1AIGeneric ans6 ans10_2 ans11 ans12_1 area area_p area_I = block1 &+ block2 &+ block3 &+ block4 
        where 
            block1 = contrATens1 (0,0) $ contrATens1 (2,1) $ ans10_2 &* interArea &* area
            block2 = contrATens1 (0,0) $ contrATens3 (0,1) $ contrATens1 (2,1) $ contrATens3 (2,2) $ ans11 &* interEqn2 &* area_p
            block3 = contrATens1 (0,0) $ contrATens2 (0,0) $ contrATens1 (2,1) $ contrATens2 (2,1) $ ans12_1 &* interEqn3 &* area_I
            block4 = mapTo6 (ansVarToAreaVar) $ contrATens1 (0,0) $ contrATens2 (0,0) $ ans6 &* interEqn3

    eqn2AGeneric :: ATens 1 0 0 0 1 0 AnsVar -> ATens 2 0 0 0 1 0 AnsVar -> ATens 2 0 1 0 0 0 AnsVar -> ATens 0 1 0 0 0 0 (AreaVar Rational) -> ATens 0 1 0 0 0 1 (AreaVar Rational) -> ATens 1 0 0 0 2 1 (AreaVar AnsVar)
    eqn2AGeneric ans5 ans9 ans10_2 area area_p = block1 &+ block2 &+ block3 
        where 
            block1 = symATens5 (0,1) $ contrATens1 (1,0) $ contrATens1 (2,1) $ ans9 &* interArea &* area
            block2 = mapTo6 (ansVarToAreaVar) $ symATens5 (0,1) $ contrATens1 (0,0) $ ans5 &* interArea
            block3 = contrATens1 (1,0) $ contrATens2 (0,0) $ contrATens1 (2,1) $ contrATens3 (0,1) $ ans10_2 &* interEqn4 &* area_p 

    eqn2AaGeneric :: ATens 1 0 1 0 0 0 AnsVar -> ATens 2 0 0 0 2 0 AnsVar -> ATens 2 0 1 0 1 0 AnsVar -> ATens 0 1 0 0 0 0 (AreaVar Rational) -> ATens 0 1 0 0 0 1 (AreaVar Rational) -> ATens 1 0 0 0 3 1 (AreaVar AnsVar)
    eqn2AaGeneric ans6 ans10_1 ans11 area area_p = block1 &+ block2 &+ block3 
        where 
            block1 = symATens5 (1,2) $ contrATens1 (1,0) $ contrATens1 (2,1) $ ans10_1 &* interArea &* area
            block2 = contrATens1 (1,0) $ contrATens2 (0,0) $ contrATens1 (2,1) $ contrATens3 (1,1) $ ans11 &* interEqn4 &* area_p 
            block3 = mapTo6 (ansVarToAreaVar) $ contrATens1 (0,0) $ contrATens2 (0,0) $ ans6 &* interEqn4

    eqn2AIGeneric :: ATens 2 0 1 0 1 0 AnsVar -> ATens 2 0 2 0 0 0 AnsVar -> ATens 0 1 0 0 0 0 (AreaVar Rational) -> ATens 0 1 0 0 0 1 (AreaVar Rational) -> ATens 1 0 1 0 2 1 (AreaVar AnsVar)
    eqn2AIGeneric ans11 ans12_1 area area_p = block1 &+ block2
        where 
            block1 = symATens5 (0,1) $ contrATens1 (1,0) $ contrATens1 (2,1) $ ans11 &* interArea &* area
            block2 = contrATens1 (1,0) $ contrATens2 (1,0) $ contrATens1 (2,1) $ contrATens3 (0,1) $ ans12_1 &* interEqn4 &* area_p 

    eqn3AGeneric :: ATens 1 0 1 0 0 0 AnsVar -> ATens 2 0 1 0 0 0 AnsVar -> ATens 0 1 0 0 0 0 (AreaVar Rational) -> ATens 1 0 0 0 3 1 (AreaVar AnsVar)
    eqn3AGeneric ans6 ans10_2 area = block1 &+ block2 
        where 
            block1 = contrATens1 (1,0) $ contrATens2 (0,0) $ contrATens1 (2,1) $ ans10_2 &* interEqn5 &* area 
            block2 = mapTo6 (ansVarToAreaVar) $ contrATens1 (0,0) $ contrATens2 (0,0) $ ans6 &* interEqn5 
        
    eqn3AaGeneric :: ATens 2 0 1 0 1 0 AnsVar -> ATens 0 1 0 0 0 0 (AreaVar Rational) -> ATens 1 0 0 0 4 1 (AreaVar AnsVar)
    eqn3AaGeneric ans11 area = contrATens1 (1,0) $ contrATens2 (0,0) $ contrATens1 (2,1) $ ans11 &* interEqn5 &* area
    
    eqn3AIGeneric :: ATens 2 0 2 0 0 0 AnsVar -> ATens 0 1 0 0 0 0 (AreaVar Rational) -> ATens 1 0 1 0 3 1 (AreaVar AnsVar)
    eqn3AIGeneric ans12_1 area = contrATens1 (1,0) $ contrATens2 (1,0) $ contrATens1 (2,1) $ ans12_1 &* interEqn5 &* area
    









   