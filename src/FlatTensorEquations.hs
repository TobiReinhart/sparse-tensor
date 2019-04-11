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

module FlatTensorEquations (
    ansatzA, ansatzAI, ansatzAB, ansatzAaBb, ansatzABI, ansatzAIBJ, ansatzABC, ansatzABCI, ansatzABbCc, ansatzAaBbCI, ansatzABICJ,
    eqn1, ansatzAIBJCK, ansatzABCDJ, ansatzABCcDd, eqn3, eqn3AI, eqn1A, eqn1AI, eqn2Aa, eqn3A

) where

    import TensorTreeNumeric4_2 
    import BasicTensors4_2

    --first the flat equations 

    --the mass subgraph, i.e no derivatives 

    --order 0

    eqn1 :: ATens 1 0 0 0 0 0 AnsVar -> ATens 0 0 0 0 1 1 AnsVar
    eqn1 ans4 = contrATens1 (0,0) $ ans4 &* flatInter

    --order 1

    eqn1A :: ATens 1 0 0 0 0 0 AnsVar -> ATens 2 0 0 0 0 0 AnsVar -> ATens 1 0 0 0 1 1 AnsVar
    eqn1A ans4 ans8 = block1 &+ block2 
            where
                block1 = contrATens1 (0,0) $ ans4 &* interArea 
                block2 = contrATens1 (0,0) $ ans8 &* flatInter 

    --order 2

    eqn1AB :: ATens 2 0 0 0 0 0 AnsVar -> ATens 3 0 0 0 0 0 AnsVar -> ATens 2 0 0 0 1 1 AnsVar 
    eqn1AB ans8 ans12 = block1 &+ block2 
            where
                block1 = symATens1 (0,1) $ contrATens1 (0,0) $ ans8 &* interArea 
                block2 = contrATens1 (0,0) $ ans12 &* flatInter

    --order 3

    eqn1ABC :: ATens 3 0 0 0 0 0 AnsVar -> ATens 4 0 0 0 0 0 AnsVar -> ATens 3 0 0 0 1 1 AnsVar 
    eqn1ABC ans12 ans16 = block1 &+ block2 &+ block3 &+ block4
            where 
                block1 = contrATens1 (0,0) $ ans12 &* interArea 
                block2 = tensorTrans1 (0,2) block1 
                block3 = tensorTrans1 (1,2) block1 
                block4 = contrATens1 (0,0) $ ans16 &* flatInter

    --the subgraph with 2 total derivative 

    --order 0

    eqn3 :: ATens 1 0 1 0 0 0 AnsVar -> ATens 0 0 0 0 3 1 AnsVar 
    eqn3 ans6 = contrATens2 (0,0) $ contrATens1 (0,0) $ ans6 &* (contrATens1 (0,0) $ interEqn5 &* flatArea)

    --order 1

    eqn1AI :: ATens 1 0 1 0 0 0 AnsVar -> ATens 2 0 1 0 0 0 AnsVar -> ATens 1 0 1 0 1 1 AnsVar 
    eqn1AI ans6 ans10_2 = block1 &+ block2 
            where 
                block1 = contrATens1 (0,0) $ ans10_2 &* flatInter 
                block2 = contrATens2 (0,0) $ contrATens1 (0,0) $ ans6 &* interEqn3

    eqn2Aa :: ATens 1 0 1 0 0 0 AnsVar -> ATens 2 0 0 0 2 0 AnsVar -> ATens 1 0 0 0 3 1 AnsVar 
    eqn2Aa ans6 ans10_1 = block1 &+ block2
                where 
                    block1 = symATens6 (0,2) $ contrATens1 (0,0) $ ans10_1 &* flatInter 
                    block2' = tensorTrans5 (0,1) $ contrATens2 (0,0) $ contrATens1 (0,0) $ ans6 &* interEqn4 
                    block2 = symATens5 (0,2) block2'

    eqn3A :: ATens 1 0 1 0 0 0 AnsVar -> ATens  2 0 1 0 0 0 AnsVar -> ATens 1 0 0 0 3 1 AnsVar 
    eqn3A ans6 ans10_2 = block1 &+ block2 
            where 
                block1 = contrATens1 (0,0) $ contrATens2 (0,0) $ ans6 &* interEqn5 
                block2 = contrATens2 (0,0) $ contrATens1 (1,0) $ contrATens1 (2,1) $ ans10_2 &* interEqn5 &* flatArea   

    --order 2

    eqn1ABI :: ATens 2 0 1 0 0 0 AnsVar -> ATens 3 0 1 0 0 0 AnsVar -> ATens 2 0 1 0 1 1 AnsVar 
    eqn1ABI ans10_2 ans14_2 = block1 &+ block2 &+ block3  
            where 
                block1 = contrATens1 (0,0) $ ans14_2 &* flatInter 
                block2 = contrATens1 (1,0) $ interArea &* ans10_2
                block3 = contrATens2 (0,0) $ contrATens1 (1,0) $ ans10_2 &* interEqn3 

    eqn3AB :: ATens 2 0 1 0 0 0 AnsVar -> ATens 3 0 1 0 0 0 AnsVar -> ATens 2 0 0 0 3 1 AnsVar
    eqn3AB ans10_2 ans14_2 = block1 &+ block2
            where 
                block1 = symATens1 (0,1) $ contrATens2 (0,0) $ contrATens1 (1,0) $ ans10_2 &* interEqn5
                block2 = contrATens2 (0,0) $ contrATens1 (2,0) $ ans14_2 &* (contrATens1 (0,1) $ interEqn5 &* flatArea)

    eqn2ABb :: ATens 2 0 0 0 2 0 AnsVar -> ATens 2 0 1 0 0 0 AnsVar -> ATens 3 0 0 0 2 0 AnsVar -> ATens 2 0 0 0 3 1 AnsVar
    eqn2ABb ans10_1 ans10_2 ans14_1 = block1 &+ block2 &+ block3
            where 
                block1 = symATens5 (0,2) $ contrATens1 (1,0) $ ans14_1 &* flatInter
                block2' = tensorTrans5 (0,1) $ contrATens2 (0,0) $ contrATens1 (1,0) $ ans10_2 &* interEqn4 
                block2 = symATens5 (0,2) block2' 
                block3 = tensorTrans1 (0,1) $ symATens5 (0,2) $ contrATens1 (0,0) $ ans10_1 &* interArea

    eqn1AaBb :: ATens 2 0 0 0 2 0 AnsVar -> ATens 3 0 0 0 2 0 AnsVar -> ATens 2 0 0 0 3 1 AnsVar
    eqn1AaBb ans10_1 ans14_1 = block1 &+ block2 &+ block3 
            where 
                block1 = tensorTrans5 (1,2) $ contrATens1 (0,0) $ ans14_1 &* flatInter 
                block2 = contrATens1 (0,0) $ contrATens3 (0,1) $ ans10_1 &* interEqn2 
                block3 = tensorTrans1 (0,1) $ tensorTrans5 (0,2) block2

    --order 2

    
    eqn1ABbCc :: ATens 3 0 0 0 2 0 AnsVar -> ATens 4 0 0 0 2 0 AnsVar -> ATens 3 0 0 0 3 1 AnsVar 
    eqn1ABbCc ans14_1 ans18_3 = block1 &+ block2 &+ block3 &+ block4
            where 
                block1 = tensorTrans5 (1,2) $ contrATens1 (0,0) $ ans18_3 &* flatInter 
                block2 = tensorTrans5 (0,1) $ contrATens1 (1,0) $ interArea &* ans14_1
                block3 = contrATens3 (0,1) $ contrATens1 (1,0) $ ans14_1 &* interEqn2 
                block4 = tensorTrans1 (1,2) $ tensorTrans5 (0,2) block3

    eqn1ABCI :: ATens 3 0 1 0 0 0 AnsVar -> ATens 4 0 1 0 0 0 AnsVar -> ATens 3 0 1 0 1 1 AnsVar 
    eqn1ABCI ans14_2 ans18_2 = block1 &+ block2 &+ block3 
            where 
                block1 = contrATens1 (0,0) $ ans18_2 &* flatInter 
                block2 = symATens1 (0,1) $ contrATens1 (1,0) $ interArea &* ans14_2 
                block3 = contrATens2 (0,0) $ contrATens1 (2,0) $ ans14_2 &* interEqn3 

    eqn2ABCc :: ATens 3 0 0 0 2 0 AnsVar -> ATens 3 0 1 0 0 0 AnsVar -> ATens 4 0 0 0 2 0 AnsVar -> ATens 3 0 0 0 3 1 AnsVar
    eqn2ABCc ans14_1 ans14_2 ans18_3 = block1 &+ block2 &+ block3
            where 
                block1 = symATens5 (0,2) $ contrATens1 (2,0) $ ans18_3 &* flatInter 
                block2 = symATens5 (0,2) $ tensorTrans5 (0,1) $ contrATens2 (0,0) $ contrATens1 (2,0) $ ans14_2 &* interEqn4
                block3 = symATens1 (0,1) $ tensorTrans1 (1,2) $ symATens5 (0,2) $ contrATens1 (1,0) $ ans14_1 &* interArea 

    eqn3ABC :: ATens 3 0 1 0 0 0 AnsVar -> ATens 4 0 1 0 0 0 AnsVar -> ATens 3 0 0 0 3 1 AnsVar
    eqn3ABC ans14_2 ans18_2 = block1 &+ block2 &+ block3 &+ block4 
            where 
                block1 = contrATens2 (0,0) $ contrATens1 (3,0) $ ans18_2 &* (contrATens1 (0,1) $ interEqn5 &* flatArea) 
                block2 = contrATens2 (0,0) $ contrATens1 (2,0) $ ans14_2 &* interEqn5
                block3 = tensorTrans1 (0,2) $ block2 
                block4 = tensorTrans1 (1,2) $ block2

    --the subgraph with a total of 4 derivatives 

    eqn3AI :: ATens 2 0 2 0 0 0 AnsVar -> ATens 1 0 1 0 3 1 AnsVar 
    eqn3AI ans12_1 = contrATens2 (0,0) $ contrATens1 (0,0) $ ans12_1 &* (contrATens1 (0,1) $ interEqn5 &* flatArea)

    --the next step is writing down the eqns for generic are metric dofs 

    

    --the ansatz integrabillity conditions (when perturbing around eta*eta-eta*eta-epsilon)

    ansatzA :: ATens 1 0 0 0 0 0 AnsVar -> ATens 1 0 0 0 2 0 AnsVar
    ansatzA ans4 = aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans4 &* interArea &* invEta

    ansatzAa :: ATens 1 0 0 0 1 0 AnsVar -> ATens 1 0 0 0 3 0 AnsVar 
    ansatzAa ans5 = aSymATens5 (0,2) $ contrATens1 (0,0) $ contrATens3 (0,0) $ contrATens3 (3,0) $ ans5 &* interEqn2 &* invEta 

    ansatzAI :: ATens 1 0 1 0 0 0 AnsVar -> ATens 1 0 1 0 2 0 AnsVar 
    ansatzAI ans6 = aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens2 (0,0) $ contrATens3 (1,0) $ ans6 &* interEqn3 &* invEta 

    ansatzAB :: ATens 2 0 0 0 0 0 AnsVar -> ATens 2 0 0 0 2 0 AnsVar 
    ansatzAB ans8 = symATens1 (0,1) $ aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans8 &* interArea &* invEta 

    ansatzAaBb :: ATens 2 0 0 0 2 0 AnsVar -> ATens 2 0 0 0 4 0 AnsVar 
    ansatzAaBb ans10_1 = block1 &+ block2 
        where 
            block1 = aSymATens5 (1,3) $ contrATens1 (0,0) $ contrATens3 (0,0) $ contrATens3 (4,0) $ ans10_1 &* interEqn2 &* invEta 
            block2 = tensorTrans1 (0,1) $ tensorTrans5 (0,2) $ block1

    ansatzABI :: ATens 2 0 1 0 0 0 AnsVar -> ATens 2 0 1 0 2 0 AnsVar
    ansatzABI ans10_2 = block1 &+ block2 
        where 
            block1 = aSymATens5 (0,1) $ contrATens1 (1,0) $ contrATens2 (0,0) $ contrATens3 (1,0) $ ans10_2 &* interEqn3 &* invEta 
            block2 = tensorTrans1 (0,1) $ aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans10_2 &* interArea &* invEta 

    ansatzAIBJ :: ATens 2 0 2 0 0 0 AnsVar -> ATens 2 0 2 0 2 0 AnsVar 
    ansatzAIBJ ans12_1 = block1 &+ block2 
        where 
            block1 = aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens2 (0,0) $ contrATens3 (1,0) $ ans12_1 &* interEqn3 &* invEta 
            block2 = tensorTrans1 (0,1) $ tensorTrans3 (0,1) block1

    ansatzABC :: ATens 3 0 0 0 0 0 AnsVar -> ATens 3 0 0 0 2 0 AnsVar 
    ansatzABC ans12 = block1 &+ block2 &+ block3 
        where 
            block1 = aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans12 &* interArea &* invEta 
            block2 = tensorTrans1 (1,2) block1
            block3 = tensorTrans1 (0,2) block1

    ansatzABbCc :: ATens 3 0 0 0 2 0 AnsVar -> ATens 3 0 0 0 4 0 AnsVar 
    ansatzABbCc ans14_1 = block1 &+ block2 &+ block3
        where 
            block1 = aSymATens5 (1,3) $ contrATens1 (1,0) $ contrATens3 (0,0) $ contrATens3 (4,0) $ ans14_1 &* interEqn2 &* invEta
            block2 = tensorTrans1 (1,2) $ tensorTrans5 (0,2) block1 
            block3 = tensorTrans5 (1,2) $ tensorTrans1 (0,1) $ tensorTrans1 (1,2) $ aSymATens5 (2,3) $ contrATens1 (0,0) $ contrATens3 (3,0) $ ans14_1 &* interArea &* invEta

    ansatzABCI :: ATens 3 0 1 0 0 0 AnsVar -> ATens 3 0 1 0 2 0 AnsVar 
    ansatzABCI ans14_2 = block1 &+ block2 
        where
            block1 = symATens1 (0,2) $ contrATens1 (0,0) $ ans14_2 &* (aSymATens5 (0,1) $ contrATens3 (1,0) $ interArea &* invEta)
            block2 = tensorTrans1 (1,2) $ contrATens2 (0,0) $ contrATens1 (2,0) $ ans14_2 &* (aSymATens5 (0,1) $ contrATens3 (1,0) $ interEqn3 &* invEta) 

    ansatzAaBbCI :: ATens 3 0 1 0 2 0 AnsVar -> ATens 3 0 1 0 4 0 AnsVar 
    ansatzAaBbCI ans16_1 = block1 &+ block2 &+ block3
        where 
            block1 = aSymATens5 (1,3) $ contrATens1 (0,0) $ contrATens3 (0,0) $ contrATens3 (4,0) $ ans16_1 &* interEqn2 &* invEta 
            block2 = tensorTrans1 (0,2) $ tensorTrans5 (0,2) block1
            block3 = tensorTrans5 (1,2) $ tensorTrans1 (1,2) $ aSymATens5 (2,3) $ contrATens2 (0,0) $ contrATens1 (2,0) $ contrATens3 (4,0) $ ans16_1 &* interEqn3 &* invEta 

    ansatzABICJ :: ATens 3 0 2 0 0 0 AnsVar -> ATens 3 0 2 0 2 0 AnsVar 
    ansatzABICJ ans16_2 = block1 &+ block2 &+ block3 
        where 
            block1 = aSymATens5 (0,1) $ contrATens1 (1,0) $ contrATens2 (0,0) $ contrATens3 (1,0) $ ans16_2 &* interEqn3 &* invEta 
            block2 = tensorTrans1 (1,2) $ tensorTrans3 (0,1) block1 
            block3 = tensorTrans3 (0,1) $ tensorTrans1 (0,2) $ aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans16_2 &* interArea &* invEta 

    ansatzAIBJCK :: ATens 3 0 3 0 0 0 AnsVar -> ATens 3 0 3 0 2 0 AnsVar 
    ansatzAIBJCK ans18 = block1 &+ block2 &+ block3
        where 
            block1 = contrATens1 (0,0) $ contrATens2 (0,0) $ ans18 &* (aSymATens5 (0,1) $ contrATens3 (1,0) $ interEqn3 &* invEta) 
            block2 = tensorTrans1 (0,2) $ tensorTrans3 (0,2) block1 
            block3 = tensorTrans1 (1,2) $ tensorTrans3 (1,2) block1

    --needs to be checked
    ansatzABCD :: ATens 4 0 0 0 0 0 AnsVar -> ATens 4 0 0 0 2 0 AnsVar 
    ansatzABCD ans16 = block1 &+ block2 &+ block3 &+ block4 
        where 
            block1 = aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans16 &* interArea &* invEta 
            block2 = tensorTrans1 (0,3) block1 
            block3 = tensorTrans1 (1,3) block1
            block4 = tensorTrans1 (2,3) block1 
        
    ansatzABCDJ :: ATens 4 0 1 0 0 0 AnsVar -> ATens 4 0 1 0 2 0 AnsVar 
    ansatzABCDJ ans18_2 = block1 &+ block2 &+ block3 &+ block4 
        where
            block1 = aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans18_2 &* interArea &* invEta
            block2 = tensorTrans1 (0,3) block1
            block3 = tensorTrans1 (1,3) block1 
            block4 = tensorTrans1 (2,3) $ aSymATens5 (0,1) $ contrATens2 (0,0) $ contrATens1 (3,0) $ contrATens3 (1,0) $ ans18_2 &* interEqn3 &* invEta
            
    ansatzABCcDd :: ATens 4 0 0 0 2 0 AnsVar -> ATens 4 0 0 0 4 0 AnsVar 
    ansatzABCcDd ans18_3 = block1 &+ block2 &+ block3 &+ block4 
        where 
            block1 = contrATens1 (0,0) $ ans18_3 &* (aSymATens5 (0,1) $ contrATens3 (1,0) $ interArea &* invEta)
            block2 = tensorTrans1 (0,3) block1 
            block3' = contrATens3 (0,0) $ contrATens1 (2,0) $ ans18_3 &* (aSymATens5 (0,2) $ contrATens3 (2,0) $ interEqn2 &* invEta)
            block3 = resortTens1 [3,0,2,1] $ resortTens5 [1,2,0,3] block3' 
            block4 = tensorTrans1 (1,2) $ tensorTrans5 (0,1) block3

    
    


    




