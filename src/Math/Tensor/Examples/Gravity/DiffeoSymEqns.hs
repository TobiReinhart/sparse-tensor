{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver   #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

{-# OPTIONS_GHC -dcore-lint #-}

{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}

module Math.Tensor.Examples.Gravity.DiffeoSymEqns (
    ansatzA, ansatzAI, ansatzAB, ansatzAaBb, ansatzABI, ansatzAIBJ, ansatzABC, ansatzABCI, ansatzABbCc, ansatzAaBbCI, ansatzABICJ,
    eqn1, ansatzAIBJCK, ansatzABCDJ, ansatzABCcDd, eqn3, eqn3AI, eqn1A, eqn1AI, eqn2Aa, eqn3A, eqn1ABI, eqn1AaBb, eqn2ABb, eqn3AB,
    eqn1ABbCc, eqn1ABCI, eqn2ABCc, eqn3ABC, eqn1AB, eqn1ABC, ansatzABCD,
    linMass, linKin, quadKin1, quadKin2, quadKin3, quadMass, polyTensEqn, polyDensEqn,
    eqn1Met, eqn3Met, eqn1AMet, eqn1AIMet, eqn3AMet, eqn1ABMet, eqn2AaMet, eqn2ABbMet, eqn3ABMet, eqn1ABIMet, eqn1AaBbMet
   
) where

    import Math.Tensor
    import Math.Tensor.Examples.Gravity

    import qualified Data.Map.Strict as M 
    import Data.List
    import qualified Data.IntMap.Strict as I


    --the mass subgraph, i.e no derivatives 

    --order 0

    eqn1 :: ATens 0 0 0 0 0 0 (AnsVar Rational) -> ATens 1 0 0 0 0 0 (AnsVar Rational) -> ATens 0 0 0 0 1 1 (AnsVar Rational)
    eqn1 ans0 ans4 = contrATens1 (0,0) (ans4 &* flatInter) &+ (ans0 &* delta3)

    --order 1

    eqn1A :: ATens 1 0 0 0 0 0 (AnsVar Rational) -> ATens 2 0 0 0 0 0 (AnsVar Rational) -> ATens 1 0 0 0 1 1 (AnsVar Rational)
    eqn1A ans4 ans8 = block1 &+ block2 &+ block3
            where
                block1 = contrATens1 (0,0) $ ans4 &* interArea 
                block2 = contrATens1 (0,0) $ ans8 &* flatInter 
                block3 = ans4 &* delta3

    --order 2

    eqn1AB :: ATens 2 0 0 0 0 0 (AnsVar Rational) -> ATens 3 0 0 0 0 0 (AnsVar Rational) -> ATens 2 0 0 0 1 1 (AnsVar Rational) 
    eqn1AB ans8 ans12 = block1 &+ block2 &+ block3
            where
                block1 = symATens1 (0,1) $ contrATens1 (0,0) $ ans8 &* interArea 
                block2 = contrATens1 (0,0) $ ans12 &* flatInter
                block3 = ans8 &* delta3

    --order 3

    eqn1ABC :: ATens 3 0 0 0 0 0 (AnsVar Rational) -> ATens 4 0 0 0 0 0 (AnsVar Rational) -> ATens 3 0 0 0 1 1 (AnsVar Rational) 
    eqn1ABC ans12 ans16 = block1 &+ block2 &+ block3 &+ block4
            where 
                block1 = contrATens1 (0,0) $ ans12 &* interArea 
                block2 = tensorTrans1 (0,2) block1 
                block3 = tensorTrans1 (1,2) block1 
                block4 = contrATens1 (0,0) $ ans16 &* flatInter

    --the subgraph with 2 total derivative 

    --order 0

    eqn3 :: ATens 1 0 1 0 0 0 (AnsVar Rational) -> ATens 0 0 0 0 3 1 (AnsVar Rational) 
    eqn3 ans6 = contrATens2 (0,0) $ contrATens1 (0,0) $ ans6 &* contrATens1 (0,1) (interEqn5 &* flatArea)

    --order 1

    eqn1AI :: ATens 1 0 1 0 0 0 (AnsVar Rational) -> ATens 2 0 1 0 0 0 (AnsVar Rational) -> ATens 1 0 1 0 1 1 (AnsVar Rational) 
    eqn1AI ans6 ans10_2 = block1 &+ block2 &+ block3
            where 
                block1 = contrATens1 (0,0) $ ans10_2 &* flatInter 
                block2 = contrATens2 (0,0) $ contrATens1 (0,0) $ ans6 &* interEqn3
                block3 = ans6 &* delta3

    eqn2Aa :: ATens 1 0 1 0 0 0 (AnsVar Rational) -> ATens 2 0 0 0 2 0 (AnsVar Rational) -> ATens 1 0 0 0 3 1 (AnsVar Rational) 
    eqn2Aa ans6 ans10_1 = block1 &+ block2
                where 
                    block1 = symATens5 (1,2) $ contrATens1 (1,0) $ ans10_1 &* flatInter 
                    block2 = symATens5 (1,2) $ contrATens2 (0,0) $ contrATens1 (0,0) $ ans6 &* interEqn4 

    eqn3A :: ATens 1 0 1 0 0 0 (AnsVar Rational) -> ATens  2 0 1 0 0 0 (AnsVar Rational) -> ATens 1 0 0 0 3 1 (AnsVar Rational) 
    eqn3A ans6 ans10_2 = block1 &+ block2 
            where 
                block1 = contrATens1 (0,0) $ contrATens2 (0,0) $ ans6 &* interEqn5 
                block2 = contrATens2 (0,0) $ contrATens1 (1,0) $ contrATens1 (2,1) $ ans10_2 &* interEqn5 &* flatArea   

    --order 2

    eqn1ABI :: ATens 2 0 1 0 0 0 (AnsVar Rational) -> ATens 3 0 1 0 0 0 (AnsVar Rational) -> ATens 2 0 1 0 1 1 (AnsVar Rational) 
    eqn1ABI ans10_2 ans14_2 = block1 &+ block2 &+ block3 &+ block4 
            where 
                block1 = contrATens1 (0,0) $ ans14_2 &* flatInter 
                block2 = contrATens1 (1,0) $ interArea &* ans10_2
                block3 = contrATens2 (0,0) $ contrATens1 (1,0) $ ans10_2 &* interEqn3 
                block4 = ans10_2 &* delta3

    eqn3AB :: ATens 2 0 1 0 0 0 (AnsVar Rational) -> ATens 3 0 1 0 0 0 (AnsVar Rational) -> ATens 2 0 0 0 3 1 (AnsVar Rational)
    eqn3AB ans10_2 ans14_2 = block1 &+ block2
            where 
                block1 = symATens1 (0,1) $ contrATens2 (0,0) $ contrATens1 (1,0) $ ans10_2 &* interEqn5
                block2 = contrATens2 (0,0) $ contrATens1 (2,0) $ ans14_2 &* contrATens1 (0,1) (interEqn5 &* flatArea)

    eqn2ABb :: ATens 2 0 0 0 2 0 (AnsVar Rational) -> ATens 2 0 1 0 0 0 (AnsVar Rational) -> ATens 3 0 0 0 2 0 (AnsVar Rational) -> ATens 2 0 0 0 3 1 (AnsVar Rational)
    eqn2ABb ans10_1 ans10_2 ans14_1 = block1 &+ block2 &+ block3
            where 
                block1 = symATens5 (0,2) $ contrATens1 (1,0) $ ans14_1 &* flatInter
                block2' = tensorTrans5 (0,1) $ contrATens2 (0,0) $ contrATens1 (1,0) $ ans10_2 &* interEqn4 
                block2 = symATens5 (0,2) block2' 
                block3 = tensorTrans1 (0,1) $ symATens5 (0,2) $ contrATens1 (0,0) $ ans10_1 &* interArea

    eqn1AaBb :: ATens 2 0 0 0 2 0 (AnsVar Rational) -> ATens 3 0 0 0 2 0 (AnsVar Rational) -> ATens 2 0 0 0 3 1 (AnsVar Rational)
    eqn1AaBb ans10_1 ans14_1 = block1 &+ block2 &+ block3 &+ block4
            where 
                block1 = tensorTrans5 (1,2) $ contrATens1 (0,0) $ ans14_1 &* flatInter 
                block2 = contrATens1 (0,0) $ contrATens3 (0,1) $ ans10_1 &* interEqn2 
                block3 = tensorTrans1 (0,1) $ tensorTrans5 (0,2) block2
                block4 = tensorTrans5 (1,2) $ ans10_1 &* delta3

    --order 2

    
    eqn1ABbCc :: ATens 3 0 0 0 2 0 (AnsVar Rational) -> ATens 4 0 0 0 2 0 (AnsVar Rational) -> ATens 3 0 0 0 3 1 (AnsVar Rational) 
    eqn1ABbCc ans14_1 ans18_3 = block1 &+ block2 &+ block3 &+ block4
            where 
                block1 = tensorTrans5 (1,2) $ contrATens1 (0,0) $ ans18_3 &* flatInter 
                block2 = tensorTrans5 (0,1) $ contrATens1 (1,0) $ interArea &* ans14_1
                block3 = contrATens3 (0,1) $ contrATens1 (1,0) $ ans14_1 &* interEqn2 
                block4 = tensorTrans1 (1,2) $ tensorTrans5 (0,2) block3

    eqn1ABCI :: ATens 3 0 1 0 0 0 (AnsVar Rational) -> ATens 4 0 1 0 0 0 (AnsVar Rational) -> ATens 3 0 1 0 1 1 (AnsVar Rational) 
    eqn1ABCI ans14_2 ans18_2 = block1 &+ block2 &+ block3 
            where 
                block1 = contrATens1 (0,0) $ ans18_2 &* flatInter 
                block2 = symATens1 (0,1) $ contrATens1 (1,0) $ interArea &* ans14_2 
                block3 = contrATens2 (0,0) $ contrATens1 (2,0) $ ans14_2 &* interEqn3 

    eqn2ABCc :: ATens 3 0 0 0 2 0 (AnsVar Rational) -> ATens 3 0 1 0 0 0 (AnsVar Rational) -> ATens 4 0 0 0 2 0 (AnsVar Rational) -> ATens 3 0 0 0 3 1 (AnsVar Rational)
    eqn2ABCc ans14_1 ans14_2 ans18_3 = block1 &+ block2 &+ block3
            where 
                block1 = symATens5 (0,2) $ contrATens1 (2,0) $ ans18_3 &* flatInter 
                block2 = symATens5 (0,2) $ tensorTrans5 (0,1) $ contrATens2 (0,0) $ contrATens1 (2,0) $ ans14_2 &* interEqn4
                block3 = symATens1 (0,1) $ tensorTrans1 (1,2) $ symATens5 (0,2) $ contrATens1 (1,0) $ ans14_1 &* interArea 

    eqn3ABC :: ATens 3 0 1 0 0 0 (AnsVar Rational) -> ATens 4 0 1 0 0 0 (AnsVar Rational) -> ATens 3 0 0 0 3 1 (AnsVar Rational)
    eqn3ABC ans14_2 ans18_2 = block1 &+ block2 &+ block3 &+ block4 
            where 
                block1 = contrATens2 (0,0) $ contrATens1 (3,0) $ ans18_2 &* contrATens1 (0,1) (interEqn5 &* flatArea) 
                block2 = contrATens2 (0,0) $ contrATens1 (2,0) $ ans14_2 &* interEqn5
                block3 = tensorTrans1 (0,2) block2 
                block4 = tensorTrans1 (1,2) block2

    --the subgraph with a total of 4 derivatives 

    eqn3AI :: ATens 2 0 2 0 0 0 (AnsVar Rational) -> ATens 1 0 1 0 3 1 (AnsVar Rational) 
    eqn3AI ans12_1 = contrATens2 (0,0) $ contrATens1 (0,0) $ ans12_1 &* contrATens1 (0,1) (interEqn5 &* flatArea)

    
    --the ansatz integrabillity conditions (when perturbing around eta*eta-eta*eta-epsilon)

    ansatzA :: ATens 1 0 0 0 0 0 (AnsVar Rational) -> ATens 1 0 0 0 2 0 (AnsVar Rational)
    ansatzA ans4 = aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans4 &* interArea &* invEta

    ansatzAa :: ATens 1 0 0 0 1 0 (AnsVar Rational) -> ATens 1 0 0 0 3 0 (AnsVar Rational) 
    ansatzAa ans5 = aSymATens5 (0,2) $ contrATens1 (0,0) $ contrATens3 (0,0) $ contrATens3 (3,0) $ ans5 &* interEqn2 &* invEta 

    ansatzAI :: ATens 1 0 1 0 0 0 (AnsVar Rational) -> ATens 1 0 1 0 2 0 (AnsVar Rational) 
    ansatzAI ans6 = aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens2 (0,0) $ contrATens3 (1,0) $ ans6 &* interEqn3 &* invEta 

    ansatzAB :: ATens 2 0 0 0 0 0 (AnsVar Rational) -> ATens 2 0 0 0 2 0 (AnsVar Rational) 
    ansatzAB ans8 = symATens1 (0,1) $ aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans8 &* interArea &* invEta 

    ansatzAaBb :: ATens 2 0 0 0 2 0 (AnsVar Rational) -> ATens 2 0 0 0 4 0 (AnsVar Rational) 
    ansatzAaBb ans10_1 = block1 &+ block2 
        where 
            block1 = aSymATens5 (1,3) $ contrATens1 (0,0) $ contrATens3 (0,0) $ contrATens3 (4,0) $ ans10_1 &* interEqn2 &* invEta 
            block2 = tensorTrans1 (0,1) $ tensorTrans5 (0,2) block1

    ansatzABI :: ATens 2 0 1 0 0 0 (AnsVar Rational) -> ATens 2 0 1 0 2 0 (AnsVar Rational)
    ansatzABI ans10_2 = block1 &+ block2 
        where 
            block1 = aSymATens5 (0,1) $ contrATens1 (1,0) $ contrATens2 (0,0) $ contrATens3 (1,0) $ ans10_2 &* interEqn3 &* invEta 
            block2 = tensorTrans1 (0,1) $ aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans10_2 &* interArea &* invEta 

    ansatzAIBJ :: ATens 2 0 2 0 0 0 (AnsVar Rational) -> ATens 2 0 2 0 2 0 (AnsVar Rational) 
    ansatzAIBJ ans12_1 = block1 &+ block2 
        where 
            block1 = aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens2 (0,0) $ contrATens3 (1,0) $ ans12_1 &* interEqn3 &* invEta 
            block2 = tensorTrans1 (0,1) $ tensorTrans3 (0,1) block1

    ansatzABC :: ATens 3 0 0 0 0 0 (AnsVar Rational) -> ATens 3 0 0 0 2 0 (AnsVar Rational) 
    ansatzABC ans12 = block1 &+ block2 &+ block3 
        where 
            block1 = aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans12 &* interArea &* invEta 
            block2 = tensorTrans1 (1,2) block1
            block3 = tensorTrans1 (0,2) block1

    ansatzABbCc :: ATens 3 0 0 0 2 0 (AnsVar Rational) -> ATens 3 0 0 0 4 0 (AnsVar Rational) 
    ansatzABbCc ans14_1 = block1 &+ block2 &+ block3
        where 
            block1 = aSymATens5 (1,3) $ contrATens1 (1,0) $ contrATens3 (0,0) $ contrATens3 (4,0) $ ans14_1 &* interEqn2 &* invEta
            block2 = tensorTrans1 (1,2) $ tensorTrans5 (0,2) block1 
            block3 = tensorTrans5 (1,2) $ tensorTrans1 (0,1) $ tensorTrans1 (1,2) $ aSymATens5 (2,3) $ contrATens1 (0,0) $ contrATens3 (3,0) $ ans14_1 &* interArea &* invEta

    ansatzABCI :: ATens 3 0 1 0 0 0 (AnsVar Rational) -> ATens 3 0 1 0 2 0 (AnsVar Rational) 
    ansatzABCI ans14_2 = block1 &+ block2 
        where
            block1 = symATens1 (0,2) $ contrATens1 (0,0) $ ans14_2 &* aSymATens5 (0,1) (contrATens3 (1,0) $ interArea &* invEta)
            block2 = tensorTrans1 (1,2) $ contrATens2 (0,0) $ contrATens1 (2,0) $ ans14_2 &* aSymATens5 (0,1) (contrATens3 (1,0) $ interEqn3 &* invEta) 

    ansatzAaBbCI :: ATens 3 0 1 0 2 0 (AnsVar Rational) -> ATens 3 0 1 0 4 0 (AnsVar Rational) 
    ansatzAaBbCI ans16_1 = block1 &+ block2 &+ block3
        where 
            block1 = aSymATens5 (1,3) $ contrATens1 (0,0) $ contrATens3 (0,0) $ contrATens3 (4,0) $ ans16_1 &* interEqn2 &* invEta 
            block2 = tensorTrans1 (0,2) $ tensorTrans5 (0,2) block1
            block3 = tensorTrans5 (1,2) $ tensorTrans1 (1,2) $ aSymATens5 (2,3) $ contrATens2 (0,0) $ contrATens1 (2,0) $ contrATens3 (4,0) $ ans16_1 &* interEqn3 &* invEta 

    ansatzABICJ :: ATens 3 0 2 0 0 0 (AnsVar Rational) -> ATens 3 0 2 0 2 0 (AnsVar Rational) 
    ansatzABICJ ans16_2 = block1 &+ block2 &+ block3 
        where 
            block1 = aSymATens5 (0,1) $ contrATens1 (1,0) $ contrATens2 (0,0) $ contrATens3 (1,0) $ ans16_2 &* interEqn3 &* invEta 
            block2 = tensorTrans1 (1,2) $ tensorTrans3 (0,1) block1 
            block3 = tensorTrans3 (0,1) $ tensorTrans1 (0,2) $ aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans16_2 &* interArea &* invEta 

    ansatzAIBJCK :: ATens 3 0 3 0 0 0 (AnsVar Rational) -> ATens 3 0 3 0 2 0 (AnsVar Rational) 
    ansatzAIBJCK ans18 = block1 &+ block2 &+ block3
        where 
            block1 = contrATens1 (0,0) $ contrATens2 (0,0) $ ans18 &* removeZeros6 (aSymATens5 (0,1) $ contrATens3 (1,0) $ interEqn3 &* invEta) 
            block2 = tensorTrans1 (0,2) $ tensorTrans3 (0,2) block1 
            block3 = tensorTrans1 (1,2) $ tensorTrans3 (1,2) block1

    ansatzABCD :: ATens 4 0 0 0 0 0 (AnsVar Rational) -> ATens 4 0 0 0 2 0 (AnsVar Rational) 
    ansatzABCD ans16 = block1 &+ block2 &+ block3 &+ block4 
        where 
            block1 = aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans16 &* interArea &* invEta 
            block2 = tensorTrans1 (0,3) block1 
            block3 = tensorTrans1 (1,3) block1
            block4 = tensorTrans1 (2,3) block1 
        
    ansatzABCDJ :: ATens 4 0 1 0 0 0 (AnsVar Rational) -> ATens 4 0 1 0 2 0 (AnsVar Rational) 
    ansatzABCDJ ans18_2 = block1 &+ block2 &+ block3 &+ block4 
        where
            block1 = aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans18_2 &* interArea &* invEta
            block2 = tensorTrans1 (0,3) block1
            block3 = tensorTrans1 (1,3) block1 
            block4 = tensorTrans1 (2,3) $ aSymATens5 (0,1) $ contrATens2 (0,0) $ contrATens1 (3,0) $ contrATens3 (1,0) $ ans18_2 &* interEqn3 &* invEta
            
    ansatzABCcDd :: ATens 4 0 0 0 2 0 (AnsVar Rational) -> ATens 4 0 0 0 4 0 (AnsVar Rational) 
    ansatzABCcDd ans18_3 = block1 &+ block2 &+ block3 &+ block4 
        where 
            block1 = contrATens1 (0,0) $ ans18_3 &* aSymATens5 (0,1) (contrATens3 (1,0) $ interArea &* invEta)
            block2 = tensorTrans1 (0,3) block1 
            block3' = contrATens3 (0,0) $ contrATens1 (2,0) $ ans18_3 &* aSymATens5 (0,2) (contrATens3 (2,0) $ interEqn2 &* invEta)
            block3 = resortTens1 [3,0,2,1] $ resortTens5 [1,2,0,3] block3' 
            block4 = tensorTrans1 (1,2) $ tensorTrans5 (0,1) block3

    --tensor trafo equations for rom calculations (use density or scalar) 
    --ansätze must be computed from lagrangian ansätze

    --linear order 

    linMass :: ATens 2 0 0 0 0 0 (AnsVar Rational) -> ATens 1 0 0 0 1 1 (AnsVar Rational)
    linMass ans8 = tens1 
        where 
            tens1 = contrATens1 (1,0) $ ans8 &* flatInter

    linKin :: ATens 2 0 0 0 2 0 (AnsVar Rational) -> ATens 1 0 0 0 3 1 (AnsVar Rational)
    linKin ans10 = tens1 
        where 
            tens1 = cyclicSymATens5 [0,1,2] $ contrATens1 (1,0) $ ans10 &* flatInter
    
    --quadratic order

    quadMass :: ATens 3 0 0 0 0 0 (AnsVar Rational) -> ATens 2 0 0 0 0 0 (AnsVar Rational) -> ATens 2 0 0 0 1 1 (AnsVar Rational)
    quadMass ans12 ans8 = tens1 &+ tens2 &+ tens3 &+ dens
        where 
            dens = (2 &.) $ ans8 &* delta3
            tens1 = (6 &.) $ contrATens1 (1,0) $ ans12 &* flatInter
            tens2 = (2 &.) $ contrATens1 (1,0) $ ans8 &* interArea
            tens3 = (2 &.) $ tensorTrans1 (0,1) $ contrATens1 (0,0) $ ans8 &* interArea

    quadKin1 :: ATens 3 0 0 0 2 0 (AnsVar Rational) -> ATens 2 0 0 0 2 0 (AnsVar Rational) -> ATens 2 0 0 0 3 1 (AnsVar Rational)
    quadKin1 ans14 ans10 = tens1 &+ tens2 &+ tens3 &+ dens &- (tens4_1 &+ tens4_2)
        where 
            dens = ans10 &* delta3   
            tens1 = ((1/2) &.) $ symATens5 (0,1) $ contrATens1 (2,0) $ ans14 &* flatInter
            tens2 = contrATens1 (1,0) $ ans10 &* interArea 
            tens3 = tensorTrans1 (0,1) $ contrATens1 (0,0) $ ans10 &* interArea
            tens4_1 = tensorTrans5 (1,2) $ ans10 &* delta3
            tens4_2 = resortTens5 [1,2,0] $ ans10 &* delta3


    quadKin2 :: ATens 3 0 0 0 2 0 (AnsVar Rational) -> ATens 2 0 0 0 2 0 (AnsVar Rational) -> ATens 2 0 0 0 3 1 (AnsVar Rational)
    quadKin2 ans14 ans10 = symATens5 (0,2) $ tens1 &+ tens3 &+ tens4 &- (tens0 &+ tens2)
        where 
            tens0 = tensorTrans5 (1,2) $ ans10 &* delta3 
            tens1 = (2 &.) $ contrATens1 (1,0) $ ans10 &* interArea 
            tens2 = tensorTrans1 (0,1) $ contrATens1 (0,0) $ ans14 &* flatInter  
            tens3 = contrATens1 (2,0) $ ans14 &* flatInter  
            tens4 = contrATens1 (0,0) $ ans14 &* flatInter  


    quadKin3 :: ATens 3 0 0 0 2 0 (AnsVar Rational) -> ATens 2 0 0 0 2 0 (AnsVar Rational) -> ATens 2 0 0 0 3 1 (AnsVar Rational)
    quadKin3 ans14 ans10 = cyclicSymATens5 [0,1,2] $ tens1 &+ tens2 
        where 
            tens1 = contrATens1 (1,0) $ ans10 &* interArea
            tens2 = contrATens1 (1,0) $ ans14 &* flatInter


    --principal polynomial equations 

    polyAns2 :: ATens 0 0 1 0 0 0 (AnsVar Rational)
    polyAns2 = fromListT6 $ map (\(x,y) -> ((Empty, Empty, singletonInd $ Ind9 x, Empty, Empty, Empty),AnsVar $ I.singleton 1 y)) [(0,-1),(4,1),(7,1),(9,1)]

    polyTensEqn :: ATens 1 0 1 0 0 0 (AnsVar Rational) -> ATens 0 0 1 0 1 1 (AnsVar Rational)
    polyTensEqn ans6 = total
            where 
                ans6' = shiftLabels6 1 ans6 
                ans2  = polyAns2
                tens1 = contrATens1 (0,0) $ ans6' &* flatInter 
                tens2 = contrATens2 (0,0) $ ans2 &* interMetric
                total = tens1 &+ tens2

    polyDensEqn :: ATens 1 0 1 0 0 0 (AnsVar Rational) -> ATens 0 0 1 0 1 1 (AnsVar Rational)
    polyDensEqn ans6 = polyTensEqn &+ (ans2 &* delta3)
            where 
                ans2  = polyAns2
        
    --aditional equations for the metric case 

    --the mass subgraph, i.e no derivatives 

    --order 0

    eqn1Met :: ATens 0 0 0 0 0 0 (AnsVar Rational) -> ATens 0 0 1 0 0 0 (AnsVar Rational) -> ATens 0 0 0 0 1 1 (AnsVar Rational)
    eqn1Met ans0 ans2 = contrATens2 (0,0) (ans2 &* flatInterMetric) &+ (ans0 &* delta3)

    --order 1

    eqn1AMet :: ATens 0 0 1 0 0 0 (AnsVar Rational) -> ATens 0 0 2 0 0 0 (AnsVar Rational) -> ATens 0 0 1 0 1 1 (AnsVar Rational)
    eqn1AMet ans2 ans4 = block1 &+ block2 &+ block3
            where
                block1 = contrATens2 (0,0) $ ans2 &* interMetric 
                block2 = contrATens2 (0,0) $ ans4 &* flatInterMetric 
                block3 = ans2 &* delta3

    --order 2

    eqn1ABMet :: ATens 0 0 2 0 0 0 (AnsVar Rational) -> ATens 0 0 3 0 0 0 (AnsVar Rational) -> ATens 0 0 2 0 1 1 (AnsVar Rational) 
    eqn1ABMet ans4 ans6 = block1 &+ block2 &+ block3
            where
                block1 = symATens3 (0,1) $ contrATens2 (0,0) $ ans4 &* interMetric 
                block2 = contrATens2 (0,0) $ ans6 &* flatInterMetric
                block3 = ans4 &* delta3


    --the subgraph with 2 total derivative 

    --order 0

    eqn3Met :: ATens 0 0 2 0 0 0 (AnsVar Rational) -> ATens 0 0 0 0 3 1 (AnsVar Rational) 
    eqn3Met ans4 = contrATens2 (0,0) $ contrATens2 (1,0) $ ans4 &* contrATens2 (0,2) (interEqn5Metric &* etaA)

    --order 1

    eqn1AIMet :: ATens 0 0 2 0 0 0 (AnsVar Rational) -> ATens 0 0 3 0 0 0 (AnsVar Rational) -> ATens 0 0 2 0 1 1 (AnsVar Rational) 
    eqn1AIMet ans4 ans6 = block1 &+ block2 &+ block3
            where 
                block1 = contrATens2 (0,0) $ ans6 &* flatInterMetric 
                block2 = contrATens2 (0,0) $ contrATens2 (1,1) $ ans4 &* interEqn3Metric 
                block3 = ans4 &* delta3

    eqn2AaMet :: ATens 0 0 2 0 0 0 (AnsVar Rational) -> ATens 0 0 2 0 2 0 (AnsVar Rational) -> ATens 0 0 1 0 3 1 (AnsVar Rational) 
    eqn2AaMet ans4 ans6 = block1 &+ block2
                where 
                    block1 = symATens5 (1,2) $ contrATens2 (1,0) $ ans6 &* flatInterMetric 
                    block2 = symATens5 (1,2) $ contrATens2 (0,0) $ contrATens2 (1,0) $ ans4 &* interEqn4Metric 

    eqn3AMet :: ATens 0 0 2 0 0 0 (AnsVar Rational) -> ATens  0 0 3 0 0 0 (AnsVar Rational) -> ATens 0 0 1 0 3 1 (AnsVar Rational) 
    eqn3AMet ans4 ans6 = block1 &+ block2 
            where 
                block1 = contrATens2 (0,0) $ contrATens2 (1,0) $ ans4 &* interEqn5Metric 
                block2 = contrATens2 (1,0) $ contrATens2 (2,0) $ ans6 &* contrATens2 (0,2) (interEqn5Metric &* etaA)  

    --order 2

    eqn1ABIMet :: ATens 0 0 3 0 0 0 (AnsVar Rational) -> ATens 0 0 4 0 0 0 (AnsVar Rational) -> ATens 0 0 3 0 1 1 (AnsVar Rational) 
    eqn1ABIMet ans6 ans8 = block1 &+ block2 &+ block3  &+ block4
            where 
                block1 = contrATens2 (0,0) $ ans8 &* flatInterMetric 
                block2 = contrATens2 (1,0) $ interMetric &* ans6
                block3 = contrATens2 (1,0) $ contrATens2 (2,1) $ ans6 &* interEqn3Metric 
                block4 = ans6 &* delta3

    eqn3ABMet :: ATens 0 0 3 0 0 0 (AnsVar Rational) -> ATens 0 0 4 0 0 0 (AnsVar Rational) -> ATens 0 0 2 0 3 1 (AnsVar Rational)
    eqn3ABMet ans6 ans8 = block1 &+ block2
            where 
                block1 = symATens3 (0,1) $ contrATens2 (1,0) $ contrATens2 (2,0) $ ans6 &* interEqn5Metric
                block2 = contrATens2 (2,0) $ contrATens2 (3,0) $ ans8 &* contrATens2 (0,2) (interEqn5Metric &* etaA) 

    eqn2ABbMet :: ATens 0 0 2 0 2 0 (AnsVar Rational) -> ATens 0 0 3 0 0 0 (AnsVar Rational) -> ATens 0 0 3 0 2 0 (AnsVar Rational) -> ATens 0 0 2 0 3 1 (AnsVar Rational)
    eqn2ABbMet ans6_1 ans6_2 ans8 = block1 &+ block2 &+ block3
            where 
                block1 = symATens5 (0,2) $ contrATens2 (1,0) $ ans8 &* flatInterMetric
                block2' = tensorTrans5 (0,1) $ contrATens2 (1,0) $ contrATens2 (1,1) $ ans6_2 &* interEqn4Metric 
                block2 = symATens5 (0,2) block2' 
                block3 = tensorTrans3 (0,1) $ symATens5 (0,2) $ contrATens2 (0,0) $ ans6_1 &* interMetric

    eqn1AaBbMet :: ATens 0 0 2 0 2 0 (AnsVar Rational) -> ATens 0 0 3 0 2 0 (AnsVar Rational) -> ATens 0 0 2 0 3 1 (AnsVar Rational)
    eqn1AaBbMet ans6 ans8 = block1 &+ block2 &+ block3 &+ block4 
            where 
                block1 = tensorTrans5 (1,2) $ contrATens2 (0,0) $ ans8 &* flatInterMetric 
                block2 = contrATens2 (0,0) $ contrATens3 (0,1) $ ans6 &* interEqn2Metric 
                block3 = tensorTrans3 (0,1) $ tensorTrans5 (0,2) block2
                block4 = tensorTrans5 (1,2) $ ans6 &* delta3
