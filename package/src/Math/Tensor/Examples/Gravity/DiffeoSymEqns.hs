-----------------------------------------------------------------------------
-- |
-- Module      :  Math.Tensor.Examples.Gravity.DiffeoSymEqns
-- Copyright   :  (c) 2019 Tobias Reinhart and Nils Alex
-- License     :  MIT
-- Maintainer  :  tobi.reinhart@fau.de, nils.alex@fau.de
--
--
-- This module collects several tensorial equations.
-- These equations arise in the research topic of perturbative constructive gravity and are used there to encode perturbative diffeomorphism invariance.
--
-- Providing further usage examples of the sparse-tensor package the equations included here nicely illustrate the syntax that is used when entering tensors.
-- They also show how the sparse tensor package can be used to manipulate not only tenors but linear tensorial equations. The sparse-tensor package can for instance be used to extract the information that is contained in such a tensorial equation in matrix form. This then obviously allows one
-- to computate the rank of the linear tensorial equation or even explicitly solve it.
--
--
-- All equations that are contained in this module are functions that take possibly several tensors of type @'ATens' 'AnsVarR'@ as input. These tensors then represent
-- the individual unknown tensors in the equation. The output that is computed by the functions is also of this type.
--
-- When illustrating how the individual such equations that are included in this module are defined we will use again the same convention as in the "Math.Tensor.Examples.Gravity" module, i.e. we label indices of type @'Ind20'@ by \(A,B,C,D,...\), indices of type
-- \(I,J,K,L,...\) and spacetime indices of type @'ind3'@ are labeled by \(a,b,c,d,...\). Hence a general such tensor is displayed as \(T^{A_1...A_m I_1...I_r a_1...a_p}_{B_1...B_n J_1...J_s b_1...b_s} \).
-- Such a tensor then has the type @'ATens' m n r s p q@.
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Math.Tensor.Examples.Gravity.DiffeoSymEqns (
-- * Lorentz Invariance
-- ** Area Metric
-- | The following equations encode the requirement that the input tensors are Lorentz invariant. They can be used to verify the Lorentz invariant tensors that are obtained by
-- the "Math.Tensor.LorentzGenerator" module. If the input tensor is Lorentz invariant the functions return a tensor with all values being zero, and thus for instance applying @'tensorRank6''@ to these
-- returns zero.
ansatzA, ansatzAI, ansatzAB, ansatzAaBb, ansatzABI, ansatzAIBJ, ansatzABC, ansatzABCI, ansatzABbCc, ansatzAaBbCI, ansatzABICJ,
ansatzAIBJCK, ansatzABCD, ansatzABCDJ, ansatzABCcDd,
-- * Perturbative Diffeomorphism Equivariance
-- ** Area Metric
-- | The following equations can be used with the Lorentz invariant ansätze for the area metric \(G_A = J_A^{abcd}G_{abcd}\) with the symmetries \(G_{abcd} = G_{cdab} = -G_{bacd} \) as input tensors.
-- In the following documentation these input tensors are labeled as \(a_0,a^{A}, a^{AI}, a^{AB},\) etc. For the definition of the further included tensors see  "Math.Tensor.Examples.Gravity".
eqn1, eqn3, eqn1A, eqn1AI,  eqn2Aa, eqn3A, eqn1AB, eqn1ABI, eqn1AaBb, eqn2ABb, eqn3AB,
-- ** Metric
-- | The following equations can be used with the Lorentz invariant ansätze for the a traditional metric metric \(g_I = J_I^{ab}g_{ab}\) as input.
-- In the following documentation these input tensors are labeled as \(a_0,a^{A}, a^{AJ}, a^{AB},\) etc. Care must be taken as for the case of the metric equations all indices that are labeled by \(A,B,C,D,... \) are also of type @'Ind9'@,
-- but are distinguished from the indices labeled by \(I,J,K,L,...\) as they describe the metric components and the latter ones describe symmetric spacetime derivative pairs.
-- For the definition of the further included tensors see  "Math.Tensor.Examples.Gravity".
eqn1Met, eqn3Met, eqn2AaMet, eqn3AMet, eqn1AMet, eqn1AIMet, eqn1ABMet, eqn1ABIMet, eqn1AaBbMet, eqn2ABbMet, eqn3ABMet
) where

import Math.Tensor
import Math.Tensor.Examples.Gravity

import qualified Data.IntMap.Strict as I

--the ansatz integrability conditions (when perturbing around eta*eta-eta*eta-epsilon)

ansatzA :: ATens 1 0 0 0 0 0 AnsVarR -> ATens 1 0 0 0 2 0 AnsVarR
ansatzA ans4 = aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans4 &* interArea &* invEtaA

ansatzAa :: ATens 1 0 0 0 1 0 AnsVarR -> ATens 1 0 0 0 3 0 AnsVarR
ansatzAa ans5 = aSymATens5 (0,2) $ contrATens1 (0,0) $ contrATens3 (0,0) $ contrATens3 (3,0) $ ans5 &* interEqn2 &* invEtaA

ansatzAI :: ATens 1 0 1 0 0 0 AnsVarR -> ATens 1 0 1 0 2 0 AnsVarR
ansatzAI ans6 = aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens2 (0,0) $ contrATens3 (1,0) $ ans6 &* interEqn3 &* invEtaA

ansatzAB :: ATens 2 0 0 0 0 0 AnsVarR -> ATens 2 0 0 0 2 0 AnsVarR
ansatzAB ans8 = symATens1 (0,1) $ aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans8 &* interArea &* invEtaA

ansatzAaBb :: ATens 2 0 0 0 2 0 AnsVarR -> ATens 2 0 0 0 4 0 AnsVarR
ansatzAaBb ans10_1 = block1 &+ block2
    where
        block1 = aSymATens5 (1,3) $ contrATens1 (0,0) $ contrATens3 (0,0) $ contrATens3 (4,0) $ ans10_1 &* interEqn2 &* invEtaA
        block2 = tensorTrans1 (0,1) $ tensorTrans5 (0,2) block1

ansatzABI :: ATens 2 0 1 0 0 0 AnsVarR -> ATens 2 0 1 0 2 0 AnsVarR
ansatzABI ans10_2 = block1 &+ block2
    where
        block1 = aSymATens5 (0,1) $ contrATens1 (1,0) $ contrATens2 (0,0) $ contrATens3 (1,0) $ ans10_2 &* interEqn3 &* invEtaA
        block2 = tensorTrans1 (0,1) $ aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans10_2 &* interArea &* invEtaA

ansatzAIBJ :: ATens 2 0 2 0 0 0 AnsVarR -> ATens 2 0 2 0 2 0 AnsVarR
ansatzAIBJ ans12_1 = block1 &+ block2
    where
        block1 = aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens2 (0,0) $ contrATens3 (1,0) $ ans12_1 &* interEqn3 &* invEtaA
        block2 = tensorTrans1 (0,1) $ tensorTrans3 (0,1) block1

ansatzABC :: ATens 3 0 0 0 0 0 AnsVarR -> ATens 3 0 0 0 2 0 AnsVarR
ansatzABC ans12 = block1 &+ block2 &+ block3
    where
        block1 = aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans12 &* interArea &* invEtaA
        block2 = tensorTrans1 (1,2) block1
        block3 = tensorTrans1 (0,2) block1

ansatzABbCc :: ATens 3 0 0 0 2 0 AnsVarR -> ATens 3 0 0 0 4 0 AnsVarR
ansatzABbCc ans14_1 = block1 &+ block2 &+ block3
    where
        block1 = aSymATens5 (1,3) $ contrATens1 (1,0) $ contrATens3 (0,0) $ contrATens3 (4,0) $ ans14_1 &* interEqn2 &* invEtaA
        block2 = tensorTrans1 (1,2) $ tensorTrans5 (0,2) block1
        block3 = tensorTrans5 (1,2) $ tensorTrans1 (0,1) $ tensorTrans1 (1,2) $ aSymATens5 (2,3) $ contrATens1 (0,0) $ contrATens3 (3,0) $ ans14_1 &* interArea &* invEtaA

ansatzABCI :: ATens 3 0 1 0 0 0 AnsVarR -> ATens 3 0 1 0 2 0 AnsVarR
ansatzABCI ans14_2 = block1 &+ block2
    where
        block1 = symATens1 (0,2) $ contrATens1 (0,0) $ ans14_2 &* aSymATens5 (0,1) (contrATens3 (1,0) $ interArea &* invEtaA)
        block2 = tensorTrans1 (1,2) $ contrATens2 (0,0) $ contrATens1 (2,0) $ ans14_2 &* aSymATens5 (0,1) (contrATens3 (1,0) $ interEqn3 &* invEtaA)

ansatzAaBbCI :: ATens 3 0 1 0 2 0 AnsVarR -> ATens 3 0 1 0 4 0 AnsVarR
ansatzAaBbCI ans16_1 = block1 &+ block2 &+ block3
    where
        block1 = aSymATens5 (1,3) $ contrATens1 (0,0) $ contrATens3 (0,0) $ contrATens3 (4,0) $ ans16_1 &* interEqn2 &* invEtaA
        block2 = tensorTrans1 (0,2) $ tensorTrans5 (0,2) block1
        block3 = tensorTrans5 (1,2) $ tensorTrans1 (1,2) $ aSymATens5 (2,3) $ contrATens2 (0,0) $ contrATens1 (2,0) $ contrATens3 (4,0) $ ans16_1 &* interEqn3 &* invEtaA

ansatzABICJ :: ATens 3 0 2 0 0 0 AnsVarR -> ATens 3 0 2 0 2 0 AnsVarR
ansatzABICJ ans16_2 = block1 &+ block2 &+ block3
    where
        block1 = aSymATens5 (0,1) $ contrATens1 (1,0) $ contrATens2 (0,0) $ contrATens3 (1,0) $ ans16_2 &* interEqn3 &* invEtaA
        block2 = tensorTrans1 (1,2) $ tensorTrans3 (0,1) block1
        block3 = tensorTrans3 (0,1) $ tensorTrans1 (0,2) $ aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans16_2 &* interArea &* invEtaA

ansatzAIBJCK :: ATens 3 0 3 0 0 0 AnsVarR -> ATens 3 0 3 0 2 0 AnsVarR
ansatzAIBJCK ans18 = block1 &+ block2 &+ block3
    where
        block1 = contrATens1 (0,0) $ contrATens2 (0,0) $ ans18 &* removeZeros6 (aSymATens5 (0,1) $ contrATens3 (1,0) $ interEqn3 &* invEtaA)
        block2 = tensorTrans1 (0,2) $ tensorTrans3 (0,2) block1
        block3 = tensorTrans1 (1,2) $ tensorTrans3 (1,2) block1

ansatzABCD :: ATens 4 0 0 0 0 0 AnsVarR -> ATens 4 0 0 0 2 0 AnsVarR
ansatzABCD ans16 = block1 &+ block2 &+ block3 &+ block4
    where
        block1 = aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans16 &* interArea &* invEtaA
        block2 = tensorTrans1 (0,3) block1
        block3 = tensorTrans1 (1,3) block1
        block4 = tensorTrans1 (2,3) block1

ansatzABCDJ :: ATens 4 0 1 0 0 0 AnsVarR -> ATens 4 0 1 0 2 0 AnsVarR
ansatzABCDJ ans18_2 = block1 &+ block2 &+ block3 &+ block4
    where
        block1 = aSymATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (1,0) $ ans18_2 &* interArea &* invEtaA
        block2 = tensorTrans1 (0,3) block1
        block3 = tensorTrans1 (1,3) block1
        block4 = tensorTrans1 (2,3) $ aSymATens5 (0,1) $ contrATens2 (0,0) $ contrATens1 (3,0) $ contrATens3 (1,0) $ ans18_2 &* interEqn3 &* invEtaA

ansatzABCcDd :: ATens 4 0 0 0 2 0 AnsVarR -> ATens 4 0 0 0 4 0 AnsVarR
ansatzABCcDd ans18_3 = block1 &+ block2 &+ block3 &+ block4
    where
        block1 = contrATens1 (0,0) $ ans18_3 &* aSymATens5 (0,1) (contrATens3 (1,0) $ interArea &* invEtaA)
        block2 = tensorTrans1 (0,3) block1
        block3' = contrATens3 (0,0) $ contrATens1 (2,0) $ ans18_3 &* aSymATens5 (0,2) (contrATens3 (2,0) $ interEqn2 &* invEtaA)
        block3 = resortTens1 [3,0,2,1] $ resortTens5 [1,2,0,3] block3'
        block4 = tensorTrans1 (1,2) $ tensorTrans5 (0,1) block3


--the mass sub graph, i.e no derivatives

--order 0

-- | The equation is given by: \(0 = a^A C_{An}^{Bm}N_B + a_0 \delta^m_n \).
eqn1 :: ATens 0 0 0 0 0 0 AnsVarR -> ATens 1 0 0 0 0 0 AnsVarR -> ATens 0 0 0 0 1 1 AnsVarR
eqn1 ans0 ans4 = contrATens1 (0,0) (ans4 &* flatInter) &+ (ans0 &* delta3A)

--order 1

-- | The equation is given by: \( 0 = a^A C_{An}^{Bm} + 2 a^{AB}C_{An}^{Cm}N_C + a^B\delta^m_n \).
eqn1A :: ATens 1 0 0 0 0 0 AnsVarR -> ATens 2 0 0 0 0 0 AnsVarR -> ATens 1 0 0 0 1 1 AnsVarR
eqn1A ans4 ans8 = block1 &+ block2 &+ block3
        where
            block1 = contrATens1 (0,0) $ ans4 &* interArea
            block2 = contrATens1 (0,0) $ ans8 &* flatInter
            block3 = ans4 &* delta3A

--order 2

-- | The equation is given by: \( 0 = 2 a^{AC}C_{An}^{Bm} + 2a^{AB}C_{An}^{Cm} + 6 a^{ABC}C_{An}^{Dm} N_D + 2a^{BC} \delta^m_n \)
eqn1AB :: ATens 2 0 0 0 0 0 AnsVarR -> ATens 3 0 0 0 0 0 AnsVarR -> ATens 2 0 0 0 1 1 AnsVarR
eqn1AB ans8 ans12 = block1 &+ block2 &+ block3
        where
            block1 = symATens1 (0,1) $ contrATens1 (0,0) $ ans8 &* interArea
            block2 = contrATens1 (0,0) $ ans12 &* flatInter
            block3 = ans8 &* delta3A

--order 3

eqn1ABC :: ATens 3 0 0 0 0 0 AnsVarR -> ATens 4 0 0 0 0 0 AnsVarR -> ATens 3 0 0 0 1 1 AnsVarR
eqn1ABC ans12 ans16 = block1 &+ block2 &+ block3 &+ block4
        where
            block1 = contrATens1 (0,0) $ ans12 &* interArea
            block2 = tensorTrans1 (0,2) block1
            block3 = tensorTrans1 (1,2) block1
            block4 = contrATens1 (0,0) $ ans16 &* flatInter

--the sub-graph with 2 total derivative

--order 0

-- | The equation is given by: \( 0 = a^{AI}C_{An}^{B(m\vert }N_B J^{\vert pq)}_I \).
eqn3 :: ATens 1 0 1 0 0 0 AnsVarR -> ATens 0 0 0 0 3 1 AnsVarR
eqn3 ans6 = contrATens2 (0,0) $ contrATens1 (0,0) $ ans6 &* contrATens1 (0,1) (interEqn5 &* flatArea)

--order 1

-- | The equation is given by: \( 0 = a^{AI}\left [C_{An}^{Bm}\delta^I _J- 2 \delta^A_B J_I^{pm}I^J_{pn} \right ] + a^{ABJ}C_{An}^{Cm}N_C + a^{BJ} \delta^m_n \).
eqn1AI :: ATens 1 0 1 0 0 0 AnsVarR -> ATens 2 0 1 0 0 0 AnsVarR -> ATens 1 0 1 0 1 1 AnsVarR
eqn1AI ans6 ans10_2 = block1 &+ block2 &+ block3
        where
            block1 = contrATens1 (0,0) $ ans10_2 &* flatInter
            block2 = contrATens2 (0,0) $ contrATens1 (0,0) $ ans6 &* interEqn3
            block3 = ans6 &* delta3A

-- | The equation is given by: \( 0 = 2a^{A(p\vert Bq}C_{An}^{C\vert m)}N_C + a^{AI} \left [C_{An}^{B(m\vert} 2 J_{I}^{\vert p)q} - \delta_A^BJ_I^{pm}\delta^q_n \right ] \).
eqn2Aa :: ATens 1 0 1 0 0 0 AnsVarR -> ATens 2 0 0 0 2 0 AnsVarR -> ATens 1 0 0 0 3 1 AnsVarR
eqn2Aa ans6 ans10_1 = block1 &+ block2
            where
                block1 = symATens5 (1,2) $ contrATens1 (1,0) $ ans10_1 &* flatInter
                block2 = symATens5 (1,2) $ contrATens2 (0,0) $ contrATens1 (0,0) $ ans6 &* interEqn4

-- | The equation is given by: \( 0 = a^{BAI}C_{An}^{C(m\vert}N_CJ_I^{\vert pq)} + a^{AI}C_{An}^{B(m \vert} J_I^{\vert pq)} \).
eqn3A :: ATens 1 0 1 0 0 0 AnsVarR -> ATens  2 0 1 0 0 0 AnsVarR -> ATens 1 0 0 0 3 1 AnsVarR
eqn3A ans6 ans10_2 = block1 &+ block2
        where
            block1 = contrATens1 (0,0) $ contrATens2 (0,0) $ ans6 &* interEqn5
            block2 = contrATens2 (0,0) $ contrATens1 (1,0) $ contrATens1 (2,1) $ ans10_2 &* interEqn5 &* flatArea

--order 2

-- | The equation is given by: \( 0 = a^{CAI} \left [C_{An}^{Bm}\delta^I _J- 2 \delta^A_B J_I^{pm}I^J_{pn} \right ] + 2 a^{ACBJ} C_{An}^{Dm} N_D + a^{CBJ} \delta ^m _n \).
eqn1ABI :: ATens 2 0 1 0 0 0 AnsVarR -> ATens 3 0 1 0 0 0 AnsVarR -> ATens 2 0 1 0 1 1 AnsVarR
eqn1ABI ans10_2 ans14_2 = block1 &+ block2 &+ block3 &+ block4
        where
            block1 = contrATens1 (0,0) $ ans14_2 &* flatInter
            block2 = contrATens1 (1,0) $ interArea &* ans10_2
            block3 = contrATens2 (0,0) $ contrATens1 (1,0) $ ans10_2 &* interEqn3
            block4 = ans10_2 &* delta3A

-- | The equation is given by: \( 0 = 2 a^{BCAI}C_{An}^{D(m \vert}N_DJ_I^{\vert pq)} + a^{CAI}C_{An}^{B(m \vert} J_I^{\vert pq)} \).
eqn3AB :: ATens 2 0 1 0 0 0 AnsVarR -> ATens 3 0 1 0 0 0 AnsVarR -> ATens 2 0 0 0 3 1 AnsVarR
eqn3AB ans10_2 ans14_2 = block1 &+ block2
        where
            block1 = symATens1 (0,1) $ contrATens2 (0,0) $ contrATens1 (1,0) $ ans10_2 &* interEqn5
            block2 = contrATens2 (0,0) $ contrATens1 (2,0) $ ans14_2 &* contrATens1 (0,1) (interEqn5 &* flatArea)

-- | The equation is given by: \( 0 = 2 a^{C A(p \vert B q} C_{An}^{D \vert m )} N_D + a^{CAI} \left [C_{An}^{B(m\vert} 2 J_{I}^{\vert p)q} - \delta_A^BJ_I^{pm}\delta^q_n \right ] \).
eqn2ABb :: ATens 2 0 0 0 2 0 AnsVarR -> ATens 2 0 1 0 0 0 AnsVarR -> ATens 3 0 0 0 2 0 AnsVarR -> ATens 2 0 0 0 3 1 AnsVarR
eqn2ABb ans10_1 ans10_2 ans14_1 = block1 &+ block2 &+ block3
        where
            block1 = symATens5 (0,2) $ contrATens1 (1,0) $ ans14_1 &* flatInter
            block2' = tensorTrans5 (0,1) $ contrATens2 (0,0) $ contrATens1 (1,0) $ ans10_2 &* interEqn4
            block2 = symATens5 (0,2) block2'
            block3 = tensorTrans1 (0,1) $ symATens5 (0,2) $ contrATens1 (0,0) $ ans10_1 &* interArea

-- | The equation is given by: \( 0 = 2 a^{BqCr} \left [ C_{An}^{Bm} \delta ^q_p - \delta^B_A \delta^m_n \right ] +2 a^{A Bq Cr} C_{An}^{Dm} N_D + 2 a^{BqCr} \delta^m_n \).
eqn1AaBb :: ATens 2 0 0 0 2 0 AnsVarR -> ATens 3 0 0 0 2 0 AnsVarR -> ATens 2 0 0 0 3 1 AnsVarR
eqn1AaBb ans10_1 ans14_1 = block1 &+ block2 &+ block3 &+ block4
        where
            block1 = tensorTrans5 (1,2) $ contrATens1 (0,0) $ ans14_1 &* flatInter
            block2 = contrATens1 (0,0) $ contrATens3 (0,1) $ ans10_1 &* interEqn2
            block3 = tensorTrans1 (0,1) $ tensorTrans5 (0,2) block2
            block4 = tensorTrans5 (1,2) $ ans10_1 &* delta3A

--order 2: further equations of the next order


eqn1ABbCc :: ATens 3 0 0 0 2 0 AnsVarR -> ATens 4 0 0 0 2 0 AnsVarR -> ATens 3 0 0 0 3 1 AnsVarR
eqn1ABbCc ans14_1 ans18_3 = block1 &+ block2 &+ block3 &+ block4
        where
            block1 = tensorTrans5 (1,2) $ contrATens1 (0,0) $ ans18_3 &* flatInter
            block2 = tensorTrans5 (0,1) $ contrATens1 (1,0) $ interArea &* ans14_1
            block3 = contrATens3 (0,1) $ contrATens1 (1,0) $ ans14_1 &* interEqn2
            block4 = tensorTrans1 (1,2) $ tensorTrans5 (0,2) block3

eqn1ABCI :: ATens 3 0 1 0 0 0 AnsVarR -> ATens 4 0 1 0 0 0 AnsVarR -> ATens 3 0 1 0 1 1 AnsVarR
eqn1ABCI ans14_2 ans18_2 = block1 &+ block2 &+ block3
        where
            block1 = contrATens1 (0,0) $ ans18_2 &* flatInter
            block2 = symATens1 (0,1) $ contrATens1 (1,0) $ interArea &* ans14_2
            block3 = contrATens2 (0,0) $ contrATens1 (2,0) $ ans14_2 &* interEqn3

eqn2ABCc :: ATens 3 0 0 0 2 0 AnsVarR -> ATens 3 0 1 0 0 0 AnsVarR -> ATens 4 0 0 0 2 0 AnsVarR -> ATens 3 0 0 0 3 1 AnsVarR
eqn2ABCc ans14_1 ans14_2 ans18_3 = block1 &+ block2 &+ block3
        where
            block1 = symATens5 (0,2) $ contrATens1 (2,0) $ ans18_3 &* flatInter
            block2 = symATens5 (0,2) $ tensorTrans5 (0,1) $ contrATens2 (0,0) $ contrATens1 (2,0) $ ans14_2 &* interEqn4
            block3 = symATens1 (0,1) $ tensorTrans1 (1,2) $ symATens5 (0,2) $ contrATens1 (1,0) $ ans14_1 &* interArea

eqn3ABC :: ATens 3 0 1 0 0 0 AnsVarR -> ATens 4 0 1 0 0 0 AnsVarR -> ATens 3 0 0 0 3 1 AnsVarR
eqn3ABC ans14_2 ans18_2 = block1 &+ block2 &+ block3 &+ block4
        where
            block1 = contrATens2 (0,0) $ contrATens1 (3,0) $ ans18_2 &* contrATens1 (0,1) (interEqn5 &* flatArea)
            block2 = contrATens2 (0,0) $ contrATens1 (2,0) $ ans14_2 &* interEqn5
            block3 = tensorTrans1 (0,2) block2
            block4 = tensorTrans1 (1,2) block2

--the sub graph with a total of 4 derivatives: further equations that contain more derivatives

eqn3AI :: ATens 2 0 2 0 0 0 AnsVarR -> ATens 1 0 1 0 3 1 AnsVarR
eqn3AI ans12_1 = contrATens2 (0,0) $ contrATens1 (0,0) $ ans12_1 &* contrATens1 (0,1) (interEqn5 &* flatArea)


--tensor trafo equations for rom calculations (use density or scalar)
--ansätze must be computed from lagrangian ansätze

--linear order

linMass :: ATens 2 0 0 0 0 0 AnsVarR -> ATens 1 0 0 0 1 1 AnsVarR
linMass ans8 = tens1
    where
        tens1 = contrATens1 (1,0) $ ans8 &* flatInter

linKin :: ATens 2 0 0 0 2 0 AnsVarR -> ATens 1 0 0 0 3 1 AnsVarR
linKin ans10 = tens1
    where
        tens1 = cyclicSymATens5 [0,1,2] $ contrATens1 (1,0) $ ans10 &* flatInter

--quadratic order

quadMass :: ATens 3 0 0 0 0 0 AnsVarR -> ATens 2 0 0 0 0 0 AnsVarR -> ATens 2 0 0 0 1 1 AnsVarR
quadMass ans12 ans8 = tens1 &+ tens2 &+ tens3 &+ dens
    where
        dens = (SField (2 :: Rational) &.) $ ans8 &* delta3A
        tens1 = (SField (6 :: Rational) &.) $ contrATens1 (1,0) $ ans12 &* flatInter
        tens2 = (SField (2 :: Rational) &.) $ contrATens1 (1,0) $ ans8 &* interArea
        tens3 = (SField (2 :: Rational) &.) $ tensorTrans1 (0,1) $ contrATens1 (0,0) $ ans8 &* interArea

quadKin1 :: ATens 3 0 0 0 2 0 AnsVarR -> ATens 2 0 0 0 2 0 AnsVarR -> ATens 2 0 0 0 3 1 AnsVarR
quadKin1 ans14 ans10 = tens1 &+ tens2 &+ tens3 &+ dens &- (tens4_1 &+ tens4_2)
    where
        dens = ans10 &* delta3A
        tens1 = (SField (1/2 :: Rational) &.) $ symATens5 (0,1) $ contrATens1 (2,0) $ ans14 &* flatInter
        tens2 = contrATens1 (1,0) $ ans10 &* interArea
        tens3 = tensorTrans1 (0,1) $ contrATens1 (0,0) $ ans10 &* interArea
        tens4_1 = tensorTrans5 (1,2) $ ans10 &* delta3A
        tens4_2 = resortTens5 [1,2,0] $ ans10 &* delta3A


quadKin2 :: ATens 3 0 0 0 2 0 AnsVarR -> ATens 2 0 0 0 2 0 AnsVarR -> ATens 2 0 0 0 3 1 AnsVarR
quadKin2 ans14 ans10 = symATens5 (0,2) $ tens1 &+ tens3 &+ tens4 &- (tens0 &+ tens2)
    where
        tens0 = tensorTrans5 (1,2) $ ans10 &* delta3A
        tens1 = (SField (2 :: Rational) &.) $ contrATens1 (1,0) $ ans10 &* interArea
        tens2 = tensorTrans1 (0,1) $ contrATens1 (0,0) $ ans14 &* flatInter
        tens3 = contrATens1 (2,0) $ ans14 &* flatInter
        tens4 = contrATens1 (0,0) $ ans14 &* flatInter


quadKin3 :: ATens 3 0 0 0 2 0 AnsVarR -> ATens 2 0 0 0 2 0 AnsVarR -> ATens 2 0 0 0 3 1 AnsVarR
quadKin3 ans14 ans10 = cyclicSymATens5 [0,1,2] $ tens1 &+ tens2
    where
        tens1 = contrATens1 (1,0) $ ans10 &* interArea
        tens2 = contrATens1 (1,0) $ ans14 &* flatInter


--principal polynomial equations

polyAns2 :: ATens 0 0 1 0 0 0 AnsVarR
polyAns2 = fromListT6' $ map (\(x,y) -> (([], [], [Ind9 x], [], [], []),AnsVar $ I.singleton 1 y)) [(0,-1),(4,1),(7,1),(9,1)]

polyTensEqn :: ATens 1 0 1 0 0 0 AnsVarR -> ATens 0 0 1 0 1 1 AnsVarR
polyTensEqn ans6 = total
        where
            ans6' = shiftLabels6 1 ans6
            ans2  = polyAns2
            tens1 = contrATens1 (0,0) $ ans6' &* flatInter
            tens2 = contrATens2 (0,0) $ ans2 &* interMetric
            total = tens1 &+ tens2

polyDensEqn :: ATens 1 0 1 0 0 0 AnsVarR -> ATens 0 0 1 0 1 1 AnsVarR
polyDensEqn ans6 = polyTensEqn ans6 &+ (ans2 &* delta3A)
        where
            ans2  = polyAns2

--additional equations for the metric case

--the mass sub graph, i.e no derivatives

--order 0

-- | The equation is given by: \(0 = a^A K_{An}^{Bm}\eta_B + a_0 \delta^m_n \).
eqn1Met :: ATens 0 0 0 0 0 0 AnsVarR -> ATens 0 0 1 0 0 0 AnsVarR -> ATens 0 0 0 0 1 1 AnsVarR
eqn1Met ans0 ans2 = contrATens2 (0,0) (ans2 &* flatInterMetric) &+ (ans0 &* delta3A)

--order 1

-- | The equation is given by: \( 0 = a^A K_{An}^{Bm} + 2 a^{AB}K_{An}^{Cm}\eta_C + a^B\delta^m_n \).
eqn1AMet :: ATens 0 0 1 0 0 0 AnsVarR -> ATens 0 0 2 0 0 0 AnsVarR -> ATens 0 0 1 0 1 1 AnsVarR
eqn1AMet ans2 ans4 = block1 &+ block2 &+ block3
        where
            block1 = contrATens2 (0,0) $ ans2 &* interMetric
            block2 = contrATens2 (0,0) $ ans4 &* flatInterMetric
            block3 = ans2 &* delta3A

--order 2

-- | The equation is given by: \( 0 = 2 a^{AC}K_{An}^{Bm} + 2a^{AB}K_{An}^{Cm} + 6 a^{ABC}K_{An}^{Dm} \eta_D + 2a^{BC} \delta^m_n \)
eqn1ABMet :: ATens 0 0 2 0 0 0 AnsVarR -> ATens 0 0 3 0 0 0 AnsVarR -> ATens 0 0 2 0 1 1 AnsVarR
eqn1ABMet ans4 ans6 = block1 &+ block2 &+ block3
        where
            block1 = symATens3 (0,1) $ contrATens2 (0,0) $ ans4 &* interMetric
            block2 = contrATens2 (0,0) $ ans6 &* flatInterMetric
            block3 = ans4 &* delta3A


--the sub graph with 2 total derivative

--order 0

-- | The equation is given by: \( 0 = a^{AI}K_{An}^{B(m\vert }\eta_B J^{\vert pq)}_I \).
eqn3Met :: ATens 0 0 2 0 0 0 AnsVarR -> ATens 0 0 0 0 3 1 AnsVarR
eqn3Met ans4 = contrATens2 (0,0) $ contrATens2 (1,0) $ ans4 &* contrATens2 (0,2) (interEqn5Metric &* etaAbs)

--order 1

-- | The equation is given by: \( 0 = a^{AI}\left [K_{An}^{Bm}\delta^I _J- 2 \delta^A_B J_I^{pm}I^J_{pn} \right ] + a^{ABJ}K_{An}^{Cm}\eta_C + a^{BJ} \delta^m_n \).
eqn1AIMet :: ATens 0 0 2 0 0 0 AnsVarR -> ATens 0 0 3 0 0 0 AnsVarR -> ATens 0 0 2 0 1 1 AnsVarR
eqn1AIMet ans4 ans6 = block1 &+ block2 &+ block3
        where
            block1 = contrATens2 (0,0) $ ans6 &* flatInterMetric
            block2 = contrATens2 (0,0) $ contrATens2 (1,1) $ ans4 &* interEqn3Metric
            block3 = ans4 &* delta3A

-- | The equation is given by: \( 0 = 2a^{A(p\vert Bq}K_{An}^{C\vert m)}\eta_C + a^{AI} \left [K_{An}^{B(m\vert} 2 J_{I}^{\vert p)q} - \delta_A^BJ_I^{pm}\delta^q_n \right ] \).
eqn2AaMet :: ATens 0 0 2 0 0 0 AnsVarR -> ATens 0 0 2 0 2 0 AnsVarR -> ATens 0 0 1 0 3 1 AnsVarR
eqn2AaMet ans4 ans6 = block1 &+ block2
            where
                block1 = symATens5 (1,2) $ contrATens2 (1,0) $ ans6 &* flatInterMetric
                block2 = symATens5 (1,2) $ contrATens2 (0,0) $ contrATens2 (1,0) $ ans4 &* interEqn4Metric

-- | The equation is given by: \( 0 = a^{BAI}K_{An}^{C(m\vert}\eta_CJ_I^{\vert pq)} + a^{AI}K_{An}^{B(m \vert} J_I^{\vert pq)} \).
eqn3AMet :: ATens 0 0 2 0 0 0 AnsVarR -> ATens  0 0 3 0 0 0 AnsVarR -> ATens 0 0 1 0 3 1 AnsVarR
eqn3AMet ans4 ans6 = block1 &+ block2
        where
            block1 = contrATens2 (0,0) $ contrATens2 (1,0) $ ans4 &* interEqn5Metric
            block2 = contrATens2 (1,0) $ contrATens2 (2,0) $ ans6 &* contrATens2 (0,2) (interEqn5Metric &* etaAbs)

--order 2

-- | The equation is given by: \( 0 = a^{CAI} \left [K_{An}^{Bm}\delta^I _J- 2 \delta^A_B J_I^{pm}I^J_{pn} \right ] + 2 a^{ACBJ} K_{An}^{Dm} \eta_D + a^{CBJ} \delta ^m _n \).
eqn1ABIMet :: ATens 0 0 3 0 0 0 AnsVarR -> ATens 0 0 4 0 0 0 AnsVarR -> ATens 0 0 3 0 1 1 AnsVarR
eqn1ABIMet ans6 ans8 = block1 &+ block2 &+ block3  &+ block4
        where
            block1 = contrATens2 (0,0) $ ans8 &* flatInterMetric
            block2 = contrATens2 (1,0) $ interMetric &* ans6
            block3 = contrATens2 (1,0) $ contrATens2 (2,1) $ ans6 &* interEqn3Metric
            block4 = ans6 &* delta3A

-- | The equation is given by: \( 0 = 2 a^{BCAI}K_{An}^{D(m \vert}\eta_DJ_I^{\vert pq)} + a^{CAI}K_{An}^{B(m \vert} J_I^{\vert pq)} \).
eqn3ABMet :: ATens 0 0 3 0 0 0 AnsVarR -> ATens 0 0 4 0 0 0 AnsVarR -> ATens 0 0 2 0 3 1 AnsVarR
eqn3ABMet ans6 ans8 = block1 &+ block2
        where
            block1 = symATens3 (0,1) $ contrATens2 (1,0) $ contrATens2 (2,0) $ ans6 &* interEqn5Metric
            block2 = contrATens2 (2,0) $ contrATens2 (3,0) $ ans8 &* contrATens2 (0,2) (interEqn5Metric &* etaAbs)

-- | The equation is given by: \( 0 = 2 a^{C A(p \vert B q} K_{An}^{D \vert m )} \eta_D + a^{CAI} \left [K_{An}^{B(m\vert} 2 J_{I}^{\vert p)q} - \delta_A^BJ_I^{pm}\delta^q_n \right ] \).
eqn2ABbMet :: ATens 0 0 2 0 2 0 AnsVarR -> ATens 0 0 3 0 0 0 AnsVarR -> ATens 0 0 3 0 2 0 AnsVarR -> ATens 0 0 2 0 3 1 AnsVarR
eqn2ABbMet ans6_1 ans6_2 ans8 = block1 &+ block2 &+ block3
        where
            block1 = symATens5 (0,2) $ contrATens2 (1,0) $ ans8 &* flatInterMetric
            block2' = tensorTrans5 (0,1) $ contrATens2 (1,0) $ contrATens2 (1,1) $ ans6_2 &* interEqn4Metric
            block2 = symATens5 (0,2) block2'
            block3 = tensorTrans3 (0,1) $ symATens5 (0,2) $ contrATens2 (0,0) $ ans6_1 &* interMetric

-- | The equation is given by: \( 0 = 2 a^{BqCr} \left [ K_{An}^{Bm} \delta ^q_p - \delta^B_A \delta^m_n \right ] +2 a^{A Bq Cr} K_{An}^{Dm} \eta_D + 2 a^{BqCr} \delta^m_n \).
eqn1AaBbMet :: ATens 0 0 2 0 2 0 AnsVarR -> ATens 0 0 3 0 2 0 AnsVarR -> ATens 0 0 2 0 3 1 AnsVarR
eqn1AaBbMet ans6 ans8 = block1 &+ block2 &+ block3 &+ block4
        where
            block1 = tensorTrans5 (1,2) $ contrATens2 (0,0) $ ans8 &* flatInterMetric
            block2 = contrATens2 (0,0) $ contrATens3 (0,1) $ ans6 &* interEqn2Metric
            block3 = tensorTrans3 (0,1) $ tensorTrans5 (0,2) block2
            block4 = tensorTrans5 (1,2) $ ans6 &* delta3A
