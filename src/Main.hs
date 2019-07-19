{-# LANGUAGE DataKinds #-}

module Main (
 main
) where

import PerturbationTree2_3
import TensorTreeNumeric4_2
import FlatTensorEquations
import GenericTensorEquations
import BasicTensors4_2

import System.IO

import qualified Data.ByteString.Lazy as BS
import Codec.Compression.GZip
import Data.Serialize

import Data.Maybe
import Data.List


import qualified Data.Eigen.Matrix as Mat 
import qualified Data.Eigen.SparseMatrix as Sparse
import qualified Data.Eigen.LA as Sol
import Data.Either

import qualified Data.IntMap as I 

import Data.Ratio


main = do 

    --mass term ansätze
    
    let (_,_,ans8') = mkAnsatzTensorEig 8 filterList8 symList8 areaList8IndsEta areaList8IndsEps 

    let (_,_,ans12') = mkAnsatzTensorFast 12 filterList12 symList12 areaList12IndsEta areaList12IndsEps 

    --kinetic term ansätze 

    let (_,_,ans10') = mkAnsatzTensorFast 10 filterList10Rom symList10Rom areaList10IndsEtaRom areaList10IndsEpsRom 

    let (_,_,ans14') = mkAnsatzTensorFast 14 filterList14Rom symList14Rom areaList14IndsEtaRom areaList14IndsEpsRom
    
    let r8 = tensorRank' ans8' 

    let r10 = tensorRank' ans10'

    let r12 = tensorRank' ans12'

    let r14 = tensorRank' ans14'
    
    let ans14 = ans14' 

    let ans12 = ans12' 

    let ans10 = shiftLabels6 r14 ans10'

    let ans8 = shiftLabels6 r12 ans8' 

    let linMassEqn = linMass ans8

    let linKinEqn = linKin ans10 

    let quadMassEqn = quadMass ans12 ans8 
    
    let quadKinEqn1 = quadKin1 ans14 ans10 
    
    let quadKinEqn2 = quadKin2 ans14 ans10

    let quadKinEqn3 = quadKin3 ans14 ans10 

    let totalLinMass = singletonTList linMassEqn 

    let totalQuadMass = linMassEqn &> (singletonTList quadMassEqn)

    let totalLinKin = singletonTList linKinEqn 

    let totalQuadKin = linKinEqn &> quadKinEqn1 &> quadKinEqn2 &> (singletonTList quadKinEqn3)
    
    let matLinKin = toMatList6 totalLinKin
    
    let matQuadKin = toMatList6 totalQuadKin

    let mkMatLin l = "LinSym := Matrix(21,21,{" ++ (init $ init $ unlines $ map (\(a,b,c) -> show (a,b) ++ " = " ++ c ++ ",") l) ++ "});" ++ "\n" 

    let mkMatQuad l = "Matrix(21,21,{" ++ (init $ init $ unlines $ map (\(a,b,c) -> show (a,b) ++ " = " ++ c ++ ",") l) ++ "})," ++ "\n" 

    let linSym = mkMatLin $ linSymbol ans10 

    let showFrac x = if denominator x == 1 then show (numerator x) else  "(" ++ show (numerator x) ++ "/" ++ show (denominator x) ++ ")"

    let quadSym = "QuadSymList := [" ++ (init $ init $ init $ unlines $ map mkMatQuad $ quadSymbol ans14) ++ "];" ++ "\n" 

    let mkMatLinKin l = "LinKin := Matrix (" ++ "8,121,{" ++ (init $ init $ unlines $ map (\((a,b),c) -> show (a,b) ++ " = " ++ showFrac c ++ ",") l) ++ "});" ++ "\n"
    
    let linKinList = mkMatLinKin matLinKin 

    let mkMatQuadKin l = "QuadKin := Matrix (" ++ "1174,121,{" ++ (init $ init $ unlines $ map (\((a,b),c) -> show (a,b) ++ " = " ++ showFrac c ++ ",") l) ++ "});" ++ "\n"
    
    let quadKinList = mkMatQuadKin matQuadKin 
    
    {-

    putStr linKinList 

    putStr "\n"

    putStr quadKinList

    putStr "\n"
    
    putStr linSym

    putStr "\n"
    
    putStr quadSym


    -}

    let (eta6,eps6,ans6') = mkAnsatzTensorEig 6 filterList6 symList6 areaList6IndsEta areaList6IndsEps 

    let ans6 = shiftLabels6 1 ans6' 

    let ans2 = fromListT6 $ map (\(x,y) -> ((Empty, Empty, singletonInd $ Ind9 x, Empty, Empty, Empty),AnsVar $ I.singleton 1 y)) [(0,-1),(4,1),(7,1),(9,1)] :: ATens 0 0 1 0 0 0 (AnsVar Rational)           

    let hTensList = map (\i -> LinearVar 0 (I.singleton i 1)) [0..20]
            
    let hTens = fromListT6 $ zipWith (\i j -> ((Empty, singletonInd $ Ind20 i,Empty,Empty,Empty,Empty),j)) [0..] hTensList :: ATens 0 1 0 0 0 0 (LinearVar Rational)

    let ansTens = ans6 
    
    let tens = (contrATens1 (0,0) $ ansTens &* hTens) 

    let tens2 = ans2 
    
    let l = map (\([i],y) -> "(" ++ showAnsVarLinVar y 'x' 'H' ++ ")*k[" ++ show i ++ "]") $ toListShow6 tens

    let l2 = map (\([i],y) -> "(" ++ showAnsVar y 'x' ++ ")*k[" ++ show i ++ "]") $ toListShow6 tens2

    let (_,_,ans4) = mkAnsatzTensorEig 4 filterList4 symList4 areaList4IndsEta areaList4IndsEps 

    let eqn1Mass = eqn1 ans4 

    let l3 = Mat.toList $ Sparse.toMatrix $ toEMatrix6 $ singletonTList eqn1Mass

    let l4 = "RankDefLin := Matrix(21,4,{" ++ (unlines rankDefLin) ++ "});"

    let l5 = "RankDefQuad := Matrix(21,4,{" ++ (unlines rankDefQuad) ++ "});"

    {-

    putStr l4

    putStr "\n"

    putStr l5

    -}

    let hTensList = map (\i -> AnsVar (I.singleton i 1)) [0..20]
                
    let hTens = fromListT6 $ zipWith (\i j -> ((Empty, singletonInd $ Ind20 i,Empty,Empty,Empty,Empty),j)) [0..] hTensList :: ATens 0 1 0 0 0 0 (AnsVar Rational)
    
    let hST = contrATens1 (0,0) $ interIArea &* hTens

    let flatAInv = ((tensorTrans5 (1,2) $ invEta &* invEta) &- (tensorTrans5 (1,3) $ invEta &* invEta)) &- epsilonInv

    let hInv = ((1/4) &.) $ contrATens3 (2,0) $ contrATens3 (3,1) $ contrATens3 (6,2) $ contrATens3 (7,3) $ flatAInv &* flatAInv &* hST

    let linPoly' = contrATens3 (0,0) $ contrATens3 (1,1) $ contrATens3 (2,2) $ contrATens3 (5,2) $ contrATens3 (6,4) $ contrATens3 (9,3) $ contrATens3 (10,6) $ contrATens3 (11,7) $ epsilon &* epsilon &* flatAInv &* flatAInv &* flatAInv 

    let linPoly = ((1/24) &.) $ cyclicSymATens5 [0,1,2,3] linPoly'

    let quadPoly1 = ((1/24) &.) $ cyclicSymATens5 [0,1,2,3] $ contrATens3 (0,0) $ contrATens3 (1,1) $ contrATens3 (2,2) $ contrATens3 (5,2) $ contrATens3 (6,4) $ contrATens3 (9,3) $ contrATens3 (10,6) $ contrATens3 (11,7) $ epsilon &* epsilon &* hInv &* flatAInv &* flatAInv 
   
    let quadPoly2 = ((1/24) &.) $ cyclicSymATens5 [0,1,2,3] $ contrATens3 (0,0) $ contrATens3 (1,1) $ contrATens3 (2,2) $ contrATens3 (5,2) $ contrATens3 (6,4) $ contrATens3 (9,3) $ contrATens3 (10,6) $ contrATens3 (11,7) $ epsilon &* epsilon &* flatAInv &* hInv &* flatAInv 

    let quadPoly3 = ((1/24) &.) $ cyclicSymATens5 [0,1,2,3] $ contrATens3 (0,0) $ contrATens3 (1,1) $ contrATens3 (2,2) $ contrATens3 (5,2) $ contrATens3 (6,4) $ contrATens3 (9,3) $ contrATens3 (10,6) $ contrATens3 (11,7) $ epsilon &* epsilon &* flatAInv &* flatAInv &* hInv 

    let quadPoly = quadPoly1 &+ quadPoly2 &+ quadPoly3
   
    let quadPolyL =  map (\([a,b,c,d], x) ->([a,b,c,d], showAnsVar x 'H')) $ toListShow6 quadPoly

    let linPolyL = map (\([a,b,c,d],x) -> showFrac x ++ "*" ++ "k" ++ show a ++ "*k" ++ show b ++ "*k" ++ show c ++ "*k" ++ show d ++ "+" ) $ toListShow6 linPoly

    --print $ concat $ map (\([a,b,c,d],v) -> "(" ++ v ++ ")*" ++ "k" ++ show a ++ "*k" ++ show b ++ "*k" ++ show c ++ "*k" ++ show d ++ "+") quadPolyL

    let ans1Poly = contrATens1 (0,0) $ contrATens3 (0,0) $ contrATens3 (1,1) $ contrATens3 (2,2) $ contrATens3 (3,3) $ (tensorTrans5 (1,2) $ invEta &* invEta) &* interIArea &* flatInter

    let ans2Poly = contrATens1 (0,0) $ contrATens3 (0,0) $ contrATens3 (1,1) $ contrATens3 (2,2) $ contrATens3 (3,3) $ epsilonInv &* interIArea &* flatInter

    let ans3Poly = ((1/2) &.) $ symATens5 (0,1) $ contrATens1 (0,0) $ contrATens3 (0,0) $ contrATens3 (1,1) $ contrATens3 (2,2) $ contrATens3 (3,3) $ (tensorTrans5 (3,4) $ tensorTrans5 (1,2) $ invEta &* invEta &* invEta ) &* interIArea &* flatInter

    let ans3PolyTest1 = (2 &.) $ invEta &* delta3 

    let ans3PolyTest2 = (2 &.) $ symATens5 (0,1) $ delta3 &* invEta

    --print $ toListShow6 (ans3Poly &+ (ans3PolyTest1 &+ ans3PolyTest2))

   
    let test1 = contrATens3 (0,0) $ contrATens3 (1,1) $ contrATens3 (2,2) $ contrATens3 (3,3) $ hST &* epsilonInv

    let test2 = contrATens3 (0,0) $ contrATens3 (1,1) $ contrATens3 (2,2) $ contrATens3 (3,3) $ hInv &* epsilon

    let test3 = ((-4) &.) $ contrATens3 (0,0) $ contrATens3 (1,1) $ contrATens3 (2,2) $ contrATens3 (3,3) $ hST &* (tensorTrans5 (1,2) $ invEta &* invEta)

    --print $ map (\(x,y) -> showAnsVar y 'H') $ toListShow6 test1 

    --print $ map (\(x,y) -> showAnsVar y 'H') $ toListShow6 test2 

    --print $ map (\(x,y) -> showAnsVar y 'H') $ toListShow6 test3

    --print $ map (\(x,y) -> showAnsVar y 'H') $ toListShow6 (test3 &- test2) 



    print $ toListShow6  interI2

    print $ toListShow6  interJ2