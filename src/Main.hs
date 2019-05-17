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

    let (_,_,ans6') = mkAnsatzTensorEig 6 filterList6 symList6 areaList6IndsEta areaList6IndsEps 

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

    print l3 


