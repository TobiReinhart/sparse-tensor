{-# LANGUAGE DataKinds #-}

module Main (
 main
) where

import TensorTreeNumeric4 (Tensor8, VarMap, toMatrix,
                           toListShowVar, toListShow8,
                           getTensorRank, flatAreaNoEps,
                           toSparseMatRed,
                           flatAreaInvNoEps)
import IntEquations (intEquation, ansatz8Solved, cyclic, invAreaDerivativeFlat)
import PerturbationTree2_2 (generic8Ansatz)

import Data.Serialize (encodeLazy, decodeLazy)
import Codec.Compression.GZip (compress, decompress)
import qualified Data.ByteString.Lazy as BS (writeFile, readFile)

import Data.Ratio (numerator, denominator)

readTensor :: IO (Tensor8 2 0 0 0 0 0 4 0 VarMap)
readTensor = do
                compressed <- BS.readFile "intCond.dat.gz"
                let decompressed = decompress compressed
                let Right t = decodeLazy decompressed
                return t

showMaple :: (Int, Int, [((Int, Int), Rational)]) -> String
showMaple (r, c, vs) = "mat := Matrix(" ++ show r ++ ", " ++ show c ++ ", {\n" ++ content ++ "\n});"
    where
        content = unlines $ map (\((i, j), v) -> "(" ++ show i ++ ", " ++ show j ++ ") = (" ++ show (numerator v) ++ ")/" ++ show (denominator v) ++ ",") vs

prettyT :: Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 Rational -> String
prettyT = unlines . map show . filter ((/=0) . snd) . toListShow8

    let (m1,_,eqn3List) = toSparseMatRed $ eqn3 map1Metric map2Metric map1Area map2Area ansatz6''

    let (m2,_,eqn1List) = toSparseMatRed  $ eqn1 map1Metric map2Metric map1Area map2Area ansatz4''  



    let (m3,_,eqn1AList) = toSparseMatRed  $ eqn1A map1Metric map2Metric map1Area map2Area ansatz4''  ansatz8''  

    let (m4,_,eqn1AIList) = toSparseMatRed  $ eqn1AI map1Metric map2Metric map1Area map2Area ansatz6''  ansatz10_2''  

    let (m5,_,eqn2AaList) = toSparseMatRed  $ eqn2Aa map1Metric map2Metric map1Area map2Area ansatz6''  ansatz10_1'' 
    
    let (m6,_,eqn3AList) = toSparseMatRed  $ eqn3A map1Metric map2Metric map1Area map2Area ansatz6''  ansatz10_2''
    
    let (m7,_,eqn3AIList) = toSparseMatRed  $ eqn3AI map1Metric map2Metric map1Area map2Area ansatz12_1''  

    --the extra int conditions (from the restriction of the Jetbundle)

    let (m8,_,ansatzAB2List) = toSparseMatRed  $ ansatzAB2 map1Metric map2Metric map1Area map2Area ansatz8''  

    let (m9,_,ansatzAIB2_1List) = toSparseMatRed  $ ansatzAIB2_1 map1Metric map2Metric map1Area map2Area ansatz10_2''  

    let (m10,_,ansatzAIB2_2List) = toSparseMatRed  $ ansatzAIB2_2 map1Metric map2Metric map1Area map2Area ansatz10_2''
    
    let (m11,_,ansatzAIBJ2List) = toSparseMatRed  $ ansatzAIBJ2 map1Metric map2Metric map1Area map2Area ansatz12_1''  












    {-

    let fullEqn1 = eqn1AIList ++ (map (\((x,y),z) -> ((x+m1,y),z)) eqn1ABIList) 
            ++ (map (\((x,y),z) -> ((x+m1+m2,y),z)) eqn3List)
            ++ (map (\((x,y),z) -> ((x+m1+m2+m3,y),z)) eqn3AList) 
            ++ (map (\((x,y),z) -> ((x+m1+m2+m3+m4 ,y),z)) eqn3ABList)
            ++ (map (\((x,y),z) -> ((x+m1+m2+m3+m4+m5 ,y),z)) eqn2AaList) 
            ++ (map (\((x,y),z) -> ((x+m1+m2+m3+m4+m5+m6 ,y),z)) eqn2ABbList)
            ++ (map (\((x,y),z) -> ((x+m1+m2+m3+m4+m5+m6+m7 ,y),z)) eqn1AaBbList)
            ++ (map (\((x,y),z) -> ((x+m1+m2+m3+m4+m5+m6+m7+m8 ,y),z)) eqn1ABCIList)
            ++ (map (\((x,y),z) -> ((x+m1+m2+m3+m4+m5+m6+m7+m8+m9 ,y),z)) eqn1ABbCcList)
            ++ (map (\((x,y),z) -> ((x+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10 ,y),z)) eqn2ABCcList)
            ++ (map (\((x,y),z) -> ((x+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11 ,y),z)) eqn3ABCList)

    -}
    

    let eqnOrd1 = eqn3List ++ (map (\((x,y),z) -> ((x+m1,y),z)) eqn1List)

    let eqnOrd2 = eqn1AList ++ (map (\((x,y),z) -> ((x+m3,y),z)) eqn1AIList) 
            ++ (map (\((x,y),z) -> ((x+m3+m4,y),z)) eqn2AaList)
            ++ (map (\((x,y),z) -> ((x+m3+m4+m5,y),z)) eqn3AList) 
            ++ (map (\((x,y),z) -> ((x+m3+m4+m5+m6 ,y),z)) eqn3AIList)
            ++ (map (\((x,y),z) -> ((x+m3+m4+m5+m6+m7 ,y),z)) eqn3List)
            ++ (map (\((x,y),z) -> ((x+m3+m4+m5+m6+m7+m1 ,y),z)) eqn1List)
            ++ (map (\((x,y),z) -> ((x+m3+m4+m5+m6+m7+m1+m2 ,y),z)) ansatzAB2List)
            ++ (map (\((x,y),z) -> ((x+m3+m4+m5+m6+m7+m1+m2+m8 ,y),z)) ansatzAIB2_1List)
            ++ (map (\((x,y),z) -> ((x+m3+m4+m5+m6+m7+m1+m2+m8+m9 ,y),z)) ansatzAIB2_2List)
            ++ (map (\((x,y),z) -> ((x+m3+m4+m5+m6+m7+m1+m2+m8+m9+m10 ,y),z)) ansatzAIBJ2List)

    

    let eqn1Mass = eqn1List 

    let eqn2Mass = eqn1AList 
            ++ (map (\((x,y),z) -> ((x+m3 ,y),z)) eqn1List)

    -}

    --mass equations

    {-

    let ansatzRank4 = getTensorRank ansatz4'

    let ansatzRank8 = getTensorRank ansatz8' 

    let ansatzRank12 = getTensorRank ansatz12' 

    let ansatzRank16 = getTensorRank ansatz16Tens


    let ansatz8'' = shiftVarLabels (ansatzRank16 + ansatzRank12) ansatz8'
    
    let ansatz4'' = shiftVarLabels (ansatzRank16 + ansatzRank12 + ansatzRank8) ansatz4' 

    let ansatz12'' = shiftVarLabels ansatzRank16 ansatz12' 

    let ansatz16'' = ansatz16Tens


    let eqn1Tens = eqn1 map1Metric map2Metric map1Area map2Area ansatz4''

    let eqn1ATens = eqn1A map1Metric map2Metric map1Area map2Area ansatz4'' ansatz8''

    let eqn1ABTens = eqn1AB map1Metric map2Metric map1Area map2Area ansatz8'' ansatz12''

    let eqn1ABCTens = eqn1ABC map1Metric map2Metric map1Area map2Area ansatz12'' ansatz16'' 




    let (m1,_,eqn1List) = toSparseMatRed eqn1Tens

    let (m2,_,eqn1AList) = toSparseMatRed $ eqn1ATens

    let (m3,_,eqn1ABList) = toSparseMatRed $ eqn1ABTens

    let (m4,_,eqn1ABCList) = toSparseMatRed $ eqn1ABCTens




    let eqnMass = eqn1ABCList 
                ++ (map (\((x,y),z) -> ((x+m4 ,y),z)) eqn1ABList)
                ++ (map (\((x,y),z) -> ((x+m4+m3 ,y),z)) eqn1AList)
                ++ (map (\((x,y),z) -> ((x+m4+m3+m2 ,y),z)) eqn1List)


             

    print $ (ansatzRank16, getTensorRank eqn1ABCTens) 
    print $ (ansatzRank12, getTensorRank eqn1ABTens) 
    print $ (ansatzRank8, getTensorRank eqn1ATens)
    print $ (ansatzRank4, getTensorRank eqn1Tens) 

    print $ getTensorRank4 eqn1ABCTens eqn1ABTens eqn1ATens eqn1Tens

    -}      

    --print $ m1+m2+m3

    --print $ ansatzRank4 + ansatzRank8 + ansatzRank12  

    --putStr $ unlines $ map (\((i, j), v) -> "(" ++ show i ++ "," ++ show j ++ ")" ++ "=" ++  show (numerator v) ++ "/" ++ show (denominator v) ++ "," ) eqnMass  


    --print  (ansatzRank6, ansatzRank4, ansatzRank8, ansatzRank10_1, ansatzRank10_2, ansatzRank12_1)  

    --putStr $ unlines $ map (\((i, j), v) -> "(" ++ show i ++ "," ++ show j ++ ")" ++ "=" ++  show (numerator v) ++ "/" ++ show (denominator v) ++ "," ) eqnOrd2  

    --print $ ansatzTestAB'' map1Metric map2Metric map1Area map2Area ansatz4'

    --writeMatrices

    --print $ toListShowVar intCondTest

    --print $ filter (\(x,y) -> y /= 0) $ toListShow8 $ interIntCond map1Metric map2Metric map1Area map2Area

    --print $ filter (\(a,b) -> b /= 0) $ toListShow8 $ ansatzAIntCond map1Metric map2Metric map1Area map2Area

    --print $  filter (\(a,b) -> b /= 0) $ toListShow8 $ flatIntCondCheck map1Area map2Area 

    --print $  filter (\(a,b) -> b /= 0) $ toListShow8 $ flatInter map1Area map2Area 

    --print $  filter (\(a,b) -> b /= 0) $ toListShow8 $ interAnsatzEqn1ZeroNoInv map1Area map2Area 

    let ansatz4'' = shiftVarLabels 6 ansatz4' 

    --print $ getTensorRank2 (eqn1 map1Metric map2Metric map1Area map2Area ansatz4'') (eqn1A map1Metric map2Metric map1Area map2Area ansatz4'' ansatz8') 

    --print $ getTensorRank3 (eqn1 map1Metric map2Metric map1Area map2Area ansatz4'') (ansatzEqn2Test map1Area map2Area ansatz8') (eqn1A map1Metric map2Metric map1Area map2Area ansatz4'' ansatz8') 

    print $ filter (\(a,b) -> b /= 0) $ toListShow8 $ interAnsatzEqn1ZeroTest map1Area map2Area  
