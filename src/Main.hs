{-# LANGUAGE DataKinds #-}


module Main (
 main
) where

import Matrices
import TensorTreeNumeric4
import PerturbationTree2_2 

import Codec.Compression.GZip
import Data.Serialize
import Data.Either
import qualified Data.ByteString.Lazy as BS
import qualified Data.Eigen.Matrix as Mat 
import qualified Data.Eigen.SparseMatrix as Sparse
import qualified Data.Eigen.LA as Sol 

import qualified Data.Map.Strict as M
import Data.Ratio

main = do

    let map1Area = trianMapAreaI
    let map2Area = trianMapAreaJ
    let map1Metric = trianMapI2
    let map2Metric = trianMapJ2

    let aList4 = areaEvalMap4Inds trianMapArea trianMapDerivative
    let aList6 = areaEvalMap6Inds trianMapArea trianMapDerivative
    let aList8 = areaEvalMap8Inds trianMapArea trianMapDerivative
    let aList10_1 = areaEvalMap10_1Inds trianMapArea trianMapDerivative
    let aList10_2 = areaEvalMap10_2Inds trianMapArea trianMapDerivative
    let aList12 = areaEvalMap12Inds trianMapArea trianMapDerivative
    let aList12_1 = areaEvalMap12_1Inds trianMapArea trianMapDerivative
    let aList14_1 = areaEvalMap14_1Inds trianMapArea trianMapDerivative
    let aList14_2 = areaEvalMap14_2Inds trianMapArea trianMapDerivative
    let aList16_1 = areaEvalMap16_1Inds trianMapArea trianMapDerivative
    let aList16_2 = areaEvalMap16_2Inds trianMapArea trianMapDerivative
    let aList18 = areaEvalMap18Inds trianMapArea trianMapDerivative
    let aList18_2 = areaEvalMap18_2Inds trianMapArea trianMapDerivative
    let aList18_3 = areaEvalMap18_3Inds trianMapArea trianMapDerivative
    let aList16 = areaEvalMap16Inds trianMapArea trianMapDerivative 
    let aList20 = areaEvalMap20Inds trianMapArea trianMapDerivative 




    let ansatz4' = mkAnsatzTensor 4 filterList4 symList4 1 epsMap aList4 
    let ansatz6' = mkAnsatzTensor 6 filterList6 symList6 1 epsMap aList6 
    let ansatz8' = mkAnsatzTensor 8 filterList8 symList8 1 epsMap aList8 
    let ansatz10_1' = mkAnsatzTensor 10 filterList10_1 symList10_1 1 epsMap aList10_1 
    let ansatz10_2' = mkAnsatzTensor 10 filterList10_2 symList10_2 1 epsMap aList10_2 
    let ansatz12' = mkAnsatzTensor 12 filterList12 symList12 1 epsMap aList12 
    let ansatz12_1' = mkAnsatzTensor 12 filterList12_1 symList12_1 1 epsMap aList12_1 
    let ansatz14_1' = mkAnsatzTensor 14 filterList14_1 symList14_1 1 epsMap aList14_1 
    let ansatz14_2' = mkAnsatzTensor 14 filterList14_2 symList14_2 1 epsMap aList14_2 
    let ansatz16_1' = mkAnsatzTensor 16 filterList16_1 symList16_1 1 epsMap aList16_1 
    let ansatz16_2' = mkAnsatzTensor 16 filterList16_2 symList16_2 1 epsMap aList16_2
    let ansatz18' = mkAnsatzTensor 18 filterList18 symList18 1 epsMap aList18 
    let ansatz18_2' = mkAnsatzTensor 18 filterList18_2 symList18_2 1 epsMap aList18_2 
    let ansatz18_3' = mkAnsatzTensor 18 filterList18_3 symList18_3 1 epsMap aList18_3 
    let ansatz16' = mkAnsatzTensor 16 filterList16 symList16 1 epsMap aList16 
    let ansatz20' = mkAnsatzTensor 20 filterList20 symList20 1 epsMap aList20 



 

    let ansatz4 = encodeLazy ansatz4'
    let ansatz6 = encodeLazy ansatz6'
    let ansatz8 = encodeLazy ansatz8'
    let ansatz10_1 = encodeLazy ansatz10_1'
    let ansatz10_2 = encodeLazy ansatz10_2'
    let ansatz12 = encodeLazy ansatz12'
    let ansatz12_1 = encodeLazy ansatz12_1'
    let ansatz14_1 = encodeLazy ansatz14_1'
    let ansatz14_2 = encodeLazy ansatz14_2'
    let ansatz16_1 = encodeLazy ansatz16_1'
    let ansatz16_2 = encodeLazy ansatz16_2'
    let ansatz18 = encodeLazy ansatz18'
    let ansatz18_2 = encodeLazy ansatz18_2'
    let ansatz18_3 = encodeLazy ansatz18_3'
    let ansatz16 = encodeLazy ansatz16'
    let ansatz20 = encodeLazy ansatz20'



    --BS.writeFile "/cip/austausch/cgg/ansatz4.dat.gz" $ compress ansatz4
    --BS.writeFile "/cip/austausch/cgg/ansatz6.dat.gz" $ compress ansatz6
    --BS.writeFile "/cip/austausch/cgg/ansatz8.dat.gz" $ compress ansatz8
    --BS.writeFile "/cip/austausch/cgg/ansatz10_1.dat.gz" $ compress ansatz10_1
    --BS.writeFile "/cip/austausch/cgg/ansatz10_2.dat.gz" $ compress ansatz10_2
    --BS.writeFile "/cip/austausch/cgg/ansatz12.dat.gz" $ compress ansatz12
    --BS.writeFile "/cip/austausch/cgg/ansatz12_1.dat.gz" $ compress ansatz12_1
    --BS.writeFile "/cip/austausch/cgg/ansatz14_1.dat.gz" $ compress ansatz14_1
    --BS.writeFile "/cip/austausch/cgg/ansatz14_2.dat.gz" $ compress ansatz14_2
    --BS.writeFile "/cip/austausch/cgg/ansatz16_1.dat.gz" $ compress ansatz16_1
    --BS.writeFile "/cip/austausch/cgg/ansatz16_2.dat.gz" $ compress ansatz16_2
    --BS.writeFile "/cip/austausch/cgg/ansatz18.dat.gz" $ compress ansatz18
    --BS.writeFile "/cip/austausch/cgg/ansatz16.dat.gz" $ compress ansatz16
    --BS.writeFile "/cip/austausch/cgg/ansatz20.dat.gz" $ compress ansatz20




    --e' <- BS.readFile "tensor_bs.dat.gz"
    --let d = (fromRight undefined $ decodeLazy $ decompress e') :: Tensor8 3 0 0 0 1 0 0 0 VarMap

    byteString16 <- BS.readFile "/cip/austausch/cgg/ansatz16.dat.gz"

    let ansatz16Tens = (fromRight undefined $ decodeLazy $ decompress byteString16) :: Tensor8 4 0 0 0 0 0 0 0 VarMap

    let ansatzEqn16Tens = ansatzABCD map1Metric map2Metric map1Area map2Area ansatz16Tens 


    {-

    byteString14_1 <- BS.readFile "/cip/austausch/cgg/ansatz14_1.dat.gz"

    byteString14_2 <- BS.readFile "/cip/austausch/cgg/ansatz14_2.dat.gz"

    byteString18 <- BS.readFile "/cip/austausch/cgg/ansatz18.dat.gz"

    byteString18_2 <- BS.readFile "/cip/austausch/cgg/ansatz18_2Ord4.dat.gz"

    byteString18_3 <- BS.readFile "/cip/austausch/cgg/ansatz18_3Ord4.dat.gz"

    let ansatz14_1Tens = (fromRight undefined $ decodeLazy $ decompress byteString14_1) :: Tensor8 3 0 0 0 0 0 2 0 VarMap

    let ansatz14_2Tens = (fromRight undefined $ decodeLazy $ decompress byteString14_2) :: Tensor8 3 0 0 0 1 0 0 0 VarMap

    let ansatz18Tens = (fromRight undefined $ decodeLazy $ decompress byteString18) :: Tensor8 3 0 0 0 0 3 0 0 VarMap

    let ansatz18_2Tens = (fromRight undefined $ decodeLazy $ decompress byteString18_2) :: Tensor8 4 0 0 0 1 0 0 0 VarMap

    let ansatz18_3Tens = (fromRight undefined $ decodeLazy $ decompress byteString18_3) :: Tensor8 4 0 0 0 0 0 2 0 VarMap





    let ansatz18_2'' = ansatz18_2Tens 
    
    let ansatz18_2Rank = getTensorRank ansatz18_2''

    let ansatz18_3'' = shiftVarLabels ansatz18_2Rank ansatz18_3Tens

    let ansatz18_3Rank = getTensorRank ansatz18_3Tens 

    let ansatz14_1'' = shiftVarLabels (ansatz18_2Rank + ansatz18_3Rank) ansatz14_1Tens 

    let ansatz14_1Rank = getTensorRank ansatz14_1Tens 

    let ansatz14_2'' = shiftVarLabels (ansatz18_2Rank + ansatz18_3Rank + ansatz14_1Rank) ansatz14_2Tens 

    let ansatz14_2Rank = getTensorRank ansatz14_2Tens 

    let ansatz10_1'' = shiftVarLabels (ansatz18_2Rank + ansatz18_3Rank + ansatz14_1Rank + ansatz14_2Rank) ansatz10_1' 

    let ansatz10_1Rank = getTensorRank ansatz10_1' 

    let ansatz10_2'' = shiftVarLabels (ansatz18_2Rank + ansatz18_3Rank + ansatz14_1Rank + ansatz14_2Rank + ansatz10_1Rank) ansatz10_2' 

    let ansatz10_2Rank = getTensorRank ansatz10_2' 

    let ansatz6'' = shiftVarLabels (ansatz18_2Rank + ansatz18_3Rank + ansatz14_1Rank + ansatz14_2Rank + ansatz10_1Rank + ansatz10_2Rank) ansatz6'

    let ansatz6Rank = getTensorRank ansatz6' 




    let (m1,_,eqn1AIList) = toSparseMatRed $ eqn1AI map1Metric map2Metric map1Area map2Area ansatz6'' ansatz10_2'' 

    let (m2,_,eqn1ABIList) = toSparseMatRed $ eqn1ABI map1Metric map2Metric map1Area map2Area ansatz10_2'' ansatz14_2''

    let (m4,_,eqn3AList) = toSparseMatRed $ eqn3A map1Metric map2Metric map1Area map2Area ansatz6'' ansatz10_2''

    let (m5,_,eqn3ABList) = toSparseMatRed $ eqn3AB map1Metric map2Metric map1Area map2Area ansatz10_2'' ansatz14_2'

    let (m6,_,eqn2AaList) = toSparseMatRed $ eqn2Aa map1Metric map2Metric map1Area map2Area ansatz6'' ansatz10_1''

    let (m7,_,eqn2ABbList) = toSparseMatRed $ eqn2ABb map1Metric map2Metric map1Area map2Area ansatz10_1'' ansatz10_2'' ansatz14_1''

    let (m8,_,eqn1AaBbList) = toSparseMatRed $ eqn1AaBb map1Metric map2Metric map1Area map2Area ansatz10_1'' ansatz14_1''

    let (m9,_,eqn1ABCIList) = toSparseMatRed $ eqn1ABCI map1Metric map2Metric map1Area map2Area ansatz14_2'' ansatz18_2''

    let (m10,_,eqn1ABbCcList) = toSparseMatRed $ eqn1ABbCc map1Metric map2Metric map1Area map2Area ansatz14_1'' ansatz18_3''

    let (m11,_,eqn2ABCcList) = toSparseMatRed $ eqn2ABCc map1Metric map2Metric map1Area map2Area ansatz14_1'' ansatz14_2'' ansatz18_3''

    let (m12,_,eqn3ABCList) = toSparseMatRed $ eqn3ABC map1Metric map2Metric map1Area map2Area ansatz14_2'' ansatz18_2''

    


    let ansatz12_1'' = ansatz12_1'

    let ansatzRank12_1 = getTensorRank ansatz12_1' 

    let ansatz10_1'' = shiftVarLabels ansatzRank12_1 ansatz10_1' 

    let ansatzRank10_1 = getTensorRank ansatz10_1' 

    let ansatz10_2'' = shiftVarLabels (ansatzRank12_1 + ansatzRank10_1) ansatz10_2' 

    let ansatzRank10_2 = getTensorRank ansatz10_2' 

    let ansatz8'' = shiftVarLabels (ansatzRank12_1 + ansatzRank10_1 + ansatzRank10_2) ansatz8'

    let ansatzRank8 = getTensorRank ansatz8' 

    let ansatz6'' = shiftVarLabels (ansatzRank12_1 + ansatzRank10_1 + ansatzRank10_2 + ansatzRank8) ansatz6' 

    let ansatzRank6 = getTensorRank ansatz6' 

    let ansatz4'' = shiftVarLabels (ansatzRank12_1 + ansatzRank10_1 + ansatzRank10_2 + ansatzRank8 + ansatzRank6) ansatz4' 

    let ansatzRank4 = getTensorRank ansatz4' 


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

    print 1
    

    