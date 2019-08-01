{-# LANGUAGE DataKinds #-}

module Main (main
) where

  import LorentzGenerator
  import SparseTensor
  import Data.List
  import BasicTens 
  import FlatEqns 
 
  main = do


    let sym14_1 = ([], [(1,2),(3,4),(5,6),(7,8),(10,11),(12,13)], [([1,2],[3,4]),([5,6],[7,8]),([10,11],[12,13]),([5,6,7,8,9],[10,11,12,13,14])], [], [])

    let (eta14_1,eps14_1,ans14_1') = mkAnsatzTensorEig' 14 sym14_1 :: (AnsatzForestEta, AnsatzForestEpsilon, STTens 14 0 (AnsVar Rational))

    let ans14ATens = Scalar $ Scalar $ Scalar $ Scalar ans14_1' :: ATens 0 0 0 0 14 0 (AnsVar Rational)  

    let ans14_1 = contrATens3 (0,0) $ contrATens3 (1,1) $ contrATens3 (2,2) $ contrATens3 (3,3) $ contrATens3 (4,4) $ contrATens3 (5,5) $ contrATens3 (6,6) $ contrATens3 (7,7) $ contrATens3 (9,8) $ contrATens3 (10,9) $ contrATens3 (11,10) $ contrATens3 (12,11) $ ans14ATens &* interIArea &* interIArea &* interIArea 

    print $ tensorRank6' ans14_1 


    