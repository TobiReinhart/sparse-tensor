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

    let (eta14_1,eps14_1,ans14_1) = mkAnsatzTensorEig' 14 sym14_1 :: (AnsatzForestEta, AnsatzForestEpsilon, STTens 14 0 (AnsVar Rational))
    
    print $ tensorRank2' ans14_1 


    