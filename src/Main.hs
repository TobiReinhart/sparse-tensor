{-# LANGUAGE DataKinds #-}

module Main (main
) where

  import LorentzGenerator
  import SparseTensor
  import Data.List
  import BasicTens 
  import FlatEqns 
 
  main = do

    let (eta14_1,eps14_1,ans14_1) = mkAnsatzTensorEig' 14 symList14_1 :: (AnsatzForestEta, AnsatzForestEpsilon, STTens 14 0 (AnsVar Rational))

    let (eta14_1',eps14_1',ans14_1') = mkAnsatzTensorFastAbs 14 symList14_1 areaList14_1 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 3 0 0 0 2 0 (AnsVar Rational))


    print $ tensorRank2' ans14_1 

    print $ tensorRank6' ans14_1'


    