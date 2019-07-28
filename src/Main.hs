{-# LANGUAGE DataKinds #-}

module Main (main
) where

  import LorentzGenerator
  import SparseTensor
  
  
  main = do

    let sym4 = ([], [(1,2),(3,4)], [([1,2],[3,4])], [], [])

    let sym8 = ([], [(1,2),(3,4),(5,6),(7,8)], [([1,2],[3,4]),([5,6],[7,8]),([1,2,3,4],[5,6,7,8])], [], [])

    let sym10_1 = ([], [(1,2),(3,4),(6,7),(8,9)], [([1,2],[3,4]),([6,7],[8,9]),([1,2,3,4,5],[6,7,8,9,10])], [], [])

    let sym10_2 = ([(9,10)], [(1,2),(3,4),(5,6),(7,8)], [([1,2],[3,4]),([5,6],[7,8])], [], [])

    let (eta4,eps4,ans4) = mkAnsatzTensorFast' 4 sym4 :: (AnsatzForestEta, AnsatzForestEpsilon, STTens 4 0 (AnsVar Rational))

    let (eta8,eps8,ans8) = mkAnsatzTensorFast' 8 sym8 :: (AnsatzForestEta, AnsatzForestEpsilon, STTens 8 0 (AnsVar Rational))

    let (eta10_1,eps10_1,ans10_1) = mkAnsatzTensorFast' 10 sym10_1 :: (AnsatzForestEta, AnsatzForestEpsilon, STTens 10 0 (AnsVar Rational))

    let (eta10_2,eps10_2,ans10_2) = mkAnsatzTensorFast' 10 sym10_2 :: (AnsatzForestEta, AnsatzForestEpsilon, STTens 10 0 (AnsVar Rational))


    print $ tensorRank2' ans4
    
    