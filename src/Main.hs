{-# LANGUAGE DataKinds #-}

module Main (main
) where

  --import LorentzGenerator
  import PerturbationTree2_3
  
  
  main = do
      
    --print $ length $ getEpsilonInds [1..14] ([], [(1,2),(3,4),(5,6),(7,8),(10,11),(12,13)], [([1,2],[3,4]),([5,6],[7,8]),([10,11],[12,13]),([5,6,7,8,9],[10,11,12,13,14])], [], [])
    print $ length $ getEpsilonInds [1..14] filterList14_1 symList14_1 

    