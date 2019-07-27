{-# LANGUAGE DataKinds #-}

module Main (main
) where

  --import LorentzGenerator
  import PerturbationTree2_3
  
  
  main = do
      
    --print $ length $  getEpsilonInds [1..16] ([(9,10),(15,16)], [(1,2),(3,4),(5,6),(7,8),(11,12),(13,14)], [([1,2],[3,4]),([5,6],[7,8]),([11,12],[13,14]),([5,6,7,8,9,10],[11,12,13,14,15,16])], [], [])
    print $ length $ getEpsilonInds [1..16] filterList16_2 symList16_2 

    