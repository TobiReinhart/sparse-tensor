module PerturbationTree (


) where

    import Data.Tree

    type Eta = (Int,Int)

    evalEta :: (Int,Int) -> Int 
    evalEta (i,j) 
        | diag && i == 0 = 1
        | diag = -1
        | otherwise = 0
            where
                diag = i == j