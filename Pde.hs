--this module defines the stuff necessary for modeling pdes
--for the moment we are only interessted in scalar pdes


module Pde (

) where

    import Data.List
    import qualified Data.IntMap.Strict as I
    import qualified Data.Map.Strict as M
    import Numeric.Natural 
    import Ivar 
   

    newtype MultiIndex  = MultiIndex (I.IntMap Natural) deriving (Eq,Ord,Show) 

    diffOrder :: MultiIndex -> Int
    diffOrder (MultiIndex iMap) = I.size iMap

    addMultiInds :: MultiIndex -> MultiIndex -> MultiIndex
    addMultiInds (MultiIndex iMap1) (MultiIndex iMap2) = MultiIndex $ I.unionWith (+) iMap1 iMap2

    newtype Pde a = Pde (M.Map MultiIndex a) deriving Show

    --derivative works only (and is only needed) for difforder 1 multiInds (safety check ?)

    isDerivableIvar1 :: MultiIndex -> Ivar a -> Bool
    isDerivableIvar1 (MultiIndex map1) (Ivar a map2)
                | I.size map1  /= 1 = error "only works (and should be neededd) for diffOrder 1 multi Inds"
                | otherwise = I.member pos map2
                    where 
                        pos = (I.keys map1) !! 0

    deriveIvar1 :: MultiIndex -> Ivar a -> Ivar a
    deriveIvar1 (MultiIndex map1) (Ivar s map2) = Ivar i I.empty
                    where 
                        pos = (I.keys map1) !! 0
                        i = (I.!) map2 pos
            
    --the idea is to first filter the list of keys by isDerivableIvar1 and then only derive the remaining keys

    prolongPdeConst :: MultiIndex -> M.Map MultiIndex a -> M.Map MultiIndex a
    prolongPdeConst mult map1 = M.mapKeys (addMultiInds mult) map1

    prolongPdeIvar :: MultiIndex -> M.Map MultiIndex (Ivar a) -> M.Map MultiIndex (Ivar a)
    prolongPdeIvar mult map1 = map2
                    where
                        mapFilter = M.filter (isDerivableIvar1 mult) map1
                        map2 = M.map (deriveIvar1 mult) mapFilter

    prolongPde :: Num a => MultiIndex -> Pde (Ivar a) -> Pde (Ivar a)
    prolongPde mult (Pde map1) =Pde $ M.unionWith addIvar (prolongPdeConst mult map1) (prolongPdeIvar mult map1)

    --we need functions for printing a pde

    multInd2Number :: MultiIndex -> [Int]
    multInd2Number (MultiIndex map) = I.keys map

    --store the order of the snd derivatives in a map

    triangleMap :: Int -> (M.Map [Int] Int) 
    triangleMap i = M.fromList $ zip j k
                    where
                        j = [ [a,b] | a <- [1..i], b <- [a..i] ]
                        k = [1..]

    multInd2MatrixNr :: MultiIndex -> Int -> M.Map [Int] Int -> Int
    multInd2MatrixNr mult nopsIvars triangle 
                    | diff == 0 = 1
                    | diff == 1 = 1 + (num !! 0)
                    | diff == 2 = nopsIvars + ( (M.!) triangle num ) 
                        where 
                            diff = diffOrder mult 
                            num = multInd2Number mult 

    print2Maple :: Show a => Int -> M.Map [Int] Int -> (Pde (Ivar a),Int) -> String
    print2Maple nopsIvar triangle ((Pde map1),i) = concat l2 
                    where 
                        l = M.assocs map1 
                        l2 = map (\(x,y) -> ',' : (show (i,multInd2MatrixNr x nopsIvar triangle)) ++ '=' : show y ++ "\n") l

    printSys2Maple :: Show a => Int -> M.Map [Int] Int -> [Pde (Ivar a)] -> String
    printSys2Maple nopsIvar triangle sys = '{' : (tail $ concat l) ++ "}"
                    where 
                        z = zip sys [1..]
                        l = map (print2Maple nopsIvar triangle) z

    --we need functions to construct pdes out of the tensor output




    





