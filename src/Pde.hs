--this module defines the stuff necessary for modeling pdes
--for the moment we are only interessted in scalar pdes


module Pde (
    prolongPde, prolongPdeAll, print2Maple, mkPdefromTens, evalPdeRand, triangleMap, mkAllMultInds, number2MultInd, prolongPdeConst, prolongPdeIvar,
    combinePdes, prolongSymbolAll, deriveIvar1, multInd2Number1, addMultiInds, isDerivableIvar1, print2MaplePde

) where

    import Data.List
    import qualified Data.IntMap.Strict as I
    import qualified Data.Map.Strict as M
    import Numeric.Natural 
    import Ivar 
   

    newtype MultiIndex  = MultiIndex (I.IntMap Natural) deriving (Eq,Ord,Show) 

    diffOrder :: MultiIndex -> Int
    diffOrder (MultiIndex iMap) = fromIntegral $ I.foldr (+) 0 iMap 
                

    --there is a problem with difforder !!! (1 key multiple "entries")

    addMultiInds :: MultiIndex -> MultiIndex -> MultiIndex
    addMultiInds (MultiIndex iMap1) (MultiIndex iMap2) = MultiIndex $ I.unionWith (+) iMap1 iMap2

    newtype Pde a = Pde (M.Map (Int,MultiIndex) a) deriving Show

    getPdeMap :: Pde a -> M.Map (Int,MultiIndex) a
    getPdeMap (Pde map1) = map1

    --or encode the equation index in the map (pde sys is a map not a list of maps ?)

    --derivative works only (and is only needed) for difforder 1 multiInds (safety check ?)

    isDerivableIvar1 :: MultiIndex -> Ivar a -> Bool
    isDerivableIvar1 (MultiIndex map1) (Ivar a map2)
                | diffOrder (MultiIndex map1)  /= 1 = error "only works (and should be needed) for diffOrder 1 multi Inds"
                | otherwise = I.member pos map2
                    where 
                        pos = (I.keys map1) !! 0

    --there is a problem when 0 is stored !! -> it might happen that adding Ivars yields zeros stored in the ivar (corrected in Ivar)

    deriveIvar1 :: Num a => MultiIndex -> Ivar a -> Ivar a
    deriveIvar1 (MultiIndex map1) (Ivar s map2) = Ivar (i) I.empty
                    where 
                        pos = (I.keys map1) !! 0
                        i = (I.!) map2 pos

    --there was a small error
            
    --the idea is to first filter the list of keys by isDerivableIvar1 and then only derive the remaining keys

    prolongPdeConst :: MultiIndex -> Pde a -> Pde a
    prolongPdeConst mult (Pde map1) = Pde $ M.mapKeys (\(x,y) -> (136*(multInd2Number1 mult)+x,addMultiInds mult y)) map1

    prolongPdeIvar :: Num a => MultiIndex -> Pde (Ivar a) -> Pde (Ivar a)
    prolongPdeIvar mult (Pde map1) = Pde map3
                    where
                        mapFilter = M.filter (isDerivableIvar1 mult) map1
                        map2 = M.map (deriveIvar1 mult) mapFilter
                        map3 = M.mapKeys (\(x,y) -> (136*(multInd2Number1 mult)+x,y)) map2 

    prolongPde :: (Num a, Eq a) => MultiIndex -> Pde (Ivar a) -> Pde (Ivar a)
    prolongPde mult sys = Pde $ M.unionWith addIvar (getPdeMap $ prolongPdeConst mult sys) (getPdeMap $ prolongPdeIvar mult sys)

    --the only problem is getting the new eqn number when prolonging -> solved by using M.mapKeys 

    prolongPdeAll :: (Num a, Eq a) => [MultiIndex] -> Pde (Ivar a) -> Pde (Ivar a)
    prolongPdeAll mults pde = Pde $ M.unions pdeMapList
                    where
                        pdeMapList = map (\x -> getPdeMap $ prolongPde x pde) mults

    --this is probably not necessary

    prolongSymbolAll :: Num a => [MultiIndex] -> Pde (Ivar a) -> Pde (Ivar a)
    prolongSymbolAll mults pde = Pde $ M.unions pdeMapList
                    where
                        pdeMapList = map (\x -> getPdeMap $ prolongPdeConst x pde) mults


    --we need functions for printing a pde

    multInd2Number :: MultiIndex -> [Int]
    multInd2Number (MultiIndex map1) = concat l2
                where 
                    l = I.assocs map1
                    l2 = map (\(x,y) -> replicate (fromIntegral y) x) l


    multInd2Number1 :: MultiIndex -> Int
    multInd2Number1 mult 
                    | length l == 1 = fromIntegral $ (multInd2Number mult) !! 0 
                    | otherwise = error "expect multiind with size one at ths point !"
                     where
                        l = multInd2Number mult

    --there is still the problem with numbers other than 1 stored


    number2MultInd :: Int -> MultiIndex
    number2MultInd 0 = MultiIndex $ I.empty 
    number2MultInd i = MultiIndex $ I.singleton i 1

    mkAllMultInds :: Int -> [MultiIndex]
    mkAllMultInds i = map number2MultInd [1..i]

    --store the order of the snd derivatives in a map

    triangleMap :: Int -> M.Map [Int] Int 
    triangleMap i = M.fromList $ zip j k
                    where
                        j = [ [a,b] | a <- [1..i], b <- [a..i] ]
                        k = [1..]

    multInd2MatrixNr :: MultiIndex -> Int -> M.Map [Int] Int -> Int
    multInd2MatrixNr mult nopsIvars triangle 
                    | diff == 0 = 1
                    | diff == 1 = 1 + (num !! 0)
                    | diff == 2 = 1 +  nopsIvars + ( (M.!) triangle num ) 
                        where 
                            diff = diffOrder mult 
                            num = multInd2Number mult 

    print2Maple :: Int -> M.Map [Int] Int -> Pde (Ivar Rational) -> String
    print2Maple nopsIvar triangle (Pde map1) = "{" ++ (tail $ concat l2) ++ "}"
                    where 
                        l = M.assocs map1 
                        l2 = map (\((x,y),z) -> ',' : (show (x,multInd2MatrixNr y nopsIvar triangle)) ++ '=' : showIvarRational z ++ "\n") l

    --we need functions to construct pdes out of the tensor output and for prolonging the whole system

    mkPdefromTens :: Show a => M.Map (Int,Int) (Ivar a) -> Pde (Ivar a)
    mkPdefromTens map1 = Pde map2 
                where 
                    map2 = M.mapKeys (\(x,y) -> (x,number2MultInd (y-1))) map1

    evalPdeRand :: Int -> M.Map [Int] Int -> I.IntMap Int -> Pde (Ivar Rational) -> String 
    evalPdeRand nopsIvar triangle randMap (Pde map1) = "{" ++ (tail $ concat l2) ++ "}"
                    where 
                        map2 = M.map (mkIvarRandom randMap) map1
                        l = M.assocs map2 
                        l2 = map (\((x,y),z) -> ',' : (show (x,multInd2MatrixNr y nopsIvar triangle)) ++ '=' : show (truncate z) ++ "\n") l


    combinePdes :: Pde a -> Pde a -> Pde a
    combinePdes (Pde map1) (Pde map2) = Pde $ M.union map1 map2


    print2MaplePde :: Pde (Ivar Rational) -> String
    print2MaplePde (Pde map1) = "[" ++ tail (foldr (++) " "  $ I.map (\y -> "," ++ y ) map2) ++ "]"
                    where 
                        l = M.assocs map1 
                        l2 = map (\((x,y),z) -> (x, "(" ++ showIvarRational z ++ ")" ++ "*" ++ "L" ++  (show (multInd2Number y)) ++ "\n")) l
                        map2 = I.fromListWith (\a b -> a ++ "+" ++ b) l2 


    --is there an error? -> with the use of multiInd2MatrixNr




    





