--this module is inteded for modeling the ivars that occur in the equivariance equations

--ivars must be added, multiplied by a factor end derived (vector like)

module Ivar (
    Ivar(..), showIvar, showIvarRational, sMultIvar, addIvar, subIvar, constrAllIvars, number2Ivar, mkConstIvar,
    mkIvarRandom

) where

    --we do not need to modify the length of the "Ivar Vector" -> we do not need to store the length as information

    import Data.List
    import qualified Data.IntMap.Strict as I
    import qualified Data.Map.Strict as M
    import Control.Monad
    import System.Random.TF
    import System.Random.TF.Gen 
    import System.Random.TF.Instances
    

    --store the scalar and the vector information in Ivar

    data Ivar a = Ivar a (I.IntMap a) deriving (Eq, Show)

    instance Functor Ivar where
        fmap f (Ivar a map) = Ivar (f a) (I.map f map)

    mkConstIvar :: a -> Ivar a
    mkConstIvar a = Ivar a (I.empty)

    showIvar :: (Show a) => Ivar a -> String 
    showIvar (Ivar a map1) = (show a) ++ (concat ivarString)
                where 
                    pairList = I.assocs map1
                    ivarString = map (\(x,y) -> "+" ++ "(" ++ (show y) ++ "*" ++ "V" ++ (show x) ++ ")") pairList

    --there was an error !!! (same as last week in showIvarRational)

    showIvarRational :: Ivar Rational -> String
    showIvarRational (Ivar a map1) = (show $ truncate a) ++ (concat ivarString)
                where 
                    pairList = I.assocs map1
                    ivarString = map (\(x,y) -> "+" ++ "(" ++ (show $ truncate y) ++ "*" ++ "V" ++ (show x) ++ ")") pairList


    sMultIvar :: (Num a, Eq a) => a -> Ivar a -> Ivar a
    sMultIvar 0 ivar = mkConstIvar 0
    sMultIvar x ivar = fmap ((*) x) ivar

    addIvar :: (Num a, Eq a) => Ivar a -> Ivar a -> Ivar a
    addIvar (Ivar x1 map1) (Ivar x2 map2) = Ivar (x1+x2) $ I.filter (/=0) (I.unionWith (+) map1 map2)

    --we do not want zeros to be stored

    subIvar :: (Num a,Eq a) => Ivar a -> Ivar a -> Ivar a
    subIvar ivar1 ivar2 = addIvar ivar1 (sMultIvar (-1) ivar2) 

    --construct all purely variable Ivars (for i variables)

    constrAllIvars :: (Num a) => Int -> [Ivar a]
    constrAllIvars i = map (\x ->(Ivar 0 (I.fromList x))) ivarList 
                where
                    ivarList = [[(a,1)] | a <- [1..i]]

    number2Ivar :: (Num a) => Int -> Ivar a
    number2Ivar i = Ivar 0 $ I.singleton i 1

    --we need a derivative function for ivars

    --defined in Pde

    --we also need a function for inserting random values for the ivars (probably best at tensor level)

    --later on the random numbers are stored in a map from keys = ivar number to values = random Ints (use random integers)

    mkRandomMap :: RandomGen g => g -> Int -> M.Map Int Int  
    mkRandomMap gen i = M.fromList $ zip [1..i] (randoms gen) 

    mkIvarRandom :: Num a => I.IntMap Int -> Ivar a -> a 
    mkIvarRandom ranMap (Ivar a map1) = a + (sum ranList)
                    where
                        ranList = I.elems $ I.mapWithKey (\k v -> (*) v (fromIntegral $ (I.!) ranMap k)) map1

    --there was an error with keys having values other than 1

    

