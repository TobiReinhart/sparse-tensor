--this module is inteded for modeling the ivars that occur in the equivariance equations

--ivars must be added, multiplied by a factor end derived (vector like)

module Ivar (
    Ivar(..), showIvar, showIvarRational, sMultIvar, addIvar, subIvar, constrAllIvars, number2Ivar, mkConstIvar

) where

    --we do not need to modify the length of the "Ivar Vector" -> we do not need to store the length as information

    import Data.List
    import qualified Data.IntMap.Strict as I

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
                    ivarString = map (\(x,y) -> (show y) ++ "*" ++ "V" ++ (show x)) pairList

    showIvarRational :: Ivar Rational -> String
    showIvarRational (Ivar a map1) = (show $ truncate a) ++ (concat ivarString)
                where 
                    pairList = I.assocs map1
                    ivarString = map (\(x,y) -> (show $ truncate y) ++ "*" ++ "V" ++ (show x)) pairList


    sMultIvar :: Num a => a -> Ivar a -> Ivar a
    sMultIvar x ivar = fmap ((*) x) ivar

    addIvar :: Num a => Ivar a -> Ivar a -> Ivar a
    addIvar (Ivar x1 map1) (Ivar x2 map2) = Ivar (x1+x2) (I.unionWith (+) map1 map2)

    subIvar :: Num a => Ivar a -> Ivar a -> Ivar a
    subIvar (Ivar x1 map1) (Ivar x2 map2) = Ivar (x1+x2) (I.unionWith (-) map1 map2)

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
