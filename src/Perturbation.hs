module Perturbation (



)
where

    import Ansatz 
    import Data.List
    import Symmetrize
    import qualified Data.Sequence as S
    import qualified Data.Map.Strict as M
    import Data.Maybe
    import qualified Data.Set as Set 
    import qualified Data.IntMap.Strict as I
    import qualified Data.Foldable as F

    type PertAnsatz a = M.Map (S.Seq [Int] ) a

    swapLabel :: (Eq a, Ord a) => (a,a) -> S.Seq [a] -> S.Seq [a]
    swapLabel (x,y) seq = fmap f seq 
            where
                f = sort.(map (swapLabelF (x,y)))

    swapLabelF :: Eq a => (a,a) -> a -> a 
    swapLabelF (x,y) z
            | x == z = y
            | y == z = x
            | otherwise = z 

    swapBlockLabel :: (Eq a, Ord a) => ([a],[a]) -> S.Seq [a] -> S.Seq [a]
    swapBlockLabel (i,j) s 
            | length i /= length j = error "only blocks with the same lenght can be symmetrized"
            | otherwise = foldr swapLabel s pairList
                where
                    pairList = zip i j

    update1CycleLabel :: (Eq a, Ord a) => ([a],[a]) -> (S.Seq [a]) -> (S.Seq [a])
    update1CycleLabel (i,j) s = fmap f s 
            where 
                f = sort.(map (update1CycleLabelF (i,j)))

    
    update1CycleLabelF :: (Eq a, Ord a) => ([a],[a]) -> a -> a 
    update1CycleLabelF (x,y) z
            | isJust mVal = fromJust mVal
            | otherwise = z 
                where 
                    zipList = zip x y 
                    zipMap = M.fromList zipList 
                    mVal = M.lookup z zipMap
    

    cyclicSwapLabel :: (Eq a, Ord a) => [a] -> (S.Seq [a]) -> [(S.Seq [a])]
    cyclicSwapLabel l s = s : ( map (\x -> update1CycleLabel x s) cList )
            where 
                perm = tail $ permutations l 
                cList = zip (repeat l) perm 

    cyclicSwapBlockLabel :: (Eq a, Ord a) => [[a]] -> S.Seq [a] -> [S.Seq [a]]
    cyclicSwapBlockLabel l s = s : ( map (\x -> update1CycleLabel x s) cList )
            where
                perm = map concat $ tail $ permutations l
                cList = zip (repeat $ concat l) perm


    symAnsatzLabel :: (Fractional a) => (Int,Int) -> PertAnsatz a -> PertAnsatz a
    symAnsatzLabel pair a = M.unionWith f (M.map (* (1/2)) a) (M.map (* (1/2)) swapAnsatz) 
            where 
                f = \x y -> x + y
                swapAnsatz = M.mapKeys (swapLabel pair) a

    aSymAnsatzLabel :: (Fractional a) => (Int,Int) -> PertAnsatz a -> PertAnsatz a
    aSymAnsatzLabel pair a = M.unionWith f (M.map (* (1/2)) a) (M.map (* (-1/2)) swapAnsatz)  
            where 
                f = \x y -> x + y 
                swapAnsatz = M.mapKeys (swapLabel pair) a

    symBlockAnsatzLabel :: (Fractional a) => ([Int],[Int]) -> PertAnsatz a -> PertAnsatz a
    symBlockAnsatzLabel pair a = M.unionWith f (M.map (* (1/2)) a) (M.map (* (1/2)) swapAnsatz) 
            where 
                f = \x y -> x + y 
                swapAnsatz = M.mapKeys (swapBlockLabel pair) a

    aSymBlockAnsatzLabel :: (Fractional a) => ([Int],[Int]) -> PertAnsatz a -> PertAnsatz a
    aSymBlockAnsatzLabel pair a = M.unionWith f (M.map (* (1/2)) a) (M.map (* (-1/2)) swapAnsatz)  
            where 
                f = \x y -> x + y 
                swapAnsatz = M.mapKeys (swapBlockLabel pair) a

    factorial :: (Num a, Eq a) => a -> a
    factorial 0 = error "Int must be positiv!"
    factorial 1 =  1
    factorial n = n*factorial (n-1)

    symCycleLabel :: (Fractional a) => [Int] -> PertAnsatz a -> PertAnsatz a
    symCycleLabel l ans =  M.fromListWith f newAnsatzList 
            where 
                norm = 1/ (fromIntegral $ factorial $ length l )
                f = \x y -> x + y 
                g = \(x,y) -> (zip (cyclicSwapLabel l x) (repeat $ norm * y))
                ansatzList = M.assocs ans
                newAnsatzList = concat $ map g ansatzList 

    symCycleBlockLabel :: (Fractional a) => [[Int]] -> PertAnsatz a -> PertAnsatz a
    symCycleBlockLabel l ans =  M.fromListWith f newAnsatzList 
            where 
                norm = 1/ (fromIntegral $ factorial $ length l )
                f = \x y -> x + y 
                g = \(x,y) -> (zip (cyclicSwapBlockLabel l x) (repeat $ norm * y))
                ansatzList = M.assocs ans
                newAnsatzList = concat $ map g ansatzList

    symAnsatz :: (Fractional a) => Symmetry -> PertAnsatz a -> PertAnsatz a 
    symAnsatz (labPair, labAPair, labBlock, labCycle, labBlockCycle) ans = 
        foldr symCycleBlockLabel (
            foldr symCycleLabel (
                foldr symBlockAnsatzLabel (
                    foldr aSymAnsatzLabel (
                        foldr symAnsatzLabel ans labPair
                    ) labAPair
                ) labBlock
            ) labCycle
        ) labBlockCycle  

    isZero :: (Fractional a, Eq a) => PertAnsatz a -> Bool
    isZero ans = elem 0 $ M.elems ans

    symAnsSetPert :: (Eq a, Fractional a) => Symmetry -> [PertAnsatz a] -> [PertAnsatz a]
    symAnsSetPert sym1 [] = []
    symAnsSetPert sym1 (x:xs) 
            | isZero symx = symAnsSetPert sym1 $ rmAnsatz symx xs 
            | otherwise = symx : (symAnsSetPert sym1 $ rmAnsatz symx xs)
                where
                    symx = symAnsatz sym1 x 
    
    rmAnsatz :: Fractional a => PertAnsatz a -> [PertAnsatz a] -> [PertAnsatz a]
    rmAnsatz ans ansList = filter (rmFunction rSet) ansList 
            where 
                rSet = M.keysSet ans 
                

    rmFunction :: Fractional a => Set.Set (S.Seq [Int]) -> PertAnsatz a -> Bool
    rmFunction rSet ans = (Set.size inter) == 0  
            where
                inter = Set.intersection rSet $ M.keysSet ans 

    --we need functions to construct an PertAnsatz from a given indcex list

    mkPertAns :: (Fractional a) => S.Seq Int -> PertAnsatz a
    mkPertAns seq = M.fromList [(seqList,1)]
                where 
                    chunks = S.chunksOf 2 seq 
                    seqList = (fmap F.toList) chunks 

    

    


