module Perturbation (

mkPertAns, mkPertAnsEpsilon, PertAnsatz, PertAnsatzEpsilon, symAnsSetPert, symAnsSetPertEpsilon, indexPermSeqPert, getRepIndsPert, evalPertAns, areaList14, areaEvalMap14, evalFullAns, areaEvalMap10

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

    type PertAnsatzEpsilon a = M.Map ( ([Int],S.Seq [Int]) ) a

    --write this in 1 Map only ?

    swapLabel :: (Eq a, Ord a) => (a,a) -> S.Seq [a] -> S.Seq [a]
    swapLabel (x,y) seq = S.sort $ fmap f seq 
            where
                f = sort.(map (swapLabelF (x,y)))

    --we need to take care of the sign when using epsilon

    epsilonSign ::(Num a) => [Int] -> a
    epsilonSign [a,b,c,d] = (-1)^(length $  filter (==True) [b>a,c>a,d>a,c>b,d>c,d>c])
    epsilonSign x = error "wrong list length in epsilon"
               
    
    swapLabelEpsilon :: (Num a) => (Int,Int) -> (([Int],S.Seq [Int]),a) -> (([Int],S.Seq [Int]),a)
    swapLabelEpsilon (x,y) ((eps,seq),s) = ((newEpsSort,newSeq),s*(epsilonSign newEps))
                where
                    newEps = map (swapLabelF (x,y)) eps 
                    newEpsSort = sort newEps
                    newSeq = swapLabel (x,y) seq 

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

    swapBlockLabelEpsilon :: (Num a) => ([Int],[Int]) -> (([Int],S.Seq [Int]),a) -> (([Int],S.Seq [Int]),a)
    swapBlockLabelEpsilon (i,j) s 
            | length i /= length j = error "only blocks with the same lenght can be symmetrized"
            | otherwise = foldr swapLabelEpsilon s pairList
                where
                    pairList = zip i j

    update1CycleLabel :: (Eq a, Ord a) => ([a],[a]) -> (S.Seq [a]) -> (S.Seq [a])
    update1CycleLabel (i,j) s = fmap f s 
            where 
                f = sort.(map (update1CycleLabelF (i,j)))

    update1CycleLabelEpsilon :: (Num a) => ([Int],[Int]) -> (([Int],S.Seq [Int]),a) -> (([Int],S.Seq [Int]),a)
    update1CycleLabelEpsilon (i,j) ((eps,seq),s) = ((newEpsSort, newS),s*(epsilonSign newEps))
            where 
                newS = update1CycleLabel (i,j) seq
                newEps = map (update1CycleLabelF (i,j)) eps 
                newEpsSort = sort newEps

    
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

    cyclicSwapLabelEpsilon :: (Num a) => [Int] -> (([Int],S.Seq [Int]),a) -> [(([Int],S.Seq [Int]),a)]
    cyclicSwapLabelEpsilon l s = s : ( map (\x -> update1CycleLabelEpsilon x s) cList )
            where 
                perm = tail $ permutations l 
                cList = zip (repeat l) perm 

    cyclicSwapBlockLabel :: (Eq a, Ord a) => [[a]] -> S.Seq [a] -> [S.Seq [a]]
    cyclicSwapBlockLabel l s = s : ( map (\x -> update1CycleLabel x s) cList )
            where
                perm = map concat $ tail $ permutations l
                cList = zip (repeat $ concat l) perm

    cyclicSwapBlockLabelEpsilon :: (Num a) => [[Int]] -> (([Int],S.Seq [Int]),a) -> [(([Int],S.Seq [Int]),a)]
    cyclicSwapBlockLabelEpsilon l s = s : ( map (\x -> update1CycleLabelEpsilon x s) cList )
            where
                perm = map concat $ tail $ permutations l
                cList = zip (repeat $ concat l) perm


    symAnsatzLabel :: (Fractional a) => (Int,Int) -> PertAnsatz a -> PertAnsatz a
    symAnsatzLabel pair a = M.unionWith f (M.map (* (1/2)) a) (M.map (* (1/2)) swapAnsatz) 
            where 
                f = \x y -> x + y
                swapAnsatz = M.mapKeys (swapLabel pair) a

    symAnsatzLabelEpsilon :: (Fractional a) => (Int,Int) -> PertAnsatzEpsilon a -> PertAnsatzEpsilon a
    symAnsatzLabelEpsilon pair a = M.unionWith f (M.map (* (1/2)) a) (M.map (* (1/2)) swapAnsatz) 
            where 
                f = \x y -> x + y
                ansList = M.assocs a
                swapAnsatz = M.fromList $ map (swapLabelEpsilon pair) ansList

    aSymAnsatzLabel :: (Fractional a) => (Int,Int) -> PertAnsatz a -> PertAnsatz a
    aSymAnsatzLabel pair a = M.unionWith f (M.map (* (1/2)) a) (M.map (* (-1/2)) swapAnsatz)  
            where 
                f = \x y -> x + y 
                swapAnsatz = M.mapKeys (swapLabel pair) a

    aSymAnsatzLabelEpsilon :: (Fractional a) => (Int,Int) -> PertAnsatzEpsilon a -> PertAnsatzEpsilon a
    aSymAnsatzLabelEpsilon pair a = M.unionWith f (M.map (* (1/2)) a) (M.map (* (-1/2)) swapAnsatz)  
            where 
                f = \x y -> x + y
                ansList = M.assocs a
                swapAnsatz = M.fromList $ map (swapLabelEpsilon pair) ansList

    symBlockAnsatzLabel :: (Fractional a) => ([Int],[Int]) -> PertAnsatz a -> PertAnsatz a
    symBlockAnsatzLabel pair a = M.unionWith f (M.map (* (1/2)) a) (M.map (* (1/2)) swapAnsatz) 
            where 
                f = \x y -> x + y 
                swapAnsatz = M.mapKeys (swapBlockLabel pair) a

    symBlockAnsatzLabelEpsilon :: (Fractional a) => ([Int],[Int]) -> PertAnsatzEpsilon a -> PertAnsatzEpsilon a
    symBlockAnsatzLabelEpsilon pair a = M.unionWith f (M.map (* (1/2)) a) (M.map (* (1/2)) swapAnsatz) 
            where 
                f = \x y -> x + y
                ansList = M.assocs a
                swapAnsatz = M.fromList $ map (swapBlockLabelEpsilon pair) ansList

    aSymBlockAnsatzLabel :: (Fractional a) => ([Int],[Int]) -> PertAnsatz a -> PertAnsatz a
    aSymBlockAnsatzLabel pair a = M.unionWith f (M.map (* (1/2)) a) (M.map (* (-1/2)) swapAnsatz)  
            where 
                f = \x y -> x + y 
                swapAnsatz = M.mapKeys (swapBlockLabel pair) a

    aSymBlockAnsatzLabelEpsilon :: (Fractional a) => ([Int],[Int]) -> PertAnsatzEpsilon a -> PertAnsatzEpsilon a
    aSymBlockAnsatzLabelEpsilon pair a = M.unionWith f (M.map (* (1/2)) a) (M.map (* (-1/2)) swapAnsatz) 
            where 
                f = \x y -> x + y
                ansList = M.assocs a
                swapAnsatz = M.fromList $ map (swapBlockLabelEpsilon pair) ansList

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

    symCycleLabelEpsilon :: (Fractional a) => [Int] -> PertAnsatzEpsilon a -> PertAnsatzEpsilon a
    symCycleLabelEpsilon l ans =  M.fromListWith f newAnsatzList 
            where 
                norm = 1/ (fromIntegral $ factorial $ length l )
                f = \x y -> x + y 
                ansatzList = M.assocs ans
                newAnsatzList = map (\(a,b) -> (a,norm*b)) $ concat $ map (cyclicSwapLabelEpsilon l) ansatzList 

    symCycleBlockLabel :: (Fractional a) => [[Int]] -> PertAnsatz a -> PertAnsatz a
    symCycleBlockLabel l ans =  M.fromListWith f newAnsatzList 
            where 
                norm = 1/ (fromIntegral $ factorial $ length l )
                f = \x y -> x + y 
                g = \(x,y) -> (zip (cyclicSwapBlockLabel l x) (repeat $ norm * y))
                ansatzList = M.assocs ans
                newAnsatzList = concat $ map g ansatzList

    symCycleBlockLabelEpsilon :: (Fractional a) => [[Int]] -> PertAnsatzEpsilon a -> PertAnsatzEpsilon a
    symCycleBlockLabelEpsilon l ans =  M.fromListWith f newAnsatzList 
            where 
                norm = 1/ (fromIntegral $ factorial $ length l )
                f = \x y -> x + y 
                ansatzList = M.assocs ans
                newAnsatzList = map (\(a,b) -> (a,norm*b)) $ concat $ map (cyclicSwapBlockLabelEpsilon l) ansatzList 

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

    symAnsatzEpsilon :: (Fractional a) => Symmetry -> PertAnsatzEpsilon a -> PertAnsatzEpsilon a 
    symAnsatzEpsilon (labPair, labAPair, labBlock, labCycle, labBlockCycle) ans = 
        foldr symCycleBlockLabelEpsilon (
            foldr symCycleLabelEpsilon (
                foldr symBlockAnsatzLabelEpsilon (
                    foldr aSymAnsatzLabelEpsilon (
                        foldr symAnsatzLabelEpsilon ans labPair
                    ) labAPair
                ) labBlock
            ) labCycle
        ) labBlockCycle  


    isZero :: (Fractional a, Eq a) => PertAnsatz a -> Bool
    isZero ans = elem 0 $ M.elems ans

    isZeroEpsilon :: (Fractional a, Eq a) => PertAnsatzEpsilon a -> Bool
    isZeroEpsilon ans = elem 0 $ M.elems ans

    symAnsSetPert :: (Eq a, Fractional a) => Symmetry -> [PertAnsatz a] -> [PertAnsatz a]
    symAnsSetPert sym1 [] = []
    symAnsSetPert sym1 (x:xs) 
            | isZero symx = symAnsSetPert sym1 $ rmAnsatz symx xs 
            | otherwise = symx : (symAnsSetPert sym1 $ rmAnsatz symx xs)
                where
                    symx = symAnsatz sym1 x 

    symAnsSetPertEpsilon :: (Eq a, Fractional a) => Symmetry -> [PertAnsatzEpsilon a] -> [PertAnsatzEpsilon a]
    symAnsSetPertEpsilon sym1 [] = []
    symAnsSetPertEpsilon sym1 (x:xs) 
            | isZeroEpsilon symx = symAnsSetPertEpsilon sym1 $ rmAnsatzEpsilon symx xs 
            | otherwise = symx : (symAnsSetPertEpsilon sym1 $ rmAnsatzEpsilon symx xs)
                where
                    symx = symAnsatzEpsilon sym1 x 
    
    rmAnsatz :: Fractional a => PertAnsatz a -> [PertAnsatz a] -> [PertAnsatz a]
    rmAnsatz ans ansList = filter (rmFunction rSet) ansList 
            where 
                rSet = M.keysSet ans 

    rmAnsatzEpsilon :: Fractional a => PertAnsatzEpsilon a -> [PertAnsatzEpsilon a] -> [PertAnsatzEpsilon a]
    rmAnsatzEpsilon ans ansList = filter (rmFunctionEpsilon rSet) ansList 
            where 
                rSet = M.keysSet ans 
                

    rmFunction :: Fractional a => Set.Set (S.Seq [Int]) -> PertAnsatz a -> Bool
    rmFunction rSet ans = (Set.size inter) == 0  
            where
                inter = Set.intersection rSet $ M.keysSet ans 

    rmFunctionEpsilon :: Fractional a => Set.Set (([Int],S.Seq [Int])) -> PertAnsatzEpsilon a -> Bool
    rmFunctionEpsilon rSet ans = (Set.size inter) == 0  
            where
                inter = Set.intersection rSet $ M.keysSet ans 


    --we need functions to construct an PertAnsatz from a given indcex list

    mkPertAns :: (Fractional a) => S.Seq Int -> PertAnsatz a
    mkPertAns seq = M.fromList [(seqList,1)]
                where 
                    chunks = S.chunksOf 2 seq 
                    seqList = (fmap F.toList) chunks 

    mkPertAnsEpsilon :: (Fractional a) => S.Seq Int -> PertAnsatzEpsilon a
    mkPertAnsEpsilon seq = M.fromList [((F.toList epsInds , seqList) ,1)]
                where 
                    epsInds = S.take 4 seq 
                    chunks = S.chunksOf 2 $ S.drop 4 seq 
                    seqList = (fmap F.toList) chunks 

    --the rest of this module is for reading out and is not important for the moment 

    type Var = I.IntMap Rational 

    --now we need tree data types for the 2 ansätze

    data AnsatzTree = EpsilonNode [Int] (S.Seq AnsatzTree) |
                      EpsilonLeaf [Int] Var | 
                      EtaNode [Int] (S.Seq AnsatzTree) | 
                      EtaLeaf [Int] Var deriving (Show)

    getNodeVal :: AnsatzTree -> [Int]
    getNodeVal (EpsilonNode i j) = i
    getNodeVal (EtaNode i j) = i
    getNodeVal (EpsilonLeaf i j) = i
    getNodeVal (EtaLeaf i j) = i

    getSubForest :: AnsatzTree -> AnsatzForest
    getSubForest (EpsilonNode i s) = s
    getSubForest (EtaNode i s) = s
    getSubForest x = error "wrong type of forest"

    isLeaf :: AnsatzTree -> Bool
    isLeaf (EtaLeaf i j) = True
    isLeaf (EpsilonLeaf i j) = True
    isLeaf x = False

    addVarLeaf :: Var -> AnsatzTree -> AnsatzTree
    addVarLeaf var (EtaLeaf i j) = EtaLeaf i (I.unionWith (+) var j)
    addVarLeaf var (EpsilonLeaf i j) = EtaLeaf i (I.unionWith (+) var j)
    addVarLeaf var x = error "wrong ansatzTree"


    type AnsatzForest = S.Seq AnsatzTree

    insertSeqEta :: Var -> AnsatzForest -> S.Seq [Int] -> AnsatzForest 
    insertSeqEta var ans seq 
                    | S.length ans == 0 = mkSeqAnsatzForestEta var seq
                    | seq1 == (getNodeVal ans1) && (isLeaf ans1) = (S.<|) (addVarLeaf var ans1) ansRest  
                    | seq1 == (getNodeVal ans1) = (S.<|) (EtaNode (getNodeVal ans1) (insertSeqEta var (getSubForest ans1) seqRest)) ansRest 
                    | otherwise = (S.<|) ans1 $ insertSeqEta var ansRest seq
                    where
                        ans1 = S.index ans 0
                        seq1 = S.index seq 0
                        ansRest = S.index (S.tails ans ) 1
                        seqRest = S.index (S.tails seq ) 1

    mkSeqAnsatzForestEta :: Var -> S.Seq [Int] -> AnsatzForest
    mkSeqAnsatzForestEta var seq 
                        | S.length seq == 1 = S.singleton $ EtaLeaf (S.index seq 0) var  
                        | otherwise = S.singleton $ EtaNode (S.index seq 0) $ mkSeqAnsatzForestEta var (S.index (S.tails seq ) 1) 


    mkIndListtoEta :: I.IntMap Char -> [Int] -> String
    mkIndListtoEta iMap inds = "[" ++ (intersperse ',' eta) ++ "]"
                where
                    eta = map (\x -> (I.!) iMap x) inds

    indexPermSeqPert :: S.Seq [Int] -> I.IntMap Char -> String 
    indexPermSeqPert a b
                | 2*(S.length a) /= (I.size b) = error "indexList and permutation do  not fit togehter"
                | otherwise = "[" ++ ( concat (intersperse "," indList) ) ++ "]"  
                        where
                            indSeq = fmap (mkIndListtoEta b) a
                            indList = F.toList indSeq

    getRepPert :: (Fractional a) => [PertAnsatz a] -> [S.Seq [Int]]
    getRepPert ans = map (\x -> (M.keys x) !! 0) ans 

    getRepIndsPert :: (Fractional a) => String -> [PertAnsatz a] -> String 
    getRepIndsPert inds ans = "[" ++ (concat (intersperse "," (map (\x -> indexPermSeqPert x iMap) seqList))) ++ "]"
            where 
                seqList = getRepPert ans 
                iMap = mkIndMap inds

    --the evaluation is to slow 

    --we need to write everything into a tree for the evaluation

    evalSeq :: (Num a) => M.Map Int Int -> S.Seq [Int] -> a
    evalSeq iMap seq = S.foldlWithIndex (\z ind val -> z * (etaF iMap val)) 1 seq 

    etaF :: (Num a) => M.Map Int Int -> [Int] -> a
    etaF etaMap [a,b]
            | (i == j) && (i == 0) = 1
            | i == j = -1
            | otherwise = 0
                where
                    (i,j) = ((M.!) etaMap a, (M.!) etaMap b)
    etaF etaMap x = error "wrong list size!"

    evalPertAns :: (Num a) => M.Map Int Int -> PertAnsatz a -> a
    evalPertAns iMap ans = M.foldrWithKey (\ k v s -> (evalSeq iMap k)*v + s) 0 ans 

    --we need all indices for the 14er are metric tensor

    areaList14 :: [[Int]]
    areaList14 = list
        where 
            list = [[a,b,c,d,e,f,g,h,i,j,k,l,p,q] | a <- [0..2], b <- [a+1..3], c <- [a..2], d <- [c+1..3], 
                                                              e <- [0..2], f <- [e+1..3], g <- [e..2], h <- [g+1..3],
                                                              i <- [0..2], j <- [i+1..3], k <- [i..2], l <- [k+1..3],
                                                              p <- [0..3], q <- [p..3], (isAreaSorted a b c d) && (isAreaSorted e f g h) && (isAreaSorted i j k l) && (p<=q) ]
    
    areaList10 :: [[Int]]
    areaList10 = list
        where 
            list = [[a,b,c,d,e,f,g,h,p,q] | a <- [0..2], b <- [a+1..3], c <- [a..2], d <- [c+1..3], 
                                                              e <- [0..2], f <- [e+1..3], g <- [e..2], h <- [g+1..3],
                                                              p <- [0..3], q <- [p..3], (isAreaSorted a b c d) && (isAreaSorted e f g h) && (p<=q) ]
    
    isAreaSorted :: Int -> Int -> Int -> Int -> Bool
    isAreaSorted a b c d 
            | a < c || (a == c && b <= d) = True
            | otherwise = False 

    areaEvalMap14 :: [M.Map Int Int]
    areaEvalMap14 = map M.fromList l 
        where 
            area14 = areaList14 
            l = map (\x -> zip [1,2,3,4,5,6,7,8,9,10,11,12,13,14] x) area14

    areaEvalMap10 :: [M.Map Int Int]
    areaEvalMap10 = map M.fromList l 
        where 
            area10 = areaList10
            l = map (\x -> zip [1,2,3,4,5,6,7,8,9,10] x) area10

    evalFullAns :: [M.Map Int Int] -> [PertAnsatz Rational] -> [[Rational]]
    evalFullAns evalMap ans = map (\f -> map f ans) areaF 
        where
            areaF = map (\x -> evalPertAns x) evalMap

    
            

    

    

    


