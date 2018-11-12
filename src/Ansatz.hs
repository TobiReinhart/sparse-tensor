--this module is intended for generating tensorial ansätze (built from the inverse geometry) , i.e symmetrized delta products

module Ansatz (

) where

    import qualified Data.Set as Set
    import qualified Data.Sequence as S
    import qualified Data.IntMap.Strict as I
    import Data.List 

    --all we need to store in the tree are Ints that label the index position

    data Tree = Leaf Int | Node Int (S.Seq Tree) deriving (Show)

    getRootVal :: Tree -> Int
    getRootVal (Leaf i) = i
    getRootVal (Node j t) = j

    type Forest = S.Seq Tree 

    type Edge = (Int,Int)

    type Root = Int

    --the nest step is constructing the index trees/forests from a givne list of relations amongst the indices

    --carefull as we do not check for cycles!

    mkTree :: Root -> [Edge] -> Tree
    mkTree root edges 
            | level1Edges == [] = Leaf root 
            | otherwise = Node root $ S.fromList $ map (\x -> mkTree x remainingEdges) level1Nodes 
                where
                    level1Nodes = getOutNodes root edges 
                    level1Edges = getOutEdges root edges 
                    remainingEdges = filter (\x -> not $ elem x level1Edges) edges


    getOutEdges :: Root -> [Edge] -> [Edge]
    getOutEdges root edges = filter (\(a,b) -> a == root) edges 

    getOutNodes :: Root -> [Edge] -> [Root]
    getOutNodes root edges = map snd $ getOutEdges root edges 

    sequencetoList :: S.Seq a -> [a]
    sequencetoList seq = foldr (\x y -> x : y) [] seq 

    getEdges :: Tree -> [Edge] 
    getEdges (Leaf i) = []
    getEdges (Node i tree) = (map (\x -> (i,getRootVal x)) treeL) ++ (concat $ map getEdges treeL)
                where 
                    treeL = sequencetoList tree 
                   

    getFirstNodes :: [Edge] -> [Root]
    getFirstNodes edges = Set.elems $ Set.fromList $ map (\(a,b) -> a) $ filter (\(a,b) -> not $ elem a sndNodes) edges
            where
                sndNodes = Set.fromList $ map (\(a,b) -> b) edges

    --for all of the functions above permormance is not really important as they are called only once for constructiing the symmetry forest

    --it is also possible to have single leaves

    mkForest :: [Edge] -> [Root] -> Forest
    mkForest edges leafs = S.fromList $ (map Leaf leafs) ++ (map (\x -> mkTree x edges) $ getFirstNodes edges)

    getEdgesForest :: Forest -> S.Seq [Edge]
    getEdgesForest =  S.mapWithIndex (\x y -> getEdges y) 

    --the nest step is implementing the topSort algorythm for the kind of forests we sue to repreesent the tensorial symmetry

    removeRoot :: Tree -> Forest 
    removeRoot (Leaf i) = S.empty
    removeRoot (Node i forest) = forest 

    getSubForest :: Tree -> Forest 
    getSubForest (Leaf i) = S.empty
    getSubForest (Node i seq) = seq

    removeRootForest :: Int -> Forest -> Forest 
    removeRootForest i forest = (S.><) newForest iSubForest 
                where
                    newForest = S.deleteAt i forest 
                    iSubForest = getSubForest $ S.index forest i 


    getTopSorts :: Forest -> [[Int]]
    getTopSorts forest = concat $ S.mapWithIndex f forest 
            where 
                f = \x y -> topSortAppend (getRootVal y) $ getTopSorts $ removeRootForest x forest 
               
    topSortAppend :: a -> [[a]] -> [[a]]
    topSortAppend i [] = [[i]]
    topSortAppend i list = map (\x -> i : x) list  
    
    --we need a function for converting the inverse permutations obtained from getTopSorts 

    --for the are symmetry of the indices we actually do not need the inverse permutation

    invertPerm :: [Int] -> [Int]
    invertPerm [] = []
    invertPerm l = foldr f [] [1..length l] 
                where 
                    f = \ a b -> (1 + (length $ takeWhile (/= a) l) : b )

    getTopSortsInverse :: Forest -> [[Int]]
    getTopSortsInverse forest = map invertPerm $ getTopSorts forest 

    indexPerm :: [Int] -> I.IntMap Char  -> String 
    indexPerm a b
                | length a /= I.size b = error "indexList and permutation do  not fit togehter"
                | otherwise = "[" ++ (intersperse ',' indList) ++ "]"
                        where
                            indList = map (\x -> (I.!) b x) a   

    mkIndMap :: String -> I.IntMap Char 
    mkIndMap inds = I.fromList $ zip [1..] inds  

    getTopSortsIndex :: Forest -> String ->  [String]
    getTopSortsIndex forest inds = map (\x -> indexPerm x (mkIndMap inds)) $ getTopSorts forest 


    getTopSortsIndexInverse :: Forest -> String ->  [String]
    getTopSortsIndexInverse forest inds = map (\x -> indexPerm x (mkIndMap inds)) $ getTopSortsInverse forest 

    --there might occur zeros due to contraction of symmetric and anti-symmetric indices
    --zeros must be specified by hand

    removeZeros :: [Int] -> [[Int]] -> [[Int]]
    removeZeros  zeroCombs perms = filter (\x -> not $ isInfixOf zeroCombs x) perms  

    getTopSortsIndexNoZero :: Forest -> String -> [Int] -> [String]
    getTopSortsIndexNoZero forest inds zeros = map (\x -> indexPerm x (mkIndMap inds)) filteredsorts 
                where 
                    sorts = getTopSorts forest 
                    filteredsorts = removeZeros zeros sorts

    getTopSortsIndexInverseNoZero :: Forest -> String -> [Int] -> [String]
    getTopSortsIndexInverseNoZero forest inds zeros = map (\x -> indexPerm x (mkIndMap inds)) filteredsorts 
                where 
                    sorts = getTopSortsInverse forest 
                    filteredsorts = removeZeros zeros sorts

    --in total we can proceed as follows 

    -- 1) find all combinations of the lower inds that are canonically sorted w.r.t. the contraction labels (index names) of the lower inds, non inverse

    -- 2) find the sublist that is canonical sorted w.r.t. position (induced from the upper inds) inverse

    -- 3) remove zeros

    getInds :: Forest -> Forest -> String -> [String]
    getInds upperInds lowerInds indlabels =  map (\x -> indexPerm x (mkIndMap indlabels)) totalList
            where 
                upperList = getTopSortsInverse upperInds 
                lowerList = getTopSorts lowerInds
                totalList = intersect upperList lowerList

    --there is a problem with removing zeros !!

    --and a probelm with dublicates

    filterArea :: [Int] -> [Int] -> Bool
    filterArea pos list 
                | a < b && c < d && a < d = True
                | otherwise = False
                where
                    a = list !! ( pos !! 0)
                    b = list !! ( pos !! 1)
                    c = list !! ( pos !! 2)
                    d = list !! ( pos !! 3)
                

    getIndsTest :: Forest ->  String -> [String]
    getIndsTest  lowerInds indlabels =  map (\x -> indexPerm x (mkIndMap indlabels)) totalList
            where 
                lowerList = getTopSorts lowerInds
                totalList = filter (filterArea [0,1,2,3]) lowerList

    --finally a function that filters for an arbitrary given symmetry (for that purpose it is better to work with Sequences)

    topSortSeqAppend :: a -> [S.Seq a] -> [S.Seq a]
    topSortSeqAppend i [] = [S.fromList [i]]
    topSortSeqAppend i list = map (\x -> (S.<|) i x) list  

    getTopSortsSeq :: Forest -> [S.Seq Int]
    getTopSortsSeq forest = concat $ S.mapWithIndex f forest 
            where 
                f = \x y -> topSortSeqAppend (getRootVal y) $ getTopSortsSeq $ removeRootForest x forest 


    filter1Sym :: S.Seq Int -> (Int,Int) -> Bool 
    filter1Sym seq (i,j) 
            | a < b = True
            | otherwise = False  
             where
                a = S.index seq (i-1)
                b = S.index seq (j-1)

    filterSym :: S.Seq Int -> [(Int,Int)] -> Bool
    filterSym seq list = foldl (\x y -> x && y) True boolList
            where 
                boolList = map (filter1Sym seq) list  

    
    getAllInds :: [Edge] -> [Root] -> [(Int,Int)] -> [S.Seq Int]
    getAllInds edges roots symList = filter (\x -> filterSym x symList) topSorts 
            where
                forest = mkForest edges roots 
                topSorts = getTopSortsSeq forest 

    indexPermSeq :: S.Seq Int -> I.IntMap Char -> String 
    indexPermSeq a b
                | S.length a /= I.size b = error "indexList and permutation do  not fit togehter"
                | otherwise = "[" ++ (intersperse ',' indList) ++ "]"
                        where
                            indList = S.foldrWithIndex (\i x y -> ((I.!) b x) : y) [] a

    getAllIndsLabel :: String -> [Edge] -> [Root] -> [(Int,Int)] -> [String]
    getAllIndsLabel inds edges roots symList = map (\x -> indexPermSeq x (mkIndMap inds)) $ getAllInds edges roots symList

    






    --the problem is not solved with this version -> it seems like the symmetry orderings do not commute!!
