--this module is intended for generating tensorial ansätze (built from the inverse geometry) , i.e symmetrized delta products

module Ansatz (

) where

    import qualified Data.Set as Set
    import qualified Data.Sequence as S

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