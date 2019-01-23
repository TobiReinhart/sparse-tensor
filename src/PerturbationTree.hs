module PerturbationTree (
    Eta, Epsilon, Var, AnsatzNode, Symmetry, mkAllVarsfrom2, printTree, printForest, mkEtaSeq, mkEpsilonSeq, mkTreeEta, mkTreeEpsilon, swapLabelForest, swapBlockLabelForest, swapLabelTree, swapBlockLabelTree, addForests, symAnsatzForest, searchForestEtaSeq, searchForestEpsilonSeq,
    reduceAnsatzEta, reduceAnsatzEpsilon, getVarsTree, getVarsForest, sortIndexEtas, sortForest



) where

    import Data.List
    import qualified Data.IntMap.Strict as I
    import qualified Data.Map.Strict as M
    import qualified Data.Sequence as S
    import Data.Foldable
    import Data.Maybe
    import Data.Tree 
    import Data.Ord

    type Eta = (Int,Int)

    type Epsilon = (Int,Int,Int,Int)

    type Var = I.IntMap Rational 

    isZeroVar :: Var -> Bool
    isZeroVar i 
        | (nub $ I.elems i) == [0] = True 
        | otherwise = False

    showVar :: Var -> String 
    showVar var = tail (concat varString)
                where 
                    pairList = I.assocs var
                    varString = map (\(x,y) -> "+" ++ "(" ++ (show y) ++ "*" ++ "e" ++ (show x) ++ ")") pairList

    mkAllVarsfrom2 :: (Int,Int) -> [Var]
    mkAllVarsfrom2 (i,j) = map (\x -> I.fromList [(x,1)]) [i..j]

    --use the following data type for nodes in Data.Tree !!!

    data AnsatzNode = EpsilonNode Epsilon | EtaNode Eta | EpsilonLeaf Epsilon Var | EtaLeaf Eta Var deriving (Show, Eq, Ord)

    printAnsatzNode :: AnsatzNode -> String
    printAnsatzNode (EtaNode eta) = show eta
    printAnsatzNode (EtaLeaf eta var) = (show eta) ++  (showVar var)
    printAnsatzNode (EpsilonNode eps) = show eps
    printAnstzNode (EpsilonLeaf eps var) = (show eps) ++  (showVar var)

    printTree :: Tree AnsatzNode -> String
    printTree tree = drawTree $ fmap printAnsatzNode tree

    printForest :: Forest AnsatzNode -> String
    printForest forest = drawForest $ map (fmap printAnsatzNode) forest

    sortEta :: Eta -> Eta 
    sortEta (i,j) = (min i j, max i j)

    sortEpsilon :: Epsilon -> Epsilon
    sortEpsilon (i,j,k,l) = (a,b,c,d)
        where
            [a,b,c,d] = sort [i,j,k,l]

    signEpsilon :: Epsilon -> Int
    signEpsilon (i,j,k,l) = (-1)^(length $  filter (==True) [j>i,k>i,l>i,k>j,l>j,l>k])


    mkEtaSeq :: S.Seq Int -> S.Seq Eta 
    mkEtaSeq seq = fmap f seq2
            where 
                seq2 = S.chunksOf 2 seq 
                f = \s -> (S.index s 0, S.index s 1)

    mkEpsilonSeq :: S.Seq Int -> (Epsilon,S.Seq Eta)
    mkEpsilonSeq seq = (eps,etaSeq)
            where 
                eps = (S.index seq 0, S.index seq 1, S.index seq 2, S.index seq 3)
                etaSeq = mkEtaSeq $ S.drop 4 seq 

    --the next step is making a tree from the top Sort Seq

    mkTreeEta :: Var -> S.Seq Eta -> Tree AnsatzNode 
    mkTreeEta var seq 
                | S.length seq == 1 = Node (EtaLeaf (S.index seq 0) var) []  
                | otherwise = Node (EtaNode (S.index seq 0)) [mkTreeEta var (S.drop 1 seq)]

    mkTreeEpsilon :: Var -> (Epsilon,S.Seq Eta) -> Tree AnsatzNode 
    mkTreeEpsilon var (eps,seq) 
                | S.length seq == 0 = Node (EpsilonLeaf eps var) []
                | otherwise = Node (EpsilonNode eps) [mkTreeEta var seq]

    getEta :: AnsatzNode -> Eta
    getEta (EtaNode i ) = i
    getEta (EtaLeaf i j) = i
    getEta x = error "Node is an Epsilon Node!"

    getEpsilon :: AnsatzNode -> Epsilon
    getEpsilon (EpsilonNode i ) = i
    getEpsilon (EpsilonLeaf i j) = i
    getEpsilon x = error "Node is an Eta Node!"

    getVar :: AnsatzNode -> Var
    getVar (EtaLeaf i j) = j
    getVar (EpsilonLeaf i j) = j
    getVar x = error "only leafs contain vars"

    --the next step is symmetrizing the trees

    multVarNode :: Rational -> AnsatzNode -> AnsatzNode
    multVarNode n (EtaLeaf i var) = EtaLeaf i $ I.map ((*) n) var 
    multVarNode n (EpsilonLeaf i var) = EpsilonLeaf i $ I.map ((*) n) var 
    multVarNode n i = i 

    multTree :: Rational -> Tree AnsatzNode -> Tree AnsatzNode 
    multTree n = fmap (multVarNode n) 

    multForest :: Rational -> Forest AnsatzNode -> Forest AnsatzNode
    multForest n = map (multTree n) 

    swapLabelF :: (Int,Int) -> Int -> Int 
    swapLabelF (x,y) z
            | x == z = y
            | y == z = x
            | otherwise = z 

    swapBlockLabelF :: ([Int],[Int]) -> Int -> Int 
    swapBlockLabelF (x,y) z = swapF
            where
                swapF = foldr swapLabelF z $ zip x y
            

    swapLabelEta :: (Int,Int) -> Eta -> Eta
    swapLabelEta l (e1,e2) = sortEta (f e1, f e2)
                where f = swapLabelF l

    swapLabelEpsilon :: (Int,Int) -> Epsilon -> (Int,Epsilon)
    swapLabelEpsilon l (e1,e2,e3,e4) = (epsSign,sortEpsilon newEps)
                    where
                        f = swapLabelF l
                        newEps = (f e1, f e2, f e3, f e4)
                        epsSign = signEpsilon newEps

    swapBlockLabelEta :: ([Int],[Int]) -> Eta -> Eta
    swapBlockLabelEta l (e1,e2) = sortEta (f e1, f e2)
                        where
                            f = swapBlockLabelF l

    swapBlockLabelEpsilon :: ([Int],[Int]) -> Epsilon -> (Int,Epsilon)
    swapBlockLabelEpsilon l (e1,e2,e3,e4) = (epsSign,sortEpsilon newEps)
                    where
                        f = swapBlockLabelF l
                        newEps = (f e1, f e2, f e3, f e4)
                        epsSign = signEpsilon newEps


    swapLabelNodeEta :: (Int,Int) -> AnsatzNode -> AnsatzNode
    swapLabelNodeEta j (EtaNode eta) = EtaNode (swapLabelEta j eta)
    swapLabelNodeEta j (EtaLeaf eta var) = EtaLeaf (swapLabelEta j eta) var


    swapLabelNodeEpsilon :: (Int,Int) -> AnsatzNode -> (Int,AnsatzNode)
    swapLabelNodeEpsilon j (EpsilonNode eps) = (epsSign, EpsilonNode (epsSwap))
                        where
                            (epsSign,epsSwap) = swapLabelEpsilon j eps
    swapLabelNodeEpsilon j (EpsilonLeaf eps var) = (1, EpsilonLeaf epsSwap $ I.map ((*) ( fromIntegral epsSign)) var )
                        where
                         (epsSign,epsSwap) = swapLabelEpsilon j eps

    swapBlockLabelNodeEta :: ([Int],[Int]) -> AnsatzNode -> AnsatzNode
    swapBlockLabelNodeEta j (EtaNode eta) = EtaNode (swapBlockLabelEta j eta)
    swapBlockLabelNodeEta j (EtaLeaf eta var) = EtaLeaf (swapBlockLabelEta j eta) var


    swapBlockLabelNodeEpsilon :: ([Int],[Int]) -> AnsatzNode -> (Int,AnsatzNode)
    swapBlockLabelNodeEpsilon j (EpsilonNode eps) = (epsSign, EpsilonNode (epsSwap))
                        where
                            (epsSign,epsSwap) = swapBlockLabelEpsilon j eps
    swapBlockLabelNodeEpsilon j (EpsilonLeaf eps var) = (1, EpsilonLeaf epsSwap $ I.map ((*) ( fromIntegral epsSign)) var )
                        where
                         (epsSign,epsSwap) = swapBlockLabelEpsilon j eps

    swapLabelTree :: (Int,Int) -> Tree AnsatzNode -> Tree AnsatzNode
    swapLabelTree inds (Node x subTree)
                    | isEpsilonNode x = Node epsNodeSwap $ map (fmap ((swapLabelNodeEta inds).(multVarNode $ fromIntegral epsSign))) subTree
                    | otherwise = fmap (swapLabelNodeEta inds) (Node x subTree)
                        where
                            (epsSign,epsNodeSwap) = swapLabelNodeEpsilon inds x


    swapLabelTree2 :: (Int,Int) -> Tree AnsatzNode -> Tree AnsatzNode 
    swapLabelTree2 j (Node (EtaNode eta) subTree) = Node etaNodeSwap  $ map (swapLabelTree j) subTree
                where
                    etaNodeSwap = swapLabelNodeEta j (EtaNode eta)
    swapLabelTree2 j (Node (EtaLeaf eta var) []) = Node etaLeafSwap []
                where
                    etaLeafSwap = swapLabelNodeEta j (EtaLeaf eta var)
    swapLabelTree2 j (Node (EpsilonNode eps) subTree) = Node epsNodeSwap  $ map ((swapLabelTree j). (fmap (multVarNode $ fromIntegral epsSign))) subTree
                where
                    (epsSign,epsNodeSwap) = swapLabelNodeEpsilon j (EpsilonNode eps)
    swapLabelTree2 j (Node (EpsilonLeaf eps var) [] ) = Node epsLeafSwap []
                where
                    (_,epsLeafSwap) = swapLabelNodeEpsilon j (EpsilonLeaf eps var)
    swapLabelTree2 j x = error "pattern not matched"

    swapBlockLabelTree2 :: ([Int],[Int]) -> Tree AnsatzNode -> Tree AnsatzNode 
    swapBlockLabelTree2 j (Node (EtaNode eta) subTree) = Node etaNodeSwap  $ map (swapBlockLabelTree j) subTree
                where
                    etaNodeSwap = swapBlockLabelNodeEta j (EtaNode eta)
    swapBlockLabelTree2 j (Node (EtaLeaf eta var) []) = Node etaLeafSwap []
                where
                    etaLeafSwap = swapBlockLabelNodeEta j (EtaLeaf eta var)
    swapBlockLabelTree2 j (Node (EpsilonNode eps) subTree) = Node epsNodeSwap  $ map ((swapBlockLabelTree j). (fmap (multVarNode $ fromIntegral epsSign))) subTree
                where
                    (epsSign,epsNodeSwap) = swapBlockLabelNodeEpsilon j (EpsilonNode eps)
    swapBlockLabelTree2 j (Node (EpsilonLeaf eps var) [] ) = Node epsLeafSwap []
                where
                    (_,epsLeafSwap) = swapBlockLabelNodeEpsilon j (EpsilonLeaf eps var)
    swapBlockLabelTree2 j x = error "pattern not matched"

    swapBlockLabelTree :: ([Int],[Int]) -> Tree AnsatzNode -> Tree AnsatzNode
    swapBlockLabelTree inds (Node x subTree)
                    | isEpsilonNode x = Node epsNodeSwap $ map (fmap ((swapBlockLabelNodeEta inds).(multVarNode $ fromIntegral epsSign))) subTree
                    | otherwise = fmap (swapBlockLabelNodeEta inds) (Node x subTree)
                        where
                            (epsSign,epsNodeSwap) = swapBlockLabelNodeEpsilon inds x
    
    sortIndexEtas :: Tree AnsatzNode -> Forest AnsatzNode
    sortIndexEtas ( Node (EtaLeaf eta var) []) = [Node (EtaLeaf eta var) []]
    sortIndexEtas (Node (EtaNode eta) subTree) 
                    | eta <= (getEta.getTopNode.head $ sortedSubTree) = [Node (EtaNode eta) sortedSubTree]
                    | otherwise = newForest
                        where
                            sortedSubTree =  sortForest $ concat $ map sortIndexEtas subTree
                            etaList = map (getEta.getTopNode) sortedSubTree 
                            newEtaNodes = map (\x -> (EtaNode x)) etaList
                            newTrees = map (\(Node x y) -> Node (updateEta (\z -> eta) x) y) sortedSubTree
                            newTreesSorted = map sortIndexEtas newTrees
                            newForest = zipWith (\x y -> addTopNode x y) newEtaNodes newTreesSorted
                        
    --we need to improve groupForests

    groupForests :: Forest AnsatzNode -> Forest AnsatzNode
    groupForests forest 
                | isLeaf node1 = forest
                | isEpsilonNode node1 && isGroupingEps = map (\(Node x y) -> (Node x $ groupForests y)) $ map (groupForestsF) epsGroups
                | (not $ isEpsilonNode node1) && isGroupingEta = map (\(Node x y) -> (Node x $ groupForests y)) $ map (groupForestsF) etaGroups
                | otherwise = forest
                    where
                        node1 = getTopNode.head $ forest
                        epsGroups = groupBy (\x y ->  (getEpsilon.getTopNode $ x) == (getEpsilon.getTopNode $ y)) forest
                        etaGroups = groupBy (\x y -> (getEta.getTopNode $ x) == (getEta.getTopNode $ y)) forest
                        isGroupingEps = not $ (length epsGroups ) == length forest
                        isGroupingEta = not $ ( length etaGroups ) == length forest 

    groupForestsF :: Forest AnsatzNode -> Tree AnsatzNode
    groupForestsF l = Node (getTopNode $ head l) $ sortOn getTopNode $ concat $ map (getSubForest) l

    isLeaf :: AnsatzNode -> Bool
    isLeaf (EtaLeaf i j) = True
    isLeaf (EpsilonLeaf i j) = True
    isLeaf x = False


    addTopNode :: AnsatzNode -> Forest AnsatzNode -> Tree AnsatzNode
    addTopNode x forest = Node x forest

    updateEta :: (Eta -> Eta) -> AnsatzNode -> AnsatzNode
    updateEta f (EtaNode eta)  = EtaNode (f eta) 
    updateEta f (EtaLeaf eta var)  = EtaLeaf (f eta) var 
    updateEta f x = error "Node is an epsilon Node"


    sortForest :: Forest AnsatzNode -> Forest AnsatzNode 
    sortForest ans = groupForests $ sortOn getTopNode ans

    sortSubForestEta :: Tree AnsatzNode -> Tree AnsatzNode
    sortSubForestEta (Node x subForest) = Node x $ sortForest $ concat $ map sortIndexEtas subForest
                
    swapLabelForestEta :: (Int,Int) -> Forest AnsatzNode -> Forest AnsatzNode
    swapLabelForestEta inds ans = sortForest $ concat $ map (sortIndexEtas.(swapLabelTree inds)) ans

    swapLabelForestEpsilon :: (Int,Int) -> Forest AnsatzNode -> Forest AnsatzNode
    swapLabelForestEpsilon inds ans = sortForest $ map (sortSubForestEta.swapLabelTree inds) ans

    swapBlockLabelForestEta :: ([Int],[Int]) -> Forest AnsatzNode -> Forest AnsatzNode
    swapBlockLabelForestEta inds ans = sortForest $ concat $ map (sortIndexEtas.(swapBlockLabelTree inds)) ans

    swapBlockLabelForestEpsilon :: ([Int],[Int]) -> Forest AnsatzNode -> Forest AnsatzNode
    swapBlockLabelForestEpsilon inds ans = sortForest $ map (sortSubForestEta.swapBlockLabelTree inds) ans
                    

    isEpsilonNode :: AnsatzNode -> Bool
    isEpsilonNode (EpsilonLeaf x y) = True
    isEpsilonNode (EpsilonNode x) = True
    isEpsilonNode x = False

    swapLabelForest :: (Int,Int) -> Forest AnsatzNode -> Forest AnsatzNode
    swapLabelForest inds [] = []
    swapLabelForest inds forest
                | isEpsilon = swapLabelForestEpsilon inds forest
                | otherwise = swapLabelForestEta inds forest
                 where
                    isEpsilon = isEpsilonNode $ getTopNode $ head forest

    swapBlockLabelForest :: ([Int],[Int]) -> Forest AnsatzNode -> Forest AnsatzNode
    swapBlockLabelForest inds [] = []
    swapBlockLabelForest inds forest
                | isEpsilon = swapBlockLabelForestEpsilon inds forest
                | otherwise = swapBlockLabelForestEta inds forest
                 where
                    isEpsilon = isEpsilonNode $ getTopNode $ head forest

    --now adding two trees

    addVar2Leaf :: Var -> AnsatzNode -> Maybe AnsatzNode
    addVar2Leaf var (EtaLeaf eta i)
                    | isZero = Nothing
                    | otherwise = Just $ EtaLeaf eta newVar
                    where
                        newVar = I.unionWith (+) var i
                        isZero = isZeroVar newVar 
    addVar2Leaf var (EpsilonLeaf epsilon i)
                    | isZero = Nothing
                    | otherwise = Just $ EpsilonLeaf epsilon newVar
                    where
                        newVar = I.unionWith (+) var i
                        isZero = isZeroVar newVar 
    addVar2Leaf var x = error "can only add to Leaf"

    --we need to remove leafs that are zero !!

    add2Leafs :: AnsatzNode -> [AnsatzNode] -> [AnsatzNode]
    add2Leafs (EtaLeaf eta var) ans 
                | isJust etaPos && (not isNonZero) = part1 ++ (tail part2)
                | isJust etaPos = part1 ++ fromJust (addVar2Leaf var $ head part2) : (tail part2)
                | otherwise = insertBy (comparing getEta) (EtaLeaf eta var) ans
            where
                etaPos = findIndex (\a -> (getEta a) == eta ) ans
                (part1,part2) = splitAt (fromMaybe 0 etaPos) ans
                newAnsNode = (addVar2Leaf var $ head part2)
                isJustEtaPos = isJust etaPos 
                isNonZero = isJust newAnsNode
    add2Leafs (EpsilonLeaf epsilon var) ans 
                | isJust epsPos && (not isNonZero) = part1 ++ (tail part2)
                | isJust epsPos = part1 ++ fromJust (addVar2Leaf var $ head part2) : (tail part2)
                | otherwise = insertBy (comparing getEpsilon) (EpsilonLeaf epsilon var) ans
            where
                epsPos = findIndex (\a -> (getEpsilon a) == epsilon ) ans
                (part1,part2) = splitAt (fromMaybe 0 epsPos) ans
                newAnsNode = (addVar2Leaf var $ head part2)
                isJustEpsPos = isJust epsPos 
                isNonZero = isJust newAnsNode
    add2Leafs a b = error "can only add Leaf to Leafs"

    getTopNode :: Tree AnsatzNode -> AnsatzNode 
    getTopNode (Node x subTree) = x

    getSubForest :: Tree AnsatzNode -> Forest AnsatzNode
    getSubForest (Node x subForest) = subForest

    searchNodeTopLevel :: Forest AnsatzNode -> AnsatzNode -> Maybe Int
    searchNodeTopLevel f (EtaNode eta1) = findIndex (\a -> (getEta (getTopNode a))==eta1) f 
    searchNodeTopLevel f (EpsilonNode eps1) = findIndex (\a -> (getEpsilon (getTopNode a))==eps1) f 
    searchNodeTopLevel f (EtaLeaf eta1 var) = findIndex (\a -> (getEta (getTopNode a))==eta1) f 
    searchNodeTopLevel f (EpsilonLeaf eps1 var) = findIndex (\a -> (getEpsilon (getTopNode a))==eps1) f 



    addTree2Forest :: Tree AnsatzNode -> Forest AnsatzNode -> Forest AnsatzNode
    addTree2Forest (Node (EtaLeaf eta1 var1) []) forest = map (\x -> Node x []) (add2Leafs (EtaLeaf eta1 var1) (map getTopNode forest))
    addTree2Forest (Node (EpsilonLeaf eps1 var1) []) forest = map (\x -> Node x [])  (add2Leafs (EpsilonLeaf eps1 var1) (map getTopNode forest))
    addTree2Forest (Node x subForest) forest
                | isNothing ind = insertBy (comparing getTopNode) (Node x subForest) forest
                | isJustInd && (newSubForest == []) = part1 ++ (tail part2)
                | otherwise = part1 ++ ( Node x newSubForest )  : (tail part2)
                    where
                        ind = searchNodeTopLevel forest x
                        isJustInd = isJust ind
                        (part1,part2) = splitAt (fromMaybe 0 ind) forest
                        newSubForest = filter (not.isZeroTree) (foldr addTree2Forest (getSubForest $ head part2) subForest)

    isZeroTree :: Tree AnsatzNode -> Bool
    isZeroTree (Node (EtaNode i) []) = True
    isZeroTree (Node (EpsilonNode i) []) = True
    isZeroTree t = False


    addForests :: Forest AnsatzNode -> Forest AnsatzNode -> Forest AnsatzNode
    addForests f1 f2 = foldr addTree2Forest f2 f1 

    pairSymForest :: (Int,Int) -> Forest AnsatzNode -> Forest AnsatzNode
    pairSymForest inds forest = addForests swapForest forest
                where
                    swapForest = swapLabelForest inds forest 

    pairASymForest :: (Int,Int) -> Forest AnsatzNode -> Forest AnsatzNode
    pairASymForest inds forest = addForests swapForest forest
                where
                    swapForest = multForest (-1) $ swapLabelForest inds forest 

    blockSymForest :: ([Int],[Int]) -> Forest AnsatzNode -> Forest AnsatzNode
    blockSymForest (i,j) forest = addForests swapForest forest
                where
                   swapForest = swapBlockLabelForest (i,j) forest

    getAllSwaps :: Int -> [[(Int,Int)]]
    getAllSwaps 2 = [[(1,2)]]
    getAllSwaps n = newSwaps ++ combinedSwaps ++ oldSwaps
            where
                newSwaps = fmap (\x -> [x]) $ zip [1..n-1] (repeat n)
                oldSwaps = getAllSwaps (n-1)
                combinedSwaps = fmap (\x -> (++) x ) newSwaps <*> oldSwaps

    swapF :: (Int,Int) -> Int -> Int 
    swapF (i,j) x 
        | x == i = j
        | x == j = i
        | otherwise = x   
        
    swapFList :: (Int,Int) -> [Int] -> [Int]
    swapFList (i,j) x = map (swapF (i,j)) x
            
    getAllPermutations :: Int -> [[Int]]
    getAllPermutations n = map (\x -> x inds) swapFs
        where
            swaps = getAllSwaps n
            swapFs' = map ((map swapFList)) swaps 
            swapFs = map (foldl (.) id) swapFs' 
            inds = take n [1..]

    getAllSwapsLabel :: [Int] -> [[(Int,Int)]]
    getAllSwapsLabel l 
            | n == 2 = [[(l !! 0, l !! 1)]]
            | otherwise = newSwaps ++ combinedSwaps ++ oldSwaps
            where
                n = length l
                newSwaps = fmap (\x -> [x]) $ zip (tail l) (repeat $ head l)
                oldSwaps = getAllSwapsLabel (tail l)
                combinedSwaps = fmap (\x -> (++) x ) newSwaps <*> oldSwaps

    getAllBlockSwapsLabel :: [[Int]] -> [[([Int],[Int])]]
    getAllBlockSwapsLabel l 
            | n == 2 = [[(l !! 0, l !! 1)]]
            | otherwise = newSwaps ++ combinedSwaps ++ oldSwaps
            where
                n = length l
                newSwaps = fmap (\x -> [x]) $ zip (tail l) (repeat $ head l)
                oldSwaps = getAllBlockSwapsLabel (tail l)
                combinedSwaps = fmap (\x -> (++) x ) newSwaps <*> oldSwaps

    getAllSwapsBlockCycleLabel :: [[Int]] -> [[(Int,Int)]]
    getAllSwapsBlockCycleLabel inds = map (concat . (map (\(a,b) -> zip a b))) blockSwaps
                where
                    blockSwaps = getAllBlockSwapsLabel inds 

    cyclicSymForest :: [Int] -> Forest AnsatzNode -> Forest AnsatzNode
    cyclicSymForest inds forest = foldr (addForests) forest allForests
            where
                allSwaps = getAllSwapsLabel inds 
                allForests = map (\sList -> foldr swapLabelForest forest sList) allSwaps

    cyclicBlockSymForest :: [[Int]] -> Forest AnsatzNode -> Forest AnsatzNode
    cyclicBlockSymForest inds forest = foldr (addForests) forest allForests
            where
             allSwaps = getAllBlockSwapsLabel inds 
             allForests = map (\sList -> foldr swapBlockLabelForest forest sList) allSwaps

    --symmetry type (syms, aSyms, blockSyms, cyclicSyms, cyclicBlockSyms)

    type Symmetry = ( [(Int,Int)] , [(Int,Int)] , [([Int],[Int])] , [[Int]], [[[Int]]] )

    symAnsatzForest :: Symmetry -> Forest AnsatzNode -> Forest AnsatzNode 
    symAnsatzForest (sym,asym,blocksym,cyclicsym,cyclicblocksym) ans =
        foldr cyclicBlockSymForest (
            foldr cyclicSymForest (
                foldr blockSymForest (
                    foldr pairASymForest (
                        foldr pairSymForest ans sym
                    ) asym
                ) blocksym
            ) cyclicsym
        ) cyclicblocksym  


    --the last step is searching the forest w.r.t. a given ansatz sequence

    searchForestEtaSeq :: S.Seq Eta -> Forest AnsatzNode -> Bool
    searchForestEtaSeq ((S.:<|) eta S.Empty) forest = elem eta etasForest
            where
                etasForest = map (getEta.getTopNode) forest
    searchForestEtaSeq seq forest 
                | isJust ind1Maybe = searchForestEtaSeq restEtas restForest
                | otherwise = False
                where
                    eta1 = S.index seq 0
                    ind1Maybe = findIndex (\x ->  (getEta.getTopNode) x == eta1 ) forest
                    restEtas = S.index (S.tails seq) 1
                    ind1 = fromMaybe 0 ind1Maybe 
                    restForest = getSubForest $ forest !! ind1

    searchForestEpsilonSeq :: (Epsilon,S.Seq Eta) -> Forest AnsatzNode -> Bool
    searchForestEpsilonSeq ( eps , seq ) forest
            | isJust epsIndMaybe = searchForestEtaSeq seq restForest
            | otherwise = False
             where
                epsIndMaybe = findIndex (\x ->  (getEpsilon.getTopNode) x == eps ) forest
                ind1 = fromMaybe 0 epsIndMaybe
                restForest = getSubForest $ forest !! ind1
        
    addForestEtaSeq :: Symmetry -> (S.Seq Eta, Var) -> Forest AnsatzNode -> Forest AnsatzNode
    addForestEtaSeq sym (seq,var) forest 
                | searchForestEtaSeq seq forest = forest
                | otherwise = addForests symForest forest 
                    where
                        symForest = symAnsatzForest sym $ [mkTreeEta var seq]

    addForestEpsilonSeq :: Symmetry -> ((Epsilon,S.Seq Eta), Var) -> Forest AnsatzNode -> Forest AnsatzNode
    addForestEpsilonSeq sym (epsSeq,var) forest 
                | searchForestEpsilonSeq epsSeq forest = forest
                | otherwise = addForests symForest forest 
                    where
                        symForest = symAnsatzForest sym $ [mkTreeEpsilon var epsSeq]

{-
    reduceAnsatzEta :: Symmetry -> [(S.Seq Eta,Var)] -> Forest AnsatzNode
    reduceAnsatzEta sym [] = [] 
    reduceAnsatzEta sym ((seq,var) : []) = symAnsatzForest sym $ [mkTreeEta var seq]
    reduceAnsatzEta sym ((seq1,var1) : rest) = addForests forest1 $ reduceAnsatzEta sym filteredRest
                where
                    forest1 =  symAnsatzForest sym $ [mkTreeEta var1 seq1]
                    filteredRest = filter (\(x,y) -> not $ searchForestEtaSeq x forest1) rest
-}
    reduceAnsatzEta :: Symmetry -> [(S.Seq Eta,Var)] -> Forest AnsatzNode
    reduceAnsatzEta sym [] = [] 
    reduceAnsatzEta sym seqList = foldr (addForestEtaSeq sym) forest1 seqList
            where
                (seq1,var1) = head seqList
                forest1 = symAnsatzForest sym $ [mkTreeEta var1 seq1]

{-
    reduceAnsatzEpsilon :: Symmetry -> [((Epsilon,S.Seq Eta),Var)] -> Forest AnsatzNode
    reduceAnsatzEpsilon sym [] = [] 
    reduceAnsatzEpsilon sym ((seq,var) : []) = symAnsatzForest sym $ [mkTreeEpsilon var seq]
    reduceAnsatzEpsilon sym ((seq1,var1) : rest) = addForests forest1 $ reduceAnsatzEpsilon sym filteredRest
                where
                    forest1 =  symAnsatzForest sym $ [mkTreeEpsilon var1 seq1]
                    filteredRest = filter (\(x,y) -> not $ searchForestEpsilonSeq x forest1) rest
-}
    reduceAnsatzEpsilon :: Symmetry -> [((Epsilon,S.Seq Eta),Var)] -> Forest AnsatzNode
    reduceAnsatzEpsilon sym [] = [] 
    reduceAnsatzEpsilon sym seqList = foldr (addForestEpsilonSeq sym) forest1 seqList
            where
                (seq1,var1) = head seqList
                forest1 = symAnsatzForest sym $ [mkTreeEpsilon var1 seq1]

    --version 2 seems to be a little bit faster


    getVarsTree :: Tree AnsatzNode -> [[Int]]
    getVarsTree tree = nub $ map (I.keys) $ filter (not.isZeroVar) $ map getVar $ last $ levels tree  

    getVarsForest :: Forest AnsatzNode -> [[Int]]
    getVarsForest forest = nub $ concat $ map getVarsTree forest

    --the second option for adding trees is flatten 1 tree and inserting the lists into the second tree

    flattenTreeEta :: Tree AnsatzNode -> [([Eta],Var)]
    flattenTreeEta (Node (EtaLeaf eta var) []) = [([eta],var)]
    flattenTreeEta (Node (EtaNode eta) subTree) = newList
            where
                rest = concat $ map flattenTreeEta subTree 
                newList = map (\(y,z) -> (insert eta y,z)) rest 
                
    flattenTreeEpsilon :: Tree AnsatzNode -> [((Epsilon,[Eta]),Var)]
    flattenTreeEpsilon (Node (EpsilonLeaf eps var) []) = [((eps,[]),var)]
    flattenTreeEpsilon (Node (EpsilonNode eps) subTree) = newList
            where
                rest = concat $ map flattenTreeEta subTree 
                newList = map (\(y,z) ->  ((eps,y),z)) rest 

