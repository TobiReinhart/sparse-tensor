--improved version of perturbationTree




module PerturbationTree2 (

) where

    import qualified Data.IntMap.Strict as I
    import qualified Data.Map.Strict as M
    import Data.Foldable
    import Data.List 
    import Data.Maybe

    getAllIndsEta :: [Int] -> [[Int]]
    getAllIndsEta [a,b] = [[a,b]]
    getAllIndsEta (x:xs) = res
            where
                l = map (\y -> ([x,y],delete y xs)) xs 
                res = concat $ map (\(a,b) -> (++) a <$> (getAllIndsEta b)) l
    getAllInds x = error "wrong list length"

    getIndsEpsilon :: Int -> [[Int]]
    getIndsEpsilon i = [ [a,b,c,d] | a <- [1..i-3], b <- [a+1..i-2], c <- [b+1..i-1], d <- [c+1..i] ]

    getAllIndsEpsilon :: [Int] -> [[Int]]
    getAllIndsEpsilon l = l3
            where
                s = length l
                l2 = getIndsEpsilon s
                l3 = concat $ map (\x -> (++) x <$> (getAllIndsEta (foldr delete l x))) l2

    filter1Sym :: [Int] -> (Int,Int) -> Bool 
    filter1Sym l (i,j)   
            | first == i = True
            | otherwise = False  
             where
               first = fromJust $ find (\x -> x == i || x == j) l

    filterSym :: [Int] -> [(Int,Int)] -> Bool
    filterSym l inds = and boolList 
            where
               boolList = map (filter1Sym l) inds 

    getEtaInds :: [Int] -> [(Int,Int)] -> [[Int]]
    getEtaInds l sym = filter (\x -> filterSym x sym) $ getAllIndsEta l

    getEpsilonInds :: [Int] -> [(Int,Int)] -> [[Int]]
    getEpsilonInds l sym = filter (\x -> filterSym x sym) $ getAllIndsEpsilon l

    type Eta = (Int,Int)

    type Epsilon = (Int,Int,Int,Int)

    type Var = (Rational,Int) 

    sortEta :: Eta -> Eta 
    sortEta (i,j) = (min i j, max i j)

    sortEpsilon :: Epsilon -> Epsilon
    sortEpsilon (i,j,k,l) = (a,b,c,d)
        where
            [a,b,c,d] = sort [i,j,k,l]

    signEpsilon :: Epsilon -> Int
    signEpsilon (i,j,k,l) = (-1)^(length $  filter (==True) [j>i,k>i,l>i,k>j,l>j,l>k])

    data AnsatzNode = EtaNode Eta | EpsilonNode Epsilon deriving (Show, Eq)

    instance Ord AnsatzNode where
        compare (EtaNode eta1) (EtaNode eta2) = compare eta1 eta2
        compare (EpsilonNode eps1) (EpsilonNode eps2) = compare eps1 eps2
        compare (EtaNode _) (EpsilonNode _) =  GT

    data AnsatzForest = Leaf Var | Forest { forestMap :: (M.Map AnsatzNode AnsatzForest)}| EmptyForest deriving (Show, Eq)

    isLeaf :: AnsatzForest -> Bool
    isLeaf (Leaf var) = True
    isLeaf forest = False

    mapNodes :: (AnsatzNode -> AnsatzNode) -> AnsatzForest -> AnsatzForest
    mapNodes f (Leaf var) = Leaf var
    mapNodes f (Forest m) = Forest $ (M.mapKeys f).(M.map (mapNodes f)) $ m

    mapVars :: (Var -> Var) -> AnsatzForest -> AnsatzForest 
    mapVars f (Leaf var) = Leaf $ f var
    mapVars f ans = ans

    --add 2 sorted forests

    addForests :: AnsatzForest -> AnsatzForest -> AnsatzForest
    addForests ans EmptyForest = ans
    addForests EmptyForest ans = ans 
    addForests (Leaf (fac1,lab1)) (Leaf (fac2,lab2))
            | lab1 /= lab2 = error "should not be necessary to add Forests with different label"
            | fac1 + fac2 == 0 = EmptyForest
            | otherwise = Leaf (fac1+fac2,lab1)
    addForests (Forest m1) (Forest m2) = Forest $ M.unionWith filteredSum m1 m2
            where
                filteredSum = \ a b -> rmZeros $ addForests a b 
           

    rmZeros :: AnsatzForest -> AnsatzForest
    rmZeros (Leaf x) = Leaf x
    remZeros (Forest m) = Forest $ M.filter (/= EmptyForest) m 

    --for adding Forests must be sorted s.t. each Node at one Level is LT each Node at the Level below

    isSortedNode :: AnsatzNode -> AnsatzForest -> Bool
    isSortedNode node (Leaf var) = True
    isSortedNode node (Forest m) 
                | isLeaf (Forest m) = True
                | otherwise = node < nextNode
                where
                    nextNode = head $ M.keys m
    
    {-
    sortForest :: AnsatzForest -> AnsatzForest
    sortForest EmptyForest = EmptyForest
    sortForest (Leaf var) = Leaf var
    sortForest (Forest m) = foldr addForests newM 
            where
                sortedSubForest = (M.map (forestMap.sortForest) m 
                sortedF = \k f ->  if (isSortedNode k f) then f else (swap1Node k f)
                newM = map sortedF $ M.assocs $ sortedSubForest
    
             
    swap1Node :: AnsatzNode -> AnsatzForest -> AnsatzForest
    swap1Node node (Forest m) = foldr addForests EmptyForest $ map (\(k,f) -> Forest $ M.singleton k $ sortForest $ Forest $ M.singleton node f) l 
                where
                    l = M.assocs m 
    
    
    -}

    


