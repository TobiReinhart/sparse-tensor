--improved version of perturbationTree2




module PerturbationTree3 (
    AnsatzForest(..), AnsatzNode(..), mkEtaList, mkEpsilonList, Symmetry, getEtaInds, getEpsilonInds, mkAllVars, mkForestFromAscList, getEtaForest, getEpsForest, flattenForest, relabelAnsatzForest, getForestLabels, printAnsatz, showAnsatzNode, addVars,
    symAnsatzNodes

) where

    import qualified Data.IntMap.Strict as I
    import qualified Data.Map.Strict as M
    import BinaryTree
    import Data.Foldable
    import Data.List 
    import Data.Maybe
    import Data.List

    --getAllInds might be better with S.Seq

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

    data AnsatzNode a = Epsilon a a a a | Eta a a | Var Rational Int  deriving (Show, Eq, Ord)

    mkAllVars :: Int -> [AnsatzNode a] 
    mkAllVars i = map (Var 1) [i..]

    sortList :: Ord a => [a] -> [a]
    sortList [] = [] 
    sortList (x:xs) = insert x $ sortList xs 

    sortAnsatzNode :: Ord a => AnsatzNode a ->  AnsatzNode a 
    sortAnsatzNode (Eta x y) = (Eta x' y')
            where
                [x',y'] = sortList [x,y] 
    sortAnsatzNode (Epsilon i j k l) = ( Epsilon i' j' k' l')
            where
                [i',j',k',l'] = sortList [i,j,k,l]
    sortAnsatzNode (Var x y) = (Var x y)

    isEpsilon :: AnsatzNode a -> Bool
    isEpsilon (Epsilon i j k l) = True
    isEpsilon x = False

    getEpsSign :: Ord a => AnsatzNode a -> Rational 
    getEpsSign (Epsilon i j k l) = (-1)^(length $  filter (==True) [j>i,k>i,l>i,k>j,l>j,l>k])
    getEpsSign x = error "should only be called for Epsilon"

    
    addVars :: AnsatzNode a -> AnsatzNode a -> Maybe (AnsatzNode a) 
    addVars (Var x y) (Var x' y') 
            | rightVar && (val == 0) = Nothing 
            | rightVar = Just (Var val y) 
                where
                    rightVar = y == y'
                    val = x + x'
            
    multVar :: Rational -> AnsatzNode a -> AnsatzNode a
    multVar x (Var x' y) = Var (x * x') y
    multVar x y = y 
   
    data AnsatzForest a = Forest (BiTree a (AnsatzForest a))| FLeaf a | EmptyForest  deriving (Show, Eq)

    mapLeafs :: (a -> a) -> AnsatzForest a -> AnsatzForest a
    mapLeafs f EmptyForest = EmptyForest 
    mapLeafs f (FLeaf x) = FLeaf (f x) 
    mapLeafs f (Forest m) = Forest $ mapElemsTree (mapLeafs f) m 

    filterForest :: (AnsatzForest a -> Bool) -> AnsatzForest a -> AnsatzForest a
    filterForest f (Forest m) = Forest (filterTree f m) 
    filterForest f t = t 


    flattenForest :: Ord a => AnsatzForest a -> [[a]]
    flattenForest EmptyForest = [[]]
    flattenForest (FLeaf var) = [[var]]
    flattenForest (Forest m) = concat l 
            where
                mPairs = toAscList m 
                l = fmap (\(k,v) ->  fmap (insert k) $ flattenForest v) mPairs  
                
    mkForestFromAscList :: Ord a => [a] -> AnsatzForest a 
    mkForestFromAscList [] = EmptyForest
    mkForestFromAscList [x] = FLeaf x
    mkForestFromAscList (x:xs) = Forest $ fromAscListWithLength 1 [(x, mkForestFromAscList xs)]
  

    swapLabelF :: Ord a =>  (a,a) -> a -> a 
    swapLabelF (x,y) z
            | x == z = y
            | y == z = x
            | otherwise = z 

    swapBlockLabelMap :: Ord a => ([a],[a]) -> M.Map a a
    swapBlockLabelMap (x,y) = swapF 
            where
                swapF = M.fromList $ (zip x y)++(zip y x)
            
    swapLabelNode :: Ord a => (a,a) -> AnsatzNode a -> AnsatzNode a
    swapLabelNode inds (Eta i j) = Eta (f i) (f j)
                where
                    f = swapLabelF inds
    swapLabelNode inds (Epsilon i j k l) = Epsilon (f i) (f j) (f k) (f l)
                where
                    f = swapLabelF inds
    swapLabelNode inds (Var x y) = Var x y

    swapLabelNodes :: Ord a => (a,a) -> [AnsatzNode a] -> [AnsatzNode a]
    swapLabelNodes inds l = map (swapLabelNode inds) l 


    swapBlockLabelNode :: Ord a => M.Map a a -> AnsatzNode a -> AnsatzNode a
    swapBlockLabelNode swapF (Eta i j) = Eta (f i) (f j)
                where
                    f = \z -> fromMaybe z $ M.lookup z swapF 
    swapBlockLabelNode swapF (Epsilon i j k l) = Epsilon (f i) (f j) (f k) (f l)
                where
                    f = \z -> fromMaybe z $ M.lookup z swapF 
    swapBlockLabelNode sapF (Var x y) = Var x y

    swapBlockLabelNodes :: Ord a => M.Map a a  -> [AnsatzNode a] -> [AnsatzNode a]
    swapBlockLabelNodes inds l = map (swapBlockLabelNode inds) l 


    multAnsatzNodes :: Rational -> [AnsatzNode a] -> [AnsatzNode a]
    multAnsatzNodes s [x] = [multVar s x]
    multAnsatzNodes s (x:xs) = x : (multAnsatzNodes s xs)

    canonicalizeAnsatzNodes :: Ord a => [AnsatzNode a] -> [AnsatzNode a]
    canonicalizeAnsatzNodes [x] = [x]
    canonicalizeAnsatzNodes (x:xs) 
                    | isEpsilon x = sortList $ multAnsatzNodes s $ map sortAnsatzNode (x:xs)
                    | otherwise = sortList $ map sortAnsatzNode (x:xs)
                            where
                                s = getEpsSign x

    pairSymAnsatzNodes :: Ord a => (a,a) -> [[AnsatzNode a]] -> [[AnsatzNode a]]
    pairSymAnsatzNodes inds l = l ++ swapped 
            where
                swapped = map ( canonicalizeAnsatzNodes . (swapLabelNodes inds)) l 

    pairASymAnsatzNodes :: Ord a => (a,a) -> [[AnsatzNode a]] -> [[AnsatzNode a]]
    pairASymAnsatzNodes inds l = l ++ swapped 
            where
                swapped =  map ( (multAnsatzNodes (-1)) . canonicalizeAnsatzNodes . (swapLabelNodes inds)) l 

    pairBlockSymAnsatzNodes :: Ord a => M.Map a a -> [[AnsatzNode a]] -> [[AnsatzNode a]]
    pairBlockSymAnsatzNodes inds l = l ++ swapped 
            where
                swapped = map ( canonicalizeAnsatzNodes . (map (swapBlockLabelNode inds))) l 

    pairBlockASymAnsatzNodes :: Ord a => M.Map a a -> [[AnsatzNode a]] -> [[AnsatzNode a]]
    pairBlockASymAnsatzNodes inds l = l ++ swapped 
            where
                swapped =  map ( (multAnsatzNodes (-1)) . canonicalizeAnsatzNodes . (swapBlockLabelNodes inds)) l 

    cyclicSymAnsatzNodes :: Ord a => [a] -> [[AnsatzNode a]] -> [[AnsatzNode a]]
    cyclicSymAnsatzNodes inds l = l ++ newL
                where
                    perms = map (\a -> M.fromList (zip inds a)) $ tail $ permutations inds
                    newL = concat $ map (\y -> map (\x -> canonicalizeAnsatzNodes $ swapBlockLabelNodes x y) perms ) l

    cyclicBlockSymAnsatzNodes :: Ord a => [[a]] -> [[AnsatzNode a]] -> [[AnsatzNode a]]
    cyclicBlockSymAnsatzNodes inds l = l ++ newL
                where
                    perms = map (\a -> M.fromList $ zip (concat inds) (concat a)) $ tail $ permutations inds
                    newL = concat $ map (\y -> map (\x -> canonicalizeAnsatzNodes $ swapBlockLabelNodes x y) perms ) l


    type Symmetry a = ( [(a,a)] , [(a,a)] , [([a],[a])] , [[a]], [[[a]]] )

    symAnsatzNodes :: (Ord a, Show a) => Symmetry a -> [[AnsatzNode a]] -> [[AnsatzNode a]] 
    symAnsatzNodes (sym,asym,blocksym,cyclicsym,cyclicblocksym) ans =
        foldr cyclicBlockSymAnsatzNodes (
            foldr cyclicSymAnsatzNodes (
                foldr pairBlockSymAnsatzNodes (
                    foldr pairASymAnsatzNodes (
                        foldr pairSymAnsatzNodes ans sym
                    ) asym
                ) blockSymMap
            ) cyclicsym
        ) cyclicblocksym  
        where
            blockSymMap = map swapBlockLabelMap blocksym

    --if symmetrizing an ansatz this way is too slow we can symmetrize by using a map and then insert in the big tree 
    
    mkEtaList :: AnsatzNode a -> [a] -> [AnsatzNode a]
    mkEtaList var [] = [var] 
    mkEtaList var x = (Eta a b) : (mkEtaList var rest) 
            where
                [a,b] = take 2 x
                rest = drop 2 x

    mkEpsilonList :: AnsatzNode a -> [a] -> [AnsatzNode a]
    mkEpsilonList var [] = [var]
    mkEpsilonList var x = (Epsilon i j k l) : (mkEtaList var rest) 
            where
                [i,j,k,l] = take 4 x
                rest = drop 4 x
    

    --look up a 1d Forest (obtained from the index list) in the given Forest

    addToForest :: (Eq a, Ord a) => [AnsatzNode a] -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    addToForest [] t = t 
    addToForest x EmptyForest = mkForestFromAscList x
    addToForest [x] (FLeaf y)
                | isJust val = FLeaf (fromJust val)
                | otherwise = EmptyForest
                 where 
                    val = addVars x y
    addToForest (x:xs) (Forest m) = Forest $ insertTreeWithSwapped mkForestFromAscList (\x' y -> filterForest (/= EmptyForest) $ addToForest x' y) x xs m
                
    
    
    -- | isJust mForest = filterForest ( /= EmptyForest) $ addToForest xs $ fromJust mForest 
    -- | otherwise = mkForestFromAscList (x:xs)
      --          where
        --            mForest = lookupTree x m


    isElem :: Ord a => [AnsatzNode a] -> AnsatzForest (AnsatzNode a) -> Bool
    isElem [] x = True
    isElem x (FLeaf y) = True
    isElem x EmptyForest = False 
    isElem  (x:xs) (Forest m) 
                | isJust mForest = isElem xs $ fromJust mForest
                | otherwise = False
                where
                    mForest = lookupTree x m

    reduceAnsatz :: (Ord a, Show a) => Symmetry a -> [[AnsatzNode a]] -> AnsatzForest (AnsatzNode a)
    reduceAnsatz sym [] = EmptyForest
    reduceAnsatz sym l = foldr addOrRem EmptyForest l
            where
                addOrRem = \ans f -> if (isElem ans f) then f else foldr addToForest f (symAnsatzNodes sym [ans])

    getEtaForest :: [Int] -> [(Int,Int)] -> Int -> Symmetry Int -> AnsatzForest (AnsatzNode Int)
    getEtaForest inds filters label1 syms = reduceAnsatz syms allForests
                where
                    allInds = getEtaInds inds filters
                    allVars = mkAllVars label1 
                    allForests = zipWith mkEtaList allVars allInds

    getEpsForest :: [Int] -> [(Int,Int)] -> Int -> Symmetry Int -> AnsatzForest (AnsatzNode Int)
    getEpsForest inds filters label1 syms = reduceAnsatz syms allForests
                where
                    allInds = getEpsilonInds inds filters
                    allVars = mkAllVars label1 
                    allForests = zipWith mkEpsilonList allVars allInds

    getLeafVals :: AnsatzForest a -> [a]
    getLeafVals (FLeaf var) = [var]
    getLeafVals (Forest m) = rest
            where
                rest = concatMap getLeafVals $ toList m

    getVarLabels :: AnsatzNode a -> Int
    getVarLabels (Var i j) = j
    getVarLabels x = error "only can get label of node"

    getForestLabels :: AnsatzForest (AnsatzNode a) -> [Int]
    getForestLabels ans = nub $ map getVarLabels $ getLeafVals ans

    relabelVar :: (Int -> Int) -> AnsatzNode a -> AnsatzNode a
    relabelVar f (Var i j) = Var i (f j)
    relabelVar f x = x

    relabelAnsatzForest :: Ord a => AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    relabelAnsatzForest ans = mapLeafs update ans
            where
                vars = getForestLabels ans 
                relabMap = I.fromList $ zip vars [1..]
                update = relabelVar ((I.!) relabMap) 

    showAnsatzNode :: Show a => AnsatzNode a -> String 
    showAnsatzNode (Var i j) = (show i) ++ "*" ++ "x" ++ (show j)
    showAnsatzNode (Eta i b) = show (i,b)
    showAnsatzNode (Epsilon i j k l) = show (i,j,k,l)
    

    shiftAnsatzForest :: AnsatzForest String -> AnsatzForest String
    shiftAnsatzForest EmptyForest = EmptyForest
    shiftAnsatzForest (FLeaf var) = FLeaf var 
    shiftAnsatzForest (Forest m) = Forest $ fmap shiftAnsatzForest shiftedForestMap
            where
                mapElems f (Forest m) =  Forest $ mapKeysTree f m
                mapElems f (FLeaf var) = FLeaf (f var)
                shiftedForestMap = fmap (\f -> mapElems (\x -> "     " ++ x) f) m

    printAnsatz ::  AnsatzForest String -> [String]
    printAnsatz (FLeaf var) = [var] 
    printAnsatz (Forest m) = map (init.unlines) subForests
            where
                shiftedForest = shiftAnsatzForest (Forest m)
                pairs = toAscList $ forestMap shiftedForest
                forestMap (Forest m) = m
                subForests = map (\(k,v) -> k : (printAnsatz v)) pairs
                