--improved version of perturbationTree




module PerturbationTree2 (
    AnsatzForest, AnsatzNode, mkEtaList, mkEpsilonList, Symmetry, reduceAnsatzEta, reduceAnsatzEps, getEtaInds, getEpsilonInds, mkAllVars, symAnsatzForestEta, symAnsatzForestEps, mkForestFromAscList, getEtaForest, getEpsForest, flattenForest, relabelAnsatzForest, getForestLabels, printAnsatz, showAnsatzNode, mapNodes

) where

    import qualified Data.IntMap.Strict as I
    import qualified Data.Map.Strict as M
    import Data.Foldable
    import Data.List 
    import Data.Maybe

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

    sortAnsatzNode :: Ord a => AnsatzNode a ->  AnsatzNode a 
    sortAnsatzNode (Eta x y) = (Eta x' y')
            where
                [x',y'] = sort [x,y] 
    sortAnsatzNode (Epsilon i j k l) = ( Epsilon i' j' k' l')
            where
                [i',j',k',l'] = sort [i,j,k,l]
    sortAnsatzNode (Var x y) = (Var x y)

    isEpsilon :: AnsatzNode a -> Bool
    isEpsilon (Epsilon i j k l) = True
    isEpsilon x = False

    getEpsSign :: Ord a => AnsatzNode a -> Rational 
    getEpsSign (Epsilon i j k l) = (-1)^(length $  filter (==True) [j>i,k>i,l>i,k>j,l>j,l>k])
    getEpsSign x = error "should only be called for Epsilon"

    
    addVars :: AnsatzNode a -> AnsatzNode a -> AnsatzNode a 
    addVars (Var x y) (Var x' y') 
            | y == y' = Var (x + x') y
            | otherwise = error "should only be necessary to add vars with the same label"
    addVars x y = error "can only add Vars"

    multVar :: Rational -> AnsatzNode a -> AnsatzNode a
    multVar x (Var x' y) = Var (x * x') y
    multVar x y = y 

    isZeroVar :: AnsatzNode a -> Bool
    isZeroVar (Var 0 x) = True
    isZeroVar x = False 
   
    data AnsatzForest a = Forest (M.Map a (AnsatzForest a))| Leaf a | EmptyForest  deriving (Show, Eq)

    forestMap :: AnsatzForest a -> M.Map a (AnsatzForest a)
    forestMap (Forest m) = m
    forestMap x = error "Forest is Leaf or Empty"

    mapNodes :: (Ord a, Ord b) => (a -> b) -> AnsatzForest a -> AnsatzForest b
    mapNodes f EmptyForest = EmptyForest
    mapNodes f (Leaf var) = Leaf (f var)
    mapNodes f (Forest m) = Forest $ (M.mapKeys f).(M.map (mapNodes f)) $ m

    --add 2 sorted forests (are all zeros removed ?)

    addForests :: Ord a => (a -> Bool) -> (a -> a -> a) -> AnsatzForest a -> AnsatzForest a -> AnsatzForest a
    addForests isZero f ans EmptyForest = ans
    addForests isZero f EmptyForest ans = ans 
    addForests isZero f (Leaf var1) (Leaf var2)
            | isZero newLeafVal = EmptyForest
            | otherwise = Leaf newLeafVal
            where
                newLeafVal = (f var1 var2)
    addForests isZero f (Forest m1) (Forest m2) 
            | M.null newMap = EmptyForest
            | otherwise = Forest newMap
             where
                sum = \ a b -> addForests isZero f a b 
                newMap = M.filter (/= EmptyForest) $ M.unionWith sum m1 m2

    --flatten Forest to AscList Branches
    
    flattenForest :: Ord a => AnsatzForest a -> [[a]]
    flattenForest EmptyForest = [[]]
    flattenForest (Leaf var) = [[var]]
    flattenForest (Forest m) = concat l 
            where
                mPairs = M.assocs m 
                l = fmap (\(k,v) ->  fmap (insert k) $ flattenForest v) mPairs  
                
    mkForestFromAscList :: Ord a => [a] -> AnsatzForest a 
    mkForestFromAscList [] = EmptyForest
    mkForestFromAscList [x] = Leaf x
    mkForestFromAscList (x:xs) = Forest $ M.singleton x $ mkForestFromAscList xs
    
    sortForest :: Ord a => (a -> Bool) -> AnsatzForest a -> AnsatzForest a
    sortForest isZero f = foldr (addForests isZero const) EmptyForest fList 
                where
                    fList = map mkForestFromAscList $ flattenForest f

    swapLabelF :: Ord a =>  (a,a) -> a -> a 
    swapLabelF (x,y) z
            | x == z = y
            | y == z = x
            | otherwise = z 

    swapBlockLabelF :: Ord a => ([a],[a]) -> a -> a 
    swapBlockLabelF (x,y) z = swapF
            where
                swapF = foldr swapLabelF z $ zip x y
            
    swapLabelNode :: Ord a => (a,a) -> AnsatzNode a -> AnsatzNode a
    swapLabelNode inds (Eta i j) = Eta (f i) (f j)
                where
                    f = swapLabelF inds
    swapLabelNode inds (Epsilon i j k l) = Epsilon (f i) (f j) (f k) (f l)
                where
                    f = swapLabelF inds
    swapLabelNode inds (Var x y) = Var x y


    swapBlockLabelNode :: Ord a => ([a],[a]) -> AnsatzNode a -> AnsatzNode a
    swapBlockLabelNode inds (Eta i j) = Eta (f i) (f j)
                where
                    f = swapBlockLabelF inds
    swapBlockLabelNode inds (Epsilon i j k l) = Epsilon (f i) (f j) (f k) (f l)
                where
                    f = swapBlockLabelF inds
    swapBlockLabelNode inds (Var x y) = Var x y

    canonicalizeAnsatzEta :: Ord a => AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    canonicalizeAnsatzEta  = mapNodes sortAnsatzNode

    canonicalizeAnsatzEpsilon :: Ord a => AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    canonicalizeAnsatzEpsilon EmptyForest = EmptyForest
    canonicalizeAnsatzEpsilon (Leaf var) = Leaf var
    canonicalizeAnsatzEpsilon (Forest m) = Forest newMap
                where
                    newMap = M.mapKeys sortAnsatzNode  $ M.mapWithKey (\ k v -> mapNodes ((multVar $ getEpsSign k).sortAnsatzNode) v ) m
            

    swapLabelEta :: Ord a => (a,a) -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    swapLabelEta inds ans = (sortForest isZeroVar).canonicalizeAnsatzEta $ swapAnsatz
            where
                f = swapLabelNode inds 
                swapAnsatz = mapNodes f ans

    swapLabelEps :: Ord a => (a,a) -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    swapLabelEps inds ans = (sortForest isZeroVar).canonicalizeAnsatzEpsilon $ swapAnsatz
            where
                f = swapLabelNode inds 
                swapAnsatz = mapNodes f ans           

    swapBlockLabelEta :: Ord a => ([a],[a]) -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    swapBlockLabelEta inds ans = (sortForest isZeroVar).canonicalizeAnsatzEta $ swapAnsatz
            where
                f = swapBlockLabelNode inds 
                swapAnsatz = mapNodes f ans

    swapBlockLabelEps :: Ord a => ([a],[a]) -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    swapBlockLabelEps inds ans = (sortForest isZeroVar).canonicalizeAnsatzEpsilon $ swapAnsatz
            where
                f = swapBlockLabelNode inds 
                swapAnsatz = mapNodes f ans
            

    pairSymForestEta :: Ord a => (a,a) -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    pairSymForestEta inds ans = (addForests isZeroVar addVars) ans $ swapLabelEta inds ans 

    pairSymForestEps :: Ord a => (a,a) -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    pairSymForestEps inds ans = (addForests isZeroVar addVars) ans $ swapLabelEps inds ans 

    pairASymForestEta :: Ord a => (a,a) -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    pairASymForestEta inds ans = (addForests isZeroVar addVars) ans $ mapNodes (multVar (-1)) $ swapLabelEta inds ans 

    pairASymForestEps :: Ord a => (a,a) -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    pairASymForestEps inds ans = (addForests isZeroVar addVars) ans $ mapNodes (multVar (-1)) $ swapLabelEps inds ans 

    pairBlockSymForestEta :: Ord a => ([a],[a]) -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    pairBlockSymForestEta inds ans = (addForests isZeroVar addVars) ans $ swapBlockLabelEta inds ans 

    pairBlockSymForestEps :: Ord a => ([a],[a]) -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    pairBlockSymForestEps inds ans = (addForests isZeroVar addVars) ans $ swapBlockLabelEps inds ans 

    pairBlockASymForestEta :: Ord a => ([a],[a]) -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    pairBlockASymForestEta inds ans = (addForests isZeroVar addVars) ans $ mapNodes (multVar (-1)) $ swapBlockLabelEta inds ans

    pairBlockASymForestEps :: Ord a => ([a],[a]) -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    pairBlockASymForestEps inds ans = (addForests isZeroVar addVars) ans $ mapNodes (multVar (-1)) $ swapBlockLabelEps inds ans
    
    cyclicSymForestEta :: Ord a => [a] -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    cyclicSymForestEta inds ans = foldr (\y x -> (addForests isZeroVar addVars) x $ swapBlockLabelEta y ans ) ans perms
            where
                perms = map (\a -> (inds,a)) $ tail $ permutations inds 

    cyclicSymForestEps :: Ord a => [a] -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    cyclicSymForestEps inds ans = foldr (\y x -> (addForests isZeroVar addVars) x $ swapBlockLabelEps y ans ) ans perms
            where
                perms = map (\a -> (inds,a)) $ tail $ permutations inds 


    cyclicBlockSymForestEta :: Ord a => [[a]] -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    cyclicBlockSymForestEta inds ans = foldr (\y x -> (addForests isZeroVar addVars) x $ swapBlockLabelEta y ans ) ans perms
            where
                perms = map (\a -> ( concat inds, concat a)) $ tail $ permutations inds 

    cyclicBlockSymForestEps :: Ord a => [[a]] -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    cyclicBlockSymForestEps inds ans = foldr (\y x -> (addForests isZeroVar addVars) x $ swapBlockLabelEps y ans ) ans perms
            where
                perms = map (\a -> ( concat inds, concat a)) $ tail $ permutations inds 

    type Symmetry a = ( [(a,a)] , [(a,a)] , [([a],[a])] , [[a]], [[[a]]] )

    symAnsatzForestEta :: Ord a => Symmetry a -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a) 
    symAnsatzForestEta (sym,asym,blocksym,cyclicsym,cyclicblocksym) ans =
        foldr cyclicBlockSymForestEta (
            foldr cyclicSymForestEta (
                foldr pairBlockSymForestEta (
                    foldr pairASymForestEta (
                        foldr pairSymForestEta ans sym
                    ) asym
                ) blocksym
            ) cyclicsym
        ) cyclicblocksym  

    symAnsatzForestEps :: Ord a => Symmetry a -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a) 
    symAnsatzForestEps (sym,asym,blocksym,cyclicsym,cyclicblocksym) ans =
          foldr cyclicBlockSymForestEps (
              foldr cyclicSymForestEps (
                  foldr pairBlockSymForestEps (
                      foldr pairASymForestEps (
                          foldr pairSymForestEps ans sym
                      ) asym
                  ) blocksym
              ) cyclicsym
          ) cyclicblocksym  
    

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

    isElem :: Ord a => [AnsatzNode a] -> AnsatzForest (AnsatzNode a) -> Bool
    isElem [] x = True
    isElem x (Leaf y) = True
    isElem x EmptyForest = False 
    isElem  (x:xs) (Forest m) 
                | isJust mForest = isElem xs $ fromJust mForest
                | otherwise = False
                where
                    mForest = M.lookup x m

    reduceAnsatzEta :: Ord a => Symmetry a -> [[AnsatzNode a]] -> AnsatzForest (AnsatzNode a)
    reduceAnsatzEta sym [] = EmptyForest
    reduceAnsatzEta sym l = foldr addOrRem EmptyForest l
            where
                addOrRem = \ans f -> if (isElem ans f) then f else (addForests isZeroVar addVars) f (symAnsatzForestEta sym $ mkForestFromAscList ans)

    reduceAnsatzEps :: Ord a => Symmetry a -> [[AnsatzNode a]] -> AnsatzForest (AnsatzNode a)
    reduceAnsatzEps sym [] = EmptyForest
    reduceAnsatzEps sym l = foldr addOrRem EmptyForest l
            where
                addOrRem = \ans f -> if (isElem ans f) then f else (addForests isZeroVar addVars) f (symAnsatzForestEps sym $ mkForestFromAscList ans)


    getEtaForest :: [Int] -> [(Int,Int)] -> Int -> Symmetry Int -> AnsatzForest (AnsatzNode Int)
    getEtaForest inds filters label1 syms = reduceAnsatzEta syms allForests
                where
                    allInds = getEtaInds inds filters
                    allVars = mkAllVars label1 
                    allForests = zipWith mkEtaList allVars allInds

    getEpsForest :: [Int] -> [(Int,Int)] -> Int -> Symmetry Int -> AnsatzForest (AnsatzNode Int)
    getEpsForest inds filters label1 syms = reduceAnsatzEps syms allForests
                where
                    allInds = getEpsilonInds inds filters
                    allVars = mkAllVars label1 
                    allForests = zipWith mkEpsilonList allVars allInds

    getLeafVals :: AnsatzForest a -> [a]
    getLeafVals (Leaf var) = [var]
    getLeafVals (Forest m) = rest
            where
                rest = concatMap getLeafVals $ M.elems m

    getVarLabels :: AnsatzNode a -> Int
    getVarLabels (Var i j) = j
    getVarLabels x = error "only can get label of node"

    getForestLabels :: AnsatzForest (AnsatzNode a) -> [Int]
    getForestLabels ans = nub $ map getVarLabels $ getLeafVals ans

    relabelVar :: (Int -> Int) -> AnsatzNode a -> AnsatzNode a
    relabelVar f (Var i j) = Var i (f j)
    relabelVar f x = x

    relabelAnsatzForest :: Ord a => AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    relabelAnsatzForest ans = mapNodes update ans
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
    shiftAnsatzForest (Leaf var) = Leaf var 
    shiftAnsatzForest (Forest m) = Forest $ M.map shiftAnsatzForest shiftedForestMap
            where
                mapElems f (Forest m) =  Forest $ M.mapKeys f m
                mapElems f (Leaf var) = Leaf (f var)
                shiftedForestMap = M.map (\f -> mapElems (\x -> "     " ++ x) f) m

    printAnsatz ::  AnsatzForest String -> [String]
    printAnsatz (Leaf var) = [var] 
    printAnsatz (Forest m) = map (init.unlines) subForests
            where
                shiftedForest = shiftAnsatzForest (Forest m)
                pairs = M.assocs $ forestMap shiftedForest
                subForests = map (\(k,v) -> k : (printAnsatz v)) pairs
                