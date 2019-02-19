--improved version of perturbationTree

--seems to be the best soultion !!!


module PerturbationTree2 (
    AnsatzForest(..), AnsatzNode(..), mkEtaList, mkEpsilonList, Symmetry, reduceAnsatzEta, reduceAnsatzEps, getEtaInds, getEpsilonInds, mkAllVars, symAnsatzForestEta, symAnsatzForestEps, mkForestFromAscList, getEtaForest, getEpsForest, flattenForest, relabelAnsatzForest, getForestLabels, printAnsatz, showAnsatzNode, mapNodes, addForests, isZeroVar, addVars,
    epsMap, evalAnsatzForest, evalAllAnsatzForest, evalAllMatrixSp,
    evalAnsatzForestList, getLeafVals, ansatzRank, ansatzKernel, evalAllMatrix, ansatzImage, ansatzHasMatrix, ansatzHasRR,
    --ansatzHasLU, testBasisLabels, ansatzLinMatrix, ansatzLinLU,
    filterList10_1, symList10_1, areaEvalMap10_1,
    getPivots, actOnRightRest, rowReduce, ansatzBasisLabels

) where

    import qualified Data.IntMap.Strict as I
    import qualified Data.Map.Strict as M
    import Data.Foldable
    import Data.List 
    import Data.Maybe
    import Data.List
    import qualified Data.Eigen.Matrix as Mat 
    import qualified Data.Eigen.SparseMatrix as Sparse
    import qualified Data.Eigen.LA as Sol 
    import qualified Data.Matrix as HasMat 
    import qualified Data.Vector as Vec
    import qualified Numeric.LinearAlgebra as Lin 
    import qualified Numeric.LinearAlgebra.Data as LinDat

    

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

    --filter the are metric symmetries

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

    data AnsatzNode a = Epsilon a a a a | Eta a a | Var Rational Int  deriving (Show, Read,  Eq, Ord)

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

    
    addVars :: Show a => AnsatzNode a -> AnsatzNode a -> AnsatzNode a 
    addVars (Var x y) (Var x' y') 
            | y == y' = Var (x + x') y
            | otherwise = error $ "should only be necessary to add vars with the same label: tried to add:" ++ "  " ++ (show y) ++ "  and   " ++ (show y')
    addVars x y = error "can only add Vars"

    multVar :: Rational -> AnsatzNode a -> AnsatzNode a
    multVar x (Var x' y) = Var (x * x') y
    multVar x y = y 

    isZeroVar :: AnsatzNode a -> Bool
    isZeroVar (Var 0 x) = True
    isZeroVar x = False 
   
    data AnsatzForest a = Forest (M.Map a (AnsatzForest a))| Leaf a | EmptyForest  deriving (Show, Read, Eq)

    forestMap :: AnsatzForest a -> M.Map a (AnsatzForest a)
    forestMap (Forest m) = m
    forestMap x = error "Forest is Leaf or Empty"

    --mapNodes requires resorting

    mapNodes :: (Ord a, Ord b) => (a -> b) -> AnsatzForest a -> AnsatzForest b
    mapNodes f EmptyForest = EmptyForest
    mapNodes f (Leaf var) = Leaf (f var)
    mapNodes f (Forest m) = Forest $ (M.mapKeys f).(M.map (mapNodes f)) $ m

    multForest :: (a -> a) -> AnsatzForest a -> AnsatzForest a
    multForest f EmptyForest = EmptyForest
    multForest f (Leaf var) = Leaf (f var)
    multForest f (Forest m) = Forest $ (M.map (multForest f)) $ m


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

    --there is a problem        

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


    swapBlockLabelNode :: Ord a => M.Map a a -> AnsatzNode a -> AnsatzNode a
    swapBlockLabelNode swapF (Eta i j) = Eta (f i) (f j)
                where
                    f = \z -> fromMaybe z $ M.lookup z swapF 
    swapBlockLabelNode swapF (Epsilon i j k l) = Epsilon (f i) (f j) (f k) (f l)
                where
                    f = \z -> fromMaybe z $ M.lookup z swapF 
    swapBlockLabelNode sapF (Var x y) = Var x y

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

    swapBlockLabelEta :: Ord a => M.Map a a -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    swapBlockLabelEta swapF ans = (sortForest isZeroVar).canonicalizeAnsatzEta $ swapAnsatz
            where
                f = swapBlockLabelNode swapF 
                swapAnsatz = mapNodes f ans

    swapBlockLabelEps :: Ord a => M.Map a a -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    swapBlockLabelEps swapF ans = (sortForest isZeroVar).canonicalizeAnsatzEpsilon $ swapAnsatz
            where
                f = swapBlockLabelNode swapF 
                swapAnsatz = mapNodes f ans
            

    pairSymForestEta :: (Ord a, Show a) => (a,a) -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    pairSymForestEta inds ans = (addForests isZeroVar addVars) ans $ swapLabelEta inds ans 

    pairSymForestEps :: (Ord a, Show a) => (a,a) -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    pairSymForestEps inds ans = (addForests isZeroVar addVars) ans $ swapLabelEps inds ans 

    pairASymForestEta :: (Ord a, Show a) => (a,a) -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    pairASymForestEta inds ans = (addForests isZeroVar addVars) ans $ mapNodes (multVar (-1)) $ swapLabelEta inds ans 

    pairASymForestEps :: (Ord a, Show a) => (a,a) -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    pairASymForestEps inds ans = (addForests isZeroVar addVars) ans $ mapNodes (multVar (-1)) $ swapLabelEps inds ans 

    pairBlockSymForestEta :: (Ord a, Show a) => M.Map a a -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    pairBlockSymForestEta swapF ans = (addForests isZeroVar addVars) ans $ swapBlockLabelEta swapF ans 

    pairBlockSymForestEps :: (Ord a, Show a) => M.Map a a -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    pairBlockSymForestEps swapF ans = (addForests isZeroVar addVars) ans $ swapBlockLabelEps swapF ans 

    pairBlockASymForestEta :: (Ord a, Show a) => M.Map a a -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    pairBlockASymForestEta swapF ans = (addForests isZeroVar addVars) ans $ mapNodes (multVar (-1)) $ swapBlockLabelEta swapF ans

    pairBlockASymForestEps :: (Ord a, Show a) => M.Map a a -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    pairBlockASymForestEps swapF ans = (addForests isZeroVar addVars) ans $ mapNodes (multVar (-1)) $ swapBlockLabelEps swapF ans

    --cyclic symmetrization does not work !!! -> There is a problem 
    
    cyclicSymForestEta :: (Ord a, Show a) => [a] -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    cyclicSymForestEta inds ans = foldr (\y x -> (addForests isZeroVar addVars) x $ swapBlockLabelEta y ans ) ans perms
            where
                perms = map (\a -> M.fromList (zip inds a)) $ tail $ permutations inds 

    cyclicSymForestEps :: (Ord a, Show a) => [a] -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    cyclicSymForestEps inds ans = foldr (\y x -> (addForests isZeroVar addVars) x $ swapBlockLabelEps y ans ) ans perms
            where
                perms = map (\a -> M.fromList (zip inds a)) $ tail $ permutations inds 


    cyclicBlockSymForestEta :: (Ord a, Show a) => [[a]] -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    cyclicBlockSymForestEta inds ans = foldr (\y x -> (addForests isZeroVar addVars) x $ swapBlockLabelEta y ans ) ans perms
            where
                perms = map (\a -> M.fromList $ zip (concat inds) (concat a)) $ tail $ permutations inds 

    cyclicBlockSymForestEps :: (Ord a, Show a) => [[a]] -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a)
    cyclicBlockSymForestEps inds ans = foldr (\y x -> (addForests isZeroVar addVars) x $ swapBlockLabelEps y ans ) ans perms
            where
                perms = map (\a -> M.fromList $ zip (concat inds) (concat a)) $ tail $ permutations inds 

    type Symmetry a = ( [(a,a)] , [(a,a)] , [([a],[a])] , [[a]], [[[a]]] )

    symAnsatzForestEta :: (Ord a, Show a) => Symmetry a -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a) 
    symAnsatzForestEta (sym,asym,blocksym,cyclicsym,cyclicblocksym) ans =
        foldr cyclicBlockSymForestEta (
            foldr cyclicSymForestEta (
                foldr pairBlockSymForestEta (
                    foldr pairASymForestEta (
                        foldr pairSymForestEta ans sym
                    ) asym
                ) blockSymMap
            ) cyclicsym
        ) cyclicblocksym  
        where
            blockSymMap = map swapBlockLabelMap blocksym

    symAnsatzForestEps :: (Ord a, Show a) => Symmetry a -> AnsatzForest (AnsatzNode a) -> AnsatzForest (AnsatzNode a) 
    symAnsatzForestEps (sym,asym,blocksym,cyclicsym,cyclicblocksym) ans =
          foldr cyclicBlockSymForestEps (
              foldr cyclicSymForestEps (
                  foldr pairBlockSymForestEps (
                      foldr pairASymForestEps (
                          foldr pairSymForestEps ans sym
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

    isElem :: Ord a => [AnsatzNode a] -> AnsatzForest (AnsatzNode a) -> Bool
    isElem [] x = True
    isElem x (Leaf y) = True
    isElem x EmptyForest = False 
    isElem  (x:xs) (Forest m) 
                | isJust mForest = isElem xs $ fromJust mForest
                | otherwise = False
                where
                    mForest = M.lookup x m

    reduceAnsatzEta :: (Ord a, Show a) => Symmetry a -> [[AnsatzNode a]] -> AnsatzForest (AnsatzNode a)
    reduceAnsatzEta sym [] = EmptyForest
    reduceAnsatzEta sym l = foldr addOrRem EmptyForest l
            where
                addOrRem = \ans f -> if (isElem ans f) then f else (addForests isZeroVar addVars) f (symAnsatzForestEta sym $ mkForestFromAscList ans)

    reduceAnsatzEps :: (Ord a, Show a) => Symmetry a -> [[AnsatzNode a]] -> AnsatzForest (AnsatzNode a)
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

    --the next step is evaluating the tree 

    evalNode :: M.Map [Int] Int -> I.IntMap Int -> AnsatzNode Int -> Int
    evalNode epsM iMap (Eta x y) 
                | a == b && a == 0 = (-1) 
                | a == b = 1
                | otherwise = 0
                 where 
                    [a,b] = [(I.!) iMap x, (I.!) iMap y]
    evalNode epsM iMap (Epsilon w x y z) = M.findWithDefault 0 l epsM
                 where
                    l = [(I.!) iMap w, (I.!) iMap x, (I.!) iMap y, (I.!) iMap z]               

    epsMap :: M.Map [Int] Int 
    epsMap = M.fromList $ map (\x -> (x, epsSign x)) $ permutations [0,1,2,3]
                where
                   epsSign [i,j,k,l] = (-1)^(length $  filter (==True) [j>i,k>i,l>i,k>j,l>j,l>k])

    --row (1st number) is labeled by eqNr coloum (2nd number) is labeled by varNr


    evalAnsatzForestList :: M.Map [Int] Int -> I.IntMap Int -> AnsatzForest (AnsatzNode Int) -> [(Int,Rational)]
    evalAnsatzForestList epsM evalM (Leaf (Var x y)) = [(y,x)]
    evalAnsatzForestList epsM evalM (Forest m) = M.foldrWithKey foldF [] m 
                where
                    foldF k a b = let nodeVal = evalNode epsM evalM k 
                                  in if nodeVal == 0 then b 
                                     else  (evalAnsatzForestList epsM evalM $ multForest (multVar (fromIntegral nodeVal)) a)  ++  b

    --basic tree eval function

    evalAnsatzForest :: M.Map [Int] Int -> I.IntMap Int -> AnsatzForest (AnsatzNode Int) -> I.IntMap Rational
    evalAnsatzForest epsM evalM (Leaf (Var x y)) = I.singleton y x
    evalAnsatzForest epsM evalM (Forest m) = M.foldrWithKey foldF I.empty m 
                where
                    foldF k a b = let nodeVal = evalNode epsM evalM k 
                                  in if nodeVal == 0 then b 
                                     else I.unionWith (+) (I.map ((*) (fromIntegral nodeVal)) (evalAnsatzForest epsM evalM a)) b

    --eval All Inds (list of lists with (VarNr, Factor, multiplicity))

    evalAllAnsatzForest :: M.Map [Int] Int -> M.Map (I.IntMap Int) Int -> AnsatzForest (AnsatzNode Int) -> [([(Int,Rational)],Int)]
    evalAllAnsatzForest epsM evalMs f = nL 
                where
                    l = M.mapKeys (\x -> normalizeEqn $ filter (\(a,b) -> b /= 0) $ I.assocs $ evalAnsatzForest epsM x f) evalMs
                    nL = M.assocs $ M.delete [] l
                    
    normalizeEqn :: [(a, Rational)] -> [(a, Rational)]
    normalizeEqn [] = []
    normalizeEqn ((x,y):xs) =  map (\(a,b) -> (a, b/y)) $ (x,y) : xs

    showMatrixMatLab ::  [[((Int,Rational),Int)]] -> String
    showMatrixMatLab l = unlines $ map (\(x,y,z) -> show x ++ " " ++ show y ++ " " ++ show z ) l'
            where
                l' = concat $ zipWith (\l z -> map (\((x,y),_) -> (z, x, y)) l) l [1..]

    

    --using Data.Matrix (Rational)  


    ansatzHasMatrix :: [([(Int, Rational)], Int)] -> AnsatzForest (AnsatzNode Int) -> HasMat.Matrix Rational 
    ansatzHasMatrix l f = HasMat.matrix n m (\x -> M.findWithDefault 0 x lMap ) 
                where
                    l' = concat $ zipWith (\r z -> map (\(x,y) -> (z, x, y)) $ fst r) l [1..]
                    n = length l
                    lMap = M.fromList $ map (\(a,b,c) -> ((a, b), c)) l'
                    m = length $ getForestLabels f

    rowReduce :: HasMat.Matrix Rational -> HasMat.Matrix Rational 
    rowReduce m 
            | HasMat.nrows m == 1 = m
            | HasMat.ncols m == 1 = if Vec.null pivots then m else HasMat.fromLists $ [1] : ( replicate ((HasMat.nrows m)-1) [0] )
            | Vec.null pivots = actOnRightRest rowReduce m
            | otherwise = actOnRightSub rowReduce redMat
            where
                pivots = getPivots m 
                pivot1 = Vec.head pivots
                restpivots = Vec.drop 1 pivots
                sortMat = HasMat.switchRows pivot1 1 m 
                normMat = HasMat.scaleRow (1/(HasMat.getElem 1 1 sortMat)) 1 sortMat 
                redMat = foldr (\a b -> HasMat.combineRows a (-(HasMat.getElem a 1 b)) 1 b) normMat restpivots 

    actOnRightSub :: (HasMat.Matrix Rational -> HasMat.Matrix Rational) -> HasMat.Matrix Rational -> HasMat.Matrix Rational 
    actOnRightSub f m = (HasMat.<|>) c1Mat restMat 
                where
                    (newMat1, newMat2, newMat3, newMat4) = HasMat.splitBlocks 1 1 m 
                    c1Mat = (HasMat.<->) newMat1 newMat3 
                    restMat = (HasMat.<->) newMat2 $ f newMat4

    actOnRightRest :: (HasMat.Matrix Rational -> HasMat.Matrix Rational) -> HasMat.Matrix Rational -> HasMat.Matrix Rational 
    actOnRightRest f m = (HasMat.<|>) c1Mat $ f restMat 
                where
                    (newMat1, newMat2, newMat3, newMat4) = HasMat.splitBlocks 1 1 m 
                    c1Mat = (HasMat.<->) newMat1 $ newMat3 
                    restMat = (HasMat.<->) newMat2 $ newMat4

    getPivots :: HasMat.Matrix Rational -> Vec.Vector Int
    getPivots m = fmap ((+) 1) $ Vec.findIndices (/= 0) c1 
            where
                c1 = HasMat.getCol 1 m 

    get1stPivots :: HasMat.Matrix Rational -> [Int] 
    get1stPivots m = mapMaybe (\x -> getPiv $ HasMat.getRow x m) [1..(HasMat.nrows m)]
                where
                    getPiv v = fmap ((+) 1) $ Vec.findIndex (/= 0) v

    ansatzHasRR :: [([(Int, Rational)], Int)] -> AnsatzForest (AnsatzNode Int) -> HasMat.Matrix Rational 
    ansatzHasRR l f = rowReduce $ ansatzHasMatrix l f

    ansatzBasisLabels :: [([(Int, Rational)], Int)] -> AnsatzForest (AnsatzNode Int) -> (HasMat.Matrix Rational, [Int]) 
    ansatzBasisLabels l f = (mat, p)
                    where
                        mat = rowReduce $ ansatzHasMatrix l f
                        p = get1stPivots mat

    --is somehow too slow maybe use doubles ??
    ansatzHasLUFullPivoting :: [([(Int, Rational)], Int)] -> AnsatzForest (AnsatzNode Int) -> (HasMat.Matrix Rational, [Int])
    ansatzHasLUFullPivoting l f = (tri , map ((I.!) varMap) pivots)
            where
                mat = ansatzHasMatrix l f 
                (tri,_,_,perm,_,_) = fromJust $ HasMat.luDecomp' mat 
                perm' = HasMat.transpose perm 
                xVec = HasMat.fromLists $  [[fromIntegral i] | i <- [1..HasMat.nrows perm] ] 
                newXVec = perm' * xVec 
                i = map truncate $ HasMat.toList newXVec
                varMap = I.fromList $ zip [1..HasMat.nrows perm] i 
                pivots = get1stPivots tri 

    ansatzHasLU :: [([(Int, Rational)], Int)] -> AnsatzForest (AnsatzNode Int) -> (HasMat.Matrix Rational, [Int])
    ansatzHasLU l f = (tri, pivots)
            where
                mat = ansatzHasMatrix l f 
                (tri,_,_,_) = fromJust $ HasMat.luDecomp mat         
                pivots = get1stPivots tri 

    
    --testing alternative packages for the matrix computations (all other packages only work numerical)


    --using eigen (numeric)

    --ordering: Vars to the right, Eqns down 
    evalAllMatrixSp :: [([(Int, Rational)], Int)] -> AnsatzForest (AnsatzNode Int) -> Sparse.SparseMatrixXd 
    evalAllMatrixSp l f = Sparse.fromList n m l''
                where
                    l' = concat $ zipWith (\r z -> map (\(x,y) -> (z, x, y)) $ fst r) l [1..]
                    n = length l 
                    l'' = map (\(a,b,c) -> (a-1, b-1, fromRational c)) l'
                    m = length $ getForestLabels f 

    evalAllMatrix :: [([(Int, Rational)], Int)] -> AnsatzForest (AnsatzNode Int) -> Mat.MatrixXd 
    evalAllMatrix l f = Sparse.toMatrix $ Sparse.fromList n m l''
                    where
                        l' = concat $ zipWith (\r z -> map (\(x,y) -> (z, x, y)) $ fst r) l [1..]
                        n = length l 
                        l'' = map (\(a,b,c) -> (a-1, b-1, fromRational c)) l'
                        m = length $ getForestLabels f 

    ansatzRank :: [([(Int, Rational)], Int)] -> AnsatzForest (AnsatzNode Int) -> Int 
    ansatzRank l f = Sol.rank Sol.FullPivLU $ evalAllMatrix l f 
    
    --coloumns form basis of Image 
    ansatzImage :: [([(Int, Rational)], Int)] -> AnsatzForest (AnsatzNode Int) -> Mat.MatrixXd 
    ansatzImage l f = Sol.image Sol.FullPivLU $ Mat.transpose $ evalAllMatrix l f

    --coloumns (down) form basis of nullspace
    ansatzKernel :: [([(Int, Rational)], Int)] -> AnsatzForest (AnsatzNode Int)-> Mat.MatrixXd 
    ansatzKernel l f = Sol.kernel Sol.FullPivLU $ evalAllMatrix l f

    --using hmatrix 

    ansatzLinMatrix :: [([(Int, Rational)], Int)] -> AnsatzForest (AnsatzNode Int) -> LinDat.Matrix Double 
    ansatzLinMatrix l f = LinDat.assoc (n,m) 0 l''
                where
                    n = length l 
                    l' = concat $ zipWith (\r z -> map (\(x,y) -> (z, x, y)) $ fst r) l [1..]
                    l'' = map (\(a,b,c) -> ((a-1, b-1), fromRational c)) l'
                    m = length $ getForestLabels f

    ansatzLinLU :: [([(Int, Rational)], Int)] -> AnsatzForest (AnsatzNode Int) -> (LinDat.Matrix Double, [Int])
    ansatzLinLU l f = (b, getPivotNrs b 1.0e-10)
                where
                    mat = ansatzLinMatrix l f
                    (_,b,_,_) = Lin.lu mat 
                    matRows = LinDat.toRows b      

    getPivotNrs :: LinDat.Matrix Double -> Double -> [Int]
    getPivotNrs m bound = mapMaybe (getPiv bound) matRows
            where
                matRows = LinDat.toRows m 
                

    getPiv :: Double -> LinDat.Vector Double -> Maybe Int 
    getPiv bound v = let vMaybe = fmap ((+) 1) $ LinDat.find (\x -> abs(x) <= bound) v in 
                if (length vMaybe) == 0 then Nothing else Just $ head vMaybe

    --finally the eval maps 

    --A
    areaList4 :: M.Map [Int] Int
    areaList4 = M.fromList list
        where 
            list = [ ([a,b,c,d], areaMult a b c d) | a <- [0..2], b <- [a+1..3], c <- [a..2], d <- [c+1..3], 
                                                              (isAreaSorted a b c d)]
    --AI
    areaList6 :: M.Map [Int] Int
    areaList6 = M.fromList list
        where 
            list = [ ([a,b,c,d,p,q], (areaMult a b c d) * (iMult2 p q)) | a <- [0..2], b <- [a+1..3], c <- [a..2], d <- [c+1..3], 
                                                                        p <- [0..3], q <- [p..3], (isAreaSorted a b c d)]

    --A:B
    areaList8 :: M.Map [Int] Int
    areaList8 = M.fromList list
        where 
            list = [ ([a,b,c,d,e,f,g,h], (areaMult a b c d) * (areaMult e f g h)) | a <- [0..2], b <- [a+1..3], c <- [a..2], 
                                                                                    d <- [c+1..3],  e <- [a..2], f <- [e+1..3], g <- [e..2], h <- [g+1..3], (isAreaSorted a b c d) && (isAreaSorted e f g h) ]

    --Ap:Bq
    areaList10_1 :: M.Map [Int] Int
    areaList10_1 = M.fromList list
        where 
            list = [ ([a,b,c,d,p,e,f,g,h,q], (areaMult a b c d) * (areaMult e f g h)) | a <- [0..2], b <- [a+1..3], c <- [a..2],
                                                                                         d <- [c+1..3], e <- [a..2], f <- [e+1..3],g <- [e..2], h <- [g+1..3], p <- [0..3], q <- [0..3], (isAreaSorted a b c d) && (isAreaSorted e f g h)  ]

    --A:BI                                     
    areaList10_2 :: M.Map [Int] Int
    areaList10_2 = M.fromList list
        where 
            list = [ ([a,b,c,d,e,f,g,h,p,q], (areaMult a b c d) * (areaMult e f g h) * (iMult2 p q)) | a <- [0..2], b <- [a+1..3],                                                  c <- [a..2], d <- [c+1..3], 
                                                              e <- [a..2], f <- [e+1..3], g <- [e..2], h <- [g+1..3],
                                                              p <- [0..3], q <- [p..3], (isAreaSorted a b c d) && (isAreaSorted e f g h)  ]

    --A:B:C
    areaList12 :: M.Map [Int] Int
    areaList12 = M.fromList list
        where 
            list = [ ([a,b,c,d,e,f,g,h,i,j,k,l], (areaMult a b c d) * (areaMult e f g h) * (areaMult i j k l)) | a <- [0..2], 
                                                              b <- [a+1..3], c <- [a..2], d <- [c+1..3], 
                                                              e <- [a..2], f <- [e+1..3], g <- [e..2], h <- [g+1..3],
                                                              i <- [e..2], j <- [i+1..3], k <- [i..2], l <- [k+1..3],
                                                              (isAreaSorted a b c d) && (isAreaSorted e f g h) && (isAreaSorted i j k l)]

    --A:Bp:Cq
    areaList14_1 :: M.Map [Int] Int
    areaList14_1 = M.fromList list
        where 
            list = [ ([a,b,c,d,e,f,g,h,p,i,j,k,l,q], (areaMult a b c d) * (areaMult e f g h) * (areaMult i j k l)) | a <- [0..2],                                                    b <- [a+1..3], c <- [a..2], d <- [c+1..3], 
                                                              e <- [0..2], f <- [e+1..3], g <- [e..2], h <- [g+1..3],
                                                              i <- [e..2], j <- [i+1..3], k <- [i..2], l <- [k+1..3],
                                                              p <- [0..3], q <- [0..3], (isAreaSorted a b c d) && (isAreaSorted e f g h) && (isAreaSorted i j k l)]


    --A:B:CI
    areaList14_2 :: M.Map [Int] Int
    areaList14_2 = M.fromList list
        where 
            list = [ ([a,b,c,d,e,f,g,h,i,j,k,l,p,q], (areaMult a b c d) * (areaMult e f g h) * (areaMult i j k l) * (iMult2 p q)) |                                                 a <- [0..2], b <- [a+1..3], c <- [a..2], d <- [c+1..3], 
                                                              e <- [a..2], f <- [e+1..3], g <- [e..2], h <- [g+1..3],
                                                              i <- [0..2], j <- [i+1..3], k <- [i..2], l <- [k+1..3],
                                                              p <- [0..3], q <- [p..3], (isAreaSorted a b c d) && (isAreaSorted e f g h) && (isAreaSorted i j k l)]

    --Am:Bn:CI
    areaList16_1 :: M.Map [Int] Int
    areaList16_1 = M.fromList list
        where 
            list = [ ([a,b,c,d,m,e,f,g,h,n,i,j,k,l,p,q], (areaMult a b c d) * (areaMult e f g h) * (areaMult i j k l) * (iMult2 p q)) | a <- [0..2], b <- [a+1..3], c <- [a..2], d <- [c+1..3], m <- [0..3],
                  e <- [a..2], f <- [e+1..3], g <- [e..2], h <- [g+1..3], n <- [0..3],
                  i <- [0..2], j <- [i+1..3], k <- [i..2], l <- [k+1..3],
                 p <- [0..3], q <- [p..3], (isAreaSorted a b c d) && (isAreaSorted e f g h) && (isAreaSorted i j k l)]

    --A:BI:CJ
    areaList16_2 :: M.Map [Int] Int
    areaList16_2 = M.fromList list
        where 
            list = [ ([a,b,c,d,e,f,g,h,m,n,i,j,k,l,p,q], (areaMult a b c d) * (areaMult e f g h) * (areaMult i j k l) * (iMult2 p q) * (iMult2 m n) ) | a <- [0..2], b <- [a+1..3], c <- [a..2], d <- [c+1..3],
                                 e <- [0..2], f <- [e+1..3], g <- [e..2], h <- [g+1..3], m <- [0..3], n <- [m..3],
                                 i <- [e..2], j <- [i+1..3], k <- [i..2], l <- [k+1..3],
                                p <- [0..3], q <- [p..3], (isAreaSorted a b c d) && (isAreaSorted e f g h) && (isAreaSorted i j k l)]

    --AI:BJ:CK
    areaList18 :: M.Map [Int] Int
    areaList18 = M.fromList list
        where 
            list = [ ([a,b,c,d,r,s,e,f,g,h,m,n,i,j,k,l,p,q], (areaMult a b c d) * (areaMult e f g h) * (areaMult i j k l) * (iMult2 p q) * (iMult2 r s)) | a <- [0..2], b <- [a+1..3], c <- [a..2], d <- [c+1..3], r <- [0..3], s <- [r..3],
                                          e <- [a..2], f <- [e+1..3], g <- [e..2], h <- [g+1..3], m <- [0..3], n <- [m..3],
                                          i <- [e..2], j <- [i+1..3], k <- [i..2], l <- [k+1..3],
                                          p <- [0..3], q <- [p..3], (isAreaSorted a b c d) && (isAreaSorted e f g h) && (isAreaSorted i j k l)]
   
    
    isAreaSorted :: Int -> Int -> Int -> Int -> Bool
    isAreaSorted a b c d 
            | a < c || (a == c && b <= d) = True
            | otherwise = False 
   
    areaMult :: Int -> Int -> Int -> Int -> Int
    areaMult a b c d 
            | a == c && b == d = 4 
            | otherwise = 8

    iMult2 :: Int -> Int -> Int 
    iMult2 a b = if a == b then 1 else 2 

    areaEvalMap4 :: M.Map (I.IntMap Int) Int
    areaEvalMap4 = l
        where 
            area4 = areaList4
            l = M.mapKeysMonotonic (\x -> I.fromList $ zip [1..4] x) area4

    areaEvalMap6 :: M.Map (I.IntMap Int) Int
    areaEvalMap6 = l
        where 
            area6 = areaList6
            l = M.mapKeysMonotonic (\x -> I.fromList $ zip [1..6] x) area6

    areaEvalMap8 :: M.Map (I.IntMap Int) Int
    areaEvalMap8 = l
        where 
            area8 = areaList8
            l = M.mapKeysMonotonic (\x -> I.fromList $ zip [1..8] x) area8

    areaEvalMap10_1 :: M.Map (I.IntMap Int) Int
    areaEvalMap10_1 = l
        where 
            area10_1 = areaList10_1
            l = M.mapKeysMonotonic (\x -> I.fromList $ zip [1..10] x) area10_1

    areaEvalMap10_2 :: M.Map (I.IntMap Int) Int
    areaEvalMap10_2 = l
        where 
            area10_2 = areaList10_2
            l = M.mapKeysMonotonic (\x -> I.fromList $ zip [1..10] x) area10_2

    areaEvalMap12 :: M.Map (I.IntMap Int) Int
    areaEvalMap12 = l
        where 
            area12 = areaList12
            l = M.mapKeysMonotonic (\x -> I.fromList $ zip [1..12] x) area12

    areaEvalMap14_1 :: M.Map (I.IntMap Int) Int
    areaEvalMap14_1 = l
        where 
            area14_1 = areaList14_1
            l = M.mapKeysMonotonic (\x -> I.fromList $ zip [1..14] x) area14_1

    areaEvalMap14_2 :: M.Map (I.IntMap Int) Int
    areaEvalMap14_2 = l
        where 
            area14_2 = areaList14_2
            l = M.mapKeysMonotonic (\x -> I.fromList $ zip [1..14] x) area14_2

    areaEvalMap16_1 :: M.Map (I.IntMap Int) Int
    areaEvalMap16_1 = l
        where 
            area16_1 = areaList16_1
            l = M.mapKeysMonotonic (\x -> I.fromList $ zip [1..16] x) area16_1

    areaEvalMap16_2 :: M.Map (I.IntMap Int) Int
    areaEvalMap16_2 = l
        where 
            area16_2 = areaList16_2
            l = M.mapKeysMonotonic (\x -> I.fromList $ zip [1..16] x) area16_2

    areaEvalMap18 :: M.Map (I.IntMap Int) Int
    areaEvalMap18 = l
        where 
            area18 = areaList18
            l = M.mapKeysMonotonic (\x -> I.fromList $ zip [1..18] x) area18


    filterList4 :: [(Int,Int)]
    filterList4 = [(1,2),(1,3),(3,4)]

    symList4 :: Symmetry Int 
    symList4 = ([], [(1,2),(3,4)], [([1,2],[3,4])], [], [])

    filterList6 :: [(Int,Int)]
    filterList6 = [(1,2),(1,3),(3,4),(5,6)]

    symList6 :: Symmetry Int 
    symList6 = ([(5,6)], [(1,2),(3,4)], [([1,2],[3,4])], [], [])

    filterList8 :: [(Int,Int)]
    filterList8 = [(1,2),(1,3),(3,4),(1,5),(5,6),(5,7),(7,8)]

    symList8 :: Symmetry Int 
    symList8 = ([], [(1,2),(3,4),(5,6),(7,8)], [([1,2],[3,4]),([5,6],[7,8]),([1,2,3,4],[5,6,7,8])], [], [])

    filterList10_1 :: [(Int,Int)]
    filterList10_1 = [(1,2),(1,3),(3,4),(1,6),(6,7),(6,8),(8,9)]

    symList10_1 :: Symmetry Int 
    symList10_1 = ([], [(1,2),(3,4),(6,7),(8,9)], [([1,2],[3,4]),([6,7],[8,9]),([1,2,3,4,5],[6,7,8,9,10])], [], [])

    filterList10_2 :: [(Int,Int)]
    filterList10_2 = [(1,2),(1,3),(3,4),(5,6),(5,7),(7,8),(9,10)]

    symList10_2 :: Symmetry Int 
    symList10_2 = ([(9,10)], [(1,2),(3,4),(5,6),(7,8)], [([1,2],[3,4]),([5,6],[7,8])], [], [])

    filterList12 :: [(Int,Int)]
    filterList12 = [(1,2),(1,3),(3,4),(1,5),(5,6),(5,7),(7,8),(5,9),(9,10),(9,11),(11,12)]

    symList12 :: Symmetry Int 
    symList12 = ([], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12])], [], 
                [[[1,2,3,4],[5,6,7,8],[9,10,11,12]]])

    filterList14_1 :: [(Int,Int)]
    filterList14_1 = [(1,2),(1,3),(3,4),(5,6),(5,7),(7,8),(5,10),(10,11),(10,12),(12,13)]
    
    symList14_1 :: Symmetry Int 
    symList14_1 = ([], [(1,2),(3,4),(5,6),(7,8),(10,11),(12,13)], [([1,2],[3,4]),([5,6],[7,8]),([10,11],[12,13]),
                ([5,6,7,8,9],[10,11,12,13,14])], [], [])

    filterList14_2 :: [(Int,Int)]
    filterList14_2 = [(1,2),(1,3),(3,4),(1,5),(5,6),(5,7),(7,8),(9,10),(9,11),(11,12),(13,14)]

    symList14_2 :: Symmetry Int 
    symList14_2 = ([(13,14)], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12]),([1,2,3,4],[5,6,7,8])], [], [])

    filterList16_1 :: [(Int,Int)]
    filterList16_1 = [(1,2),(1,3),(3,4),(1,6),(6,7),(6,8),(8,9),(11,12),(11,13),(13,14),(15,16)]

    symList16_1 :: Symmetry Int 
    symList16_1 = ([(15,16)], [(1,2),(3,4),(6,7),(8,9),(11,12),(13,14)], [([1,2],[3,4]),([6,7],[8,9]),([11,12],[13,14]),
                ([1,2,3,4,5],[6,7,8,9,10])], [], [])

    filterList16_2 :: [(Int,Int)]
    filterList16_2 = [(1,2),(1,3),(3,4),(5,6),(5,7),(7,8),(9,10),(5,11),(11,12),(11,13),(13,14),(15,16)]

    symList16_2 :: Symmetry Int 
    symList16_2 = ([(9,10),(15,16)], [(1,2),(3,4),(5,6),(7,8),(11,12),(13,14)], [([1,2],[3,4]),([5,6],[7,8]),([11,12],[13,14]),
                ([5,6,7,8,9,10],[11,12,13,14,15,16])], [], [])

    filterList18 :: [(Int,Int)]
    filterList18 = [(1,2),(1,3),(3,4),(1,7),(5,6),(7,8),(7,9),(9,10),(7,13),(11,12),(13,14),(13,15),(15,16),(17,18)]

    symList18 :: Symmetry Int 
    symList18 = ([(5,6),(11,12),(17,18)], [(1,2),(3,4),(7,8),(9,10),(13,14),(15,16)], [([1,2],[3,4]),([7,8],[9,10]),
                ([13,14],[15,16])], [], [[[1,2,3,4,5,6],[7,8,9,10,11,12],[13,14,15,16,17,18]]])


    
    {-

    testBasisLabelMat :: [Int] -> [([(Int, Rational)], Int)] -> Mat.MatrixXd 
    testBasisLabelMat vars l = mat 
            where
                varMap = I.fromList $ zip vars [1..]
                n = length l 
                l' = concat $ zipWith (\r z -> map (\(x,y) -> (z, x, y)) $ fst r) l [1..]
                l'' = mapMaybe (\(x,y,z) -> let val = I.lookup y varMap in if isJust val then Just (x-1, (fromJust val) - 1, fromRational z) else Nothing ) l'
                m = length vars
                mat = Sparse.toMatrix $ Sparse.fromList n m l''

    testBasisLabels :: [Int] -> [([(Int, Rational)], Int)] -> (Int,Int)
    testBasisLabels vars l = (Sol.rank Sol.FullPivLU $ testBasisLabelMat vars l, min n $ length vars)
                where
                    n = length l 
    
    -}

    