--improved version of perturbationTree

--seems to be the best soultion !!!

--pushes type stuff to kind stuff (prefixed with ')
{-# LANGUAGE DataKinds #-}
--matching on type constructors
{-# LANGUAGE GADTs #-}
--kind signature
{-# LANGUAGE KindSignatures #-}
--type family definitions
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
--infix type plus and mult
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}







module PerturbationTree2 (
    AnsatzForest(..), AnsatzNode(..), mkEtaList, mkEpsilonList, Symmetry, reduceAnsatzEta, reduceAnsatzEps, getEtaInds, getEpsilonInds, mkAllVars, symAnsatzForestEta, symAnsatzForestEps, mkForestFromAscList, getEtaForest, getEpsForest, flattenForest, relabelAnsatzForest, getForestLabels, printAnsatz, showAnsatzNode, mapNodes, addForests, isZeroVar, addVars,
    areaEvalMap10, epsMap, showMatrix, ansatzBasisMat, evalAnsatzForestMatrix, evalAnsatzForest, evalAllAnsatzForest,
    evalAnsatzForestList, getLeafVals, areaEvalMap14

) where

    import qualified Data.IntMap.Strict as I
    import qualified Data.Map.Strict as M
    import Data.Foldable
    import Data.List 
    import Data.Maybe
    import Data.List
    import GHC.TypeLits
    import Data.Proxy
    import qualified Eigen.Matrix as Mat
    import qualified Eigen.SparseMatrix as Sparse
    import qualified Eigen.Solver.LA as Sol

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

    evalAnsatzForest :: M.Map [Int] Int -> I.IntMap Int -> AnsatzForest (AnsatzNode Int) -> I.IntMap Rational
    evalAnsatzForest epsM evalM (Leaf (Var x y)) = I.singleton y x
    evalAnsatzForest epsM evalM (Forest m) = M.foldrWithKey foldF I.empty m 
                where
                    foldF k a b = let nodeVal = evalNode epsM evalM k 
                                  in if nodeVal == 0 then b 
                                     else I.unionWith (+) (evalAnsatzForest epsM evalM $ multForest (multVar (fromIntegral nodeVal)) a) b

    evalAllAnsatzForest :: M.Map [Int] Int -> [(I.IntMap Int, Int)] -> AnsatzForest (AnsatzNode Int) -> [[(Int,Int, Rational)]]
    evalAllAnsatzForest epsM evalMs f = nub l 
                where
                    l = map (\(x,y) -> normalizeEqn $ map (\(a,b) -> (y,a,b)) $ filter (\(a,b) -> b /= 0) $ I.assocs $ evalAnsatzForest epsM x f) evalMs
                    
    normalizeEqn :: [(a,b,Rational)] -> [(a,b, Rational)]
    normalizeEqn [] = []
    normalizeEqn ((x,y,z):xs) = map (\(a,b,c) -> (a,b, c / z)) $ (x,y,z) : xs

    evalAnsatzForestMatrix :: (KnownNat n, KnownNat m) => M.Map [Int] Int -> [(I.IntMap Int, Int)] -> AnsatzForest (AnsatzNode Int) -> Mat.Matrix n m Double
    evalAnsatzForestMatrix epsM evalMs f = mat 
                    where 
                        mat = Sparse.toMatrix $ Sparse.fromList $ map (\(a,b,c) -> (a,b, fromRational c :: Double)) $ concat $ evalAllAnsatzForest epsM evalMs f 

    ansatzBasisMat :: (KnownNat n, KnownNat m) =>  M.Map [Int] Int -> [(I.IntMap Int, Int)] -> AnsatzForest (AnsatzNode Int) -> Mat.Matrix n m Double
    ansatzBasisMat epsM evalMs f = sol 
                 where
                    mat = Sparse.toMatrix $ Sparse.fromList $ map (\(a,b,c) -> (a,b, fromRational c :: Double)) $ concat $ evalAllAnsatzForest epsM evalMs f 
                    sol = Sol.image Sol.HouseholderQR mat

    showMatrix :: (KnownNat n, KnownNat m) => Mat.Matrix n m Double -> String 
    showMatrix mat = unlines lShow
            where
                spMat = Sparse.fromMatrix mat 
                l = Sparse.toList spMat
                lShow = map (\(a,b,c) -> show a ++ " " ++ show b ++ " " ++ show c) l

   

    --finally the eval maps 

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

    areaEvalMap14 :: [I.IntMap Int]
    areaEvalMap14 = map I.fromList l 
        where 
            area14 = areaList14 
            l = map (\x -> zip [1,2,3,4,5,6,7,8,9,10,11,12,13,14] x) area14

    areaEvalMap10 :: [I.IntMap Int]
    areaEvalMap10 = map I.fromList l 
        where 
            area10 = areaList10
            l = map (\x -> zip [1,2,3,4,5,6,7,8,9,10] x) area10