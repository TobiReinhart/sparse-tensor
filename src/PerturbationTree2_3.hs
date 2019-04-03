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

--slower but hopefully less memory usage than 2_2 version

module PerturbationTree2_3 (
    mkAnsatzTensor, mkAnsatzTensorFast, getForestLabels, getForestLabelsEpsilon, getFullForest, getEtaInds, getEpsilonInds, getEpsilonIndsRed,
    areaList4Inds, areaList6Inds, areaList8Inds, areaList10_1Inds, areaList10_2Inds, areaList12Inds, areaList12_1Inds, areaList14_1Inds, areaList14_2Inds,
    areaList16_1Inds, areaList16_2Inds, areaList16Inds, areaList18Inds, areaList18_2Inds, areaList18_3Inds, areaList20Inds, 
    symList4, symList6, symList8, symList10_1, symList10_2, symList12, symList12_1, symList14_1, symList14_2, symList16, symList16_1, symList16_2,
    symList18, symList18_2, symList18_3, symList20,
    filterList4, filterList6, filterList8, filterList10_1, filterList10_2, filterList12, filterList12_1, filterList14_1, filterList14_2, filterList16, filterList16_1, filterList16_2,
    filterList18, filterList18_2, filterList18_3, filterList20,
    areaList4IndsEta, areaList6IndsEta, areaList8IndsEta, areaList10_1IndsEta, areaList10_2IndsEta, areaList12IndsEta, areaList12_1IndsEta, areaList14_1IndsEta,
    areaList14_2IndsEta, areaList16_1IndsEta, areaList16_2IndsEta, areaList16IndsEta, areaList18IndsEta, areaList18_2IndsEta, areaList18_3IndsEta, areaList20IndsEta,
    areaList4IndsEps, areaList6IndsEps, areaList8IndsEps, areaList10_1IndsEps, areaList10_2IndsEps, areaList12IndsEps, areaList12_1IndsEps, areaList14_1IndsEps, 
    areaList14_2IndsEps, areaList16_1IndsEps, areaList16_2IndsEps, areaList16IndsEps, areaList18IndsEps, areaList18_2IndsEps, areaList18_3IndsEps, areaList20IndsEps,
    symPairs4, symPairs6, symPairs8, symPairs10_1, symPairs10_2, symPairs12, symPairs12_1, symPairs14_1, symPairs14_2, symPairs16, symPairs16_1, symPairs16_2,
    symPairs18, symPairs18_2, symPairs18_3, symPairs20,
    areaBlocks4, areaBlocks6, areaBlocks8, areaBlocks10_1, areaBlocks10_2, areaBlocks12, areaBlocks12_1, areaBlocks14_1, areaBlocks14_2,
    areaBlocks16, areaBlocks16_1, areaBlocks16_2, areaBlocks18, areaBlocks18_2, areaBlocks18_3, areaBlocks20,
    canonicalizeEvalMaps

    
) where

    import qualified Data.IntMap.Strict as I
    import qualified Data.Map.Strict as M 
    import Data.List
    import Data.Maybe

    import qualified Data.Eigen.Matrix as Mat 
    import qualified Data.Eigen.SparseMatrix as Sparse
    import qualified Data.Eigen.LA as Sol 
    import qualified Data.Eigen.SparseLA as SpSol

    import Control.Parallel.Strategies
    import Control.Monad.ST (runST)

    import TensorTreeNumeric4_2

    import qualified Data.HashTable.Class as HT (toList)
    import qualified Data.HashTable.ST.Cuckoo as HTC (newSized, insert)
    import Data.Hashable (Hashable)

    import Data.Ratio


    getAllIndsEta :: [Int] -> [[Int]]
    getAllIndsEta [a,b] = [[a,b]]
    getAllIndsEta (x:xs) = res
            where
                l = map (\y -> ([x,y],delete y xs)) xs 
                res = concat $ map (\(a,b) -> (++) a <$> (getAllIndsEta b)) l
    getAllIndsEta [] = [[]]
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

    --for the epsilon indices we can apply some extra filters 

    --1) symmetric indices (e.g from higher derivatives) cannot constitute epsilon indices 

    filterSymInds :: [[Int]] -> [[Int]] -> [[Int]]
    filterSymInds symInds l = filter (filterF symInds) l 
            where
                filterF [] l' = True
                filterF ([x,y]:xs) l' 
                    | isEpsInd i j = False
                    | otherwise = filterF xs l' 
                        where 
                            (i',j') = (fromJust $ elemIndex x l', fromJust $ elemIndex y l')
                            (i,j) = (min i' j', max i' j') 


    --2) block sym pairs of are metric symmetry cannot be connected to epsilon in 2 indices and one eta in the other 2 indices

    isEtaInd :: Int -> Int -> Bool
    isEtaInd i j = even i && (j == i+1) 

    isEpsInd :: Int -> Int -> Bool
    isEpsInd i j = j<4

    filterAreaBlocks :: [[Int]] -> [[Int]] -> [[Int]]
    filterAreaBlocks blocks list = filter (filterF blocks) list
            where 
                filterF [] list' = True
                filterF ([a,b,c,d]:xs) list'
                    | (isEpsInd i k) && (isEtaInd j l) = False 
                    | otherwise = filterF xs list'
                     where 
                        [i',j',k',l'] = [fromJust $ elemIndex a list', fromJust $ elemIndex b list', fromJust $ elemIndex c list', fromJust $ elemIndex d list']
                        (i'',k'') = (min i' k', max i' k')
                        (j'',l'') = (min j' l', max j' l') 
                        (i,k,j,l) = if i'' < j'' then (i'',k'',j'',l'') else (j'',l'',i'',k'') 

    getEpsilonInds :: [Int] -> [(Int,Int)] -> [[Int]]
    getEpsilonInds l sym = filter (\x -> filterSym x sym) $ getAllIndsEpsilon l

    getEpsilonIndsRed :: [Int] -> [(Int,Int)] -> [[Int]] -> [[Int]] -> [[Int]]
    getEpsilonIndsRed l sym symPairs areaBlocks = filterAreaBlocks areaBlocks $ filterSymInds symPairs $ filter (\x -> filterSym x sym) $ getAllIndsEpsilon l
                

    data Epsilon = Epsilon {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show, Read, Eq, Ord)

    data Eta = Eta {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show, Read, Eq, Ord)

    data Var = Var {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show, Read, Eq, Ord)

    
    sortList :: Ord a => [a] -> [a]
    sortList [] = [] 
    sortList (x:xs) = insert x $ sortList xs 

    sortEta :: Eta -> Eta 
    sortEta (Eta x y) = Eta (min x y) (max x y)
    {-# INLINEABLE sortEta #-}

    sortEpsilon :: Epsilon -> Epsilon 
    sortEpsilon (Epsilon i j k l) = Epsilon i' j' k' l'
             where
                [i',j',k',l'] = sortList [i,j,k,l]
    
    getEpsSign :: Epsilon -> Int 
    getEpsSign (Epsilon i j k l) = (-1)^(length $  filter (==True) [j>i,k>i,l>i,k>j,l>j,l>k])
    {-# INLINEABLE getEpsSign #-}

    addVars :: Var -> Var -> Var 
    addVars (Var x y) (Var x' y') = Var (x + x') y
    {-# INLINEABLE addVars #-}

    multVar :: Int -> Var -> Var
    multVar x (Var x' y) = Var (x * x') y
    {-# INLINEABLE multVar #-}

    isZeroVar :: Var -> Bool
    isZeroVar (Var x _) = x==0
    {-# INLINEABLE isZeroVar #-}
   
    data AnsatzForestEta = ForestEta (M.Map Eta AnsatzForestEta)| Leaf !Var | EmptyForest  deriving (Show, Read, Eq)

    type AnsatzForestEpsilon = M.Map Epsilon AnsatzForestEta

    forestMap :: AnsatzForestEta -> M.Map Eta AnsatzForestEta
    forestMap (ForestEta m) = m
    {-# INLINEABLE forestMap #-}

    --map a function over the nodes of the AnsatzTree (map over the tensors eta and epsilon)

    mapNodes :: (Eta -> Eta) -> AnsatzForestEta -> AnsatzForestEta
    mapNodes f EmptyForest = EmptyForest
    mapNodes f (ForestEta m) = ForestEta $ (M.mapKeys f).(M.map (mapNodes f)) $ m
    mapNodes f (Leaf x) = Leaf x

    mapNodesEpsilon :: (Epsilon -> Epsilon) -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    mapNodesEpsilon f m = M.mapKeys f m

    --map over the vars 

    mapVars :: (Var -> Var) -> AnsatzForestEta -> AnsatzForestEta
    mapVars f EmptyForest = EmptyForest
    mapVars f (Leaf var) = Leaf (f var)
    mapVars f (ForestEta m) = ForestEta $ M.map (mapVars f) m

    mapVarsEpsilon :: (Var -> Var) -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    mapVarsEpsilon f m = M.map (mapVars f) $ m

    --multiplying the vars with a fixed Int 

    multVars :: Int -> AnsatzForestEta -> AnsatzForestEta 
    multVars i = mapVars (multVar i)

    multVarsEpsilon :: Int -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    multVarsEpsilon i = mapVarsEpsilon (multVar i)

    --relabel and remove Vars in the Forest 

    getLeafVals :: AnsatzForestEta -> [Var]
    getLeafVals (Leaf var) = [var]
    getLeafVals (ForestEta m) = rest
            where
                rest = concatMap getLeafVals $ M.elems m

    getLeafValsEpsilon :: AnsatzForestEpsilon -> [Var]
    getLeafValsEpsilon m = concatMap getLeafVals $ M.elems m

    getVarLabels :: Var -> Int
    getVarLabels (Var i j) = j

    getForestLabels :: AnsatzForestEta -> [Int]
    getForestLabels ans = nub $ map getVarLabels $ getLeafVals ans

    getForestLabelsEpsilon :: AnsatzForestEpsilon -> [Int]
    getForestLabelsEpsilon m = nub $ map getVarLabels $ getLeafValsEpsilon m

    relabelVar :: (Int -> Int) -> Var -> Var
    relabelVar f (Var i j) = Var i (f j)

    relabelAnsatzForest :: Int -> AnsatzForestEta -> AnsatzForestEta
    relabelAnsatzForest i ans = mapVars update ans
            where
                vars = getForestLabels ans 
                relabMap = I.fromList $ zip vars [i..]
                update = relabelVar ((I.!) relabMap) 

    removeVarsEta :: [Int] -> AnsatzForestEta -> AnsatzForestEta 
    removeVarsEta vars (Leaf (Var i j)) 
                | elem j vars = EmptyForest 
                | otherwise = (Leaf (Var i j))
    removeVarsEta vars (ForestEta m) = ForestEta $ M.filter (/= EmptyForest) $ M.map (removeVarsEta vars) m  
    removeVarsEta vars EmptyForest = EmptyForest

    relabelAnsatzForestEpsilon :: Int -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    relabelAnsatzForestEpsilon i ans =  mapVarsEpsilon update ans
            where
                vars = getForestLabelsEpsilon ans 
                relabMap = I.fromList $ zip vars [i..]
                update = relabelVar ((I.!) relabMap) 

    removeVarsEps :: [Int] -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    removeVarsEps vars m = M.filter (/= EmptyForest) $ M.map (removeVarsEta vars) m  

    --add 2 sorted forests (are all zeros removed ? -> probably yes !)

    addForests :: AnsatzForestEta -> AnsatzForestEta -> AnsatzForestEta
    addForests ans EmptyForest = ans
    addForests EmptyForest ans = ans 
    addForests (Leaf var1) (Leaf var2)
            | isZeroVar newLeafVal = EmptyForest
            | otherwise = Leaf newLeafVal
            where
                newLeafVal = (addVars var1 var2)
    addForests (ForestEta m1) (ForestEta m2) 
            | M.null newMap = EmptyForest
            | otherwise = ForestEta newMap
             where
                newMap = M.filter (/= EmptyForest) $ M.unionWith addForests m1 m2

    addForestsEpsilon :: AnsatzForestEpsilon -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    addForestsEpsilon m1 m2 = M.filter (/= EmptyForest) $ M.unionWith addForests m1 m2

    addList2Forest :: AnsatzForestEta -> ([Eta],Var) -> AnsatzForestEta 
    addList2Forest EmptyForest x = mkForestFromAscList x 
    addList2Forest (Leaf var1) ([], var2) 
            | isZeroVar newLeafVal = EmptyForest
            | otherwise = Leaf newLeafVal
            where
                newLeafVal = (addVars var1 var2)
    addList2Forest (ForestEta m1) (x:xs, var) = ForestEta $ M.insertWith (\a1 a2 -> addList2Forest a2 (xs, var)) x newVal m1
             where
                newVal = mkForestFromAscList (xs,var)

    addList2ForestEpsilon :: AnsatzForestEpsilon -> (Epsilon,[Eta],Var) -> AnsatzForestEpsilon 
    addList2ForestEpsilon m (eps,eta,var) = M.insertWith (\a1 a2 -> addList2Forest a2 (eta, var)) eps newVal m
         where
            newVal = mkForestFromAscList (eta,var)

    --flatten Forest to AscList consisting of the several Branches
    
    flattenForest :: AnsatzForestEta -> [([Eta],Var)]
    flattenForest EmptyForest = []
    flattenForest (Leaf var) = [([],var)]
    flattenForest (ForestEta m) = concat l 
            where
                mPairs = M.assocs m 
                l = fmap (\(k,v) -> map (\(i,j) -> (insert k i, j)) $ flattenForest v) mPairs 
                
    flattenForestEpsilon :: AnsatzForestEpsilon -> [(Epsilon,[Eta],Var)]
    flattenForestEpsilon m = concat l
                where
                    mPairs = M.assocs m 
                    l = fmap (\(k,v) -> map (\(i,j) -> (k, i, j)) $ flattenForest v) mPairs

    --construct a forest of a given asclist 
                
    mkForestFromAscList :: ([Eta],Var) -> AnsatzForestEta 
    mkForestFromAscList ([],var) = Leaf var
    mkForestFromAscList (x:xs, var) = ForestEta $ M.singleton x $ mkForestFromAscList (xs,var)

    mkForestFromAscListEpsilon :: (Epsilon,[Eta],Var) -> AnsatzForestEpsilon 
    mkForestFromAscListEpsilon (x,y,z) = M.singleton x $ mkForestFromAscList (y,z)

    --canonicalize the individual etas and epsilons 

    canonicalizeAnsatzEta :: AnsatzForestEta -> AnsatzForestEta
    canonicalizeAnsatzEta = mapNodes sortEta
 
    canonicalizeAnsatzEpsilon :: AnsatzForestEpsilon -> AnsatzForestEpsilon
    canonicalizeAnsatzEpsilon m = newMap
                 where
                     newMap = M.mapKeys sortEpsilon $ M.mapWithKey (\k v -> mapVars (multVar (getEpsSign k) ) v) $ M.map (mapNodes sortEta) m 

    --sort a givne AnsatzForest, i.e. bring the products of eta and epsilon to canonical order once the individual tensors are ordered canonically
    
    sortForest :: AnsatzForestEta -> AnsatzForestEta
    sortForest f = foldr (flip addList2Forest) EmptyForest fList 
                where
                    fList = flattenForest f

    sortForestEpsilon :: AnsatzForestEpsilon -> AnsatzForestEpsilon 
    sortForestEpsilon f = foldr (flip addList2ForestEpsilon) M.empty fList 
                 where
                    fList = flattenForestEpsilon f

    --the next step is symmetrizing the AnsatzForest 

    --swap functions for the symmetrization

    swapLabelF :: (Int,Int) -> Int -> Int 
    swapLabelF (x,y) z
            | x == z = y
            | y == z = x
            | otherwise = z 

    swapBlockLabelMap :: ([Int],[Int]) -> I.IntMap Int
    swapBlockLabelMap (x,y) = swapF 
            where
                swapF = I.fromList $ (zip x y)++(zip y x)

    swapLabelEta :: (Int,Int) -> Eta -> Eta 
    swapLabelEta inds (Eta x y) = Eta (f x) (f y)
            where
                f = swapLabelF inds

    swapLabelEpsilon :: (Int,Int) -> Epsilon -> Epsilon
    swapLabelEpsilon inds (Epsilon i j k l) = Epsilon (f i) (f j) (f k) (f l)
            where
                f = swapLabelF inds 

    swapBlockLabelEta :: I.IntMap Int -> Eta -> Eta
    swapBlockLabelEta swapF (Eta i j) = Eta i' j'
                where
                    i' = I.findWithDefault i i swapF
                    j' = I.findWithDefault j j swapF

    swapBlockLabelEpsilon :: I.IntMap Int -> Epsilon -> Epsilon
    swapBlockLabelEpsilon swapF (Epsilon i j k l) = Epsilon i' j' k' l'
                where
                    i' = I.findWithDefault i i swapF
                    j' = I.findWithDefault j j swapF
                    k' = I.findWithDefault k k swapF
                    l' = I.findWithDefault l l swapF

    swapLabelFEta :: (Int,Int) -> AnsatzForestEta -> AnsatzForestEta
    swapLabelFEta inds ans = sortForest.canonicalizeAnsatzEta $ swapAnsatz
            where
                f = swapLabelEta inds 
                swapAnsatz = mapNodes f ans

    swapLabelFEps :: (Int,Int) -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    swapLabelFEps inds ans = sortForestEpsilon.canonicalizeAnsatzEpsilon $ swapAnsatz
            where
                f = swapLabelEpsilon inds 
                swapAnsatz = mapNodesEpsilon f $ M.map (swapLabelFEta inds) ans          

    swapBlockLabelFEta :: I.IntMap Int -> AnsatzForestEta -> AnsatzForestEta
    swapBlockLabelFEta swapF ans = sortForest.canonicalizeAnsatzEta $ swapAnsatz
            where
                f = swapBlockLabelEta swapF 
                swapAnsatz = mapNodes f ans

    swapBlockLabelFEps :: I.IntMap Int -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    swapBlockLabelFEps swapF ans = sortForestEpsilon.canonicalizeAnsatzEpsilon $ swapAnsatz
            where
                f = swapBlockLabelEpsilon swapF 
                swapAnsatz = mapNodesEpsilon f $ M.map (swapBlockLabelFEta swapF) ans 
            
    --symmetrizer functions

    pairSymForestEta :: (Int,Int) -> AnsatzForestEta -> AnsatzForestEta
    pairSymForestEta inds ans = addForests ans $ swapLabelFEta inds ans 

    pairSymForestEps :: (Int,Int) -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    pairSymForestEps inds ans = addForestsEpsilon ans $ swapLabelFEps inds ans 

    pairASymForestEta :: (Int,Int) -> AnsatzForestEta -> AnsatzForestEta
    pairASymForestEta inds ans = addForests ans $ mapVars (multVar (-1)) $ swapLabelFEta inds ans 

    pairASymForestEps :: (Int,Int) -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    pairASymForestEps inds ans = addForestsEpsilon ans $ mapVarsEpsilon (multVar (-1)) $ swapLabelFEps inds ans 

    pairBlockSymForestEta :: I.IntMap Int -> AnsatzForestEta -> AnsatzForestEta
    pairBlockSymForestEta swapF ans = addForests ans $ swapBlockLabelFEta swapF ans 

    pairBlockSymForestEps :: I.IntMap Int -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    pairBlockSymForestEps swapF ans = addForestsEpsilon ans $ swapBlockLabelFEps swapF ans 

    pairBlockASymForestEta :: I.IntMap Int -> AnsatzForestEta -> AnsatzForestEta
    pairBlockASymForestEta swapF ans = addForests ans $ mapVars (multVar (-1)) $ swapBlockLabelFEta swapF ans

    pairBlockASymForestEps :: I.IntMap Int -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    pairBlockASymForestEps swapF ans = addForestsEpsilon ans $ mapVarsEpsilon (multVar (-1)) $ swapBlockLabelFEps swapF ans
    
    cyclicSymForestEta :: [Int] -> AnsatzForestEta -> AnsatzForestEta
    cyclicSymForestEta inds ans = foldr (\y x -> addForests x $ swapBlockLabelFEta y ans ) ans perms
            where
                perms = map (\a -> I.fromList (zip inds a)) $ tail $ permutations inds 

    cyclicSymForestEps :: [Int] -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    cyclicSymForestEps inds ans = foldr (\y x -> addForestsEpsilon x $ swapBlockLabelFEps y ans ) ans perms
            where
                perms = map (\a -> I.fromList (zip inds a)) $ tail $ permutations inds 

    cyclicBlockSymForestEta :: [[Int]] -> AnsatzForestEta -> AnsatzForestEta
    cyclicBlockSymForestEta inds ans = foldr (\y x -> addForests x $ swapBlockLabelFEta y ans ) ans perms
            where
                perms = map (\a -> I.fromList $ zip (concat inds) (concat a)) $ tail $ permutations inds 

    cyclicBlockSymForestEps :: [[Int]] -> AnsatzForestEpsilon-> AnsatzForestEpsilon
    cyclicBlockSymForestEps inds ans = foldr (\y x -> addForestsEpsilon x $ swapBlockLabelFEps y ans ) ans perms
            where
                perms = map (\a -> I.fromList $ zip (concat inds) (concat a)) $ tail $ permutations inds 

    type Symmetry = ( [(Int,Int)] , [(Int,Int)] , [([Int],[Int])] , [[Int]], [[[Int]]] )

    --generall symmetrizer function

    symAnsatzForestEta ::Symmetry -> AnsatzForestEta -> AnsatzForestEta
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

    symAnsatzForestEps :: Symmetry -> AnsatzForestEpsilon -> AnsatzForestEpsilon 
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

    --convert the indLists to lists of eta and or epsilon tensors, if present epsilons are always first 
    
    mkEtaList :: [Int] -> [Eta]
    mkEtaList [] = [] 
    mkEtaList x = (Eta a b) : (mkEtaList rest) 
            where
                [a,b] = take 2 x
                rest = drop 2 x

    mkEpsilonList :: [Int] -> (Epsilon,[Eta])
    mkEpsilonList x = (Epsilon i j k l , mkEtaList rest) 
            where
                [i,j,k,l] = take 4 x
                rest = drop 4 x

    --look up a 1d Forest (obtained from the index list) in the given Forest

    isElem :: [Eta] -> AnsatzForestEta -> Bool
    isElem [] (Leaf x) = True
    isElem x EmptyForest = False 
    isElem  (x:xs) (ForestEta m) 
                | isJust mForest = isElem xs $ fromJust mForest
                | otherwise = False
                where
                    mForest = M.lookup x m

    isElemEpsilon :: (Epsilon, [Eta]) -> AnsatzForestEpsilon -> Bool
    isElemEpsilon (eps,l) m 
                | isJust mForest = isElem l $ fromJust mForest
                | otherwise = False
                 where
                    mForest = M.lookup eps m

    --the next part is evaluating a given AnsatzTree numerically 

    --evaluate the nodes, i.e. eta and epsilon

    evalNodeEta :: M.Map [Int] Int -> I.IntMap Int -> Eta -> Maybe Int
    evalNodeEta epsM iMap (Eta x y) 
                | a == b && a == 0 = Just (-1) 
                | a == b = Just 1
                | otherwise = Nothing
                 where 
                    [a,b] = [(I.!) iMap x, (I.!) iMap y]

    evalNodeEpsilon :: M.Map [Int] Int -> I.IntMap Int -> Epsilon -> Maybe Int
    evalNodeEpsilon epsM iMap (Epsilon w x y z) = M.lookup l epsM
                 where
                    l = [(I.!) iMap w, (I.!) iMap x, (I.!) iMap y, (I.!) iMap z]               

    --check consistency with tensorTree epsilon function in flat Area --> should be right  

    epsMap :: M.Map [Int] Int 
    epsMap = M.fromList $ map (\x -> (x, epsSign x)) $ permutations [0,1,2,3]
                where
                   epsSign [i,j,k,l] = (-1)^(length $  filter (==True) [j>i,k>i,l>i,k>j,l>j,l>k])

    --basic tree eval function

    evalAnsatzForestEta :: M.Map [Int] Int -> I.IntMap Int -> AnsatzForestEta -> I.IntMap Int
    evalAnsatzForestEta epsM evalM (Leaf (Var x y)) = I.singleton y x
    evalAnsatzForestEta epsM evalM (ForestEta m) = M.foldrWithKey foldF I.empty m 
                where
                    foldF k a b = let nodeVal = evalNodeEta epsM evalM k 
                                  in if nodeVal == Nothing then b 
                                     else I.unionWith (+) (I.map ((*) (fromJust nodeVal)) (evalAnsatzForestEta epsM evalM a)) b
    evalAnsatzForestEta epsM evalM EmptyForest = I.empty

    evalAnsatzForestEpsilon :: M.Map [Int] Int -> I.IntMap Int -> AnsatzForestEpsilon -> I.IntMap Int
    evalAnsatzForestEpsilon epsM evalM m = M.foldrWithKey foldF I.empty m 
                where
                    foldF k a b = let nodeVal = evalNodeEpsilon epsM evalM k 
                                  in if nodeVal == Nothing then b 
                                     else I.unionWith (+) (I.map ((*) (fromJust nodeVal)) (evalAnsatzForestEta epsM evalM a)) b

    --for a single Ansatz we do not need the IntMap to keep track of the VarLabels 

    eval1AnsatzForestEta :: M.Map [Int] Int -> I.IntMap Int -> AnsatzForestEta -> Int
    eval1AnsatzForestEta epsM evalM (Leaf (Var x _)) = x
    eval1AnsatzForestEta epsM evalM (ForestEta m) = M.foldrWithKey foldF 0 m
                where
                    foldF k a b = let nodeVal = evalNodeEta epsM evalM k 
                                  in if nodeVal == Nothing then b 
                                     else  b + ((fromJust nodeVal) * (eval1AnsatzForestEta epsM evalM a))
    eval1AnsatzForestEta epsM evalM EmptyForest = 0

    eval1AnsatzForestEpsilon :: M.Map [Int] Int -> I.IntMap Int -> AnsatzForestEpsilon -> Int
    eval1AnsatzForestEpsilon epsM evalM m = M.foldrWithKey foldF 0 m
                where
                    foldF k a b = let nodeVal = evalNodeEpsilon epsM evalM k 
                                  in if nodeVal == Nothing then b 
                                    else  b + ((fromJust nodeVal) * (eval1AnsatzForestEta epsM evalM a))

    --eval a given ansatz to a sparse Matrix (a row vector) -> Eigen Indices start at 0 !!

    evalAnsatzEtaVecList :: M.Map [Int] Int -> [I.IntMap Int] -> AnsatzForestEta -> Maybe (Sparse.SparseMatrixXd) 
    evalAnsatzEtaVecList epsM evalM f  = vecList
            where
                dofList = zip [0..] evalM
                mkAns (i,j) = let ansVal = eval1AnsatzForestEta epsM j f 
                              in if ansVal == 0 then Nothing else Just (0,i, fromIntegral ansVal)  
                l' = mapMaybe mkAns dofList
                l = runEval $ parListChunk 500 rdeepseq l'
                max = maximum $ map (\(x,y,z) -> z) l 
                n = length evalM
                vecList = let vec = Sparse.fromList 1 n l in
                          if l == [] then Nothing else Just $ Sparse.scale (1/max) vec
                

    evalAnsatzEpsilonVecList :: M.Map [Int] Int -> [I.IntMap Int] -> AnsatzForestEpsilon -> Maybe (Sparse.SparseMatrixXd)  
    evalAnsatzEpsilonVecList epsM evalM f  = vecList
            where 
                dofList = zip [0..] evalM
                mkAns (i,j) = let ansVal = eval1AnsatzForestEpsilon epsM j f 
                              in if ansVal == 0 then Nothing else Just (0,i, fromIntegral ansVal)  
                l' = mapMaybe mkAns dofList
                l = runEval $ parListChunk 500 rdeepseq l'
                max = maximum $ map (\(x,y,z) -> z) l 
                n = length evalM
                vecList = let vec = Sparse.fromList 1 n l in
                                    if l == [] then Nothing else Just $ Sparse.scale (1/max) vec

    evalAllTensorEta :: (NFData a) => M.Map [Int] Int -> [(I.IntMap Int, Int, a)] -> AnsatzForestEta -> [([(Int,Int)],Int,a)]
    evalAllTensorEta epsM evalMs f = l'
                where
                    l = map (\(x,y,z) -> (filter (\(a,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEta epsM x f, y,z)) evalMs
                    l' = runEval $ parListChunk 500 rdeepseq l

    evalAllTensorEpsilon :: (NFData a) => M.Map [Int] Int -> [(I.IntMap Int, Int, a)] -> AnsatzForestEpsilon -> [([(Int,Int)],Int,a)]
    evalAllTensorEpsilon epsM evalMs f = l'
                where
                    l = map (\(x,y,z) -> ( filter (\(a,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEpsilon epsM x f, y,z)) evalMs
                    l' = runEval $ parListChunk 500 rdeepseq l

    --------------------------------------------------------------------------------------------------------------------------------

    --now there are two ways how we can proceed

    --1) the memory optimised way, constructing a lin indep tree from the very beginning

    --the first step is to check whether a given Ansatz is elemment of the span of the previos ansätze and therefore can be discarded 

    --function takes as arguments: current determinant of upper left block, current upper left block, the corresponding matrix inverse, current Sparse Ansatz Matrix, new Ansatz rowVector (stored as a sparse matrix)
    
    --function returns: (Det, newMatA, newMatAInv, newfullMat)

    type RankData = (Double, Mat.MatrixXd, Mat.MatrixXd, Sparse.SparseMatrixXd)

    getVarNr :: RankData -> Int 
    getVarNr (_,_,_,ans) = Sparse.rows ans

    --what is a good numerical zero for the determinant

    --the problem is probably that the matrix grows too fast ? -> normalize matrix w.r.t scalarVal ??
            
    checkNumericLinDep :: RankData -> Maybe Sparse.SparseMatrixXd -> Maybe RankData 
    checkNumericLinDep (lastDet, lastMat, lastMatInv, lastFullMat) (Just newVec) 
                | abs(newDet') < 1e-7 = Nothing
                | otherwise = Just (newDet, newMat, newInv, newAnsatzMat)
                 where
                    newVecTrans = Sparse.transpose newVec 
                    scalar = Sparse.toMatrix $ Sparse.mul newVec newVecTrans
                    scalarVal = (Mat.!) scalar (0,0)
                    prodBlock = Sparse.toMatrix $ Sparse.mul lastFullMat newVecTrans
                    prodBlockTrans = Mat.transpose prodBlock
                    newDetPart2Val = (Mat.!) (Mat.mul prodBlockTrans $ Mat.mul lastMatInv prodBlock) (0,0) 
                    newDet' = (scalarVal - newDetPart2Val)
                    newDet = lastDet * newDet'
                    newMat = concatBlockMat lastMat prodBlock prodBlockTrans scalar 
                    newInv = specialBlockInverse lastMatInv prodBlock prodBlockTrans (1/newDet)
                    newAnsatzMat = Sparse.fromRows $ (Sparse.getRows lastFullMat) ++ [newVec]
    checkNumericLinDep (lastDet, lastMat, lastMatInv, lastFullMat) Nothing = Nothing 

    
    concatBlockMat :: Mat.MatrixXd -> Mat.MatrixXd -> Mat.MatrixXd -> Mat.MatrixXd -> Mat.MatrixXd 
    concatBlockMat a b c d = newMat 
                where
                   newUpper = zipWith (++) (Mat.toList a) (Mat.toList b)
                   newLower = zipWith (++) (Mat.toList c) (Mat.toList d)
                   newMat = Mat.fromList $ newUpper ++ newLower 

    --block inverse formula -> check this (is currently used !!)

    specialBlockInverse :: Mat.MatrixXd -> Mat.MatrixXd -> Mat.MatrixXd -> Double -> Mat.MatrixXd
    specialBlockInverse invMat blockB blockC factor = newMat
        where
            block1 = Mat.add invMat $ Mat.map ((*)factor) $ Mat.mul invMat $ Mat.mul blockB $ Mat.mul blockC invMat
            block2 = Mat.map ((*)(-factor)) $ Mat.mul invMat blockB
            block3 = Mat.map ((*)(-factor)) $ Mat.mul blockC invMat
            block4 = [[factor]]
            newUpper = zipWith (++) (Mat.toList block1) (Mat.toList block2)
            newLower = zipWith (++) (Mat.toList block3) block4
            newMat = Mat.fromList $ newUpper ++ newLower 


    addOrDiscardEta :: Symmetry ->  M.Map [Int] Int -> [I.IntMap Int] -> [Eta] -> (AnsatzForestEta, RankData) -> (AnsatzForestEta, RankData)
    addOrDiscardEta symList epsM evalM etaL (ans,rDat) 
                | isElem etaL ans = (ans,rDat)
                | otherwise = case newRDat of 
                                   Nothing          -> (ans,rDat)
                                   Just newRDat'    -> (sumAns,newRDat')      
                 where
                    numVars = getVarNr rDat
                    newAns = symAnsatzForestEta symList $ mkForestFromAscList (etaL,Var 1 (numVars+1))
                    newVec = evalAnsatzEtaVecList epsM evalM newAns
                    newRDat = checkNumericLinDep rDat newVec
                    sumAns = addForests ans newAns

    addOrDiscardEpsilon :: Symmetry ->  M.Map [Int] Int -> [I.IntMap Int] -> (Epsilon,[Eta]) -> (AnsatzForestEpsilon, RankData) -> (AnsatzForestEpsilon, RankData)
    addOrDiscardEpsilon symList epsM evalM (epsL,etaL) (ans,rDat) 
                | isElemEpsilon (epsL,etaL) ans = (ans,rDat)
                | otherwise = case newRDat of 
                                   Nothing          -> (ans,rDat)
                                   Just newRDat'    -> (sumAns,newRDat')      
                 where
                    numVars = getVarNr rDat
                    newAns = symAnsatzForestEps symList $ mkForestFromAscListEpsilon (epsL,etaL, Var 1 (numVars+1))
                    newVec = evalAnsatzEpsilonVecList epsM evalM newAns
                    newRDat = checkNumericLinDep rDat newVec
                    sumAns = addForestsEpsilon ans newAns

    --construct the RankData from the first Ansatz 

    mk1stRankDataEta :: Symmetry -> [[Eta]] -> M.Map [Int] Int -> [I.IntMap Int] -> (AnsatzForestEta,RankData,[[Eta]])
    mk1stRankDataEta symL etaL epsM evalM = output
            where
                newAns = symAnsatzForestEta symL $ mkForestFromAscList (head etaL,Var 1 1)
                newVec = evalAnsatzEtaVecList epsM evalM newAns
                restList = tail etaL 
                output = case newVec of
                                    Nothing         -> mk1stRankDataEta symL restList epsM evalM 
                                    Just newVec'    -> (newAns, (newDet, newMat, newMatInv, newVec'), restList)
                                        where 
                                            newVecTrans = Sparse.transpose newVec'
                                            newMat = Sparse.toMatrix $ Sparse.mul newVec' newVecTrans
                                            newMatInv = Mat.inverse newMat
                                            newDet = (Mat.!) newMat (0,0)


    mk1stRankDataEpsilon :: Symmetry -> [(Epsilon,[Eta])] -> M.Map [Int] Int -> [I.IntMap Int] -> (AnsatzForestEpsilon,RankData,[(Epsilon,[Eta])])
    mk1stRankDataEpsilon symL epsL epsM evalM = output 
            where
                newAns = symAnsatzForestEps symL $ mkForestFromAscListEpsilon (fst $ head epsL, snd $ head epsL,Var 1 1)
                newVec = evalAnsatzEpsilonVecList epsM evalM newAns
                restList = tail epsL
                output = case newVec of
                                    Nothing         -> mk1stRankDataEpsilon symL restList epsM evalM
                                    Just newVec'    -> (newAns,(newDet, newMat, newMatInv, newVec'), restList)
                                        where 
                                            newVecTrans = Sparse.transpose newVec'
                                            newMat = Sparse.toMatrix $ Sparse.mul newVec' newVecTrans
                                            newMatInv = Mat.inverse newMat
                                            newDet = (Mat.!) newMat (0,0)



    --finally reduce the ansatzList  

    reduceAnsatzEta :: Symmetry -> [[Eta]] -> [I.IntMap Int] -> (AnsatzForestEta,Sparse.SparseMatrixXd)
    reduceAnsatzEta symL etaL evalM' = (finalForest, finalMat)
            where
                evalM = canonicalizeEvalMaps symL evalM'  
                epsM = epsMap
                (ans1,rDat1,restEtaL) = mk1stRankDataEta symL etaL epsM evalM
                (finalForest, (_,_,_,finalMat)) = foldr (addOrDiscardEta symL epsM evalM) (ans1,rDat1) restEtaL 

    reduceAnsatzEpsilon :: Symmetry -> [(Epsilon,[Eta])] -> [I.IntMap Int] -> (AnsatzForestEpsilon,Sparse.SparseMatrixXd)
    reduceAnsatzEpsilon symL epsL evalM' = (finalForest, finalMat)
            where
                evalM = canonicalizeEvalMaps symL evalM'
                epsM = epsMap
                (ans1,rDat1,restEpsL) = mk1stRankDataEpsilon symL epsL epsM evalM
                (finalForest, (_,_,_,finalMat)) = foldr (addOrDiscardEpsilon symL epsM evalM) (ans1,rDat1) restEpsL 

    getEtaForest :: [Int] -> [(Int,Int)] -> Symmetry -> [I.IntMap Int] -> (AnsatzForestEta,Sparse.SparseMatrixXd)
    getEtaForest inds filters sym evalMs = reduceAnsatzEta sym allEtaLists evalMs
            where
                allInds = getEtaInds inds filters 
                allEtaLists = map mkEtaList allInds

    getEpsForest :: [Int] -> [(Int,Int)] -> [[Int]] -> [[Int]]  -> Symmetry -> [I.IntMap Int] -> (AnsatzForestEpsilon,Sparse.SparseMatrixXd)
    getEpsForest inds filters symPairInds areaBlockInds sym evalMs = reduceAnsatzEpsilon sym allEpsLists evalMs
            where
                allInds = getEpsilonIndsRed inds filters symPairInds areaBlockInds 
                allEpsLists = map mkEpsilonList allInds

    getFullForest :: [Int] -> [(Int,Int)] -> [[Int]] -> [[Int]] -> Symmetry -> [I.IntMap Int] -> [I.IntMap Int] -> (AnsatzForestEta, AnsatzForestEpsilon, Sparse.SparseMatrixXd, Sparse.SparseMatrixXd)
    getFullForest inds filters symPairInds areaBlockInds sym evalMEta evalMEps = (etaAns, epsAns, etaMat, epsMat)
            where
                (etaAns,etaMat) = getEtaForest inds filters sym evalMEta 
                (epsAns',epsMat) = getEpsForest inds filters symPairInds areaBlockInds sym evalMEps
                epsAns = relabelAnsatzForestEpsilon (1 + (length $ getForestLabels etaAns)) epsAns'


    --finally we can evaluated the final ansatz trees to the ansatz tensor 

    evalToTens ::  M.Map [Int] Int -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6])] -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6])] -> AnsatzForestEta -> AnsatzForestEpsilon -> ATens n1 n2 n3 n4 n5 n6 AnsVar 
    evalToTens epsM evalEta evalEps ansEta ansEps = (fromListT6 etaRmL) &+ (fromListT6 epsRmL)
                where 
                    etaL = evalAllTensorEta epsM evalEta ansEta 
                    epsL = evalAllTensorEpsilon epsM evalEps ansEps 
                    etaL' = map (\(x,mult,indTuple) -> (indTuple, I.fromList $ map (\(i,r) -> (i,fromIntegral $ r*mult)) x)) etaL
                    epsL' = map (\(x,mult,indTuple) -> (indTuple, I.fromList $ map (\(i,r) -> (i,fromIntegral $ r*mult)) x)) epsL
                    etaRmL = filter (\(_,b) -> b /= I.empty) $ concat $ map (\(x,y) -> zip x (repeat y)) etaL'
                    epsRmL = filter (\(_,b) -> b /= I.empty) $ concat $ map (\(x,y) -> zip x (repeat y)) epsL'
    
    mkAnsatzTensor :: Int -> [(Int,Int)] -> [[Int]] -> [[Int]] -> Symmetry -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6])] -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6])] -> (AnsatzForestEta, AnsatzForestEpsilon, ATens n1 n2 n3 n4 n5 n6 AnsVar) 
    mkAnsatzTensor ord filters symPairInds areaBlockInds symmetries evalMEta evalMEps = (ansEta, ansEps, tens)
            where
                epsM = epsMap
                evalMapsEta = map (\(x,y,z) -> x) evalMEta
                evalMapsEps = map (\(x,y,z) -> x) evalMEps  
                indListEta = map (\(x,y,z) -> (y,z)) evalMEta
                indListEps = map (\(x,y,z) -> (y,z)) evalMEps
                (ansEta, ansEps, _, _) = getFullForest [1..ord] filters symPairInds areaBlockInds symmetries evalMapsEta evalMapsEps
                tens = evalToTens epsM evalMEta evalMEps ansEta ansEps 

    -------------------------------------------------------------------------------------------------------

    --the second way to evaluate a given Ansatz is by reducing only algebraically and later on reducing the matrix numerically 

    mkEtaList' :: Var -> [Int] -> ([Eta],Var)
    mkEtaList' var l = (mkEtaList l, var)

    mkEpsilonList' :: Var -> [Int] -> (Epsilon,[Eta],Var)
    mkEpsilonList' var l = (eps, eta, var)
            where
                (eps,eta) = mkEpsilonList l

    reduceAnsatzEta' :: Symmetry -> [([Eta],Var)] -> AnsatzForestEta
    reduceAnsatzEta' sym l = foldr addOrRem' EmptyForest l
            where
                addOrRem' = \ans f -> if (isElem (fst ans) f) then f else addForests f (symAnsatzForestEta sym $ mkForestFromAscList ans)

    reduceAnsatzEpsilon' :: Symmetry -> [(Epsilon, [Eta], Var)] -> AnsatzForestEpsilon
    reduceAnsatzEpsilon' sym l = foldr addOrRem' M.empty l
            where
                addOrRem' = \(x,y,z) f -> if (isElemEpsilon (x,y) f) then f else addForestsEpsilon f (symAnsatzForestEps sym $ mkForestFromAscListEpsilon (x,y,z))

    mkAllVars :: [Var] 
    mkAllVars = map (Var 1) [1..]

    getEtaForestFast :: [Int] -> [(Int,Int)] -> Symmetry -> AnsatzForestEta
    getEtaForestFast inds filters syms = relabelAnsatzForest 1 $ reduceAnsatzEta' syms allForests
                where
                    allInds = getEtaInds inds filters
                    allVars = mkAllVars
                    allForests = zipWith mkEtaList' allVars allInds

    getEpsForestFast :: [Int] -> [(Int,Int)] -> [[Int]] -> [[Int]] -> Symmetry -> AnsatzForestEpsilon
    getEpsForestFast inds filters symIndPairs areaBlocks syms = relabelAnsatzForestEpsilon 1 $ reduceAnsatzEpsilon' syms allForests
                where
                    allInds = getEpsilonIndsRed inds filters symIndPairs areaBlocks 
                    allVars = mkAllVars
                    allForests = zipWith mkEpsilonList' allVars allInds

    --remove duplicates of evaluated the list

    getUniques :: (Eq a, Hashable a) => Int -> [a] -> [a]
    getUniques maxSize elements = runST $
        do
            table <- HTC.newSized maxSize
            sequence_ $ map (\k -> HTC.insert table k ()) elements
            uniques <- HT.toList table
            return $ map fst uniques

    reduceAnsList :: (NFData a) => [([(Int,Int)],Int,a)] -> [[(Int,Int)]]
    reduceAnsList l =  getUniques n l''
        where
            l' = mapMaybe (scaleEqn . normalizeEqn) l
            l'' = runEval $ parListChunk 500 rdeepseq l'
            n = length l' 

    normalizeEqn :: ([(Int, Int)], Int, a) -> Maybe ([(Int, Rational)], Rational)
    normalizeEqn ([],_, _) = Nothing
    normalizeEqn ((x,y):xs, _, _) = Just $ (map (\(a,b) -> (a, (fromIntegral b)/(fromIntegral y))) $ (x,y) : xs, fromIntegral y)

    scaleEqn :: Maybe ([(Int, Rational)], Rational) -> Maybe [(Int, Int)]
    scaleEqn (Just (l,c)) = Just (map (\(x,y) -> (x, toInt (y *  c))) l)
            where 
                toInt z = if denominator z == 1 then fromIntegral $ numerator z else undefined 
    scaleEqn Nothing = Nothing 

    rmDepVars ::  I.IntMap Int -> ([(Int, Int)], Int, a) -> ([(Int, Int)], Int, a)
    rmDepVars s (l,a,b) = (mapMaybe lookupPair l, a, b) 
                    where
                        lookupPair (x,y) = let xVal = (I.lookup x s) in if isJust xVal then Just (fromJust xVal, y) else Nothing

    rmDepVarsTensList :: (NFData a) =>  Int -> [Int] -> [([(Int,Int)],Int,a)] -> [(a, AnsVar)]
    rmDepVarsTensList fstVar iDeps l = lRed
            where
                s = I.fromList $ zip iDeps [fstVar..]
                l' = map (rmDepVars s) l
                l'' = runEval $ parListChunk 500 rdeepseq l'
                lRed = map (\(x,mult,indTuple) -> (indTuple, I.fromList $ map (\(i,r) -> (i,fromIntegral $ r*mult)) x)) l''

    getPivots :: [[(Int, Int)]] -> [Int]
    getPivots l = map (1+) p
            where
                mat = evalAllMatrix l 
                p = Sol.pivots Sol.FullPivLU mat

    evalAllMatrixSp :: [[(Int, Int)]] -> Sparse.SparseMatrixXd 
    evalAllMatrixSp l = Sparse.fromList n m l''
                where
                    l' = concat $ zipWith (\r z -> map (\(x,y) -> (z, x, y)) r) l [1..]
                    n = length l 
                    l'' = map (\(a,b,c) -> (a-1, b-1, fromIntegral c)) l'
                    m = maximum $ map fst $ concat l 

    evalAllMatrix :: [[(Int, Int)]] -> Mat.MatrixXd 
    evalAllMatrix l = Sparse.toMatrix $ Sparse.fromList n m l''
                    where
                        l' = concat $ zipWith (\r z -> map (\(x,y) -> (z, x, y)) r) l [1..]
                        n = length l 
                        l'' = map (\(a,b,c) -> (a-1, b-1, fromIntegral c)) l'
                        m = maximum $ map fst $ concat l 

    getTensorInds :: M.Map [Int] Int -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6])] -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6])] -> AnsatzForestEta -> AnsatzForestEpsilon -> (AnsatzForestEta, AnsatzForestEpsilon, [(IndTuple n1 n2 n3 n4 n5 n6, AnsVar)], [(IndTuple n1 n2 n3 n4 n5 n6, AnsVar)])
    getTensorInds epsM evalMEta evalMEps ansEta ansEpsilon = (newEtaAns, newEpsAns, etaRmL, epsRmL)
            where
                etaL = evalAllTensorEta epsM evalMEta ansEta 
                epsL = evalAllTensorEpsilon epsM evalMEps ansEpsilon
                etaLRed = reduceAnsList etaL 
                epsLRed = reduceAnsList epsL 
                etaVars = getPivots etaLRed 
                epsVars = getPivots epsLRed 
                etaRmL' = rmDepVarsTensList 1 etaVars etaL 
                epsRmL' = rmDepVarsTensList (1 + length etaVars) epsVars epsL 
                etaRmL = filter (\(_,b) -> b /= I.empty) $ concat $ map (\(x,y) -> zip x (repeat y)) etaRmL'
                epsRmL = filter (\(_,b) -> b /= I.empty) $ concat $ map (\(x,y) -> zip x (repeat y)) epsRmL'
                allEtaVars = getForestLabels ansEta
                allEpsVars = getForestLabelsEpsilon ansEpsilon
                remVarsEta =  allEtaVars \\ etaVars
                remVarsEps = allEpsVars \\ epsVars
                newEtaAns = relabelAnsatzForest 1 $ removeVarsEta remVarsEta ansEta
                newEpsAns = relabelAnsatzForestEpsilon (1 + (length $ getForestLabels newEtaAns)) $ removeVarsEps remVarsEps ansEpsilon

    getTensor :: M.Map [Int] Int -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6])] -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6])] -> AnsatzForestEta -> AnsatzForestEpsilon -> (AnsatzForestEta, AnsatzForestEpsilon, ATens n1 n2 n3 n4 n5 n6 AnsVar) 
    getTensor epsM evalMEta evalMEps ansEta ansEpsilon = (ansEta', ansEps', (fromListT6 tIndsEta) &+ (fromListT6 tIndsEps)) 
                where
                    (ansEta', ansEps', tIndsEta, tIndsEps) =  getTensorInds epsM evalMEta evalMEps ansEta ansEpsilon 


    mkAnsatzTensorFast :: Int -> [(Int,Int)] -> [[Int]] -> [[Int]] -> Symmetry -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6])] -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6])] -> (AnsatzForestEta, AnsatzForestEpsilon, ATens n1 n2 n3 n4 n5 n6 AnsVar) 
    mkAnsatzTensorFast ord filters symPairInds areaBlocks symmetries evalMEta evalMEps = getTensor epsM evalMEta evalMEps ansEta ansEpsilon 
            where
                epsM = epsMap
                ansEta = getEtaForestFast [1..ord] filters symmetries 
                ansEpsilon = getEpsForestFast [1..ord] filters symPairInds areaBlocks symmetries  

    
    -----------------------------------------------------------------------------------------------------------------------------------------

    --finally the list for the evaluation 

    --trianglemaps converting from abstract indices to spacetime indices 

    trianMapArea :: I.IntMap [Int]
    trianMapArea = I.fromList $ zip [1..21] list 
            where 
                list = [ [a,b,c,d] | a <- [0..2], b <- [a+1..3], c <- [a..2], d <- [c+1..3], (isAreaSorted a b c d)]

    trianMap2 :: I.IntMap [Int] 
    trianMap2 = I.fromList $ zip [1..10] list 
            where
                list = [ [p,q] | p <- [0..3], q <- [p..3]]

    isAreaSorted :: Int -> Int -> Int -> Int -> Bool
    isAreaSorted a b c d 
             | a < c || (a == c && b <= d) = True
             | otherwise = False 

    --computing the muliplicities that result from the use of the area metric inter twiner
    
    areaMult :: [Int] -> Int
    areaMult [a,b,c,d] 
             | a == c && b == d = 4
             | otherwise = 8
 
    iMult2 :: [Int] -> Int 
    iMult2 [p,q] = if p == q then 1 else 2 

    --in addition to the symmetries from the area and derivative blocks the eval lists can be filtered due to the fact that 
    --1) eta productes only contribute if every index (betweeen 0 and 3) occurs 2n times in the list
    --2) epsilon lists only contribute if every index occurs 2n+1 times in the list 

    countEqualInds :: [Int] -> (Int,Int,Int,Int)
    countEqualInds [] = (0,0,0,0)
    countEqualInds (i:xs) 
            | i == 0 = (a+1,b,c,d)
            | i == 1 = (a,b+1,c,d)
            | i == 2 = (a,b,c+1,d)
            | i == 3 = (a,b,c,d+1)
            | otherwise = error "wrong index"
             where
                (a,b,c,d) = countEqualInds xs
 
    isEtaList :: [Int] -> Bool
    isEtaList l = let (a,b,c,d) = countEqualInds l in even a && even b && even c && even d

    isEpsilonList :: [Int] -> Bool 
    isEpsilonList l = let (a,b,c,d) = countEqualInds l in odd a && odd b && odd c && odd d 

    --the lists for evaluating the ansätze -> output = [(evalMap, multiplicity, Index8)]

    mkEvalMap :: Int -> [([Int],a,b)] -> [(I.IntMap Int,a,b)]
    mkEvalMap ord l = map (\(x,y,z) -> (I.fromList $ zip [1..ord] x, y, z)) l 

    mkEvalMapEta :: Int -> [([Int],a,b)] -> [(I.IntMap Int,a,b)]
    mkEvalMapEta ord l = map (\(x,y,z) -> (I.fromList $ zip [1..ord] x, y, z)) l'
                where 
                    l' = filter (\(a,b,c) -> isEtaList a) l

    mkEvalMapEps :: Int -> [([Int],a,b)] -> [(I.IntMap Int,a,b)]
    mkEvalMapEps ord l = map (\(x,y,z) -> (I.fromList $ zip [1..ord] x, y, z)) l'
                where 
                    l' = filter (\(a,b,c) -> isEpsilonList a) l


    --A
    areaList4Inds :: [(I.IntMap Int, Int, [IndTuple 1 0 0 0 0 0])]
    areaList4Inds = mkEvalMap 4 list 
          where 
              trianArea = trianMapArea
              list = [ let a' = (I.!) trianArea a in (a', areaMult a', [(singletonInd (Ind20 $ a-1), Empty, Empty, Empty, Empty, Empty)]) | a <- [1..21] ]
    
    areaList4IndsEta :: [(I.IntMap Int, Int, [IndTuple 1 0 0 0 0 0])]
    areaList4IndsEta = mkEvalMapEta 4 list 
          where 
              trianArea = trianMapArea
              list = [ let a' = (I.!) trianArea a in (a', areaMult a', [(singletonInd (Ind20 $ a-1), Empty, Empty, Empty, Empty, Empty)]) | a <- [1..21] ]
   
    areaList4IndsEps :: [(I.IntMap Int, Int, [IndTuple 1 0 0 0 0 0])]
    areaList4IndsEps = mkEvalMapEps 4 list 
          where 
              trianArea = trianMapArea
              list = [ let a' = (I.!) trianArea a in (a', areaMult a', [(singletonInd (Ind20 $ a-1), Empty, Empty, Empty, Empty, Empty)]) | a <- [1..21] ]
   

    --AI
    areaList6Inds :: [(I.IntMap Int, Int, [IndTuple 1 0 1 0 0 0])]
    areaList6Inds = mkEvalMap 6 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',i') = ((I.!) trianArea a, (I.!) trian2 i) in  (a' ++ i', (areaMult a') * (iMult2 i'), [(singletonInd (Ind20 $ a-1), Empty, singletonInd (Ind9 $ i-1), Empty, Empty, Empty)]) | a <- [1..21], i <- [1..10]]
 
    areaList6IndsEta :: [(I.IntMap Int, Int, [IndTuple 1 0 1 0 0 0])]
    areaList6IndsEta = mkEvalMapEta 6 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',i') = ((I.!) trianArea a, (I.!) trian2 i) in  (a' ++ i', (areaMult a') * (iMult2 i'), [(singletonInd (Ind20 $ a-1), Empty, singletonInd (Ind9 $ i-1), Empty, Empty, Empty)]) | a <- [1..21], i <- [1..10]]
 
    areaList6IndsEps :: [(I.IntMap Int, Int, [IndTuple 1 0 1 0 0 0])]
    areaList6IndsEps = mkEvalMapEps 6 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',i') = ((I.!) trianArea a, (I.!) trian2 i) in  (a' ++ i', (areaMult a') * (iMult2 i'), [(singletonInd (Ind20 $ a-1), Empty, singletonInd (Ind9 $ i-1), Empty, Empty, Empty)]) | a <- [1..21], i <- [1..10]]
 
    --A:B
    areaList8Inds :: [(I.IntMap Int, Int, [IndTuple 2 0 0 0 0 0])]
    areaList8Inds = mkEvalMap 8 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b') = ((I.!) trianArea a, (I.!) trianArea b) in  (a' ++ b', (areaMult a') * (areaMult b'), map (\[a,b] -> (Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b] )  | a <- [1..21], b <- [a..21]]
  
    areaList8IndsEta :: [(I.IntMap Int, Int, [IndTuple 2 0 0 0 0 0])]
    areaList8IndsEta = mkEvalMapEta 8 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b') = ((I.!) trianArea a, (I.!) trianArea b) in  (a' ++ b', (areaMult a') * (areaMult b'), map (\[a,b] -> (Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b] )  | a <- [1..21], b <- [a..21]]
  
    areaList8IndsEps :: [(I.IntMap Int, Int, [IndTuple 2 0 0 0 0 0])]
    areaList8IndsEps = mkEvalMapEps 8 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b') = ((I.!) trianArea a, (I.!) trianArea b) in  (a' ++ b', (areaMult a') * (areaMult b'), map (\[a,b] -> (Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b] )  | a <- [1..21], b <- [a..21]]

    --Ap:Bq
    areaList10_1Inds :: [(I.IntMap Int, Int, [IndTuple 2 0 0 0 2 0])]
    areaList10_1Inds = mkEvalMap 10 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b') = ((I.!) trianArea a, (I.!) trianArea b) in  (a' ++ p : b' ++ [q], (areaMult a') * (areaMult b'), map (\[[a,p],[b,q]] -> (Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, Empty, Empty, Append (Ind3 $ p) $ singletonInd (Ind3 $ q), Empty)) $ nub $ permutations [[a,p],[b,q]]) | a <- [1..21], b <- [a..21], p <- [0..3], q <- [0..3],  not (a==b && p>q)]
  
    areaList10_1IndsEta :: [(I.IntMap Int, Int, [IndTuple 2 0 0 0 2 0])]
    areaList10_1IndsEta = mkEvalMapEta 10 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b') = ((I.!) trianArea a, (I.!) trianArea b) in  (a' ++ p : b' ++ [q], (areaMult a') * (areaMult b'), map (\[[a,p],[b,q]] -> (Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, Empty, Empty, Append (Ind3 $ p) $ singletonInd (Ind3 $ q), Empty)) $ nub $ permutations [[a,p],[b,q]]) | a <- [1..21], b <- [a..21], p <- [0..3], q <- [0..3],  not (a==b && p>q)]
  
    areaList10_1IndsEps :: [(I.IntMap Int, Int, [IndTuple 2 0 0 0 2 0])]
    areaList10_1IndsEps = mkEvalMapEps 10 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b') = ((I.!) trianArea a, (I.!) trianArea b) in  (a' ++ p : b' ++ [q], (areaMult a') * (areaMult b'), map (\[[a,p],[b,q]] -> (Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, Empty, Empty, Append (Ind3 $ p) $ singletonInd (Ind3 $ q), Empty)) $ nub $ permutations [[a,p],[b,q]]) | a <- [1..21], b <- [a..21], p <- [0..3], q <- [0..3],  not (a==b && p>q)]
  
    --A:BI   
    areaList10_2Inds :: [(I.IntMap Int, Int, [IndTuple 2 0 1 0 0 0])]
    areaList10_2Inds = mkEvalMap 10 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trian2 i) in  (a' ++ b' ++ i', (areaMult a') * (areaMult b') * (iMult2 i'), [ (Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, singletonInd (Ind9 $ i-1), Empty, Empty, Empty)] ) | a <- [1..21], b <- [1..21], i <- [1..10] ]
  
    areaList10_2IndsEta :: [(I.IntMap Int, Int, [IndTuple 2 0 1 0 0 0])]
    areaList10_2IndsEta = mkEvalMapEta 10 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trian2 i) in  (a' ++ b' ++ i', (areaMult a') * (areaMult b') * (iMult2 i'), [ (Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, singletonInd (Ind9 $ i-1), Empty, Empty, Empty)] ) | a <- [1..21], b <- [1..21], i <- [1..10] ]
  
    areaList10_2IndsEps :: [(I.IntMap Int, Int, [IndTuple 2 0 1 0 0 0])]
    areaList10_2IndsEps = mkEvalMapEps 10 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trian2 i) in  (a' ++ b' ++ i', (areaMult a') * (areaMult b') * (iMult2 i'), [ (Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, singletonInd (Ind9 $ i-1), Empty, Empty, Empty)] ) | a <- [1..21], b <- [1..21], i <- [1..10] ]

    --A:B:C
    areaList12Inds ::  [(I.IntMap Int, Int, [IndTuple 3 0 0 0 0 0])]
    areaList12Inds = mkEvalMap 12 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c) in  (a' ++ b' ++ c', (areaMult a') * (areaMult b') * (areaMult c'), map (\[a,b,c] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ singletonInd (Ind20 $ c-1), Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b,c] )| a <- [1..21], b <- [a..21], c <- [b..21] ]
  
    areaList12IndsEta ::  [(I.IntMap Int, Int, [IndTuple 3 0 0 0 0 0])]
    areaList12IndsEta = mkEvalMapEta 12 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c) in  (a' ++ b' ++ c', (areaMult a') * (areaMult b') * (areaMult c'), map (\[a,b,c] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ singletonInd (Ind20 $ c-1), Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b,c] )| a <- [1..21], b <- [a..21], c <- [b..21] ]
  
    areaList12IndsEps ::  [(I.IntMap Int, Int, [IndTuple 3 0 0 0 0 0])]
    areaList12IndsEps = mkEvalMapEps 12 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c) in  (a' ++ b' ++ c', (areaMult a') * (areaMult b') * (areaMult c'), map (\[a,b,c] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ singletonInd (Ind20 $ c-1), Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b,c] )| a <- [1..21], b <- [a..21], c <- [b..21] ]
  
    --AI:BJ
    areaList12_1Inds ::  [(I.IntMap Int, Int, [IndTuple 2 0 2 0 0 0])]
    areaList12_1Inds = mkEvalMap 12 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',i',b',j') = ((I.!) trianArea a, (I.!) trian2 i, (I.!) trianArea b, (I.!) trian2 j) in  (a' ++ i' ++ b' ++ j' , (areaMult a') * (areaMult b') * (iMult2 i') * (iMult2 j'), map (\[[a,i],[b,j]] ->  (Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, Append (Ind9 $ i-1) $ singletonInd (Ind9 $ j-1), Empty, Empty, Empty)) $ nub $ permutations [[a,i],[b,j]] ) | a <- [1..21], b <- [a..21], i <- [1..10], j <- [1..10], not (a==b && i>j) ]
  
    areaList12_1IndsEta ::  [(I.IntMap Int, Int, [IndTuple 2 0 2 0 0 0])]
    areaList12_1IndsEta = mkEvalMapEta 12 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',i',b',j') = ((I.!) trianArea a, (I.!) trian2 i, (I.!) trianArea b, (I.!) trian2 j) in  (a' ++ i' ++ b' ++ j' , (areaMult a') * (areaMult b') * (iMult2 i') * (iMult2 j'), map (\[[a,i],[b,j]] ->  (Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, Append (Ind9 $ i-1) $ singletonInd (Ind9 $ j-1), Empty, Empty, Empty)) $ nub $ permutations [[a,i],[b,j]] ) | a <- [1..21], b <- [a..21], i <- [1..10], j <- [1..10], not (a==b && i>j) ]
  
    areaList12_1IndsEps ::  [(I.IntMap Int, Int, [IndTuple 2 0 2 0 0 0])]
    areaList12_1IndsEps = mkEvalMapEps 12 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',i',b',j') = ((I.!) trianArea a, (I.!) trian2 i, (I.!) trianArea b, (I.!) trian2 j) in  (a' ++ i' ++ b' ++ j' , (areaMult a') * (areaMult b') * (iMult2 i') * (iMult2 j'), map (\[[a,i],[b,j]] ->  (Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, Append (Ind9 $ i-1) $ singletonInd (Ind9 $ j-1), Empty, Empty, Empty)) $ nub $ permutations [[a,i],[b,j]] ) | a <- [1..21], b <- [a..21], i <- [1..10], j <- [1..10], not (a==b && i>j) ]
  
    --A:Bp:Cq
    areaList14_1Inds :: [(I.IntMap Int, Int, [IndTuple 3 0 0 0 2 0])]
    areaList14_1Inds = mkEvalMap 14 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c) in  (a' ++ b' ++ p : c' ++ [q], (areaMult a') * (areaMult b') * (areaMult c'), map (\[[b,p],[c,q]] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ singletonInd (Ind20 $ c-1), Empty, Empty, Empty, Append (Ind3 $ p) $ singletonInd (Ind3 $ q), Empty)) $ nub $ permutations [[b,p],[c,q]]) | a <- [1..21], b <- [1..21], c <- [b..21], p <- [0..3], q <- [0..3], not (b==c && p>q) ]
  
    areaList14_1IndsEta :: [(I.IntMap Int, Int, [IndTuple 3 0 0 0 2 0])]
    areaList14_1IndsEta = mkEvalMapEta 14 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c) in  (a' ++ b' ++ p : c' ++ [q], (areaMult a') * (areaMult b') * (areaMult c'), map (\[[b,p],[c,q]] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ singletonInd (Ind20 $ c-1), Empty, Empty, Empty, Append (Ind3 $ p) $ singletonInd (Ind3 $ q), Empty)) $ nub $ permutations [[b,p],[c,q]]) | a <- [1..21], b <- [1..21], c <- [b..21], p <- [0..3], q <- [0..3], not (b==c && p>q) ]
  
    areaList14_1IndsEps :: [(I.IntMap Int, Int, [IndTuple 3 0 0 0 2 0])]
    areaList14_1IndsEps = mkEvalMapEps 14 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c) in  (a' ++ b' ++ p : c' ++ [q], (areaMult a') * (areaMult b') * (areaMult c'), map (\[[b,p],[c,q]] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ singletonInd (Ind20 $ c-1), Empty, Empty, Empty, Append (Ind3 $ p) $ singletonInd (Ind3 $ q), Empty)) $ nub $ permutations [[b,p],[c,q]]) | a <- [1..21], b <- [1..21], c <- [b..21], p <- [0..3], q <- [0..3], not (b==c && p>q) ]
  
    --A:B:CI
    areaList14_2Inds :: [(I.IntMap Int, Int, [IndTuple 3 0 1 0 0 0])]
    areaList14_2Inds = mkEvalMap 14 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i) in ( a' ++ b' ++ c' ++ i', (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i'), map (\[a,b] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ singletonInd (Ind20 $ c-1), Empty, singletonInd (Ind9 $ i-1), Empty, Empty, Empty)) $ nub $ permutations [a,b] ) | a <- [1..21], b <- [a..21], c <- [1..21], i <- [1..10] ]
  
    areaList14_2IndsEta :: [(I.IntMap Int, Int, [IndTuple 3 0 1 0 0 0])]
    areaList14_2IndsEta = mkEvalMapEta 14 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i) in ( a' ++ b' ++ c' ++ i', (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i'), map (\[a,b] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ singletonInd (Ind20 $ c-1), Empty, singletonInd (Ind9 $ i-1), Empty, Empty, Empty)) $ nub $ permutations [a,b] ) | a <- [1..21], b <- [a..21], c <- [1..21], i <- [1..10] ]
  
    areaList14_2IndsEps :: [(I.IntMap Int, Int, [IndTuple 3 0 1 0 0 0])]
    areaList14_2IndsEps = mkEvalMapEps 14 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i) in ( a' ++ b' ++ c' ++ i', (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i'), map (\[a,b] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ singletonInd (Ind20 $ c-1), Empty, singletonInd (Ind9 $ i-1), Empty, Empty, Empty)) $ nub $ permutations [a,b] ) | a <- [1..21], b <- [a..21], c <- [1..21], i <- [1..10] ]

    --Ap:Bq:CI
    areaList16_1Inds :: [(I.IntMap Int, Int, [IndTuple 3 0 1 0 2 0])]
    areaList16_1Inds = mkEvalMap 16 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i) in (a' ++ p : b' ++ q : c' ++ i' , (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i'), map (\[[a,p],[b,q]] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ singletonInd (Ind20 $ c-1), Empty, singletonInd (Ind9 $ i-1), Empty, Append (Ind3 $ p) $ singletonInd (Ind3 $ q), Empty)) $ nub $ permutations [[a,p],[b,q]]) | a <- [1..21], b <- [a..21], c <- [1..21], i <- [1..10], p <- [0..3], q <- [0..3], not (a==b && p>q) ]
  
    areaList16_1IndsEta :: [(I.IntMap Int, Int, [IndTuple 3 0 1 0 2 0])]
    areaList16_1IndsEta = mkEvalMapEta 16 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i) in (a' ++ p : b' ++ q : c' ++ i' , (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i'), map (\[[a,p],[b,q]] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ singletonInd (Ind20 $ c-1), Empty, singletonInd (Ind9 $ i-1), Empty, Append (Ind3 $ p) $ singletonInd (Ind3 $ q), Empty)) $ nub $ permutations [[a,p],[b,q]]) | a <- [1..21], b <- [a..21], c <- [1..21], i <- [1..10], p <- [0..3], q <- [0..3], not (a==b && p>q) ]
  
    areaList16_1IndsEps :: [(I.IntMap Int, Int, [IndTuple 3 0 1 0 2 0])]
    areaList16_1IndsEps = mkEvalMapEps 16 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i) in (a' ++ p : b' ++ q : c' ++ i' , (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i'), map (\[[a,p],[b,q]] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ singletonInd (Ind20 $ c-1), Empty, singletonInd (Ind9 $ i-1), Empty, Append (Ind3 $ p) $ singletonInd (Ind3 $ q), Empty)) $ nub $ permutations [[a,p],[b,q]]) | a <- [1..21], b <- [a..21], c <- [1..21], i <- [1..10], p <- [0..3], q <- [0..3], not (a==b && p>q) ]
        
    --A:BI:CJ
    areaList16_2Inds :: [(I.IntMap Int, Int, [IndTuple 3 0 2 0 0 0])]
    areaList16_2Inds = mkEvalMap 16 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [let (a',b',c',i', j') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i, (I.!) trian2 j) in  (a' ++ b' ++ i' ++ c' ++ j', (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i') * (iMult2 j'), map (\[[b,i],[c,j]] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ singletonInd (Ind20 $ c-1), Empty, Append (Ind9 $ i-1) $ singletonInd (Ind9 $ j-1), Empty, Empty, Empty) ) $ nub $ permutations [[b,i],[c,j]])| a <- [1..21], b <- [1..21], c <- [b..21], i <- [1..10], j <- [1..10], not (b==c && i>j)]
  
    areaList16_2IndsEta :: [(I.IntMap Int, Int, [IndTuple 3 0 2 0 0 0])]
    areaList16_2IndsEta = mkEvalMapEta 16 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [let (a',b',c',i', j') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i, (I.!) trian2 j) in  (a' ++ b' ++ i' ++ c' ++ j', (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i') * (iMult2 j'), map (\[[b,i],[c,j]] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ singletonInd (Ind20 $ c-1), Empty, Append (Ind9 $ i-1) $ singletonInd (Ind9 $ j-1), Empty, Empty, Empty) ) $ nub $ permutations [[b,i],[c,j]])| a <- [1..21], b <- [1..21], c <- [b..21], i <- [1..10], j <- [1..10], not (b==c && i>j)]
  
    areaList16_2IndsEps :: [(I.IntMap Int, Int, [IndTuple 3 0 2 0 0 0])]
    areaList16_2IndsEps = mkEvalMapEps 16 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [let (a',b',c',i', j') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i, (I.!) trian2 j) in  (a' ++ b' ++ i' ++ c' ++ j', (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i') * (iMult2 j'), map (\[[b,i],[c,j]] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ singletonInd (Ind20 $ c-1), Empty, Append (Ind9 $ i-1) $ singletonInd (Ind9 $ j-1), Empty, Empty, Empty) ) $ nub $ permutations [[b,i],[c,j]])| a <- [1..21], b <- [1..21], c <- [b..21], i <- [1..10], j <- [1..10], not (b==c && i>j)]

    --AI:BJ:CK
    areaList18Inds :: [(I.IntMap Int, Int, [IndTuple 3 0 3 0 0 0])]
    areaList18Inds = mkEvalMap 18 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c',i', j', k') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i, (I.!) trian2 j, (I.!) trian2 k) in  (a' ++ i' ++ b' ++ j' ++ c' ++ k', (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i') * (iMult2 j') * (iMult2 k'), map (\[[a,i],[b,j],[c,k]] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ singletonInd (Ind20 $ c-1), Empty, Append (Ind9 $ i-1) $ Append (Ind9 $ j-1) $ singletonInd (Ind9 $ k-1), Empty, Empty, Empty) ) $ nub $ permutations [[a,i],[b,j],[c,k]]) | a <- [1..21], b <- [a..21], c <- [b..21], i <- [1..10], j <- [1..10], k <- [1..10], not (a==b && i>j), not (b==c && j>k) ]
     
    areaList18IndsEta :: [(I.IntMap Int, Int, [IndTuple 3 0 3 0 0 0])]
    areaList18IndsEta = mkEvalMapEta 18 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c',i', j', k') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i, (I.!) trian2 j, (I.!) trian2 k) in  (a' ++ i' ++ b' ++ j' ++ c' ++ k', (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i') * (iMult2 j') * (iMult2 k'), map (\[[a,i],[b,j],[c,k]] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ singletonInd (Ind20 $ c-1), Empty, Append (Ind9 $ i-1) $ Append (Ind9 $ j-1) $ singletonInd (Ind9 $ k-1), Empty, Empty, Empty) ) $ nub $ permutations [[a,i],[b,j],[c,k]]) | a <- [1..21], b <- [a..21], c <- [b..21], i <- [1..10], j <- [1..10], k <- [1..10], not (a==b && i>j), not (b==c && j>k) ]
     
    areaList18IndsEps :: [(I.IntMap Int, Int, [IndTuple 3 0 3 0 0 0])]
    areaList18IndsEps = mkEvalMapEps 18 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c',i', j', k') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i, (I.!) trian2 j, (I.!) trian2 k) in  (a' ++ i' ++ b' ++ j' ++ c' ++ k', (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i') * (iMult2 j') * (iMult2 k'), map (\[[a,i],[b,j],[c,k]] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ singletonInd (Ind20 $ c-1), Empty, Append (Ind9 $ i-1) $ Append (Ind9 $ j-1) $ singletonInd (Ind9 $ k-1), Empty, Empty, Empty) ) $ nub $ permutations [[a,i],[b,j],[c,k]]) | a <- [1..21], b <- [a..21], c <- [b..21], i <- [1..10], j <- [1..10], k <- [1..10], not (a==b && i>j), not (b==c && j>k) ]
     
    --order 4 
 
    --A:B:C_D
    areaList16Inds ::  [(I.IntMap Int, Int, [IndTuple 4 0 0 0 0 0])]
    areaList16Inds = mkEvalMap 16 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c', d') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d) in  (a' ++ b' ++ c' ++ d', (areaMult a') * (areaMult b') * (areaMult c') * (areaMult d'), map (\[a,b,c,d] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ Append (Ind20 $ c-1) $ singletonInd (Ind20 $ d-1), Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b,c,d] )| a <- [1..21], b <- [a..21], c <- [b..21], d <- [c..21] ]
  
    areaList16IndsEta ::  [(I.IntMap Int, Int, [IndTuple 4 0 0 0 0 0])]
    areaList16IndsEta = mkEvalMapEta 16 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c', d') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d) in  (a' ++ b' ++ c' ++ d', (areaMult a') * (areaMult b') * (areaMult c') * (areaMult d'), map (\[a,b,c,d] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ Append (Ind20 $ c-1) $ singletonInd (Ind20 $ d-1), Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b,c,d] )| a <- [1..21], b <- [a..21], c <- [b..21], d <- [c..21] ]
  
    areaList16IndsEps ::  [(I.IntMap Int, Int, [IndTuple 4 0 0 0 0 0])]
    areaList16IndsEps = mkEvalMapEps 16 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c', d') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d) in  (a' ++ b' ++ c' ++ d', (areaMult a') * (areaMult b') * (areaMult c') * (areaMult d'), map (\[a,b,c,d] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ Append (Ind20 $ c-1) $ singletonInd (Ind20 $ d-1), Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b,c,d] )| a <- [1..21], b <- [a..21], c <- [b..21], d <- [c..21] ]
  
 
    --A:B:C:DI
    areaList18_2Inds ::  [(I.IntMap Int, Int, [IndTuple 4 0 1 0 0 0])]
    areaList18_2Inds = mkEvalMap 18 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c',d',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d, (I.!) trian2 i) in  (a' ++ b' ++ c'++d'++i', (areaMult a') * (areaMult b') * (areaMult c') * (areaMult d') * (iMult2 i'), map (\[a,b,c] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ Append (Ind20 $ c-1) (singletonInd (Ind20 $ d-1)), Empty, singletonInd (Ind9 $ i-1), Empty, Empty, Empty) ) $ nub $ permutations [a,b,c] ) | a <- [1..21], b <- [a..21], c <- [b..21], d <- [1..21], i <- [1..10] ]
  
    areaList18_2IndsEta ::  [(I.IntMap Int, Int, [IndTuple 4 0 1 0 0 0])]
    areaList18_2IndsEta = mkEvalMapEta 18 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c',d',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d, (I.!) trian2 i) in  (a' ++ b' ++ c'++d'++i', (areaMult a') * (areaMult b') * (areaMult c') * (areaMult d') * (iMult2 i'), map (\[a,b,c] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ Append (Ind20 $ c-1) (singletonInd (Ind20 $ d-1)), Empty, singletonInd (Ind9 $ i-1), Empty, Empty, Empty) ) $ nub $ permutations [a,b,c] ) | a <- [1..21], b <- [a..21], c <- [b..21], d <- [1..21], i <- [1..10] ]
  
    areaList18_2IndsEps ::  [(I.IntMap Int, Int, [IndTuple 4 0 1 0 0 0])]
    areaList18_2IndsEps = mkEvalMapEps 18 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c',d',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d, (I.!) trian2 i) in  (a' ++ b' ++ c'++d'++i', (areaMult a') * (areaMult b') * (areaMult c') * (areaMult d') * (iMult2 i'), map (\[a,b,c] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ Append (Ind20 $ c-1) (singletonInd (Ind20 $ d-1)), Empty, singletonInd (Ind9 $ i-1), Empty, Empty, Empty) ) $ nub $ permutations [a,b,c] ) | a <- [1..21], b <- [a..21], c <- [b..21], d <- [1..21], i <- [1..10] ]

    --A:B:Cp:Dq
    areaList18_3Inds ::  [(I.IntMap Int, Int, [IndTuple 4 0 0 0 2 0])]
    areaList18_3Inds = mkEvalMap 18 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c',d') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d) in  (a' ++ b' ++ c'++ p : d'++[q], (areaMult a') * (areaMult b') * (areaMult c') * (areaMult d'), map ( \(a,b,c,p,d,q) -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ Append (Ind20 $ c-1) (singletonInd (Ind20 $ d-1)), Empty, Empty, Empty, Append (Ind3 p) (singletonInd (Ind3 q)), Empty) ) $ nub [(a,b,c,p,d,q),(b,a,c,p,d,q),(a,b,d,q,c,p),(b,a,d,q,c,p)] ) | a <- [1..21], b <- [a..21], c <- [1..21], d <- [c..21], p <- [0..3], q <- [0..3] , not (c == d && p > q) ]
  
    areaList18_3IndsEta ::  [(I.IntMap Int, Int, [IndTuple 4 0 0 0 2 0])]
    areaList18_3IndsEta = mkEvalMapEta 18 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c',d') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d) in  (a' ++ b' ++ c'++ p : d'++[q], (areaMult a') * (areaMult b') * (areaMult c') * (areaMult d'), map ( \(a,b,c,p,d,q) -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ Append (Ind20 $ c-1) (singletonInd (Ind20 $ d-1)), Empty, Empty, Empty, Append (Ind3 p) (singletonInd (Ind3 q)), Empty) ) $ nub [(a,b,c,p,d,q),(b,a,c,p,d,q),(a,b,d,q,c,p),(b,a,d,q,c,p)] ) | a <- [1..21], b <- [a..21], c <- [1..21], d <- [c..21], p <- [0..3], q <- [0..3] , not (c == d && p > q) ]
  
    areaList18_3IndsEps ::  [(I.IntMap Int, Int, [IndTuple 4 0 0 0 2 0])]
    areaList18_3IndsEps = mkEvalMapEps 18 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c',d') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d) in  (a' ++ b' ++ c'++ p : d'++[q], (areaMult a') * (areaMult b') * (areaMult c') * (areaMult d'), map ( \(a,b,c,p,d,q) -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ Append (Ind20 $ c-1) (singletonInd (Ind20 $ d-1)), Empty, Empty, Empty, Append (Ind3 p) (singletonInd (Ind3 q)), Empty) ) $ nub [(a,b,c,p,d,q),(b,a,c,p,d,q),(a,b,d,q,c,p),(b,a,d,q,c,p)] ) | a <- [1..21], b <- [a..21], c <- [1..21], d <- [c..21], p <- [0..3], q <- [0..3] , not (c == d && p > q) ]

    --order 5 
 
    areaList20Inds ::  [(I.IntMap Int, Int, [IndTuple 5 0 0 0 0 0])]
    areaList20Inds = mkEvalMap 20 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c', d', e') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d, (I.!) trianArea e) in  (a' ++ b' ++ c' ++ d' ++ e', (areaMult a') * (areaMult b') * (areaMult c') * (areaMult d') * (areaMult e'), map (\[a,b,c,d,e] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ Append (Ind20 $ c-1) $ Append (Ind20 $ d-1) $ singletonInd (Ind20 $ e-1), Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b,c,d,e] )| a <- [1..21], b <- [a..21], c <- [b..21], d <- [c..21], e <- [d..21] ]
  
    areaList20IndsEta ::  [(I.IntMap Int, Int, [IndTuple 5 0 0 0 0 0])]
    areaList20IndsEta = mkEvalMapEta 20 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c', d', e') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d, (I.!) trianArea e) in  (a' ++ b' ++ c' ++ d' ++ e', (areaMult a') * (areaMult b') * (areaMult c') * (areaMult d') * (areaMult e'), map (\[a,b,c,d,e] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ Append (Ind20 $ c-1) $ Append (Ind20 $ d-1) $ singletonInd (Ind20 $ e-1), Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b,c,d,e] )| a <- [1..21], b <- [a..21], c <- [b..21], d <- [c..21], e <- [d..21] ]
  
    areaList20IndsEps ::  [(I.IntMap Int, Int, [IndTuple 5 0 0 0 0 0])]
    areaList20IndsEps = mkEvalMapEps 20 list
          where 
              trian2 = trianMap2
              trianArea = trianMapArea
              list = [ let (a',b',c', d', e') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d, (I.!) trianArea e) in  (a' ++ b' ++ c' ++ d' ++ e', (areaMult a') * (areaMult b') * (areaMult c') * (areaMult d') * (areaMult e'), map (\[a,b,c,d,e] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ b-1) $ Append (Ind20 $ c-1) $ Append (Ind20 $ d-1) $ singletonInd (Ind20 $ e-1), Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b,c,d,e] )| a <- [1..21], b <- [a..21], c <- [b..21], d <- [c..21], e <- [d..21] ]
  

    --now the symmetry and filter lists 

    filterList4 :: [(Int,Int)]
    filterList4 = [(1,2),(1,3),(3,4)]

    symPairs4 :: [[Int]]
    symPairs4 = [] 

    areaBlocks4 :: [[Int]]
    areaBlocks4 = [[]]

    symList4 :: Symmetry  
    symList4 = ([], [(1,2),(3,4)], [([1,2],[3,4])], [], [])

    filterList6 :: [(Int,Int)]
    filterList6 = [(1,2),(1,3),(3,4),(5,6)]

    symPairs6 :: [[Int]]
    symPairs6 = [[5,6]] 

    areaBlocks6 :: [[Int]]
    areaBlocks6 = [[1,2,3,4]]

    symList6 :: Symmetry  
    symList6 = ([(5,6)], [(1,2),(3,4)], [([1,2],[3,4])], [], [])

    filterList8 :: [(Int,Int)]
    filterList8 = [(1,2),(1,3),(3,4),(1,5),(5,6),(5,7),(7,8)]

    symPairs8 :: [[Int]]
    symPairs8 = [] 

    areaBlocks8 :: [[Int]]
    areaBlocks8 = [[1,2,3,4],[5,6,7,8]]

    symList8 :: Symmetry  
    symList8 = ([], [(1,2),(3,4),(5,6),(7,8)], [([1,2],[3,4]),([5,6],[7,8]),([1,2,3,4],[5,6,7,8])], [], [])

    filterList10_1 :: [(Int,Int)]
    filterList10_1 = [(1,2),(1,3),(3,4),(1,6),(6,7),(6,8),(8,9)]

    symPairs10_1 :: [[Int]]
    symPairs10_1 = [] 

    areaBlocks10_1 :: [[Int]]
    areaBlocks10_1 = [[1,2,3,4],[6,7,8,9]]

    symList10_1 :: Symmetry  
    symList10_1 = ([], [(1,2),(3,4),(6,7),(8,9)], [([1,2],[3,4]),([6,7],[8,9]),([1,2,3,4,5],[6,7,8,9,10])], [], [])

    filterList10_2 :: [(Int,Int)]
    filterList10_2 = [(1,2),(1,3),(3,4),(5,6),(5,7),(7,8),(9,10)]

    symPairs10_2 :: [[Int]]
    symPairs10_2 = [[9,10]] 

    areaBlocks10_2 :: [[Int]]
    areaBlocks10_2 = [[1,2,3,4],[5,6,7,8]]

    symList10_2 :: Symmetry  
    symList10_2 = ([(9,10)], [(1,2),(3,4),(5,6),(7,8)], [([1,2],[3,4]),([5,6],[7,8])], [], [])

    filterList12 :: [(Int,Int)]
    filterList12 = [(1,2),(1,3),(3,4),(1,5),(5,6),(5,7),(7,8),(5,9),(9,10),(9,11),(11,12)]

    symPairs12 :: [[Int]]
    symPairs12 = [] 

    areaBlocks12 :: [[Int]]
    areaBlocks12 = [[1,2,3,4],[5,6,7,8],[9,10,11,12]]

    symList12 :: Symmetry  
    symList12 = ([], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12])], [], 
                [[[1,2,3,4],[5,6,7,8],[9,10,11,12]]])

    filterList12_1 :: [(Int,Int)]
    filterList12_1 = [(1,2),(1,3),(3,4),(5,6),(1,7),(7,8),(7,9),(9,10),(11,12)]

    symPairs12_1 :: [[Int]]
    symPairs12_1 = [[5,6],[11,12]] 

    areaBlocks12_1 :: [[Int]]
    areaBlocks12_1 = [[1,2,3,4],[7,8,9,10]]

    symList12_1 :: Symmetry  
    symList12_1 = ([(5,6),(11,12)], [(1,2),(3,4),(7,8),(9,10)], [([1,2],[3,4]),([7,8],[9,10]),([1,2,3,4,5,6],[7,8,9,10,11,12])], [], 
                [])

    filterList14_1 :: [(Int,Int)]
    filterList14_1 = [(1,2),(1,3),(3,4),(5,6),(5,7),(7,8),(5,10),(10,11),(10,12),(12,13)]

    symPairs14_1 :: [[Int]]
    symPairs14_1 = [] 

    areaBlocks14_1 :: [[Int]]
    areaBlocks14_1 = [[1,2,3,4],[5,6,7,8],[10,11,12,13]]
    
    symList14_1 :: Symmetry  
    symList14_1 = ([], [(1,2),(3,4),(5,6),(7,8),(10,11),(12,13)], [([1,2],[3,4]),([5,6],[7,8]),([10,11],[12,13]),
                ([5,6,7,8,9],[10,11,12,13,14])], [], [])

    filterList14_2 :: [(Int,Int)]
    filterList14_2 = [(1,2),(1,3),(3,4),(1,5),(5,6),(5,7),(7,8),(9,10),(9,11),(11,12),(13,14)]

    symPairs14_2 :: [[Int]]
    symPairs14_2 = [[13,14]] 

    areaBlocks14_2 :: [[Int]]
    areaBlocks14_2 = [[1,2,3,4],[5,6,7,8],[9,10,11,12]]

    symList14_2 :: Symmetry  
    symList14_2 = ([(13,14)], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12]),([1,2,3,4],[5,6,7,8])], [], [])

    filterList16_1 :: [(Int,Int)]
    filterList16_1 = [(1,2),(1,3),(3,4),(1,6),(6,7),(6,8),(8,9),(11,12),(11,13),(13,14),(15,16)]

    symPairs16_1 :: [[Int]]
    symPairs16_1 = [[15,16]] 

    areaBlocks16_1 :: [[Int]]
    areaBlocks16_1 = [[1,2,3,4],[6,7,8,9],[11,12,13,14]]

    symList16_1 :: Symmetry  
    symList16_1 = ([(15,16)], [(1,2),(3,4),(6,7),(8,9),(11,12),(13,14)], [([1,2],[3,4]),([6,7],[8,9]),([11,12],[13,14]),
                ([1,2,3,4,5],[6,7,8,9,10])], [], [])

    filterList16_2 :: [(Int,Int)]
    filterList16_2 = [(1,2),(1,3),(3,4),(5,6),(5,7),(7,8),(9,10),(5,11),(11,12),(11,13),(13,14),(15,16)]

    symPairs16_2 :: [[Int]]
    symPairs16_2 = [[9,10],[15,16]] 

    areaBlocks16_2 :: [[Int]]
    areaBlocks16_2 = [[1,2,3,4],[5,6,7,8],[11,12,13,14]]

    symList16_2 :: Symmetry  
    symList16_2 = ([(9,10),(15,16)], [(1,2),(3,4),(5,6),(7,8),(11,12),(13,14)], [([1,2],[3,4]),([5,6],[7,8]),([11,12],[13,14]),
                ([5,6,7,8,9,10],[11,12,13,14,15,16])], [], [])

    filterList18 :: [(Int,Int)]
    filterList18 = [(1,2),(1,3),(3,4),(1,7),(5,6),(7,8),(7,9),(9,10),(7,13),(11,12),(13,14),(13,15),(15,16),(17,18)]

    symPairs18 :: [[Int]]
    symPairs18 = [[5,6],[9,10],[15,16]] 

    areaBlocks18 :: [[Int]]
    areaBlocks18 = [[1,2,3,4],[7,8,9,10],[13,14,15,16]]

    symList18 :: Symmetry  
    symList18 = ([(5,6),(11,12),(17,18)], [(1,2),(3,4),(7,8),(9,10),(13,14),(15,16)], [([1,2],[3,4]),([7,8],[9,10]),
                ([13,14],[15,16])], [], [[[1,2,3,4,5,6],[7,8,9,10,11,12],[13,14,15,16,17,18]]])

    --order 4

    filterList16 :: [(Int,Int)]
    filterList16 = [(1,2),(1,3),(3,4),(1,5),(5,6),(5,7),(7,8),(5,9),(9,10),(9,11),(11,12),(9,13),(13,14),(13,15),(15,16)]

    symPairs16 :: [[Int]]
    symPairs16 = [] 

    areaBlocks16 :: [[Int]]
    areaBlocks16 = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]

    symList16 :: Symmetry  
    symList16 = ([], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12),(13,14),(15,16)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12]),([13,14],[15,16])], [], 
                [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]])

    filterList18_2 :: [(Int,Int)]
    filterList18_2 = [(1,2),(1,3),(3,4),(1,5),(5,6),(5,7),(7,8),(5,9),(9,10),(9,11),(11,12),(13,14),(13,15),(15,16),(17,18)]

    symPairs18_2 :: [[Int]]
    symPairs18_2 = [[17,18]] 

    areaBlocks18_2 :: [[Int]]
    areaBlocks18_2 = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]

    symList18_2 :: Symmetry  
    symList18_2 = ([(17,18)], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12),(13,14),(15,16)], [([1,2],[3,4]),([5,6],[7,8]),
                ([9,10],[11,12]),([13,14],[15,16])], [], [[[1,2,3,4],[5,6,7,8],[9,10,11,12]]])

    filterList18_3 :: [(Int,Int)]
    filterList18_3 = [(1,2),(1,3),(3,4),(1,5),(5,6),(5,7),(7,8),(9,10),(9,11),(11,12),(9,14),(14,15),(14,16),(16,17)]

    symPairs18_3 :: [[Int]]
    symPairs18_3 = [] 

    areaBlocks18_3 :: [[Int]]
    areaBlocks18_3 = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[14,15,16,17]]

    symList18_3 :: Symmetry  
    symList18_3 = ([], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12),(14,15),(16,17)], [([1,2],[3,4]),([5,6],[7,8]),
                ([9,10],[11,12]),([14,15],[16,17]),([1,2,3,4],[5,6,7,8]),([9,10,11,12,13],[14,15,16,17,18])], [], [])

    --order 5

    filterList20 :: [(Int,Int)]
    filterList20 = [(1,2),(1,3),(3,4),(1,5),(5,6),(5,7),(7,8),(5,9),(9,10),(9,11),(11,12),(9,13),(13,14),(13,15),(15,16),(13,17),(17,18),(17,19),(19,20)]

    symPairs20 :: [[Int]]
    symPairs20 = [] 

    areaBlocks20 :: [[Int]]
    areaBlocks20 = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16],[17,18,19,20]]

    symList20 :: Symmetry  
    symList20 = ([], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12),(13,14),(15,16),(17,18),(19,20)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12]),([13,14],[15,16]),([17,18],[19,20])], [], 
                [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16],[17,18,19,20]]])

    --------------------------------------------------------------------------------------------------------------------------------------

    --and the function that reduce the lists by making use of lorentzinvariance
 
    --using lorentz invariance we can actually evaluate much less and still check linear deps 

    --due to the lorentz symmetry of the basis tensors we can relabel coordinate axes and end up with the same value 

    --therefore it suffices to evaluate one representative out of every orbit that is generated under coordinate relabeling

    getSyms :: Symmetry -> ([[[Int]]],[[Int]],[[Int]])
    getSyms (pairs,aPairs,blocks,cs,blockCs) = (b,a,d)
                where
                    d = map (\(a,b) -> [a,b]) pairs 
                    a' = filter (\x -> 2 == (length $ fst x)) blocks 
                    a = map (\(a,b) -> a ++ b) $ filter (\x -> elem (head $ fst x, (fst x) !! 1) aPairs) a'
                    block' = map (\(x,y) -> [x,y]) $ blocks \\ a' 
                    b = block' ++ blockCs

    canonicalizeArea' :: [Int] -> I.IntMap Int -> I.IntMap Int 
    canonicalizeArea' [a,b,c,d] iMap = foldr (\(x,y) m -> I.adjust (const x) y m) iMap updateList
            where 
                [a',b',c',d'] = map ((I.!) iMap) [a,b,c,d] 
                [[a'',b''],[c'',d'']] = sort $ map sort [[a',b'],[c',d']]
                updateList = zip [a'',b'',c'',d''] [a,b,c,d]

    canonicalizeArea :: [[Int]] -> I.IntMap Int -> I.IntMap Int 
    canonicalizeArea inds iMap = foldr canonicalizeArea' iMap inds 

    canonicalizeDer' :: [Int] -> I.IntMap Int -> I.IntMap Int 
    canonicalizeDer' [p,q] iMap = foldr (\(x,y) m -> I.adjust (const x) y m) iMap updateList
            where 
                [p',q'] = map ((I.!) iMap) [p,q] 
                [p'',q''] = sort [p',q']
                updateList = zip [p'',q''] [p,q]

    canonicalizeDer :: [[Int]] -> I.IntMap Int -> I.IntMap Int 
    canonicalizeDer inds iMap = foldr canonicalizeDer' iMap inds 

    canonicalizeBlocks' :: [[Int]] -> I.IntMap Int -> I.IntMap Int 
    canonicalizeBlocks' blocks iMap = foldr (\(x,y) m -> I.adjust (const x) y m) iMap updateList
            where  
                vals = map (map ((I.!) iMap)) blocks  
                vals' = sort vals 
                updateList = zip (concat vals') (concat blocks)

    canonicalizeBlocks :: [[[Int]]] -> I.IntMap Int -> I.IntMap Int 
    canonicalizeBlocks inds iMap = foldr canonicalizeBlocks' iMap inds 

    canonicalizeIndsMap :: [[[Int]]] -> [[Int]] -> [[Int]] -> I.IntMap Int -> I.IntMap Int 
    canonicalizeIndsMap blocks areaInds derInds iMap = canonicalizeBlocks blocks $ canonicalizeArea areaInds $ canonicalizeDer derInds iMap 
    
    canonicalizeInds :: [[[Int]]] -> [[Int]] -> [[Int]] -> [Int] -> [Int]
    canonicalizeInds blocks areaInds derInds inds = I.elems $ canonicalizeIndsMap blocks areaInds derInds $ I.fromList $ zip [1..length inds] inds
                
    --the next step is creating all possible relabelings of coordinate axes 

    getInds :: I.IntMap Int -> [Int]
    getInds iMap = nub $ I.elems iMap 

    --if an Ansatz eval List has only 2 different indices we can lable them either [0,1] or [1,0], 3 indices are labeled [0,1,2], .. all permutations, and so on 
    
    getAllIndListsMap :: I.IntMap Int -> [I.IntMap Int]
    getAllIndListsMap iMap = map (\x -> I.map ((I.!) x) iMap) allSwaps
             where 
                inds = getInds iMap 
                n = length inds
                allSwaps = zipWith (\x y -> I.fromList $ zip x y) (repeat inds) $ permutations [0..n-1]

    getAllIndListsList :: [Int] -> [[Int]] 
    getAllIndListsList l = map I.elems $ getAllIndListsMap $ I.fromList $ zip [1..length l] l 

    filterMins :: [[Int]] -> [[Int]]
    filterMins l = map fst $ filter (\x -> n == snd x) l' 
            where
                l' = zip l $ map sum l 
                n = minimum $ map snd l' 

    canonicalizeIndList :: [[[Int]]] -> [[Int]] -> [[Int]] -> I.IntMap Int -> I.IntMap Int 
    canonicalizeIndList blocks areaInds derInds iMap = I.fromList $ zip [1..I.size iMap] canonicL
            where 
                iIndsL = map I.elems $ getAllIndListsMap iMap 
                iIndsL' = filterMins iIndsL 
                canonicL' = nub $ map (canonicalizeInds blocks areaInds derInds) iIndsL'
                canonicL = head $ sort canonicL' 
                
    canonicalizeIndListL :: [[[Int]]] -> [[Int]] -> [[Int]] -> [Int] -> [Int] 
    canonicalizeIndListL blocks areaInds derInds iL = canonicL
            where 
                iIndsL = getAllIndListsList iL 
                iIndsL' = filterMins iIndsL 
                canonicL' = nub $ map (canonicalizeInds blocks areaInds derInds) iIndsL' 
                canonicL = head $ sort canonicL'

    canonicalizeEvalMaps :: Symmetry -> [I.IntMap Int] -> [I.IntMap Int]
    canonicalizeEvalMaps sym iMaps = nub $ map (canonicalizeIndList blocks areaInds derInds) iMaps 
            where 
                (blocks, areaInds, derInds) = getSyms sym 
    


    