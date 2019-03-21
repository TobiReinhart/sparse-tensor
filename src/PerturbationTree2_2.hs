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



module PerturbationTree2_2 (
    AnsatzForestEpsilon(..), AnsatzForestEta(..),
    getEtaForest, getEpsForest, getForestLabels, getForestLabelsEpsilon, epsMap, evalAnsatzForestEta, evalAnsatzForestEpsilon,
    filterList4, symList4, areaEvalMap4,
    filterList6, symList6, areaEvalMap6,
    filterList8, symList8, areaEvalMap8,
    filterList10_1, symList10_1, areaEvalMap10_1,
    filterList10_2, symList10_2, areaEvalMap10_2,
    filterList12, symList12, areaEvalMap12,
    filterList12_1, symList12_1, areaEvalMap12_1,
    filterList14_1, symList14_1, areaEvalMap14_1,
    filterList14_2, symList14_2, areaEvalMap14_2,
    filterList16_1, symList16_1, areaEvalMap16_1,
    filterList16_2, symList16_2, areaEvalMap16_2,
    filterList18, symList18, areaEvalMap18,
    trianMapArea, trianMapDerivative,
    triangleMap2P, triangleMap3P,
     evalAllListEta, evalAllListEpsilon, reduceAnsList, ansatzRank, getRows, getPivots, rmDepVarsAnsList, rmDepVarsTensList,
     getTensor, mkAnsatzTensor,
     areaEvalMap4Inds, areaEvalMap6Inds, areaEvalMap8Inds, areaEvalMap10_1Inds, areaEvalMap10_2Inds, areaEvalMap12Inds, areaEvalMap14_1Inds, areaEvalMap14_2Inds, areaEvalMap12_1Inds,
     doubleCheckAnsatzEta, doubleCheckAnsatzEpsilon, areaEvalMap16_1Inds, areaEvalMap16_2Inds, areaEvalMap18Inds, flattenForest,
     areaEvalMap6IndsFull,
     filterList18_2, symList18_2, areaEvalMap18_2, areaEvalMap18_2Inds,
     filterList18_3, symList18_3, areaEvalMap18_3, areaEvalMap18_3Inds,
     filterList16, symList16, areaEvalMap16, areaEvalMap16Inds





    


) where

    import TensorTreeNumeric4
    import qualified Data.IntMap.Strict as I
    import qualified Data.IntSet as ISet 
    import qualified Data.Map.Strict as M
    import Data.Foldable
    import Data.List
    import Data.Maybe
    import Data.List
    import Numeric.Natural
    import GHC.TypeLits
    import Data.Proxy
    import GHC.TypeLits.Normalise
    import qualified Data.Eigen.Matrix as Mat 
    import qualified Data.Eigen.SparseMatrix as Sparse
    import qualified Data.Eigen.LA as Sol 
    import qualified Data.Eigen.SparseLA as SpSol
    import Control.Parallel.Strategies
    import Control.Monad.ST (runST)
    import qualified Data.HashTable.Class as HT (toList)
    import qualified Data.HashTable.ST.Cuckoo as HTC (newSized, insert)
    import Data.Hashable (Hashable)
    --import qualified Data.Matrix as HasMat 
    --import qualified Data.Vector as Vec
    --import qualified Numeric.LinearAlgebra as Lin 
    --import qualified Numeric.LinearAlgebra.Data as LinDat

    

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

    getEpsilonInds :: [Int] -> [(Int,Int)] -> [[Int]]
    getEpsilonInds l sym = filter (\x -> filterSym x sym) $ getAllIndsEpsilon l

    data Epsilon = Epsilon {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show, Read, Eq, Ord)

    data Eta = Eta {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show, Read, Eq, Ord)

    data Var = Var {-# UNPACK #-} !Rational {-# UNPACK #-} !Int deriving (Show, Read, Eq, Ord)

    mkAllVars :: [Var] 
    mkAllVars = map (Var 1) [1..]

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
    
    getEpsSign :: Epsilon -> Rational 
    getEpsSign (Epsilon i j k l) = (-1)^(length $  filter (==True) [j>i,k>i,l>i,k>j,l>j,l>k])
    {-# INLINEABLE getEpsSign #-}

    addVars :: Var -> Var -> Var 
    addVars (Var x y) (Var x' y') = Var (x + x') y
    {-# INLINEABLE addVars #-}

    multVar :: Rational -> Var -> Var
    multVar x (Var x' y) = Var (x * x') y
    {-# INLINEABLE multVar #-}

    isZeroVar :: Var -> Bool
    isZeroVar (Var x y) = x==0
    {-# INLINEABLE isZeroVar #-}
   
    data AnsatzForestEta = ForestEta (M.Map Eta AnsatzForestEta)| Leaf !Var | EmptyForest  deriving (Show, Read, Eq)

    type AnsatzForestEpsilon = M.Map Epsilon AnsatzForestEta

    forestMap :: AnsatzForestEta -> M.Map Eta AnsatzForestEta
    forestMap (ForestEta m) = m
    {-# INLINEABLE forestMap #-}

    --mapNodes requires resorting

    mapNodes :: (Eta -> Eta) -> AnsatzForestEta -> AnsatzForestEta
    mapNodes f EmptyForest = EmptyForest
    mapNodes f (ForestEta m) = ForestEta $ (M.mapKeys f).(M.map (mapNodes f)) $ m
    mapNodes f (Leaf x) = Leaf x

    mapNodesEpsilon :: (Epsilon -> Epsilon) -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    mapNodesEpsilon f m = M.mapKeys f m

    multForest :: (Var -> Var) -> AnsatzForestEta -> AnsatzForestEta
    multForest f EmptyForest = EmptyForest
    multForest f (Leaf var) = Leaf (f var)
    multForest f (ForestEta m) = ForestEta $ M.map (multForest f) m

    multForestEpsilon :: (Var -> Var) -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    multForestEpsilon f m = M.map (multForest f) $ m


    --add 2 sorted forests (are all zeros removed ?)

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

    addForestsEpsilon :: AnsatzForestEpsilon -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    addForestsEpsilon m1 m2 = M.filter (/= EmptyForest) $ M.unionWith addForests m1 m2

    --flatten Forest to AscList Branches
    
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
                
    mkForestFromAscList :: ([Eta],Var) -> AnsatzForestEta 
    mkForestFromAscList ([],var) = Leaf var
    mkForestFromAscList (x:xs, var) = ForestEta $ M.singleton x $ mkForestFromAscList (xs,var)

    mkForestFromAscListEpsilon :: (Epsilon,[Eta],Var) -> AnsatzForestEpsilon 
    mkForestFromAscListEpsilon (x,y,z) = M.singleton x $ mkForestFromAscList (y,z)
    
    sortForest :: AnsatzForestEta -> AnsatzForestEta
    sortForest f = foldr (flip addList2Forest) EmptyForest fList 
                where
                    fList = flattenForest f

    sortForestEpsilon :: AnsatzForestEpsilon -> AnsatzForestEpsilon 
    sortForestEpsilon f = foldr (flip addList2ForestEpsilon) M.empty fList 
                 where
                    fList = flattenForestEpsilon f

    swapLabelF :: (Int,Int) -> Int -> Int 
    swapLabelF (x,y) z
            | x == z = y
            | y == z = x
            | otherwise = z 

    --there is a problem        

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

    canonicalizeAnsatzEta :: AnsatzForestEta -> AnsatzForestEta
    canonicalizeAnsatzEta  = mapNodes sortEta

    canonicalizeAnsatzEpsilon :: AnsatzForestEpsilon -> AnsatzForestEpsilon
    canonicalizeAnsatzEpsilon m = newMap
                where
                    newMap = M.mapKeys sortEpsilon $ M.mapWithKey (\k v -> multForest (multVar (getEpsSign k) ) v) $ M.map (mapNodes sortEta) m


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
            

    pairSymForestEta :: (Int,Int) -> AnsatzForestEta -> AnsatzForestEta
    pairSymForestEta inds ans = addForests ans $ swapLabelFEta inds ans 

    pairSymForestEps :: (Int,Int) -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    pairSymForestEps inds ans = addForestsEpsilon ans $ swapLabelFEps inds ans 

    pairASymForestEta :: (Int,Int) -> AnsatzForestEta -> AnsatzForestEta
    pairASymForestEta inds ans = addForests ans $ multForest (multVar (-1)) $ swapLabelFEta inds ans 

    pairASymForestEps :: (Int,Int) -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    pairASymForestEps inds ans = addForestsEpsilon ans $ multForestEpsilon (multVar (-1)) $ swapLabelFEps inds ans 

    pairBlockSymForestEta :: I.IntMap Int -> AnsatzForestEta -> AnsatzForestEta
    pairBlockSymForestEta swapF ans = addForests ans $ swapBlockLabelFEta swapF ans 

    pairBlockSymForestEps :: I.IntMap Int -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    pairBlockSymForestEps swapF ans = addForestsEpsilon ans $ swapBlockLabelFEps swapF ans 

    pairBlockASymForestEta :: I.IntMap Int -> AnsatzForestEta -> AnsatzForestEta
    pairBlockASymForestEta swapF ans = addForests ans $ multForest (multVar (-1)) $ swapBlockLabelFEta swapF ans

    pairBlockASymForestEps :: I.IntMap Int -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    pairBlockASymForestEps swapF ans = addForestsEpsilon ans $ multForestEpsilon (multVar (-1)) $ swapBlockLabelFEps swapF ans

    --cyclic symmetrization does not work !!! -> There is a problem 
    
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

    
    mkEtaList' :: [Int] -> [Eta]
    mkEtaList' [] = [] 
    mkEtaList' x = (Eta a b) : (mkEtaList' rest) 
            where
                [a,b] = take 2 x
                rest = drop 2 x

    mkEtaList :: Var -> [Int] -> ([Eta],Var)
    mkEtaList var l = (mkEtaList' l, var)

    mkEpsilonList' :: [Int] -> (Epsilon,[Eta])
    mkEpsilonList' x = (Epsilon i j k l , mkEtaList' rest) 
            where
                [i,j,k,l] = take 4 x
                rest = drop 4 x

    mkEpsilonList :: Var -> [Int] -> (Epsilon,[Eta],Var)
    mkEpsilonList var l = (eps, eta, var)
            where
                (eps,eta) = mkEpsilonList' l
    

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

    reduceAnsatzEta :: Symmetry -> [([Eta],Var)] -> AnsatzForestEta
    reduceAnsatzEta sym l = foldr addOrRem EmptyForest l
            where
                addOrRem = \ans f -> if (isElem (fst ans) f) then f else addForests f (symAnsatzForestEta sym $ mkForestFromAscList ans)

    reduceAnsatzEps :: Symmetry -> [(Epsilon, [Eta], Var)] -> AnsatzForestEpsilon
    reduceAnsatzEps sym l = foldr addOrRem M.empty l
            where
                addOrRem = \(x,y,z) f -> if (isElemEpsilon (x,y) f) then f else addForestsEpsilon f (symAnsatzForestEps sym $ mkForestFromAscListEpsilon (x,y,z))


    getEtaForest :: [Int] -> [(Int,Int)] -> Symmetry -> AnsatzForestEta
    getEtaForest inds filters syms = relabelAnsatzForest $ reduceAnsatzEta syms allForests
                where
                    allInds = getEtaInds inds filters
                    allVars = mkAllVars
                    allForests = zipWith mkEtaList allVars allInds

    getEpsForest :: [Int] -> [(Int,Int)] -> Symmetry -> AnsatzForestEpsilon
    getEpsForest inds filters syms = relabelAnsatzForestEpsilon $ reduceAnsatzEps syms allForests
                where
                    allInds = getEpsilonInds inds filters
                    allVars = mkAllVars
                    allForests = zipWith mkEpsilonList allVars allInds

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

    relabelAnsatzForest :: AnsatzForestEta -> AnsatzForestEta
    relabelAnsatzForest ans = multForest update ans
            where
                vars = getForestLabels ans 
                relabMap = I.fromList $ zip vars [1..]
                update = relabelVar ((I.!) relabMap) 

    relabelAnsatzForestEpsilon :: AnsatzForestEpsilon -> AnsatzForestEpsilon
    relabelAnsatzForestEpsilon ans = multForestEpsilon update ans
            where
                vars = getForestLabelsEpsilon ans 
                relabMap = I.fromList $ zip vars [1..]
                update = relabelVar ((I.!) relabMap) 

   
    --the next step is evaluating the tree 

    evalNodeEta :: M.Map [Int] Rational -> I.IntMap Int -> Eta -> Maybe Rational
    evalNodeEta epsM iMap (Eta x y) 
                | a == b && a == 0 = Just (-1) 
                | a == b = Just 1
                | otherwise = Nothing
                 where 
                    [a,b] = [(I.!) iMap x, (I.!) iMap y]
    evalNodeEpsilon :: M.Map [Int] Rational -> I.IntMap Int -> Epsilon -> Maybe Rational
    evalNodeEpsilon epsM iMap (Epsilon w x y z) = M.lookup l epsM
                 where
                    l = [(I.!) iMap w, (I.!) iMap x, (I.!) iMap y, (I.!) iMap z]               

    --check consistency with tensorTree epsilon function in flat Area 

    epsMap :: M.Map [Int] Rational 
    epsMap = M.fromList $ map (\x -> (x, epsSign x)) $ permutations [0,1,2,3]
                where
                   epsSign [i,j,k,l] = (-1)^(length $  filter (==True) [j>i,k>i,l>i,k>j,l>j,l>k])

    --basic tree eval function

    evalAnsatzForestEta :: M.Map [Int] Rational -> I.IntMap Int -> AnsatzForestEta -> I.IntMap Rational
    evalAnsatzForestEta epsM evalM (Leaf (Var x y)) = I.singleton y x
    evalAnsatzForestEta epsM evalM (ForestEta m) = M.foldrWithKey foldF I.empty m 
                where
                    foldF k a b = let nodeVal = evalNodeEta epsM evalM k 
                                  in if nodeVal == Nothing then b 
                                     else I.unionWith (+) (I.map ((*) (fromJust nodeVal)) (evalAnsatzForestEta epsM evalM a)) b

    evalAnsatzForestEpsilon :: M.Map [Int] Rational -> I.IntMap Int -> AnsatzForestEpsilon -> I.IntMap Rational
    evalAnsatzForestEpsilon epsM evalM m = M.foldrWithKey foldF I.empty m 
                where
                    foldF k a b = let nodeVal = evalNodeEpsilon epsM evalM k 
                                  in if nodeVal == Nothing then b 
                                     else I.unionWith (+) (I.map ((*) (fromJust nodeVal)) (evalAnsatzForestEta epsM evalM a)) b

    --eval All Inds (list of lists with (VarNr, Factor, multiplicity))

    evalAllListEta :: M.Map [Int] Rational -> [(I.IntMap Int, Int, Int)] -> AnsatzForestEta -> [([(Int,Rational)],Int,Int)]
    evalAllListEta epsM evalMs f = l'
                where
                    l = map (\(x,y,z) -> ( filter (\(a,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEta epsM x f, y,z)) evalMs
                    l' = runEval $ parListChunk 1000 rdeepseq l

    evalAllTensorEta :: (NFData a) => M.Map [Int] Rational -> [(I.IntMap Int, Int, a)] -> AnsatzForestEta -> [([(Int,Rational)],Int,a)]
    evalAllTensorEta epsM evalMs f = l'
                where
                    l = map (\(x,y,z) -> ( filter (\(a,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEta epsM x f, y,z)) evalMs
                    l' = runEval $ parListChunk 1000 rdeepseq l

    evalAllListEpsilon :: M.Map [Int] Rational -> [(I.IntMap Int, Int, Int)] -> AnsatzForestEpsilon -> [([(Int,Rational)],Int,Int)]
    evalAllListEpsilon epsM evalMs f = l'
                where
                    l = map (\(x,y,z) -> ( filter (\(a,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEpsilon epsM x f, y,z)) evalMs
                    l' = runEval $ parListChunk 1000 rdeepseq l

    evalAllTensorEpsilon :: (NFData a) => M.Map [Int] Rational -> [(I.IntMap Int, Int, a)] -> AnsatzForestEpsilon -> [([(Int,Rational)],Int,a)]
    evalAllTensorEpsilon epsM evalMs f = l'
                where
                    l = map (\(x,y,z) -> ( filter (\(a,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEpsilon epsM x f, y,z)) evalMs
                    l' = runEval $ parListChunk 1000 rdeepseq l

    getUniques :: (Eq a, Hashable a) => Int -> [a] -> [a]
    getUniques maxSize elements = runST $
        do
            table <- HTC.newSized maxSize
            sequence_ $ map (\k -> HTC.insert table k ()) elements
            uniques <- HT.toList table
            return $ map fst uniques

    reduceAnsList :: (NFData a) => [([(Int,Rational)],Int,a)] -> [[(Int,Rational)]]
    reduceAnsList l =  getUniques n l''
        where
            l' = mapMaybe normalizeEqnNoFac l
            l'' = runEval $ parListChunk 1000 rdeepseq l'
            n = length l' 

    reduceAnsList' :: [([(Int,Rational)],Int,a)] -> [[(Int,Rational)]]
    reduceAnsList' l = map scaleEqn $ nubBy (\x y -> (fst x) == (fst y) ) $ mapMaybe normalizeEqn l 

    normalizeEqn :: ([(Int, Rational)], Int, a) -> Maybe ([(Int, Rational)], Rational)
    normalizeEqn ([],_, _) = Nothing
    normalizeEqn ((x,y):xs, _, _) = Just $ (map (\(a,b) -> (a, b/y)) $ (x,y) : xs, y)

    normalizeEqnNoFac :: ([(Int, Rational)], Int, a) -> Maybe [(Int, Rational)]
    normalizeEqnNoFac ([],_, _) = Nothing
    normalizeEqnNoFac ((x,y):xs, _, _) = Just $ (map (\(a,b) -> (a, b/y)) $ (x,y) : xs)

    scaleEqn :: ([(Int, Rational)], Rational) -> [(Int, Rational)]
    scaleEqn (l,c) = (map (\(x,y) -> (x, (y *  c))) l)

    --remove all linear dependent variables a

    rmDepVars ::  I.IntMap Int -> ([(Int, Rational)], Int, a) -> ([(Int, Rational)], Int, a)
    rmDepVars s (l,a,b) = (mapMaybe lookupPair l, a, b) 
                    where
                        lookupPair (x,y) = let xVal = (I.lookup x s) in if isJust xVal then Just (fromJust xVal, y) else Nothing

    rmDepVarsList :: [Int] -> [([(Int,Rational)],Int,Int)] -> [([(Int,Rational)],Int,Int)]
    rmDepVarsList iDeps l = map (rmDepVars s) l
            where
                s = I.fromList $ zip iDeps [1..]


    --result is (matInd (from trian Map), VarLabel, factor)
    rmDepVarsAnsList :: [Int] -> [([(Int,Rational)],Int,Int)] -> [(Int,Int,Rational)]
    rmDepVarsAnsList iDeps l = concat $ map (\(x,mult,matInd) -> map (\(i,r) -> (matInd,i,r*(fromIntegral mult))) x) lRed'
            where
                s = I.fromList $ zip iDeps [1..]
                lRed = map (rmDepVars s) l 
                lRed' = runEval $ parListChunk 1000 rdeepseq lRed

    rmDepVarsTensList :: (NFData a) =>  Int -> [Int] -> [([(Int,Rational)],Int,a)] -> [(a, VarMap)]
    rmDepVarsTensList fstVar iDeps l = lRed
            where
                s = I.fromList $ zip iDeps [fstVar..]
                l' = map (rmDepVars s) l
                l'' = runEval $ parListChunk 1000 rdeepseq l'
                lRed = map (\(x,mult,indTuple) -> (indTuple, I.fromList $ map (\(i,r) -> (i,r*(fromIntegral mult))) x)) l''

    doubleCheckAnsatzEta ::  M.Map [Int] Rational -> [(I.IntMap Int, Int, Int)] -> AnsatzForestEta -> (Int,Int)
    doubleCheckAnsatzEta epsM evalMs ans = (r1, r2)
            where
                ansL = evalAllListEta epsM evalMs ans 
                redL = reduceAnsList ansL 
                r1 = ansatzRank redL 
                ansVars = getRows redL 
                ansRm = rmDepVarsList ansVars ansL 
                ansRmRed = reduceAnsList ansRm 
                r2 = ansatzRank ansRmRed

    doubleCheckAnsatzEpsilon ::  M.Map [Int] Rational -> [(I.IntMap Int, Int, Int)] -> AnsatzForestEpsilon -> (Int,Int)
    doubleCheckAnsatzEpsilon epsM evalMs ans = (r1, r2)
            where
                ansL = evalAllListEpsilon epsM evalMs ans 
                redL = reduceAnsList ansL 
                r1 = ansatzRank redL 
                ansVars = getPivots redL 
                ansRm = rmDepVarsList ansVars ansL 
                ansRmRed = reduceAnsList ansRm 
                r2 = ansatzRank ansRmRed

    ---------------------------------------------------------------------------------------------------------------------------

    getTensorInds :: Int -> M.Map [Int] Rational -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6 n7 n8])] -> AnsatzForestEta -> AnsatzForestEpsilon -> [(IndTuple n1 n2 n3 n4 n5 n6 n7 n8, VarMap)]
    getTensorInds fstVar epsM evalMs ansEta ansEpsilon = filter (\(_,b) -> b /= I.empty) $ zipWith (\(a,b) (c,d) -> ( if a == c then a else undefined, I.unionWith (+) b d)) etaRmL epsRmL 
            where
                etaL = evalAllTensorEta epsM evalMs ansEta 
                epsL = evalAllTensorEpsilon epsM evalMs ansEpsilon
                etaLRed = reduceAnsList etaL 
                epsLRed = reduceAnsList epsL 
                etaVars = getPivots etaLRed 
                epsVars = getPivots epsLRed 
                etaRmL' = rmDepVarsTensList fstVar etaVars etaL 
                epsRmL' = rmDepVarsTensList (fstVar + length etaVars) epsVars epsL 
                etaRmL = concat $ map (\(x,y) -> zip x (repeat y)) etaRmL'
                epsRmL = concat $ map (\(x,y) -> zip x (repeat y)) epsRmL'


    getTensor :: Int -> M.Map [Int] Rational -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6 n7 n8])] -> AnsatzForestEta -> AnsatzForestEpsilon -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 VarMap 
    getTensor fstVar epsM evalMs ansEta ansEpsilon = fromListTWith8 (I.unionWith (+)) $ getTensorInds fstVar epsM evalMs ansEta ansEpsilon 


    mkAnsatzTensor :: Int -> [(Int,Int)] -> Symmetry -> Int -> M.Map [Int] Rational -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6 n7 n8])] -> Tensor8 n1 n2 n3 n4 n5 n6 n7 n8 VarMap 
    mkAnsatzTensor ord filters symmetries fstVar epsM evalMs = getTensor fstVar epsM evalMs ansEta ansEpsilon 
            where
                ansEta = getEtaForest [1..ord] filters symmetries 
                ansEpsilon = getEpsForest [1..ord] filters symmetries  

    --using eigen 

    evalAllMatrixSp :: [[(Int, Rational)]] -> Sparse.SparseMatrixXd 
    evalAllMatrixSp l = Sparse.fromList n m l''
                where
                    l' = concat $ zipWith (\r z -> map (\(x,y) -> (z, x, y)) r) l [1..]
                    n = length l 
                    l'' = map (\(a,b,c) -> (a-1, b-1, fromRational c)) l'
                    m = maximum $ map fst $ concat l 

    evalAllMatrix :: [[(Int, Rational)]] -> Mat.MatrixXd 
    evalAllMatrix l = Sparse.toMatrix $ Sparse.fromList n m l''
                    where
                        l' = concat $ zipWith (\r z -> map (\(x,y) -> (z, x, y)) r) l [1..]
                        n = length l 
                        l'' = map (\(a,b,c) -> (a-1, b-1, fromRational c)) l'
                        m = maximum $ map fst $ concat l 

    ansatzRank :: [[(Int, Rational)]] -> Int 
    ansatzRank l = Sol.rank Sol.FullPivLU $ evalAllMatrix l 
    
    --coloumns form basis of Image 
    ansatzImage :: [[(Int, Rational)]] -> Mat.MatrixXd 
    ansatzImage l = Sol.image Sol.FullPivLU $ evalAllMatrix l

    --find the correspondinding varLables -> (maybe directly reduce the full matrix)

    getRows :: [[(Int, Rational)]] -> [Int]
    getRows l = map (1+) $ mapMaybe (\x -> elemIndex x l1) l2 
            where
                mat = evalAllMatrix l 
                solMat = Sol.image Sol.FullPivLU mat
                matT = Mat.transpose mat 
                solT = Mat.transpose solMat
                l1 = Mat.toList matT
                l2 = Mat.toList solT

    getPivots :: [[(Int, Rational)]] -> [Int]
    getPivots l = map (1+) p
            where
                mat = evalAllMatrix l 
                p = Sol.pivots Sol.FullPivLU mat
                

    --coloumns (down) form basis of nullspace
    ansatzKernel :: [[(Int, Rational)]] -> Mat.MatrixXd 
    ansatzKernel l = Sol.kernel Sol.FullPivLU $ evalAllMatrix l

    --finally the eval maps 

    trianMapArea :: I.IntMap [Int]
    trianMapArea = I.fromList $ zip [1..21] list 
            where 
                list = [ [a,b,c,d] | a <- [0..2], b <- [a+1..3], c <- [a..2], d <- [c+1..3], (isAreaSorted a b c d)]

    trianMapDerivative :: I.IntMap [Int] 
    trianMapDerivative = I.fromList $ zip [1..10] list 
            where
                list = [ [p,q] | p <- [0..3], q <- [p..3]]

    triangleMap2P :: M.Map [Int] Int 
    triangleMap2P = M.fromList $ zip j k
                    where
                        j = [ [a,b] | a <- [1..315], b <- [a..315] ]
                        k = [1..]

    triangleMap3P ::  M.Map [Int] Int
    triangleMap3P = M.fromList $ zip j k
                    where
                        j = [ [a,b,c] | a <- [1..315], b <- [a..315], c <- [b..315] ]
                        k = [1..]

    triangleMap4P ::  M.Map [Int] Int
    triangleMap4P = M.fromList $ zip j k
                    where
                        j = [ [a,b,c,d] | a <- [1..315], b <- [a..315], c <- [b..315], d <- [c..315] ]
                        k = [1..]

    ind1Div :: Int -> Int -> Int 
    ind1Div a p = 21 + (a-1)*4 + p + 1

    ind2Div :: Int -> Int -> Int 
    ind2Div a i = 105 + (a-1)*10 + i

    --lists store (indexCombination, Multiplicity, AbstractTensorIndex)

    --A
    areaList4 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [([Int], Int, Int)]
    areaList4 trianArea trian2 triangle = list
        where 
            list = [ let a' = (I.!) trianArea a in (a', areaMult a', a) | a <- [1..21] ]
    --AI
    areaList6 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [([Int], Int, Int)]
    areaList6 trianArea trian2 triangle = list
        where 
            list = [ let (a',i') = ((I.!) trianArea a, (I.!) trian2 i) in  (a' ++ i', (areaMult a') * (iMult2 i'), ind2Div a i) | a <- [1..21], i <- [1..10]]

    --A:B
    areaList8 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [([Int], Int, Int)]
    areaList8 trianArea trian2 triangle = list
        where 
            list = [ let (a',b') = ((I.!) trianArea a, (I.!) trianArea b) in  (a' ++ b', (areaMult a') * (areaMult b'), (M.!) triangle [a,b])  | a <- [1..21], b <- [a..21]]

    --Ap:Bq
    areaList10_1 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [([Int], Int, Int)]
    areaList10_1 trianArea trian2 triangle = list
        where 
            list = [ let (a',b') = ((I.!) trianArea a, (I.!) trianArea b) in  (a' ++ p : b' ++ [q], (areaMult a') * (areaMult b'), (M.!) triangle [ind1Div a p, ind1Div b q]) | a <- [1..21], b <- [a..21], p <- [0..3], q <- [0..3], not (a==b && p>q)]

    --A:BI   
    areaList10_2 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [([Int], Int, Int)]
    areaList10_2 trianArea trian2 triangle = list
        where 
            list = [ let (a',b',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trian2 i) in  (a' ++ b' ++ i', (areaMult a') * (areaMult b') * (iMult2 i'), (M.!) triangle [a, ind2Div b i]) | a <- [1..21], b <- [1..21], i <- [1..10] ]

    --A:B:C
    areaList12 ::  I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [([Int], Int, Int)]
    areaList12 trianArea trian2 triangle = list
        where 
            list = [ let (a',b',c') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c) in  (a' ++ b' ++ c', (areaMult a') * (areaMult b') * (areaMult c'), (M.!) triangle [a,b,c]) | a <- [1..21], b <- [a..21], c <- [b..21] ]

    --AI:BJ
    areaList12_1 ::  I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [([Int], Int, Int)]
    areaList12_1  trianArea trian2 triangle = list
        where 
            list = [ let (a',i',b',j') = ((I.!) trianArea a, (I.!) trian2 i, (I.!) trianArea b, (I.!) trian2 j) in  (a' ++ i' ++ b' ++ j' , (areaMult a') * (areaMult b') * (iMult2 i') * (iMult2 j'), (M.!) triangle [ind2Div a i, ind2Div b j]) | a <- [1..21], b <- [a..21], i <- [1..10], j <- [1..10], not (a==b && i>j) ]


    --A:Bp:Cq
    areaList14_1 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [([Int], Int, Int)]
    areaList14_1 trianArea trian2 triangle = list
        where 
            list = [ let (a',b',c') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c) in  (a' ++ b' ++ p : c' ++ [q], (areaMult a') * (areaMult b') * (areaMult c'), (M.!) triangle [a,ind1Div b p, ind1Div c q]) | a <- [1..21], b <- [1..21], c <- [b..21], p <- [0..3], q <- [0..3], not (b==c && p>q) ]


    --A:B:CI
    areaList14_2 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [([Int], Int, Int)]
    areaList14_2 trianArea trian2 triangle = list
        where 
            list = [ let (a',b',c',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i) in ( a' ++ b' ++ c' ++ i', (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i'), (M.!) triangle [a,b,ind2Div c i]) | a <- [1..21], b <- [a..21], c <- [1..21], i <- [1..10] ]

    --Ap:Bq:CI
    areaList16_1 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [([Int], Int, Int)]
    areaList16_1 trianArea trian2 triangle = list
        where 
            list = [ let (a',b',c',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i) in (a' ++ p : b' ++ q : c' ++ i' , (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i'), (M.!) triangle [ind1Div a p, ind1Div b q, ind2Div c i]) | a <- [1..21], b <- [a..21], c <- [1..21], i <- [1..10], p <- [0..3], q <- [0..3], not (a==b && p>q) ]

    --A:BI:CJ
    areaList16_2 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [([Int], Int, Int)]
    areaList16_2 trianArea trian2 triangle = list
        where 
            list = [let (a',b',c',i', j') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i, (I.!) trian2 j) in  (a' ++ b' ++ i' ++ c' ++ j', (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i') * (iMult2 j'), (M.!) triangle [a, ind2Div b i, ind2Div c j] )| a <- [1..21], b <- [1..21], c <- [b..21], i <- [1..10], j <- [1..10], not (b==c && i>j)]

    --AI:BJ:CK
    areaList18 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [([Int], Int, Int)]
    areaList18 trianArea trian2 triangle = list
        where 
            list = [ let (a',b',c',i', j', k') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i, (I.!) trian2 j, (I.!) trian2 k) in  (a' ++ i' ++ b' ++ j' ++ c' ++ k', (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i') * (iMult2 j') * (iMult2 k'), (M.!) triangle [ind2Div a i, ind2Div b j, ind2Div c k] ) | a <- [1..21], b <- [a..21], c <- [b..21], i <- [1..10], j <- [1..10], k <- [1..10], not (a==b && i>j), not (b==c && j>k) ]
   
    --now order 4 

    --A:B:C:D
    areaList16 ::  I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [([Int], Int, Int)]
    areaList16 trianArea trian2 triangle = list
        where 
            list = [ let (a',b',c',d') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d) in  (a' ++ b' ++ c' ++ d', (areaMult a') * (areaMult b') * (areaMult c') * (areaMult d'), (M.!) triangle [a,b,c,d]) | a <- [1..21], b <- [a..21], c <- [b..21], d <- [c..21] ]


    --A:B:C:DI
    areaList18_2 ::  I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [([Int], Int, Int)]
    areaList18_2 trianArea trian2 triangle = list
        where 
            list = [ let (a',b',c',d',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d, (I.!) trian2 i) in  (a' ++ b' ++ c'++d'++i', (areaMult a') * (areaMult b') * (areaMult c') * (areaMult d') * (iMult2 i'), (M.!) triangle [a,b,c,d,i]) | a <- [1..21], b <- [a..21], c <- [b..21], d <- [1..21], i <- [1..10] ]

    --A:B:Cp:Dq
    areaList18_3 ::  I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [([Int], Int, Int)]
    areaList18_3 trianArea trian2 triangle = list
        where 
            list = [ let (a',b',c',d') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d) in  (a' ++ b' ++ c'++ p : d'++[q], (areaMult a') * (areaMult b') * (areaMult c') * (areaMult d'), (M.!) triangle [a,b,c,p,d,q]) | a <- [1..21], b <- [a..21], c <- [1..21], d <- [c..21], p <- [0..3], q <- [0..3] , not (c == d && p > q) ]


    --A
    areaList4Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [([Int], Int, [IndTuple 1 0 0 0 0 0 0 0])]
    areaList4Inds trianArea trian2 = list
         where 
             list = [ let a' = (I.!) trianArea a in (a', areaMult a', [(singletonInd (Uind20 $ a-1) , Empty, Empty, Empty, Empty, Empty, Empty, Empty)]) | a <- [1..21] ]
    --AI
    areaList6Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [([Int], Int, [IndTuple 1 0 0 0 1 0 0 0])]
    areaList6Inds trianArea trian2 = list
         where 
             list = [ let (a',i') = ((I.!) trianArea a, (I.!) trian2 i) in  (a' ++ i', (areaMult a') * (iMult2 i'), [(singletonInd (Uind20 $ a-1) , Empty, Empty, Empty, singletonInd (Uind9 $ i-1), Empty, Empty, Empty)]) | a <- [1..21], i <- [1..10]]


    --A:B
    areaList8Inds :: I.IntMap [Int] -> I.IntMap [Int]  -> [([Int], Int, [IndTuple 2 0 0 0 0 0 0 0])]
    areaList8Inds trianArea trian2 = list
         where 
             list = [ let (a',b') = ((I.!) trianArea a, (I.!) trianArea b) in  (a' ++ b', (areaMult a') * (areaMult b'), map (\[a,b] -> (Append (Uind20 $ a-1) $ singletonInd (Uind20 $ b-1) , Empty, Empty, Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b] )  | a <- [1..21], b <- [a..21]]
 
    --Ap:Bq
    areaList10_1Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [([Int], Int, [IndTuple 2 0 0 0 0 0 2 0])]
    areaList10_1Inds trianArea trian2 = list
         where 
             list = [ let (a',b') = ((I.!) trianArea a, (I.!) trianArea b) in  (a' ++ p : b' ++ [q], (areaMult a') * (areaMult b'), map (\[[a,p],[b,q]] -> (Append (Uind20 $ a-1) $ singletonInd (Uind20 $ b-1) , Empty, Empty, Empty, Empty, Empty, Append (Uind3 $ p) $ singletonInd (Uind3 $ q), Empty)) $ nub $ permutations [[a,p],[b,q]]) | a <- [1..21], b <- [a..21], p <- [0..3], q <- [0..3],  not (a==b && p>q)]
 
    --A:BI   
    areaList10_2Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [([Int], Int, [IndTuple 2 0 0 0 1 0 0 0])]
    areaList10_2Inds trianArea trian2  = list
         where 
             list = [ let (a',b',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trian2 i) in  (a' ++ b' ++ i', (areaMult a') * (areaMult b') * (iMult2 i'), [ (Append (Uind20 $ a-1) $ singletonInd (Uind20 $ b-1) , Empty, Empty, Empty, singletonInd (Uind9 $ i-1), Empty, Empty, Empty)] ) | a <- [1..21], b <- [1..21], i <- [1..10] ]
 
    --A:B:C
    areaList12Inds ::  I.IntMap [Int] -> I.IntMap [Int] -> [([Int], Int, [IndTuple 3 0 0 0 0 0 0 0])]
    areaList12Inds trianArea trian2 = list
         where 
             list = [ let (a',b',c') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c) in  (a' ++ b' ++ c', (areaMult a') * (areaMult b') * (areaMult c'), map (\[a,b,c] -> (Append (Uind20 $ a-1) $ Append (Uind20 $ b-1) $ singletonInd (Uind20 $ c-1), Empty, Empty, Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b,c] )| a <- [1..21], b <- [a..21], c <- [b..21] ]
 
    --AI:BJ
    areaList12_1Inds ::  I.IntMap [Int] -> I.IntMap [Int] -> [([Int], Int, [IndTuple 2 0 0 0 2 0 0 0])]
    areaList12_1Inds  trianArea trian2 = list
         where 
             list = [ let (a',i',b',j') = ((I.!) trianArea a, (I.!) trian2 i, (I.!) trianArea b, (I.!) trian2 j) in  (a' ++ i' ++ b' ++ j' , (areaMult a') * (areaMult b') * (iMult2 i') * (iMult2 j'), map (\[[a,i],[b,j]] ->  (Append (Uind20 $ a-1) $ singletonInd (Uind20 $ b-1) , Empty, Empty, Empty, Append (Uind9 $ i-1) $ singletonInd (Uind9 $ j-1), Empty, Empty, Empty)) $ nub $ permutations [[a,i],[b,j]] ) | a <- [1..21], b <- [a..21], i <- [1..10], j <- [1..10], not (a==b && i>j) ]
 
 
    --A:Bp:Cq
    areaList14_1Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [([Int], Int, [IndTuple 3 0 0 0 0 0 2 0])]
    areaList14_1Inds trianArea trian2 = list
         where 
             list = [ let (a',b',c') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c) in  (a' ++ b' ++ p : c' ++ [q], (areaMult a') * (areaMult b') * (areaMult c'), map (\[[b,p],[c,q]] -> (Append (Uind20 $ a-1) $ Append (Uind20 $ b-1) $ singletonInd (Uind20 $ c-1), Empty, Empty, Empty, Empty, Empty, Append (Uind3 $ p) $ singletonInd (Uind3 $ q), Empty)) $ nub $ permutations [[b,p],[c,q]]) | a <- [1..21], b <- [1..21], c <- [b..21], p <- [0..3], q <- [0..3], not (b==c && p>q) ]
 
 
    --A:B:CI
    areaList14_2Inds :: I.IntMap [Int] -> I.IntMap [Int]  -> [([Int], Int, [IndTuple 3 0 0 0 1 0 0 0])]
    areaList14_2Inds trianArea trian2 = list
         where 
             list = [ let (a',b',c',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i) in ( a' ++ b' ++ c' ++ i', (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i'), map (\[a,b] -> (Append (Uind20 $ a-1) $ Append (Uind20 $ b-1) $ singletonInd (Uind20 $ c-1) , Empty, Empty, Empty, singletonInd (Uind9 $ i-1), Empty, Empty, Empty)) $ nub $ permutations [a,b] ) | a <- [1..21], b <- [a..21], c <- [1..21], i <- [1..10] ]
 
    --Ap:Bq:CI
    areaList16_1Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [([Int], Int, [IndTuple 3 0 0 0 1 0 2 0])]
    areaList16_1Inds trianArea trian2 = list
         where 
             list = [ let (a',b',c',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i) in (a' ++ p : b' ++ q : c' ++ i' , (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i'), map (\[[a,p],[b,q]] -> (Append (Uind20 $ a-1) $ Append (Uind20 $ b-1) $ singletonInd (Uind20 $ c-1) , Empty, Empty, Empty, singletonInd (Uind9 $ i-1), Empty, Append (Uind3 $ p) $ singletonInd (Uind3 $ q), Empty)) $ nub $ permutations [[a,p],[b,q]]) | a <- [1..21], b <- [a..21], c <- [1..21], i <- [1..10], p <- [0..3], q <- [0..3], not (a==b && p>q) ]
 
    --A:BI:CJ
    areaList16_2Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [([Int], Int, [IndTuple 3 0 0 0 2 0 0 0])]
    areaList16_2Inds trianArea trian2 = list
         where 
             list = [let (a',b',c',i', j') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i, (I.!) trian2 j) in  (a' ++ b' ++ i' ++ c' ++ j', (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i') * (iMult2 j'), map (\[[b,i],[c,j]] -> (Append (Uind20 $ a-1) $ Append (Uind20 $ b-1) $ singletonInd (Uind20 $ c-1) , Empty, Empty, Empty, Append (Uind9 $ i-1) $ singletonInd (Uind9 $ j-1), Empty, Empty, Empty) ) $ nub $ permutations [[b,i],[c,j]])| a <- [1..21], b <- [1..21], c <- [b..21], i <- [1..10], j <- [1..10], not (b==c && i>j)]
 
    --AI:BJ:CK
    areaList18Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [([Int], Int, [IndTuple 3 0 0 0 3 0 0 0])]
    areaList18Inds trianArea trian2 = list
         where 
             list = [ let (a',b',c',i', j', k') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i, (I.!) trian2 j, (I.!) trian2 k) in  (a' ++ i' ++ b' ++ j' ++ c' ++ k', (areaMult a') * (areaMult b') * (areaMult c') * (iMult2 i') * (iMult2 j') * (iMult2 k'), map (\[[a,i],[b,j],[c,k]] -> (Append (Uind20 $ a-1) $ Append (Uind20 $ b-1) $ singletonInd (Uind20 $ c-1) , Empty, Empty, Empty, Append (Uind9 $ i-1) $ Append (Uind9 $ j-1) $ singletonInd (Uind9 $ k-1), Empty, Empty, Empty) ) $ nub $ permutations [[a,i],[b,j],[c,k]]) | a <- [1..21], b <- [a..21], c <- [b..21], i <- [1..10], j <- [1..10], k <- [1..10], not (a==b && i>j), not (b==c && j>k) ]
    

    --order 4 

    --A:B:C_D
    areaList16Inds ::  I.IntMap [Int] -> I.IntMap [Int] -> [([Int], Int, [IndTuple 4 0 0 0 0 0 0 0])]
    areaList16Inds trianArea trian2 = list
         where 
             list = [ let (a',b',c', d') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d) in  (a' ++ b' ++ c' ++ d', (areaMult a') * (areaMult b') * (areaMult c') * (areaMult d'), map (\[a,b,c,d] -> (Append (Uind20 $ a-1) $ Append (Uind20 $ b-1) $ Append (Uind20 $ c-1) $ singletonInd (Uind20 $ d-1), Empty, Empty, Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b,c,d] )| a <- [1..21], b <- [a..21], c <- [b..21], d <- [c..21] ]
 

    --A:B:C:DI
    areaList18_2Inds ::  I.IntMap [Int] -> I.IntMap [Int] -> [([Int], Int, [IndTuple 4 0 0 0 1 0 0 0])]
    areaList18_2Inds trianArea trian2 = list
         where 
             list = [ let (a',b',c',d',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d, (I.!) trian2 i) in  (a' ++ b' ++ c'++d'++i', (areaMult a') * (areaMult b') * (areaMult c') * (areaMult d') * (iMult2 i'), map (\[a,b,c] -> (Append (Uind20 $ a-1) $ Append (Uind20 $ b-1) $ Append (Uind20 $ c-1) (singletonInd (Uind20 $ d-1)), Empty, Empty, Empty, singletonInd (Uind9 $ i-1), Empty, Empty, Empty) ) $ nub $ permutations [a,b,c] ) | a <- [1..21], b <- [a..21], c <- [b..21], d <- [1..21], i <- [1..10] ]
 
    --A:B:Cp:Dq
    areaList18_3Inds ::  I.IntMap [Int] -> I.IntMap [Int] -> [([Int], Int, [IndTuple 4 0 0 0 0 0 2 0])]
    areaList18_3Inds trianArea trian2 = list
         where 
             list = [ let (a',b',c',d') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d) in  (a' ++ b' ++ c'++ p : d'++[q], (areaMult a') * (areaMult b') * (areaMult c') * (areaMult d'), map ( \(a,b,c,p,d,q) -> (Append (Uind20 $ a-1) $ Append (Uind20 $ b-1) $ Append (Uind20 $ c-1) (singletonInd (Uind20 $ d-1)), Empty, Empty, Empty, Empty, Empty, Append (Uind3 p) (singletonInd (Uind3 q)), Empty) ) $ nub [(a,b,c,p,d,q),(b,a,c,p,d,q),(a,b,d,q,c,p),(b,a,d,q,c,p)] ) | a <- [1..21], b <- [a..21], c <- [1..21], d <- [c..21], p <- [0..3], q <- [0..3] , not (c == d && p > q) ]
 

    isAreaSorted :: Int -> Int -> Int -> Int -> Bool
    isAreaSorted a b c d 
            | a < c || (a == c && b <= d) = True
            | otherwise = False 
   
    areaMult :: [Int] -> Int
    areaMult [a,b,c,d] 
            | a == c && b == d = 4
            | otherwise = 8

    iMult2 :: [Int] -> Int 
    iMult2 [p,q] = if p == q then 1 else 2 

    --lists store (indexCombinationMap, Multiplicity, AbstractTensorIndex)

    areaEvalMap4 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [(I.IntMap Int, Int, Int)]
    areaEvalMap4 trianArea trian2 triangle = l
        where 
            area4 = areaList4 trianArea trian2 triangle
            l = map (\(x,y,z) -> (I.fromList $ zip [1..4] x, y,z)) area4

    areaEvalMap6 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [(I.IntMap Int, Int, Int)]
    areaEvalMap6 trianArea trian2 triangle = l
        where 
            area6 = areaList6 trianArea trian2 triangle
            l = map (\(x,y,z) -> (I.fromList $ zip [1..6] x, y,z)) area6

    areaEvalMap8 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [(I.IntMap Int, Int, Int)]
    areaEvalMap8 trianArea trian2 triangle = l
        where 
            area8 = areaList8 trianArea trian2 triangle
            l = map (\(x,y,z) -> (I.fromList $ zip [1..8] x, y,z)) area8

    areaEvalMap10_1 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [(I.IntMap Int, Int, Int)]
    areaEvalMap10_1 trianArea trian2 triangle = l
        where 
            area10_1 = areaList10_1 trianArea trian2 triangle
            l = map (\(x,y,z) -> (I.fromList $ zip [1..10] x, y,z)) area10_1

    areaEvalMap10_2 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [(I.IntMap Int, Int, Int)]
    areaEvalMap10_2 trianArea trian2 triangle = l
        where 
            area10_2 = areaList10_2 trianArea trian2 triangle
            l = map (\(x,y,z) -> (I.fromList $ zip [1..10] x, y,z)) area10_2

    areaEvalMap12 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [(I.IntMap Int, Int, Int)]
    areaEvalMap12 trianArea trian2 triangle = l
        where 
            area12 = areaList12 trianArea trian2 triangle
            l = map (\(x,y,z) -> (I.fromList $ zip [1..12] x, y,z)) area12

    areaEvalMap12_1 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [(I.IntMap Int, Int, Int)]
    areaEvalMap12_1 trianArea trian2 triangle = l
        where 
            area12_1 = areaList12_1 trianArea trian2 triangle
            l = map (\(x,y,z) -> (I.fromList $ zip [1..12] x, y,z)) area12_1

    areaEvalMap14_1 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [(I.IntMap Int, Int, Int)]
    areaEvalMap14_1 trianArea trian2 triangle = l
        where 
            area14_1 = areaList14_1 trianArea trian2 triangle
            l = map (\(x,y,z) -> (I.fromList $ zip [1..14] x, y,z)) area14_1

    areaEvalMap14_2 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [(I.IntMap Int, Int, Int)]
    areaEvalMap14_2 trianArea trian2 triangle = l
        where 
            area14_2 = areaList14_2 trianArea trian2 triangle
            l = map (\(x,y,z) -> (I.fromList $ zip [1..14] x, y,z)) area14_2

    areaEvalMap16_1 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [(I.IntMap Int, Int, Int)]
    areaEvalMap16_1 trianArea trian2 triangle = l
        where 
            area16_1 = areaList16_1 trianArea trian2 triangle
            l = map (\(x,y,z) -> (I.fromList $ zip [1..16] x, y,z)) area16_1

    areaEvalMap16_2 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [(I.IntMap Int, Int, Int)]
    areaEvalMap16_2 trianArea trian2 triangle = l
        where 
            area16_2 = areaList16_2 trianArea trian2 triangle
            l = map (\(x,y,z) -> (I.fromList $ zip [1..16] x, y,z)) area16_2

    areaEvalMap18 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [(I.IntMap Int, Int, Int)]
    areaEvalMap18 trianArea trian2 triangle = l
        where 
            area18 = areaList18 trianArea trian2 triangle
            l = map (\(x,y,z) -> (I.fromList $ zip [1..18] x,y,z)) area18

    --order 4

    areaEvalMap16 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [(I.IntMap Int, Int, Int)]
    areaEvalMap16 trianArea trian2 triangle = l
        where 
            area16 = areaList16 trianArea trian2 triangle
            l = map (\(x,y,z) -> (I.fromList $ zip [1..16] x, y,z)) area16


    areaEvalMap18_2 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [(I.IntMap Int, Int, Int)]
    areaEvalMap18_2 trianArea trian2 triangle = l
        where 
            area18 = areaList18_2 trianArea trian2 triangle
            l = map (\(x,y,z) -> (I.fromList $ zip [1..18] x,y,z)) area18

    areaEvalMap18_3 :: I.IntMap [Int] -> I.IntMap [Int] -> M.Map [Int] Int -> [(I.IntMap Int, Int, Int)]
    areaEvalMap18_3 trianArea trian2 triangle = l
        where 
            area18 = areaList18_3 trianArea trian2 triangle
            l = map (\(x,y,z) -> (I.fromList $ zip [1..18] x,y,z)) area18

    --lists store (indexCombinationMap, Multiplicity, AbstractIndices)

    areaEvalMap4Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [(I.IntMap Int, Int, [IndTuple 1 0 0 0 0 0 0 0])]
    areaEvalMap4Inds trianArea trian2 = l
        where 
            area4 = areaList4Inds trianArea trian2
            l = map (\(x,y,z) -> (I.fromList $ zip [1..4] x, y,z)) area4

    areaEvalMap6Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [(I.IntMap Int, Int, [IndTuple 1 0 0 0 1 0 0 0])]
    areaEvalMap6Inds trianArea trian2 = l
        where 
            area6 = areaList6Inds trianArea trian2
            l = map (\(x,y,z) -> (I.fromList $ zip [1..6] x, y,z)) area6

    areaEvalMap8Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [(I.IntMap Int, Int, [IndTuple 2 0 0 0 0 0 0 0])]
    areaEvalMap8Inds trianArea trian2 = l
        where 
            area8 = areaList8Inds trianArea trian2
            l = map (\(x,y,z) -> (I.fromList $ zip [1..8] x, y,z)) area8

    areaEvalMap10_1Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [(I.IntMap Int, Int, [IndTuple 2 0 0 0 0 0 2 0])]
    areaEvalMap10_1Inds trianArea trian2 = l
        where 
            area10_1 = areaList10_1Inds trianArea trian2
            l = map (\(x,y,z) -> (I.fromList $ zip [1..10] x, y,z)) area10_1

    areaEvalMap10_2Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [(I.IntMap Int, Int, [IndTuple 2 0 0 0 1 0 0 0])]
    areaEvalMap10_2Inds trianArea trian2 = l
        where 
            area10_2 = areaList10_2Inds trianArea trian2
            l = map (\(x,y,z) -> (I.fromList $ zip [1..10] x, y,z)) area10_2

    areaEvalMap12Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [(I.IntMap Int, Int, [IndTuple 3 0 0 0 0 0 0 0])]
    areaEvalMap12Inds trianArea trian2 = l
        where 
            area12 = areaList12Inds trianArea trian2
            l = map (\(x,y,z) -> (I.fromList $ zip [1..12] x, y,z)) area12

    areaEvalMap12_1Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [(I.IntMap Int, Int, [IndTuple 2 0 0 0 2 0 0 0])]
    areaEvalMap12_1Inds trianArea trian2 = l
        where 
            area12_1 = areaList12_1Inds trianArea trian2
            l = map (\(x,y,z) -> (I.fromList $ zip [1..12] x, y,z)) area12_1

    areaEvalMap14_1Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [(I.IntMap Int, Int, [IndTuple 3 0 0 0 0 0 2 0])]
    areaEvalMap14_1Inds trianArea trian2 = l
        where 
            area14_1 = areaList14_1Inds trianArea trian2
            l = map (\(x,y,z) -> (I.fromList $ zip [1..14] x, y,z)) area14_1

    areaEvalMap14_2Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [(I.IntMap Int, Int, [IndTuple 3 0 0 0 1 0 0 0])]
    areaEvalMap14_2Inds trianArea trian2 = l
        where 
            area14_2 = areaList14_2Inds trianArea trian2 
            l = map (\(x,y,z) -> (I.fromList $ zip [1..14] x, y,z)) area14_2

    areaEvalMap16_1Inds :: I.IntMap [Int] -> I.IntMap [Int]  -> [(I.IntMap Int, Int, [IndTuple 3 0 0 0 1 0 2 0])]
    areaEvalMap16_1Inds trianArea trian2 = l
        where 
            area16_1 = areaList16_1Inds trianArea trian2
            l = map (\(x,y,z) -> (I.fromList $ zip [1..16] x, y,z)) area16_1

    areaEvalMap16_2Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [(I.IntMap Int, Int, [IndTuple 3 0 0 0 2 0 0 0])]
    areaEvalMap16_2Inds trianArea trian2 = l
        where 
            area16_2 = areaList16_2Inds trianArea trian2
            l = map (\(x,y,z) -> (I.fromList $ zip [1..16] x, y,z)) area16_2

    areaEvalMap18Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [(I.IntMap Int, Int, [IndTuple 3 0 0 0 3 0 0 0])]
    areaEvalMap18Inds trianArea trian2 = l
        where 
            area18 = areaList18Inds trianArea trian2
            l = map (\(x,y,z) -> (I.fromList $ zip [1..18] x,y,z)) area18

    --order 4

    areaEvalMap16Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [(I.IntMap Int, Int, [IndTuple 4 0 0 0 0 0 0 0])]
    areaEvalMap16Inds trianArea trian2 = l
        where 
            area16 = areaList16Inds trianArea trian2
            l = map (\(x,y,z) -> (I.fromList $ zip [1..16] x, y,z)) area16


    areaEvalMap18_2Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [(I.IntMap Int, Int, [IndTuple 4 0 0 0 1 0 0 0])]
    areaEvalMap18_2Inds trianArea trian2 = l
        where 
            area18 = areaList18_2Inds trianArea trian2
            l = map (\(x,y,z) -> (I.fromList $ zip [1..18] x,y,z)) area18

    areaEvalMap18_3Inds :: I.IntMap [Int] -> I.IntMap [Int] -> [(I.IntMap Int, Int, [IndTuple 4 0 0 0 0 0 2 0])]
    areaEvalMap18_3Inds trianArea trian2 = l
        where 
            area18 = areaList18_3Inds trianArea trian2
            l = map (\(x,y,z) -> (I.fromList $ zip [1..18] x,y,z)) area18



    filterList4 :: [(Int,Int)]
    filterList4 = [(1,2),(1,3),(3,4)]

    symList4 :: Symmetry  
    symList4 = ([], [(1,2),(3,4)], [([1,2],[3,4])], [], [])

    filterList6 :: [(Int,Int)]
    filterList6 = [(1,2),(1,3),(3,4),(5,6)]

    symList6 :: Symmetry  
    symList6 = ([(5,6)], [(1,2),(3,4)], [([1,2],[3,4])], [], [])

    filterList8 :: [(Int,Int)]
    filterList8 = [(1,2),(1,3),(3,4),(1,5),(5,6),(5,7),(7,8)]

    symList8 :: Symmetry  
    symList8 = ([], [(1,2),(3,4),(5,6),(7,8)], [([1,2],[3,4]),([5,6],[7,8]),([1,2,3,4],[5,6,7,8])], [], [])

    filterList10_1 :: [(Int,Int)]
    filterList10_1 = [(1,2),(1,3),(3,4),(1,6),(6,7),(6,8),(8,9)]

    symList10_1 :: Symmetry  
    symList10_1 = ([], [(1,2),(3,4),(6,7),(8,9)], [([1,2],[3,4]),([6,7],[8,9]),([1,2,3,4,5],[6,7,8,9,10])], [], [])

    filterList10_2 :: [(Int,Int)]
    filterList10_2 = [(1,2),(1,3),(3,4),(5,6),(5,7),(7,8),(9,10)]

    symList10_2 :: Symmetry  
    symList10_2 = ([(9,10)], [(1,2),(3,4),(5,6),(7,8)], [([1,2],[3,4]),([5,6],[7,8])], [], [])

    filterList12 :: [(Int,Int)]
    filterList12 = [(1,2),(1,3),(3,4),(1,5),(5,6),(5,7),(7,8),(5,9),(9,10),(9,11),(11,12)]

    symList12 :: Symmetry  
    symList12 = ([], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12])], [], 
                [[[1,2,3,4],[5,6,7,8],[9,10,11,12]]])

    filterList12_1 :: [(Int,Int)]
    filterList12_1 = [(1,2),(1,3),(3,4),(5,6),(1,7),(7,8),(7,9),(9,10),(11,12)]

    symList12_1 :: Symmetry  
    symList12_1 = ([(5,6),(11,12)], [(1,2),(3,4),(7,8),(9,10)], [([1,2],[3,4]),([7,8],[9,10]),([1,2,3,4,5,6],[7,8,9,10,11,12])], [], 
                [])

    filterList14_1 :: [(Int,Int)]
    filterList14_1 = [(1,2),(1,3),(3,4),(5,6),(5,7),(7,8),(5,10),(10,11),(10,12),(12,13)]
    
    symList14_1 :: Symmetry  
    symList14_1 = ([], [(1,2),(3,4),(5,6),(7,8),(10,11),(12,13)], [([1,2],[3,4]),([5,6],[7,8]),([10,11],[12,13]),
                ([5,6,7,8,9],[10,11,12,13,14])], [], [])

    filterList14_2 :: [(Int,Int)]
    filterList14_2 = [(1,2),(1,3),(3,4),(1,5),(5,6),(5,7),(7,8),(9,10),(9,11),(11,12),(13,14)]

    symList14_2 :: Symmetry  
    symList14_2 = ([(13,14)], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12]),([1,2,3,4],[5,6,7,8])], [], [])

    filterList16_1 :: [(Int,Int)]
    filterList16_1 = [(1,2),(1,3),(3,4),(1,6),(6,7),(6,8),(8,9),(11,12),(11,13),(13,14),(15,16)]

    symList16_1 :: Symmetry  
    symList16_1 = ([(15,16)], [(1,2),(3,4),(6,7),(8,9),(11,12),(13,14)], [([1,2],[3,4]),([6,7],[8,9]),([11,12],[13,14]),
                ([1,2,3,4,5],[6,7,8,9,10])], [], [])

    filterList16_2 :: [(Int,Int)]
    filterList16_2 = [(1,2),(1,3),(3,4),(5,6),(5,7),(7,8),(9,10),(5,11),(11,12),(11,13),(13,14),(15,16)]

    symList16_2 :: Symmetry  
    symList16_2 = ([(9,10),(15,16)], [(1,2),(3,4),(5,6),(7,8),(11,12),(13,14)], [([1,2],[3,4]),([5,6],[7,8]),([11,12],[13,14]),
                ([5,6,7,8,9,10],[11,12,13,14,15,16])], [], [])

    filterList18 :: [(Int,Int)]
    filterList18 = [(1,2),(1,3),(3,4),(1,7),(5,6),(7,8),(7,9),(9,10),(7,13),(11,12),(13,14),(13,15),(15,16),(17,18)]

    symList18 :: Symmetry  
    symList18 = ([(5,6),(11,12),(17,18)], [(1,2),(3,4),(7,8),(9,10),(13,14),(15,16)], [([1,2],[3,4]),([7,8],[9,10]),
                ([13,14],[15,16])], [], [[[1,2,3,4,5,6],[7,8,9,10,11,12],[13,14,15,16,17,18]]])

    --order 4

    filterList16 :: [(Int,Int)]
    filterList16 = [(1,2),(1,3),(3,4),(1,5),(5,6),(5,7),(7,8),(5,9),(9,10),(9,11),(11,12),(9,13),(13,14),(13,15),(15,16)]

    symList16 :: Symmetry  
    symList16 = ([], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12),(13,14),(15,16)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12]),([13,14],[15,16])], [], 
                [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]])


    filterList18_2 :: [(Int,Int)]
    filterList18_2 = [(1,2),(1,3),(3,4),(1,5),(5,6),(5,7),(7,8),(5,9),(9,10),(9,11),(11,12),(13,14),(13,15),(15,16),(17,18)]

    symList18_2 :: Symmetry  
    symList18_2 = ([(17,18)], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12),(13,14),(15,16)], [([1,2],[3,4]),([5,6],[7,8]),
                ([9,10],[11,12]),([13,14],[15,16])], [], [[[1,2,3,4],[5,6,7,8],[9,10,11,12]]])

    filterList18_3 :: [(Int,Int)]
    filterList18_3 = [(1,2),(1,3),(3,4),(1,5),(5,6),(5,7),(7,8),(9,10),(9,11),(11,12),(9,14),(14,15),(14,16),(16,17)]

    symList18_3 :: Symmetry  
    symList18_3 = ([], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12),(14,15),(16,17)], [([1,2],[3,4]),([5,6],[7,8]),
                ([9,10],[11,12]),([14,15],[16,17]),([1,2,3,4],[5,6,7,8]),([9,10,11,12,13],[14,15,16,17,18])], [], [])


    --try eavluating the full tensor

    areaList6IndsFull :: [([Int], Int, [IndTuple 0 0 0 0 0 0 6 0])]
    areaList6IndsFull = list
         where 
             list = [ ([a,b,c,d,e,f], 1 , [(Empty, Empty, Empty, Empty, Empty, Empty, Append (Uind3 a) $ Append (Uind3 b) $ Append (Uind3 c) $ Append (Uind3 d) $ Append (Uind3 e) (singletonInd (Uind3 f)), Empty)]) | a <- [0..3], b <- [0..3], c <- [0..3], d <- [0..3], e <- [0..3], f <- [0..3]]

    areaEvalMap6IndsFull :: [(I.IntMap Int, Int, [IndTuple 0 0 0 0 0 0 6 0])]
    areaEvalMap6IndsFull = l
        where 
            area6 = areaList6IndsFull
            l = map (\(x,y,z) -> (I.fromList $ zip [1..6] x, y,z)) area6