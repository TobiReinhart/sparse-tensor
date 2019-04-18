{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}


module PerturbationTree2_3 (
    mkAnsatzTensorEig, mkAnsatzTensorEigIO, mkAnsatzTensorFast, getForestLabels, getForestLabelsEpsilon, getEtaInds, getEpsilonInds, getEpsForestEig, evalAllTensorEpsilon,
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
    canonicalizeEvalMaps, getSyms, epsMap,
    decodeAnsatzForestEta, decodeAnsatzForestEpsilon, encodeAnsatzForestEpsilon, encodeAnsatzForestEta, flattenForestEpsilon, getIndSyms,
    getEpsForestFast, flattenForest, getAllIndsEta, getExtraEtaSyms, maxCycleNr, findExtraSym, Var(..),
    evalToTens

    
) where

    import qualified Data.IntMap.Strict as I
    import qualified Data.Map.Strict as M 
    import Data.List
    import Data.Maybe
    import Control.Parallel.Strategies
    import Control.Monad.ST (runST)
    import Data.Ratio
    import Data.Serialize
    import GHC.Generics
    import qualified Data.ByteString.Lazy as BS
    import Codec.Compression.GZip
    import Data.Either

    import Control.Monad (foldM)
    import Control.DeepSeq

    --LinearAlgebra

    import qualified Data.Eigen.Matrix as Mat 
    import qualified Data.Eigen.SparseMatrix as Sparse
    import qualified Data.Eigen.LA as Sol 
    import qualified Data.Eigen.SparseLA as SpSol

    --Haskell Tensors 

    import TensorTreeNumeric4_2

    --construct the list of all possible different products of etas and epsilons -> use symmetries 

    --one 1,3 area pair contracted against epsilon yields an antisymmetry in 2,4 contracted against eta yields another symmetry

    get2ndAreaBlock :: [Int] -> [[Int]] -> Maybe [Int]
    get2ndAreaBlock [a,b] areaBlocks = case block of 
                                        Just block'      -> Just $ block' \\ [a,b]
                                        Nothing          -> Nothing 
        where 
            block = find (\x -> (length $ intersect [a,b] x) == 2) areaBlocks 
    get2ndAreaBlock [a,b,c,d] areaBlocks = case block of 
                                        Just block'         -> Just $ block' \\ [a,b,c,d]
                                        Nothing             -> Nothing
        where 
            block = find (\x -> (length $ intersect [a,b,c,d] x) == 2) areaBlocks 


    --etas must not be contracted against antisymmetric indices

    getAllIndsEta :: [Int] -> [[Int]] -> [[Int]]
    getAllIndsEta [a,b] aSyms = [[a,b]]
    getAllIndsEta [a,b,c,d] aSyms = filter (\[a',b',c',d'] -> (not $ elem [a',b'] aSyms) && (not $ elem [c',d'] aSyms)) [[a,b,c,d],[a,c,b,d],[a,d,b,c]]
    getAllIndsEta (x:xs) aSyms = concat $ map res l
            where
                l = mapMaybe (\y -> if not $ elem [x,y] aSyms then Just ([x,y],delete y xs) else Nothing) xs 
                res (a,b) = (++) a <$> (getAllIndsEta b aSyms)
    getAllIndsEta [] aSyms = [[]]
    getAllInds x aSmys = error "wrong list length"

    --a symmetric or antisymmetric pair contracted against 2 etas yields another symmetric or antisymmetric pair 

    findExtraSym :: [[Int]] -> [Int] -> Maybe [Int]
    findExtraSym inds [a,b] = Just $ sort [head newSymInd1, head newSymInd2]
            where 
                newSymInd1 = head (filter (\x -> elem a x) inds) \\ [a] 
                newSymInd2 = head (filter (\x -> elem b x) inds) \\ [b]  

    --only problem remaining is to filter numerical lin deps frommthe eta ansätze, i.e. no 5 cycles -> does not work at the moment!!

    maxCycleNr :: [[Int]] -> Int 
    maxCycleNr l = maximum $ map length cycles 
            where 
                insertAt [a,b] [] = [[[a,b]]]
                insertAt [a,b] (l:ls) = if (and $ map (elem a) l) || (and $ map (elem b) l) then (nub $ [a,b] : l) : ls else l : (insertAt [a,b] ls) 
                mkCycles [[a,b]] = [[[a,b]]]
                mkCycles ([a,b]:xs) = insertAt [a,b] $ mkCycles xs
                cycles = mkCycles l  

    getExtraEtaSyms :: [Int] -> [[Int]] -> [[Int]] -> [[Int]] -> Maybe ([[Int]], [[Int]])
    getExtraEtaSyms inds syms aSyms areaBlocks
                -- | length inds > 9 && maxCycleNr (aSyms'++aSyms) > 9 = Nothing
                | intersect (syms'++syms) (aSyms'++aSyms) /= [] = Nothing
                | otherwise = Just (syms', aSyms')
                 where 
                    etaL [a,b] = [[a,b]]
                    etaL x = take 2 x : (etaL $ drop 2 x)
                    etaL' = etaL inds
                    newAreaSyms = mapMaybe (\x -> get2ndAreaBlock x areaBlocks) etaL'
                    newSyms = mapMaybe (findExtraSym etaL') syms 
                    newASyms = mapMaybe (findExtraSym etaL') aSyms 
                    syms' = newAreaSyms ++ newSyms
                    aSyms' = newASyms

    --filter 1 representative out of every orbit under the area metric symmetries

    filter1Sym :: [Int] -> (Int,Int) -> Bool 
    filter1Sym l (i,j) = case first of  
                            Just l'   ->  if l' == i then True else False
                            Nothing   -> True  
             where
               first = find (\x -> x == i || x == j) l

    filterSym :: [Int] -> [(Int,Int)] -> Bool
    filterSym l inds = and boolList 
            where
               boolList = map (filter1Sym l) inds 

    filterSymEta :: [Int] -> [(Int,Int)] -> [[Int]] -> [[Int]] -> [[Int]] -> Bool
    filterSymEta [] filters syms aSyms areaBlocks = True  
    filterSymEta inds filters syms aSyms areaBlocks = case extra of 
                                                        Nothing              -> False 
                                                        Just (syms', aSyms') -> filterSym inds (filters ++ (map (\[x,y] -> (x,y)) $ syms' ++ aSyms'))
            where 
                extra = getExtraEtaSyms inds syms aSyms areaBlocks

    {-we can use the following observations :
        as we want to contstruct a basis it suffices to pick representatives of the different symmetry orbits module anti-sym in (>4) indices
            1) whenever 3 indices of one are metric are contracted against an epsilon we can actually express the tensor as one with 4 area indices contracted against epsilon
            2) all tensors with 2 area indices contracted against one epsilon can be expressed as tensors with the first 2 area indices contracted against epsilon 
            3) tensors with a maximum of 1 epsilon contraction per area metric can be exprerssed by those with at least one 2 area contraction 
    -}

    getIndsEpsilon :: Int -> [[Int]] -> [[Int]] -> [[Int]]
    getIndsEpsilon i syms areaBlocks = [ [a,b,c,d] | a <- [1..i-3], b <- [a+1..i-2], c <- [b+1..i-1], d <- [c+1..i], (not $ isSym syms [a,b,c,d]) && (not $ is3Area areaBlocks [a,b,c,d]) && (isValid2Area areaBlocks [a,b,c,d]) && (not $ is1Area areaBlocks [a,b,c,d]) ]
                    where 
                        isSym [] x = False
                        isSym [[a,b]] [i,j,k,l] = length (intersect [a,b] [i,j,k,l]) == 2
                        isSym (x:xs) [i,j,k,l]
                            | isSym [x] [i,j,k,l] = True 
                            | otherwise = isSym xs [i,j,k,l]
                        is3Area [[a,b,c,d]] [i,j,k,l] = length (intersect [a,b,c,d] [i,j,k,l]) == 3
                        is3Area (x:xs) [i,j,k,l]
                            | is3Area [x] [i,j,k,l] = True 
                            | otherwise = is3Area xs [i,j,k,l]
                        isValid2Area [[a,b,c,d]] [i,j,k,l] 
                            | length inter == 2 = inter == [a,b]
                            | otherwise = True 
                             where
                                inter = intersect [a,b,c,d] [i,j,k,l]
                        isValid2Area (x:xs) [i,j,k,l] 
                            | isValid2Area [x] [i,j,k,l] = isValid2Area xs [i,j,k,l]
                            | otherwise = False 
                        is1Area list [i,j,k,l] = (maximum $ map (\x -> length $ intersect [i,j,k,l] x) list) == 1 
                                
    --convert symmetry list in appropriate form 

    getIndSyms :: Symmetry -> ([[Int]],[[Int]],[[Int]])
    getIndSyms (pairs,aPairs,blocks,cs,blockCs) = (pairs',aPairs',areaBs')
                where
                    pairs' = map (\(a,b) -> [a,b]) pairs 
                    aPairs' = map (\(a,b) -> [a,b]) aPairs
                    areaBs'' = filter (\x -> 2 == (length $ fst x)) blocks 
                    areaBs' = map (\(a,b) -> a ++ b) $ filter (\x -> elem (head $ fst x, (fst x) !! 1) aPairs) areaBs''

                    
    getEtaInds :: [Int] -> [(Int,Int)] -> Symmetry -> [[Int]]
    getEtaInds l sym symList = filter (\x -> filterSymEta x sym p aP aB) $ filter (\x -> filterSym x sym) $ getAllIndsEta l aP
                    where 
                        (p,aP,aB) = getIndSyms symList
    
                         
    getEpsilonInds :: [Int] ->[(Int,Int)] -> Symmetry -> [[Int]]
    getEpsilonInds l filters symL = filter (\x -> filterSym x filters) $ l3
            where
                (syms, aSyms, areaBlocks) = getIndSyms symL
                s = length l
                l2 = filter (\x -> filterSym x filters) $ getIndsEpsilon s syms areaBlocks
                l3 = concat $ map res l2
                    where 
                        res [a,b,c,d] = case aBlock of 
                                        Just a'    ->  (++) [a,b,c,d] <$> (let newFList = remFiltersEps filters [a,b,c,d]
                                                                               newSymList = remSymsEps syms [a,b,c,d]
                                                                               newASymList = remASymsEps aSyms [a,b,c,d]
                                                                               newAreaBlocks = remAreaBlocksEps areaBlocks [a,b,c,d]
                                                                                    in filter (\x -> filterSymEta x newFList newSymList (a' : newASymList) newAreaBlocks) $ getAllIndsEta (l \\ [a,b,c,d]) (a' : newASymList))
                                        Nothing   ->  (++) [a,b,c,d] <$> (let newFList = remFiltersEps filters [a,b,c,d]
                                                                              newSymList = remSymsEps syms [a,b,c,d]
                                                                              newASymList = remASymsEps aSyms [a,b,c,d]
                                                                              newAreaBlocks = remAreaBlocksEps areaBlocks [a,b,c,d]
                                                                                    in filter (\x -> filterSymEta x newFList newSymList newASymList newAreaBlocks) $ getAllIndsEta (l \\ [a,b,c,d]) newASymList)
                            where 
                                aBlock = get2ndAreaBlock [a,b,c,d] areaBlocks
                                remFiltersEps fList epsInds = filter (\(a,b) -> not (elem a epsInds || elem b epsInds)) fList 
                                remSymsEps symL epsInds = filter (\x -> intersect x epsInds == []) symL 
                                remASymsEps aSymL epsInds = filter (\x -> intersect x epsInds == []) aSymL
                                remAreaBlocksEps areaBlocks epsInds = filter (\x -> intersect x epsInds == []) areaBlocks




    --eta and epsilon types for the tree representing a sum of products of these tensors

    data Epsilon = Epsilon {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show, Read, Eq, Ord, Generic, Serialize, NFData)

    data Eta = Eta {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show, Read, Eq, Ord, Generic, Serialize, NFData)

    data Var = Var {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show, Read, Eq, Ord, Generic, Serialize, NFData )
    
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
   
    data AnsatzForestEta = ForestEta (M.Map Eta AnsatzForestEta)| Leaf !Var | EmptyForest  deriving (Show, Read, Eq, Generic, Serialize)

    type AnsatzForestEpsilon = M.Map Epsilon AnsatzForestEta

    --save forests as bytestrings 

    encodeAnsatzForestEta :: AnsatzForestEta -> BS.ByteString 
    encodeAnsatzForestEta = compress . encodeLazy 

    encodeAnsatzForestEpsilon :: AnsatzForestEpsilon -> BS.ByteString
    encodeAnsatzForestEpsilon = compress . encodeLazy

    decodeAnsatzForestEta :: BS.ByteString -> AnsatzForestEta 
    decodeAnsatzForestEta bs = (fromRight undefined $ decodeLazy $ decompress bs)

    decodeAnsatzForestEpsilon :: BS.ByteString -> AnsatzForestEpsilon 
    decodeAnsatzForestEpsilon bs = (fromRight undefined $ decodeLazy $ decompress bs)

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

    --map over the vars, i.e. the leafs of the tree 

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

    --add 2 sorted forests (are all zeros removed ? -> probably yes!)

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

    --sort a given AnsatzForest, i.e. bring the products of eta and epsilon to canonical order once the individual tensors are ordered canonically
    
    sortForest :: AnsatzForestEta -> AnsatzForestEta
    sortForest f = foldl' addList2Forest EmptyForest fList 
                where
                    fList = flattenForest f

    sortForestEpsilon :: AnsatzForestEpsilon -> AnsatzForestEpsilon 
    sortForestEpsilon f = foldl' addList2ForestEpsilon M.empty fList 
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

    mkEtaList' :: Var -> [Int] -> ([Eta],Var)
    mkEtaList' var l = (mkEtaList l, var)

    mkEpsilonList' :: Var -> [Int] -> (Epsilon,[Eta],Var)
    mkEpsilonList' var l = (eps, eta, var)
            where
                (eps,eta) = mkEpsilonList l

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

    --reduce a list of possible ansätze w.r.t the present symmetries, no numerical evaluation

    reduceAnsatzEta' :: Symmetry -> [([Eta],Var)] -> AnsatzForestEta
    reduceAnsatzEta' sym l = foldl' addOrRem' EmptyForest l
            where
                addOrRem' = \f ans -> if (isElem (fst ans) f) then f else addForests f (symAnsatzForestEta sym $ mkForestFromAscList ans)

    reduceAnsatzEpsilon' :: Symmetry -> [(Epsilon, [Eta], Var)] -> AnsatzForestEpsilon
    reduceAnsatzEpsilon' sym l = foldl' addOrRem' M.empty l
            where
                addOrRem' = \f (x,y,z) -> if (isElemEpsilon (x,y) f) then f else addForestsEpsilon f (symAnsatzForestEps sym $ mkForestFromAscListEpsilon (x,y,z))

    mkAllVars :: [Var] 
    mkAllVars = map (Var 1) [1..]

    --construct the full algebraic forest for a given number of indices and given symmetries, no numerical reduction to a basis

    getEtaForestFast :: Int -> [(Int,Int)] -> Symmetry -> AnsatzForestEta
    getEtaForestFast ord filters syms = relabelAnsatzForest 1 $ reduceAnsatzEta' syms allForests
                where
                    allInds = getEtaInds [1..ord] filters syms
                    allVars = mkAllVars
                    allForests = zipWith mkEtaList' allVars allInds

    getEpsForestFast :: Int -> [(Int,Int)] -> Symmetry -> AnsatzForestEpsilon
    getEpsForestFast ord filters syms = relabelAnsatzForestEpsilon 1 $ reduceAnsatzEpsilon' syms allForests
                where
                    allInds = getEpsilonInds [1..ord] filters syms 
                    allVars = mkAllVars
                    allForests = zipWith mkEpsilonList' allVars allInds

    ---------------------------------------------------------------------------------------------------------------------------------------

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
    evalAnsatzForestEta epsM evalM (ForestEta m) = M.foldlWithKey' foldF I.empty m 
                where
                    foldF b k a = let nodeVal = evalNodeEta epsM evalM k 
                                  in if nodeVal == Nothing then b 
                                     else I.unionWith (+) (I.map ((*) (fromJust nodeVal)) (evalAnsatzForestEta epsM evalM a)) b
    evalAnsatzForestEta epsM evalM EmptyForest = I.empty

    evalAnsatzForestEpsilon :: M.Map [Int] Int -> I.IntMap Int -> AnsatzForestEpsilon -> I.IntMap Int
    evalAnsatzForestEpsilon epsM evalM m = M.foldlWithKey' foldF I.empty m 
                where
                    foldF b k a = let nodeVal = evalNodeEpsilon epsM evalM k 
                                  in if nodeVal == Nothing then b 
                                     else I.unionWith (+) (I.map ((*) (fromJust nodeVal)) (evalAnsatzForestEta epsM evalM a)) b

    --for a single Ansatz we do not need the IntMap to keep track of the VarLabels -> eval to a number

    eval1AnsatzForestEta :: M.Map [Int] Int -> I.IntMap Int -> AnsatzForestEta -> Int
    eval1AnsatzForestEta epsM evalM (Leaf (Var x _)) = x
    eval1AnsatzForestEta epsM evalM (ForestEta m) = M.foldlWithKey' foldF 0 m
                where
                    foldF b k a = let nodeVal = evalNodeEta epsM evalM k 
                                  in if nodeVal == Nothing then b 
                                     else  b + ((fromJust nodeVal) * (eval1AnsatzForestEta epsM evalM a))
    eval1AnsatzForestEta epsM evalM EmptyForest = 0

    eval1AnsatzForestEpsilon :: M.Map [Int] Int -> I.IntMap Int -> AnsatzForestEpsilon -> Int
    eval1AnsatzForestEpsilon epsM evalM m = M.foldlWithKey' foldF 0 m
                where
                    foldF b k a = let nodeVal = evalNodeEpsilon epsM evalM k 
                                  in if nodeVal == Nothing then b 
                                    else  b + ((fromJust nodeVal) * (eval1AnsatzForestEta epsM evalM a))

    --eval a given 1Var ansatz to a sparse Matrix (a row vector) -> Eigen Indices start at 0 !!

    evalAnsatzEtaVecListEig :: M.Map [Int] Int -> [I.IntMap Int] -> AnsatzForestEta -> Maybe (Sparse.SparseMatrixXd) 
    evalAnsatzEtaVecListEig epsM evalM EmptyForest = Nothing
    evalAnsatzEtaVecListEig epsM evalM f = vecList
            where
                dofList = zip [0..] evalM
                mkAns (i,j) = let ansVal = eval1AnsatzForestEta epsM j f 
                              in if ansVal == 0 then Nothing else Just (0,i, fromIntegral ansVal)  
                l' = mapMaybe mkAns dofList
                l = runEval $ parListChunk 500 rdeepseq l'
                lVals = map (\(x,y,z) -> z) l
                max = maximum lVals
                n = length evalM
                vecList = let vec = Sparse.fromList 1 n l in
                          if l == [] then Nothing else Just $ Sparse.scale (1/max) vec

    evalAnsatzEpsilonVecListEig :: M.Map [Int] Int -> [I.IntMap Int] -> AnsatzForestEpsilon -> Maybe (Sparse.SparseMatrixXd)  
    evalAnsatzEpsilonVecListEig epsM evalM f  = if f == M.empty then Nothing else vecList
            where 
                dofList = zip [0..] evalM
                mkAns (i,j) = let ansVal = eval1AnsatzForestEpsilon epsM j f 
                              in if ansVal == 0 then Nothing else Just (0,i, fromIntegral ansVal)  
                l' = mapMaybe mkAns dofList
                l = runEval $ parListChunk 500 rdeepseq l'
                lVals = map (\(x,y,z) -> z) l
                max = maximum lVals
                n = length evalM
                vecList = let vec = Sparse.fromList 1 n l in
                                    if l == [] then Nothing else Just $ Sparse.scale (1/max) vec

    --eval a given Forest for all inds, assocsList stores (list of scalar*var assocs, multiplicity, tensorInds) 

    type AssocsList a = [([(Int,Int)],Int,a)]

    evalAllEta :: M.Map [Int] Int -> [I.IntMap Int] -> AnsatzForestEta -> [[(Int,Int)]]
    evalAllEta epsM evalMs EmptyForest = [] 
    evalAllEta epsM evalMs f = l'
                where
                    l = map (\x -> (filter (\(a,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEta epsM x f)) evalMs
                    l' = runEval $ parListChunk 500 rdeepseq l

    evalAllTensorEta :: (NFData a) => M.Map [Int] Int -> [(I.IntMap Int, Int, a)] -> AnsatzForestEta -> AssocsList a
    evalAllTensorEta epsM evalMs EmptyForest = [] 
    evalAllTensorEta epsM evalMs f = l'
                where
                    l = map (\(x,y,z) -> (filter (\(a,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEta epsM x f, y,z)) evalMs
                    l' = runEval $ parListChunk 500 rdeepseq l

    evalAllEpsilon :: M.Map [Int] Int -> [I.IntMap Int] -> AnsatzForestEpsilon -> [[(Int,Int)]]
    evalAllEpsilon epsM evalMs f = if f == M.empty then [] else l'
                where
                    l = map (\x -> (filter (\(a,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEpsilon epsM x f)) evalMs
                    l' = runEval $ parListChunk 500 rdeepseq l

    evalAllTensorEpsilon :: (NFData a) => M.Map [Int] Int -> [(I.IntMap Int, Int, a)] -> AnsatzForestEpsilon -> [([(Int,Int)],Int,a)]
    evalAllTensorEpsilon epsM evalMs f = if f == M.empty then [] else l'
                where
                    l = map (\(x,y,z) -> ( filter (\(a,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEpsilon epsM x f, y,z)) evalMs
                    l' = runEval $ parListChunk 500 rdeepseq l

    --------------------------------------------------------------------------------------------------------------------------------

    --now there are two ways how we can proceed

    --1) the memory optimised way, constructing a lin indep tree from the very beginning

    --the first step is to check whether a given Ansatz is elemment of the span of the previos ansätze and therefore can be discarded 

    --function takes as arguments: current determinant of upper left block, current upper left block, the corresponding matrix inverse, current Sparse Ansatz Matrix, new Ansatz rowVector (stored as a sparse matrix)
    
    --function returns: (Det, newMatA, newMatAInv, newfullMat)

    type RankDataEig = (Mat.MatrixXd, Sparse.SparseMatrixXd)

    getVarNrEig :: RankDataEig -> Int 
    getVarNrEig = Sparse.rows . snd 

    --check in each step if the new ansatz vector is linear dependant w.r.t. the ansatz vectors obtained previously

    checkNumericLinDepEig :: RankDataEig -> Maybe Sparse.SparseMatrixXd -> Maybe RankDataEig 
    checkNumericLinDepEig (lastMat, lastFullMat) (Just newVec) 
                | eigenRank < maxRank = Nothing
                | otherwise = Just (newMat, newAnsatzMat)
                 where
                    newVecTrans = Sparse.transpose newVec 
                    scalar = Sparse.toMatrix $ Sparse.mul newVec newVecTrans
                    prodBlock = Sparse.toMatrix $ Sparse.mul lastFullMat newVecTrans
                    prodBlockTrans = Mat.transpose prodBlock
                    newMat = concatBlockMat lastMat prodBlock prodBlockTrans scalar 
                    eigenRank = Sol.rank Sol.FullPivLU newMat 
                    maxRank = min (Mat.cols newMat) (Mat.rows newMat)
                    newAnsatzMat = Sparse.fromRows $ (Sparse.getRows lastFullMat) ++ [newVec]
    checkNumericLinDepEig (lastMat, lastFullMat) Nothing = Nothing 

    --concat Matrces to a block Matrix, should already be implemented in eigen ??

    concatBlockMat :: Mat.MatrixXd -> Mat.MatrixXd -> Mat.MatrixXd -> Mat.MatrixXd -> Mat.MatrixXd 
    concatBlockMat a b c d = newMat 
                where
                   newUpper = zipWith (++) (Mat.toList a) (Mat.toList b)
                   newLower = zipWith (++) (Mat.toList c) (Mat.toList d)
                   newMat = Mat.fromList $ newUpper ++ newLower 

    --in each step add the new AnsatzVector to the forest iff it is lin indep of the previous vectors

    addOrDiscardEtaEigIO :: Symmetry -> Int -> M.Map [Int] Int -> [I.IntMap Int] -> (AnsatzForestEta, RankDataEig) -> (Int, [Eta]) -> IO (AnsatzForestEta, RankDataEig)
    addOrDiscardEtaEigIO symList len epsM evalM (ans,rDat) (num, etaL)
                | isElem etaL ans = do
                                        let r = getVarNrEig rDat
                                        putStrLn $ showStr ++ " : " ++ "already present, not added, ansatz rank is " ++ show r
                                        return (ans,rDat)
                | otherwise = case newRDat of 
                                   Nothing          -> do
                                                        let r = getVarNrEig rDat
                                                        putStrLn $ showStr ++ " : " ++ "not present, linearly dependent, not added, ansatz rank is " ++ show r
                                                        return (ans,rDat)
                                   Just newRDat'    -> do
                                                        let r = getVarNrEig newRDat'
                                                        putStrLn $ showStr ++ " : " ++ "not present, linearly independent, added, ansatz rank is " ++ show r
                                                        return (sumAns,newRDat')      
                 where
                    numVars = getVarNrEig rDat
                    newAns = symAnsatzForestEta symList $ mkForestFromAscList (etaL,Var 1 (numVars+1))
                    newVec = evalAnsatzEtaVecListEig epsM evalM newAns
                    newRDat = checkNumericLinDepEig rDat newVec
                    sumAns = addForests ans newAns
                    showStr = show num ++ " of " ++ show len

    addOrDiscardEtaEig :: Symmetry ->  M.Map [Int] Int -> [I.IntMap Int] -> (AnsatzForestEta, RankDataEig) -> [Eta] -> (AnsatzForestEta, RankDataEig)
    addOrDiscardEtaEig symList epsM evalM (ans,rDat) etaL 
                | isElem etaL ans = (ans,rDat)
                | otherwise = case newRDat of 
                                   Nothing          -> (ans,rDat)
                                   Just newRDat'    -> (sumAns,newRDat')      
                 where
                    numVars = getVarNrEig rDat
                    newAns = symAnsatzForestEta symList $ mkForestFromAscList (etaL,Var 1 (numVars+1))
                    newVec = evalAnsatzEtaVecListEig epsM evalM newAns
                    newRDat = checkNumericLinDepEig rDat newVec
                    sumAns = addForests ans newAns


    addOrDiscardEpsilonEigIO :: Symmetry -> Int -> M.Map [Int] Int -> [I.IntMap Int] -> (AnsatzForestEpsilon, RankDataEig) -> (Int,(Epsilon,[Eta])) -> IO (AnsatzForestEpsilon, RankDataEig)
    addOrDiscardEpsilonEigIO symList len epsM evalM (ans,rDat) (num,(epsL,etaL))
                | isElemEpsilon (epsL,etaL) ans = do
                                        let r = getVarNrEig rDat
                                        putStrLn $ showStr ++ " : " ++ "already present, not added, ansatz rank is " ++ show r
                                        return (ans,rDat)
                | otherwise = case newRDat of 
                                   Nothing          -> do
                                                        let r = getVarNrEig rDat
                                                        putStrLn $ showStr ++ " : " ++ "not present, linearly dependent, not added, ansatz rank is " ++ show r
                                                        return (ans,rDat)
                                   Just newRDat'    -> do
                                                        let r = getVarNrEig newRDat'
                                                        putStrLn $ showStr ++ " : " ++ "not present, linearly independent, added, ansatz rank is " ++ show r
                                                        return (sumAns,newRDat')      
                 where
                    numVars = getVarNrEig rDat
                    newAns = symAnsatzForestEps symList $ mkForestFromAscListEpsilon (epsL,etaL, Var 1 (numVars+1))
                    newVec = evalAnsatzEpsilonVecListEig epsM evalM newAns
                    newRDat = checkNumericLinDepEig rDat newVec
                    sumAns = addForestsEpsilon ans newAns
                    showStr = show num ++ " of " ++ show len

    addOrDiscardEpsilonEig :: Symmetry ->  M.Map [Int] Int -> [I.IntMap Int] -> (AnsatzForestEpsilon, RankDataEig) -> (Epsilon,[Eta]) -> (AnsatzForestEpsilon, RankDataEig)
    addOrDiscardEpsilonEig symList epsM evalM (ans,rDat) (epsL,etaL) 
                | isElemEpsilon (epsL,etaL) ans = (ans,rDat)
                | otherwise = case newRDat of 
                                   Nothing          -> (ans,rDat)
                                   Just newRDat'    -> (sumAns,newRDat')      
                 where
                    numVars = getVarNrEig rDat
                    newAns = symAnsatzForestEps symList $ mkForestFromAscListEpsilon (epsL,etaL, Var 1 (numVars+1))
                    newVec = evalAnsatzEpsilonVecListEig epsM evalM newAns
                    newRDat = checkNumericLinDepEig rDat newVec
                    sumAns = addForestsEpsilon ans newAns


    --construct the RankData from the first Ansatz 

    mk1stRankDataEtaEigIO :: Symmetry -> Int -> [(Int,[Eta])] -> M.Map [Int] Int -> [I.IntMap Int] -> IO (AnsatzForestEta,RankDataEig,[(Int,[Eta])])
    mk1stRankDataEtaEigIO symL numEta etaL epsM evalM =
            do
                putStrLn $ (show $ fst $ head etaL) ++ " of " ++ show numEta
                let newAns = symAnsatzForestEta symL $ mkForestFromAscList (snd $ head etaL,Var 1 1)
                let newVec = evalAnsatzEtaVecListEig epsM evalM newAns
                let restList = tail etaL 
                case newVec of
                                    Nothing         -> mk1stRankDataEtaEigIO symL numEta restList epsM evalM 
                                    Just newVec'    -> return (newAns, (newMat, newVec'), restList)
                                        where 
                                            newVecTrans = Sparse.transpose newVec'
                                            newMat = Sparse.toMatrix $ Sparse.mul newVec' newVecTrans

    mk1stRankDataEtaEig :: Symmetry -> [[Eta]] -> M.Map [Int] Int -> [I.IntMap Int] -> (AnsatzForestEta,RankDataEig,[[Eta]])
    mk1stRankDataEtaEig symL etaL epsM evalM = output
            where
                newAns = symAnsatzForestEta symL $ mkForestFromAscList (head etaL,Var 1 1)
                newVec = evalAnsatzEtaVecListEig epsM evalM newAns
                restList = tail etaL 
                output = case newVec of
                                    Nothing         -> mk1stRankDataEtaEig symL restList epsM evalM 
                                    Just newVec'    -> (newAns, (newMat, newVec'), restList)
                                        where 
                                            newVecTrans = Sparse.transpose newVec'
                                            newMat = Sparse.toMatrix $ Sparse.mul newVec' newVecTrans


    mk1stRankDataEpsilonEigIO :: Symmetry -> Int -> [(Int,(Epsilon,[Eta]))] -> M.Map [Int] Int -> [I.IntMap Int] -> IO (AnsatzForestEpsilon,RankDataEig,[(Int,(Epsilon,[Eta]))])
    mk1stRankDataEpsilonEigIO symL numEps epsL epsM evalM =
            do
                putStrLn $ (show $ fst $ head epsL) ++ " of " ++ show numEps
                let newAns = symAnsatzForestEps symL $ mkForestFromAscListEpsilon (fst $ snd $ head epsL, snd $ snd $ head epsL,Var 1 1)
                let newVec = evalAnsatzEpsilonVecListEig epsM evalM newAns
                let restList = tail epsL
                case newVec of
                                    Nothing         -> mk1stRankDataEpsilonEigIO symL numEps restList epsM evalM
                                    Just newVec'    -> return (newAns,(newMat, newVec'), restList)
                                        where 
                                            newVecTrans = Sparse.transpose newVec'
                                            newMat = Sparse.toMatrix $ Sparse.mul newVec' newVecTrans

    mk1stRankDataEpsilonEig :: Symmetry -> [(Epsilon,[Eta])] -> M.Map [Int] Int -> [I.IntMap Int] -> (AnsatzForestEpsilon,RankDataEig,[(Epsilon,[Eta])])
    mk1stRankDataEpsilonEig symL epsL epsM evalM = output 
            where
                newAns = symAnsatzForestEps symL $ mkForestFromAscListEpsilon (fst $ head epsL, snd $ head epsL,Var 1 1)
                newVec = evalAnsatzEpsilonVecListEig epsM evalM newAns
                restList = tail epsL
                output = case newVec of
                                    Nothing         -> mk1stRankDataEpsilonEig symL restList epsM evalM
                                    Just newVec'    -> (newAns,(newMat, newVec'), restList)
                                        where 
                                            newVecTrans = Sparse.transpose newVec'
                                            newMat = Sparse.toMatrix $ Sparse.mul newVec' newVecTrans


    --finally reduce the ansatzList  

    reduceAnsatzEtaEigIO :: Symmetry -> [[Eta]] -> [I.IntMap Int] -> IO (AnsatzForestEta,Sparse.SparseMatrixXd)
    reduceAnsatzEtaEigIO symL etaL evalM' =
            do
                putStrLn "canonicalizeEvalMaps ..."
                let evalM = canonicalizeEvalMaps symL evalM'
                evalM `deepseq` putStrLn " ... done!"
                let epsM = epsMap
                let etaLLength = length $ force etaL
                putStrLn $ "fast-forward to first non-vanishing ansatz in list of " ++ show etaLLength
                let zipped = zip [1..] etaL
                (ans1,rDat1,restEtaL) <- mk1stRankDataEtaEigIO symL etaLLength zipped epsM evalM
                putStrLn $ "first non-vanishing ansatz found"
                (finalForest, (_,finalMat)) <- foldM (addOrDiscardEtaEigIO symL etaLLength epsM evalM) (ans1,rDat1) restEtaL
                putStrLn $ "finished!"
                return (finalForest, finalMat)

    reduceAnsatzEtaEig :: Symmetry -> [[Eta]] -> [I.IntMap Int] -> (AnsatzForestEta,Sparse.SparseMatrixXd)
    reduceAnsatzEtaEig symL etaL evalM' = (finalForest, finalMat)
            where
                evalM = canonicalizeEvalMaps symL evalM'  
                epsM = epsMap
                (ans1,rDat1,restEtaL) = mk1stRankDataEtaEig symL etaL epsM evalM
                (finalForest, (_,finalMat)) = foldl' (addOrDiscardEtaEig symL epsM evalM) (ans1,rDat1) restEtaL 

    reduceAnsatzEpsilonEigIO :: Symmetry -> [(Epsilon,[Eta])] -> [I.IntMap Int] -> IO (AnsatzForestEpsilon,Sparse.SparseMatrixXd)
    reduceAnsatzEpsilonEigIO symL epsL evalM' =
            do
                putStrLn "canonicalizeEvalMaps ..."
                let evalM = canonicalizeEvalMaps symL evalM'
                evalM `deepseq` putStrLn " ... done!"
                let epsM = epsMap
                let epsLLength = length $ force epsL
                putStrLn $ "fast-forward to first non-vanishing ansatz in list of " ++ show epsLLength
                let zipped = zip [1..] epsL
                (ans1,rDat1,restEpsL) <- mk1stRankDataEpsilonEigIO symL epsLLength zipped epsM evalM
                putStrLn $ "first non-vanishing ansatz found"
                (finalForest, (_,finalMat)) <- foldM (addOrDiscardEpsilonEigIO symL epsLLength epsM evalM) (ans1,rDat1) restEpsL
                putStrLn $ "finished!"
                return (finalForest, finalMat)

    reduceAnsatzEpsilonEig :: Symmetry -> [(Epsilon,[Eta])] -> [I.IntMap Int] -> (AnsatzForestEpsilon,Sparse.SparseMatrixXd)
    reduceAnsatzEpsilonEig symL epsL evalM' = (finalForest, finalMat)
            where
                evalM = canonicalizeEvalMaps symL evalM'
                epsM = epsMap
                (ans1,rDat1,restEpsL) = mk1stRankDataEpsilonEig symL epsL epsM evalM
                (finalForest, (_,finalMat)) = foldl' (addOrDiscardEpsilonEig symL epsM evalM) (ans1,rDat1) restEpsL 

    --construct a basis ansatz forest 

    getEtaForestEigIO :: Int -> [(Int,Int)] -> Symmetry -> [I.IntMap Int] -> IO (AnsatzForestEta,Sparse.SparseMatrixXd)
    getEtaForestEigIO ord filters sym evalMs = reduceAnsatzEtaEigIO sym allEtaLists evalMs
            where
                allInds = getEtaInds [1..ord] filters sym
                allEtaLists = map mkEtaList allInds

    getEtaForestEig :: Int -> [(Int,Int)] -> Symmetry -> [I.IntMap Int] -> (AnsatzForestEta,Sparse.SparseMatrixXd)
    getEtaForestEig ord filters sym evalMs = reduceAnsatzEtaEig sym allEtaLists evalMs
            where
                allInds = getEtaInds [1..ord] filters sym
                allEtaLists = map mkEtaList allInds

    getEpsForestEigIO :: Int -> [(Int,Int)] -> Symmetry -> [I.IntMap Int] -> IO (AnsatzForestEpsilon,Sparse.SparseMatrixXd)
    getEpsForestEigIO ord filters sym evalMs = reduceAnsatzEpsilonEigIO sym allEpsLists evalMs
            where
                allInds = getEpsilonInds [1..ord] filters sym 
                allEpsLists = map mkEpsilonList allInds

    getEpsForestEig :: Int -> [(Int,Int)] -> Symmetry -> [I.IntMap Int] -> (AnsatzForestEpsilon,Sparse.SparseMatrixXd)
    getEpsForestEig ord filters sym evalMs = reduceAnsatzEpsilonEig sym allEpsLists evalMs
            where
                allInds = getEpsilonInds [1..ord] filters sym 
                allEpsLists = map mkEpsilonList allInds

    --eta and eps forest combined

    getFullForestEigIO :: Int -> [(Int,Int)] -> Symmetry -> [I.IntMap Int] -> [I.IntMap Int] -> IO (AnsatzForestEta, AnsatzForestEpsilon, Sparse.SparseMatrixXd, Sparse.SparseMatrixXd)
    getFullForestEigIO ord filters sym evalMEta evalMEps =
              do
                (etaAns, etaMat) <- getEtaForestEigIO ord filters sym evalMEta 
                (epsAns',epsMat) <- getEpsForestEigIO ord filters sym evalMEps
                let epsAns = relabelAnsatzForestEpsilon (1 + (length $ getForestLabels etaAns)) epsAns'
                return (etaAns, epsAns, etaMat, epsMat)

    getFullForestEig :: Int -> [(Int,Int)] -> Symmetry -> [I.IntMap Int] -> [I.IntMap Int] -> (AnsatzForestEta, AnsatzForestEpsilon, Sparse.SparseMatrixXd, Sparse.SparseMatrixXd)
    getFullForestEig ord filters sym evalMEta evalMEps = (etaAns, epsAns, etaMat, epsMat)
            where
                (etaAns,etaMat) = getEtaForestEig ord filters sym evalMEta 
                (epsAns',epsMat) = getEpsForestEig ord filters sym evalMEps
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
    
   
    --the 2 final functions, constructing the 2 AnsatzForests and the AnsatzTensor

    mkAnsatzTensorEigIO :: Int -> [(Int,Int)] -> Symmetry -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6])] -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6])] -> IO (AnsatzForestEta, AnsatzForestEpsilon, ATens n1 n2 n3 n4 n5 n6 AnsVar) 
    mkAnsatzTensorEigIO ord filters symmetries evalMEta evalMEps =
              do
                let epsM = epsMap
                let evalMapsEta = map (\(x,y,z) -> x) evalMEta
                let evalMapsEps = map (\(x,y,z) -> x) evalMEps  
                let indListEta = map (\(x,y,z) -> (y,z)) evalMEta
                let indListEps = map (\(x,y,z) -> (y,z)) evalMEps
                (ansEta, ansEps, _, _) <- getFullForestEigIO ord filters symmetries evalMapsEta evalMapsEps
                let tens = evalToTens epsM evalMEta evalMEps ansEta ansEps 
                return (ansEta, ansEps, tens)

    mkAnsatzTensorEig :: Int -> [(Int,Int)] -> Symmetry -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6])] -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6])] -> (AnsatzForestEta, AnsatzForestEpsilon, ATens n1 n2 n3 n4 n5 n6 AnsVar) 
    mkAnsatzTensorEig ord filters symmetries evalMEta evalMEps = (ansEta, ansEps, tens)
            where
                epsM = epsMap
                evalMapsEta = map (\(x,y,z) -> x) evalMEta
                evalMapsEps = map (\(x,y,z) -> x) evalMEps  
                indListEta = map (\(x,y,z) -> (y,z)) evalMEta
                indListEps = map (\(x,y,z) -> (y,z)) evalMEps
                (ansEta, ansEps, _, _) = getFullForestEig ord filters symmetries evalMapsEta evalMapsEps
                tens = evalToTens epsM evalMEta evalMEps ansEta ansEps 

    -------------------------------------------------------------------------------------------------------

    --the second way to construct a given Ansatz is by reducing only algebraically and later on reducing the matrix numerically 

    assocsToEig :: [[(Int,Int)]] -> Mat.MatrixXd 
    assocsToEig l = Sparse.toMatrix $ Sparse.fromList n m l'
        where
            l' = concat $ zipWith (\r z -> map (\(x,y) -> (z-1, x-1, fromIntegral y)) r) l [1..]
            n = (maximum $ map (\(x,_,_) -> x) l') + 1
            m = (maximum $ map (\(_,x,_) -> x) l') + 1

    --filter the lin. dependant vars from the Assocs List 

    getPivots :: [[(Int,Int)]]  -> [Int]
    getPivots l = map (1+) p
            where
                mat = assocsToEig l 
                p = Sol.pivots Sol.FullPivLU mat

    --reduce linear deps in the ansätze

    reduceLinDepsFastEta :: M.Map [Int] Int -> [I.IntMap Int] -> Symmetry -> AnsatzForestEta -> AnsatzForestEta
    reduceLinDepsFastEta epsM evalM symL ansEta = newEtaAns
            where 
                evalM' = canonicalizeEvalMaps symL evalM 
                etaL = evalAllEta epsM evalM' ansEta 
                etaVars = getPivots etaL 
                allEtaVars = getForestLabels ansEta
                remVarsEta =  allEtaVars \\ etaVars
                newEtaAns = relabelAnsatzForest 1 $ removeVarsEta remVarsEta ansEta

    reduceLinDepsFastEps :: M.Map [Int] Int -> [I.IntMap Int] -> Symmetry -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    reduceLinDepsFastEps epsM evalM symL ansEps = newEpsAns
            where 
                evalM' = canonicalizeEvalMaps symL evalM 
                epsL = evalAllEpsilon epsM evalM' ansEps 
                epsVars = getPivots epsL 
                allEpsVars = getForestLabelsEpsilon ansEps
                remVarsEps =  allEpsVars \\ epsVars
                newEpsAns = relabelAnsatzForestEpsilon 1 $ removeVarsEps remVarsEps ansEps

    --final function, fast way of constructing the ansatztrees and the 2 tensors
                
    mkAnsatzTensorFast :: Int -> [(Int,Int)] -> Symmetry -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6])] -> [(I.IntMap Int, Int, [IndTuple n1 n2 n3 n4 n5 n6])] -> (AnsatzForestEta, AnsatzForestEpsilon, ATens n1 n2 n3 n4 n5 n6 AnsVar) 
    mkAnsatzTensorFast ord filters symmetries evalMEta evalMEps = (ansEtaRed, ansEpsRed, tens) 
            where
                epsM = epsMap
                ansEta = getEtaForestFast ord filters symmetries 
                ansEpsilon = getEpsForestFast ord filters symmetries  
                ansEtaRed = reduceLinDepsFastEta epsM (map (\(x,_,_) -> x) evalMEta) symmetries ansEta
                ansEpsRed' = reduceLinDepsFastEps epsM (map (\(x,_,_) -> x) evalMEps) symmetries ansEpsilon
                ansEpsRed = relabelAnsatzForestEpsilon (1 + (length $ getForestLabels ansEtaRed)) ansEpsRed'
                tens = evalToTens epsM evalMEta evalMEps ansEtaRed ansEpsRed 

    
    -----------------------------------------------------------------------------------------------------------------------------------------

    --finally the lists for the evaluation 

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
                l' = map (\x -> (x,sum x)) l
                n = minimum $ map snd l' 

    canonicalizeIndList :: [[[Int]]] -> [[Int]] -> [[Int]] -> I.IntMap Int -> I.IntMap Int 
    canonicalizeIndList blocks areaInds derInds iMap = I.fromList $ zip [1..I.size iMap] canonicL
            where 
                iIndsL = map I.elems $ getAllIndListsMap iMap 
                iIndsL' = filterMins iIndsL 
                canonicL' = map (canonicalizeInds blocks areaInds derInds) iIndsL'
                canonicL = minimum canonicL' 
                
    canonicalizeIndListL :: [[[Int]]] -> [[Int]] -> [[Int]] -> [Int] -> [Int] 
    canonicalizeIndListL blocks areaInds derInds iL = canonicL
            where 
                iIndsL = getAllIndListsList iL 
                iIndsL' = filterMins iIndsL 
                canonicL' = map (canonicalizeInds blocks areaInds derInds) iIndsL' 
                canonicL = minimum canonicL'

    canonicalizeEvalMaps :: Symmetry -> [I.IntMap Int] -> [I.IntMap Int]
    canonicalizeEvalMaps sym iMaps = nub $ map (canonicalizeIndList blocks areaInds derInds) iMaps 
            where 
                (blocks, areaInds, derInds) = getSyms sym 

    
    


    
