--Generation of Lorentz invariant basis from given valence and symmetry

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


module LorentzGenerator (
    AnsatzForestEpsilon(..), AnsatzForestEta(..), Eta(..), Epsilon(..), Var(..), flattenForest, flattenForestEpsilon, drawAnsatzEta, drawAnsatzEpsilon,
    mkAnsatzTensorEig, mkAnsatzTensorEigIO, mkAnsatzTensorFast, mkAnsatzTensorEig', mkAnsatzTensorEigIO', mkAnsatzTensorFast',
    mkAnsatzTensorEigSym, mkAnsatzTensorEigIOSym, mkAnsatzTensorFastSym, mkAnsatzTensorEigSym', mkAnsatzTensorEigIOSym', mkAnsatzTensorFastSym',
    getForestLabels, getForestLabelsEpsilon, removeVarsEta, removeVarsEps, relabelAnsatzForest, relabelAnsatzForestEpsilon, mapVars, mapVarsEpsilon, 
    forestEtaList, forestEpsList, forestEtaListLatex, forestEpsListLatex, ansatzRank, ansatzRankEpsilon

    --what else should be exported ??


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
    import Data.Tuple
    import Numeric.Natural
    import GHC.TypeLits
    import Data.Proxy
    import GHC.TypeLits.Normalise
    import Data.Type.Equality
    import Data.Singletons
    import Data.Singletons.Decide
    import Data.Singletons.Prelude.Enum
    import Data.Singletons.TypeLits

    --LinearAlgebra subroutines

    import qualified Data.Eigen.Matrix as Mat 
    import qualified Data.Eigen.SparseMatrix as Sparse
    import qualified Data.Eigen.LA as Sol 
    import qualified Data.Eigen.SparseLA as SpSol

    --SparseTensor

    import SparseTensor

    {--
    The first step consist of pre-reducing the index list for the eta and epsilon trees as much as possible.
    This is done by using the symmetries in the sense that we try to select exactly one representative out of each class of indices
    that are equivalent under the symmetries. 
    Note that the prereduction is not necessary but increases performance.
    --}

    --symmetry type alias: (SymPairs, ASymPairs, BlockSyms, CyclicSyms, CyclicBlockSyms)

    type Symmetry = ( [(Int,Int)] , [(Int,Int)] , [([Int],[Int])] , [[Int]], [[[Int]]] )

    addSym :: Symmetry -> Symmetry -> Symmetry
    addSym (a,b,c,d,e) (f,g,h,i,j) = (union a f, union b g, union c h, union d i, union e j)

    --constructing the filter list out of the symmetry data for filtering one representative out of each symmetry class

    mkFilters :: Symmetry -> [(Int,Int)]
    mkFilters (pairs,aPairs,blocks,cycles,blockCycles) = map sortPair $ union f1 (union f2 (union f3 f4))
        where 
            sortPair (a,b) = if a < b then (a,b) else (b,a)            
            f1 =  pairs ++ aPairs 
            f2 = map (\(a,b) -> (head a, head b)) blocks 
            getPairs [a,b] = [(a,b)]
            getPairs (x:xs) = (x, head xs) : getPairs xs 
            f3 = concat $ map getPairs cycles 
            f4 = concat $ map (\x -> getPairs $ map head x) blockCycles 

    --filter the index lists 

    filter1Sym :: [Int] -> (Int,Int) -> Bool 
    filter1Sym l (i,j) = case (iPos,jPos) of  
                            (Just i', Just j')   ->  if i' < j' then True else False
                            x   -> True  
             where
               (iPos,jPos) = (elemIndex i l, elemIndex j l)

    filterSym :: [Int] -> [(Int,Int)] -> Bool
    filterSym l inds = and boolList 
            where
               boolList = map (filter1Sym l) inds 


    {--
    Note that writing specific indices from a block symmetry at an eta yields additional symmetries: for instance consider the block symmetry 
    [ab] <-> [cd] writing eta[ac] yields the new symmetry b <-> d. The 2-block symmetry is thus reduced to a 1-block symmetry. In the same way 
    etas reduce n-block symmetries to (n-1)-block symmetries. To compute these we also need to include all possible block symmetries that are specified
    in ters of a cyclic block symmetry.
    --}

    getExtraSyms1 :: [Int] -> Symmetry -> Symmetry 
    getExtraSyms1 [] syms = ([],[],[],[],[]) 
    getExtraSyms1 (a:b:xs) (pairs,aPairs,blocks,cycles,blockCycles) = addSym (newPairs, [],  newBlocks, [], []) (getExtraSyms1 xs newSyms)  
            where 
                allBlocks = blocks ++ (concat $ map mkBlocksFromBlockCycle blockCycles) 
                newBlocks' = map (\(x,y) -> unzip $ filter (\(c,d) -> not $ (c,d) == (a,b)) $ zip x y) allBlocks 
                (newBlocks, newPairs') = partition (\(a,b) -> length a > 1) newBlocks'  
                newPairs = map (\([a],[b]) -> (a,b)) newPairs' 
                newSyms = addSym (pairs,aPairs,blocks,cycles,blockCycles) (newPairs, [],  newBlocks, [], [])

    mkBlocksFromBlockCycle :: [[Int]] -> [([Int],[Int])]  
    mkBlocksFromBlockCycle [x,y] = [(x,y)] 
    mkBlocksFromBlockCycle (x:xs) = l ++ (mkBlocksFromBlockCycle xs)
            where 
                l = map (\y -> (x,y)) xs

    {--
    Furthermore distributing a symmetric or antisymmetric pair of indices over 2 etas yields an additinal symmetry or antisymmetry
    of the remaining eta indices due to the product structure: for instance consider the a <-> b symmetry,
    writing eta[ac] eta[bd] yields an additional c <-> d symmetry. Here it is additionally necessary to include the pair symmetries that are contributed by a given total symmetry 
    --}


    --given one eta, if the eta contains an index from a symmetric or antisymmetric pair return the corresponding second index and the other index of the eta

    get2nd :: [Int] -> Symmetry -> (Maybe [(Int,Int)], Maybe [(Int,Int)]) 
    get2nd [a,b] (pairs,aPairs,blocks,cycles,blockCycles) = (sndPairs, sndAPairs)
            where 
                allPairs = pairs ++ (concat $ map mkSymsFromCycle cycles)
                aPair = lookup a allPairs 
                bPair = lookup b  (map swap allPairs) 
                aAPair = lookup a aPairs
                bAPair = lookup b (map swap aPairs)
                sndPairs = case (aPair, bPair) of 
                                (Nothing, Nothing)  ->  Nothing 
                                (Just x, Nothing)   -> Just [(b,x)] 
                                (Nothing, Just y)   -> Just [(a,y)] 
                                (Just x, Just y)    -> if x == b then Nothing else Just [(b,x),(a,y)]
                sndAPairs = case (aAPair, bAPair) of 
                                 (Nothing, Nothing)  ->  Nothing 
                                 (Just x, Nothing)   -> Just [(b,x)] 
                                 (Nothing, Just y)   -> Just [(a,y)] 
                                 (Just x, Just y)    -> if x == b then Nothing else  Just [(b,x),(a,y)]


    --find the eta that contains the computed second pair index and return the other indices of this eta
                
    get2ndSyms :: Maybe [(Int,Int)] -> Symmetry -> [[Int]] -> Symmetry 
    get2ndSyms Nothing syms etas = syms
    get2ndSyms (Just i) (pairs,aPairs,blocks,cycles,blockCycles) etas = (newPairs,[],[],[],[])  
        where 
            get2ndInd l (i,j) = mapMaybe (\[a,b] -> if j == a then Just (i,b) else if j == b then Just (i,a) else Nothing) l
            newPairs = concat $ map (get2ndInd etas) i 

    mkSymsFromCycle :: [Int] -> [(Int,Int)]
    mkSymsFromCycle [x,y] = [(x,y)] 
    mkSymsFromCycle (x:xs) = l ++ (mkSymsFromCycle xs)
            where 
                l = map (\y -> (x,y)) xs


    get2ndASyms :: Maybe [(Int,Int)] -> Symmetry -> [[Int]] -> Symmetry 
    get2ndASyms Nothing syms etas = syms
    get2ndASyms (Just i) (pairs,aPairs,blocks,cycles,blockCycles) etas = ([], newAPairs,[],[],[])  
        where 
            get2ndInd l (i,j) = mapMaybe (\[a,b] -> if j == a then Just (i,b) else if j == b then Just (i,a) else Nothing) l
            newAPairs = concat $ map (get2ndInd etas) i 


    --apply to whole ind list

    getExtraSyms2 :: [Int] -> Symmetry -> Symmetry 
    getExtraSyms2 [] syms = syms 
    getExtraSyms2 (a':b':xs) syms = addSym (getExtraSyms2 xs newSyms) newSyms 
            where 
                mkEtas [] = []
                mkEtas [l,k] = [[l,k]]
                mkEtas (l:k:ls) = [l,k] : mkEtas ls  
                x = [a',b']
                (i,j) = get2nd x syms 
                (p,_,_,_,_) = get2ndSyms i syms (mkEtas xs) 
                (_,a,_,_,_) = get2ndASyms j syms (mkEtas xs) 
                newSyms = addSym (p,a,[],[],[]) syms

    --compute all extra symmetries 

    getAllExtraSyms :: [Int] -> Symmetry -> Symmetry 
    getAllExtraSyms etas syms = allSyms2
                where 
                    allSyms1 = addSym (getExtraSyms1 etas syms) syms 
                    allSyms2 = addSym (getExtraSyms2 etas allSyms1) allSyms1 
                    

    getAllIndsEta :: [Int] -> [(Int,Int)] -> [[Int]]
    getAllIndsEta [a,b] aSyms = [[a,b]]
    getAllIndsEta (x:xs) aSyms = concat $ map res firstEta
            where 
                firstEta = mapMaybe (\y -> if not $ elem (x,y) aSyms then Just ([x,y],delete y xs) else Nothing) xs 
                res (a,b) = (++) a <$> (getAllIndsEta b aSyms)

    filterEta :: [Int] -> Symmetry -> [(Int,Int)] -> Bool
    filterEta inds (p1,ap1,b1,c1,cb1) filters = (filterSym inds totFilters) && isNonZero 
            where 
                (p2,ap2,b2,c2,cb2) = getAllExtraSyms inds (p1,ap1,b1,c1,cb1) 
                extrafilters = mkFilters (p2,ap2,b2,c2,cb2)
                totFilters = union filters extrafilters
                mkEtas [] = []
                mkEtas [l,k] = [(l,k)]
                mkEtas (l:k:ls) = (l,k) : mkEtas ls  
                etas = mkEtas inds 
                isNonZero = length (intersect etas (union ap1 ap2)) == 0 

    --construct a pre-reduced list of eta indices
            
    getEtaInds :: [Int] -> Symmetry -> [[Int]]
    getEtaInds [] sym = [[]]
    getEtaInds inds (p,ap,b,c,bc) = filter (\x -> filterEta x (p,ap,b,c,bc) filters1) allInds 
            where 
                filters1 = mkFilters (p,ap,b,c,bc) 
                allInds = getAllIndsEta inds ap 

    {--
    Now we proceed in the same fashion for the epsilon ind list.
    Here we can actually from the very begining prevent some linear dependencies from occuring by noting that due to certain symmetries 
    certain expressions involving epsilon only differ by an expression that is antisymmetric in 5 or more indices and hence vanishes
    we restrict to the simplest case: two antisymmetric pairs with a block symmetry, i.e. an area block

    we can use the following observations :
        as we want to contstruct a basis it suffices to pick representatives of the different symmetry orbits module anti-sym in (>4) indices
            1) whenever 3 indices of one are metric are contracted against an epsilon we can actually express the tensor as one with 4 area indices contracted against epsilon
            2) all tensors with 2 area indices contracted against one epsilon can be expressed as tensors with the first 2 area indices contracted against epsilon 
            3) tensors with a maximum of 1 epsilon contraction per area metric can be exprerssed by those with at least one 2 area contraction 
    --}

    --get all possible epsilon inds that are allowed under the above considerations 

    getAllIndsEpsilon :: [Int] -> Symmetry  -> [[Int]]
    getAllIndsEpsilon inds (p,ap,b,cyc,cb)  = [ [a,b,c,d] | a <- [1..i-3], b <- [a+1..i-2], c <- [b+1..i-1], d <- [c+1..i], 
                                         (not $ isSym p [a,b,c,d]) && (not $ is3Area areaBlocks [a,b,c,d]) && (isValid2Area areaBlocks [a,b,c,d])
                                          && (not $ is1Area areaBlocks [a,b,c,d]) && (not $ isSymCyc cyc [a,b,c,d]) ]
                    where 
                        i = length inds 
                        blocks2 = filter (\x -> (length $ fst x) == 2)  b
                        areaBlocks = map (\(x,y) -> x ++ y) $ filter (\([a,b],[c,d]) -> (elem (a,b) ap) && (elem (c,d) ap)) blocks2
                        isSym [] x = False
                        isSym [(a,b)] [i,j,k,l] = length (intersect [a,b] [i,j,k,l]) == 2
                        isSym (x:xs) [i,j,k,l]
                            | isSym [x] [i,j,k,l] = True 
                            | otherwise = isSym xs [i,j,k,l]
                        isSymCyc [] x = False
                        isSymCyc [l'] [i,j,k,l] = length (intersect l' [i,j,k,l]) >= 2
                        isSymCyc (x:xs) [i,j,k,l]
                            | isSymCyc [x] [i,j,k,l] = True 
                            | otherwise = isSymCyc xs [i,j,k,l]
                        is3Area [] i = False 
                        is3Area [[a,b,c,d]] [i,j,k,l] = length (intersect [a,b,c,d] [i,j,k,l]) == 3
                        is3Area (x:xs) [i,j,k,l]
                            | is3Area [x] [i,j,k,l] = True 
                            | otherwise = is3Area xs [i,j,k,l]
                        isValid2Area [] i = True
                        isValid2Area [[a,b,c,d]] [i,j,k,l] 
                            | length inter == 2 = inter == [a,b]
                            | otherwise = True 
                             where
                                inter = intersect [a,b,c,d] [i,j,k,l]
                        isValid2Area (x:xs) [i,j,k,l] 
                            | isValid2Area [x] [i,j,k,l] = isValid2Area xs [i,j,k,l]
                            | otherwise = False 
                        is1Area [] i = False
                        is1Area list [i,j,k,l] = (maximum $ map (\x -> length $ intersect [i,j,k,l] x) list) == 1 

    --a 2-block symmetry with the respectively first indices at an epsilon yields an additional antisymmetry (note that we did not include higher block antisymmmetries)

    getExtraASymsEps :: [Int] -> Symmetry -> Symmetry 
    getExtraASymsEps eps (p,ap,blo,cyc,cb) = ([],newASyms, [], [], [])
            where 
                allBlocks = blo ++ (concat $ map mkBlocksFromBlockCycle cb) 
                blocks2 = filter (\(a,b) -> length a == 2) allBlocks
                newASyms = mapMaybe (\([i,j],[k,l]) -> if (length $ intersect [i,k] eps) == 2 then Just (j,l) else if (length $ intersect [j,l] eps) == 2  then Just (i,k) else Nothing) blocks2

    getEpsilonInds :: [Int] -> Symmetry -> [[Int]]
    getEpsilonInds inds sym = allIndsRed
            where 
                epsInds = getAllIndsEpsilon inds sym 
                allInds = concat $ filter (\x -> x /= []) $ map (\x -> map (\y -> x ++ y) $ getEtaInds (inds \\ x) (addSym sym (getExtraASymsEps x sym)) )epsInds 
                isSymP [] x = False
                isSymP [(a,b)] [i,j,k,l] = length (intersect [a,b] [i,j,k,l]) == 2
                isSymP (x:xs) [i,j,k,l]
                    | isSymP [x] [i,j,k,l] = True 
                    | otherwise = isSymP xs [i,j,k,l]
                filters = mkFilters sym 
                allIndsRed = filter (\x -> let symEps = addSym (getExtraASymsEps (take 4 x) sym) sym 
                                               symEta = addSym symEps (getAllExtraSyms (drop 4 x) symEps)
                                               newFilters = union filters (mkFilters symEta)
                                            in filterSym x newFilters) allInds
                
    {--
    Expressions containing sums of products of epsilon and eta with unknonwn variables are encoded as trees with nodes being given by 
    epsilons and etas and leafs being given by the variables 
    --}

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
    addVars (Var x y) (Var x' y') = if y == y' then Var (x + x') y else error "should not add different vars"
    {-# INLINEABLE addVars #-}

    multVar :: Int -> Var -> Var
    multVar x (Var x' y) = Var (x * x') y
    {-# INLINEABLE multVar #-}

    isZeroVar :: Var -> Bool
    isZeroVar (Var x _) = x==0
    {-# INLINEABLE isZeroVar #-}
   
    data AnsatzForestEta = ForestEta (M.Map Eta AnsatzForestEta)| Leaf !Var | EmptyForest  deriving (Show, Read, Eq, Generic, Serialize)

    type AnsatzForestEpsilon = M.Map Epsilon AnsatzForestEta

    --save and load forests as bytestrings 

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

    ansatzRank :: AnsatzForestEta -> Int
    ansatzRank ans = length $ getForestLabels ans 

    ansatzRankEpsilon :: AnsatzForestEpsilon -> Int 
    ansatzRankEpsilon ans = length $ getForestLabelsEpsilon ans 

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
    relabelAnsatzForestEpsilon i ans = if ans == M.empty then M.empty else mapVarsEpsilon update ans
            where
                vars = getForestLabelsEpsilon ans 
                relabMap = I.fromList $ zip vars [i..]
                update = relabelVar ((I.!) relabMap) 

    removeVarsEps :: [Int] -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    removeVarsEps vars m = M.filter (/= EmptyForest) $ M.map (removeVarsEta vars) m  

    --add 2 sorted forests 

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

    --draw the forests as ASCII picture

    drawEtaTree :: Eta -> AnsatzForestEta -> [String]
    drawEtaTree (Eta i j) (Leaf (Var a b)) =  ["(" ++ show i ++  "," ++ show j ++ ") * (" ++ show a ++ ") * x[" ++ show b ++ "]"]
    drawEtaTree (Eta i j) (ForestEta m) = lines ("(" ++ show i ++ "," ++ show j ++ ")") ++ (drawSubTrees m)
            where 
                drawSubTrees x
                    | x == M.empty = []
                    | M.size x == 1 = let [(a,b)] = M.assocs x in  "|" : shift "`---- " "   " (drawEtaTree a b)
                    | otherwise =  let  (a,b) = head $ M.assocs x in "|" : shift "+---- " "|  " (drawEtaTree a b) ++ (drawSubTrees $ M.delete a x)
                shift first other = zipWith (++) (first : repeat other)
    drawEtaTree eta EmptyForest = []

    drawEpsilonTree :: Epsilon -> AnsatzForestEta -> [String]
    drawEpsilonTree (Epsilon i j k l) (Leaf (Var a b)) = ["(" ++ show i ++ "," ++ show j ++ "," ++ show k ++ "," ++ show l ++ ") * (" ++ show a ++ ") * x[" ++ show b ++ "]"]
    drawEpsilonTree (Epsilon i j k l) (ForestEta m) = lines ("(" ++ show i ++ "," ++ show j ++ "," ++ show k ++ "," ++ show l ++ ")") ++ (drawSubTrees m)
            where 
                drawSubTrees x
                    | x == M.empty = []
                    | M.size x == 1 = let [(a,b)] = M.assocs x in  "|" : shift "`---- " "   " (drawEtaTree a b)
                    | otherwise =  let  (a,b) = head $ M.assocs x in "|" : shift "+---- " "|  " (drawEtaTree a b) ++ (drawSubTrees $ M.delete a x)
                shift first other = zipWith (++) (first : repeat other)
    drawEpsilonTree eps EmptyForest = []

    drawAnsatzEta :: AnsatzForestEta -> String 
    drawAnsatzEta (Leaf (Var a b)) = show a ++ "x[" ++ show b ++ "]"
    drawAnsatzEta (ForestEta m) = unlines $ map (\(x,y) -> unlines $ drawEtaTree x y) $ M.assocs m 
    drawAnsatzEta EmptyForest = [] 
    
    drawAnsatzEpsilon :: AnsatzForestEpsilon -> String 
    drawAnsatzEpsilon m 
            | M.size m == 0 = [] 
            | otherwise = unlines $ map (\(x,y) -> unlines $ drawEpsilonTree x y) $ M.assocs m 

    --get one representative for each Var Label

    forestEtaList :: AnsatzForestEta -> [[Eta]]
    forestEtaList f = map (\(a,b) -> a) fList''
            where 
                fList = flattenForest f 
                fList' = sortBy (\(e1, Var x1 y1 ) ((e2, Var x2 y2)) -> compare y1 y2) fList 
                fList'' = nubBy (\(e1, Var x1 y1 ) ((e2, Var x2 y2)) -> if x1 == 0 || x2 == 0 then error "zeros!!" else y1 == y2) fList' 

    forestEpsList :: AnsatzForestEpsilon -> [(Epsilon,[Eta])]
    forestEpsList f = map (\(a,b,c) -> (a,b)) fList'' 
            where 
                fList = flattenForestEpsilon f 
                fList' = sortBy (\(e1, e', Var x1 y1 ) ((e2, e2',  Var x2 y2)) -> compare y1 y2) fList 
                fList'' = nubBy (\(e1, e1', Var x1 y1 ) ((e2, e2', Var x2 y2)) -> if x1 == 0 || x2 == 0 then error "zeros!!" else y1 == y2) fList' 

    --output in latex format 

    mkEtasLatex :: String -> Eta -> String 
    mkEtasLatex inds (Eta i j) = "\\eta^{" ++ etaI : etaJ : "}"
            where
                (etaI,etaJ) = (inds !! (i-1), inds !! (j-1)  ) 

    forestEtaListLatex :: AnsatzForestEta -> String -> Char -> String 
    forestEtaListLatex f inds var =  tail $ concat etaL'' 
            where 
                etaL = sortBy (\(e1, Var x1 y1 ) ((e2, Var x2 y2)) -> compare y1 y2) $ flattenForest f 
                etaL' = nubBy (\(e1, Var x1 y1 ) ((e2, Var x2 y2)) -> if x1 == 0 || x2 == 0 then error "zeros!!" else y1 == y2) etaL 
                etaL'' = map (\(a,Var x y) -> "+" ++ var : "_{" ++ show y ++ "}\\cdot" ++ (concat $ map (mkEtasLatex inds) a)) etaL' 

    mkEpsLatex :: String -> Epsilon -> String 
    mkEpsLatex inds (Epsilon i j k l) =  "\\epsilon^{" ++ epsi : epsj : epsk : epsl : "}"
            where 
                (epsi, epsj, epsk, epsl) = (inds !! (i-1), inds !! (j-1), inds !! (k-1), inds !! (l-1))

    forestEpsListLatex :: AnsatzForestEpsilon -> String -> Char -> String 
    forestEpsListLatex f inds var = tail $ concat epsL''
            where 
                epsL = sortBy (\(e1, e1', Var x1 y1 ) ((e2, e2', Var x2 y2)) -> compare y1 y2) $ flattenForestEpsilon f 
                epsL' = nubBy (\(e1, e1', Var x1 y1 ) ((e2, e2', Var x2 y2)) -> if x1 == 0 || x2 == 0 then error "zeros!!" else y1 == y2) epsL 
                epsL'' = map (\(a,b,Var x y) -> "+" ++ var : "_{" ++ show y ++ "}\\cdot" ++ mkEpsLatex inds a ++ (concat $ map (mkEtasLatex inds) b)) epsL' 

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

    getEtaForestFast :: Int -> Symmetry -> AnsatzForestEta
    getEtaForestFast ord syms = relabelAnsatzForest 1 $ reduceAnsatzEta' syms allForests
                where
                    allInds = getEtaInds [1..ord] syms
                    allVars = mkAllVars
                    allForests = zipWith mkEtaList' allVars allInds

    getEpsForestFast :: Int -> Symmetry -> AnsatzForestEpsilon
    getEpsForestFast ord syms = if ord < 4 then M.empty else relabelAnsatzForestEpsilon 1 $ reduceAnsatzEpsilon' syms allForests
                where
                    allInds = getEpsilonInds [1..ord] syms 
                    allVars = mkAllVars
                    allForests = zipWith mkEpsilonList' allVars allInds


    {--
    The next part is evaluating a given AnsatzTree numerically. This is necessary to remove linear dependencies
    that occur due to implicit antisymmetries in 5 or more indices.
    --}

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

    --eval a given Forest for all inds

    type AssocsList a = [([(Int,Int)],a)]

    evalAllEta :: M.Map [Int] Int -> [I.IntMap Int] -> AnsatzForestEta -> [[(Int,Int)]]
    evalAllEta epsM [] f = []
    evalAllEta epsM evalMs EmptyForest = [] 
    evalAllEta epsM evalMs f = l'
                where
                    l = map (\x -> (filter (\(a,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEta epsM x f)) evalMs
                    l' = runEval $ parListChunk 500 rdeepseq l

    evalAllTensorEta :: (NFData a) => M.Map [Int] Int -> [(I.IntMap Int, a)] -> AnsatzForestEta -> AssocsList a
    evalAllTensorEta epsM [] f = []
    evalAllTensorEta epsM evalMs EmptyForest = [] 
    evalAllTensorEta epsM evalMs f = l'
                where
                    l = map (\(x,z) -> (filter (\(a,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEta epsM x f,z)) evalMs
                    l' = runEval $ parListChunk 500 rdeepseq l

    evalAllEpsilon :: M.Map [Int] Int -> [I.IntMap Int] -> AnsatzForestEpsilon -> [[(Int,Int)]]
    evalAllEpsilon epsM  [] f = []
    evalAllEpsilon epsM evalMs f = if f == M.empty then [] else l'
                where
                    l = map (\x -> (filter (\(a,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEpsilon epsM x f)) evalMs
                    l' = runEval $ parListChunk 500 rdeepseq l

    evalAllTensorEpsilon :: (NFData a) => M.Map [Int] Int -> [(I.IntMap Int, a)] -> AnsatzForestEpsilon -> AssocsList a
    evalAllTensorEpsilon epsM [] f = []
    evalAllTensorEpsilon epsM evalMs f = if f == M.empty then [] else l'
                where
                    l = map (\(x,z) -> ( filter (\(a,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEpsilon epsM x f,z)) evalMs
                    l' = runEval $ parListChunk 500 rdeepseq l


    {--
    Now there are two ways how we can proceed in removing the linear depenedncies and thus constructing a basis:

    1) the memory optimised way, constructing a lin indep tree from the very beginning
       the first step is to check whether a given Ansatz is elemment of the span of the previos ansätze and therefore can be discarded 

    2)  the second way is constructing a given Ansatz by first reducing only algebraically, and later on evaluating the whole forest
        to a matrix and reducing the matrix numerically.
        
    We start with the first way.
    --}
    
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

    --concat Matrices to a block Matrix

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


    --construct the RankData from the first nonzero Ansatz 

    mk1stRankDataEtaEigIO :: Symmetry -> Int -> [(Int,[Eta])] -> M.Map [Int] Int -> [I.IntMap Int] -> IO (AnsatzForestEta,RankDataEig,[(Int,[Eta])])
    mk1stRankDataEtaEigIO symL numEta etaL epsM evalM =
            do
                putStrLn $ (show $ fst $ head etaL) ++ " of " ++ show numEta
                let newAns = symAnsatzForestEta symL $ mkForestFromAscList (snd $ head etaL,Var 1 1)
                let newVec = evalAnsatzEtaVecListEig epsM evalM newAns
                let restList = tail etaL 
                case newVec of
                                    Nothing         -> if restList == [] then return (EmptyForest ,(Mat.fromList [], Sparse.fromList 0 0 []),[]) else mk1stRankDataEtaEigIO symL numEta restList epsM evalM 
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
                                    Nothing         -> if restList == [] then (EmptyForest,(Mat.fromList [], Sparse.fromList 0 0 []),[]) else mk1stRankDataEtaEig symL restList epsM evalM 
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
                                    Nothing         -> if restList == [] then return (M.empty,(Mat.fromList [], Sparse.fromList 0 0 []),[]) else mk1stRankDataEpsilonEigIO symL numEps restList epsM evalM
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
                                    Nothing         -> if restList == [] then (M.empty,(Mat.fromList [], Sparse.fromList 0 0 []),[]) else mk1stRankDataEpsilonEig symL restList epsM evalM
                                    Just newVec'    -> (newAns,(newMat, newVec'), restList)
                                        where 
                                            newVecTrans = Sparse.transpose newVec'
                                            newMat = Sparse.toMatrix $ Sparse.mul newVec' newVecTrans


    --finally reduce the ansatzList (IO versions print the current status for longer computations)

    reduceAnsatzEtaEigIO :: Symmetry -> [[Eta]] -> [I.IntMap Int] -> IO (AnsatzForestEta,Sparse.SparseMatrixXd)
    reduceAnsatzEtaEigIO symL etaL evalM =
            do
                let epsM = epsMap
                let etaLLength = length $ force etaL
                putStrLn $ "fast-forward to first non-vanishing ansatz in list of " ++ show etaLLength
                let zipped = zip [1..] etaL
                (ans1,rDat1,restEtaL) <- mk1stRankDataEtaEigIO symL etaLLength zipped epsM evalM
                putStrLn $ "first non-vanishing ansatz found"
                (finalForest, (_,finalMat)) <- foldM (addOrDiscardEtaEigIO symL etaLLength epsM evalM) (ans1,rDat1) restEtaL
                putStrLn $ "finished!"
                if evalM == [] 
                    then return (EmptyForest, Sparse.fromList 0 0 [])
                    else return (finalForest, finalMat)

    reduceAnsatzEtaEig :: Symmetry -> [[Eta]] -> [I.IntMap Int] -> (AnsatzForestEta,Sparse.SparseMatrixXd)
    reduceAnsatzEtaEig symL etaL evalM
            | evalM == [] = (EmptyForest, Sparse.fromList 0 0 [])
            | etaL == [] = (EmptyForest, Sparse.fromList 0 0 [])
            | otherwise = (finalForest, finalMat)
                where
                    epsM = epsMap
                    (ans1,rDat1,restEtaL) = mk1stRankDataEtaEig symL etaL epsM evalM
                    (finalForest, (_,finalMat)) = foldl' (addOrDiscardEtaEig symL epsM evalM) (ans1,rDat1) restEtaL 


    reduceAnsatzEpsilonEigIO :: Symmetry -> [(Epsilon,[Eta])] -> [I.IntMap Int] -> IO (AnsatzForestEpsilon,Sparse.SparseMatrixXd)
    reduceAnsatzEpsilonEigIO symL epsL evalM =
            do
                let epsM = epsMap
                let epsLLength = length $ force epsL
                putStrLn $ "fast-forward to first non-vanishing ansatz in list of " ++ show epsLLength
                let zipped = zip [1..] epsL
                (ans1,rDat1,restEpsL) <- mk1stRankDataEpsilonEigIO symL epsLLength zipped epsM evalM
                putStrLn $ "first non-vanishing ansatz found"
                (finalForest, (_,finalMat)) <- foldM (addOrDiscardEpsilonEigIO symL epsLLength epsM evalM) (ans1,rDat1) restEpsL
                putStrLn $ "finished!"
                if evalM == [] 
                    then return (M.empty, Sparse.fromList 0 0 [])
                    else return (finalForest, finalMat)


    reduceAnsatzEpsilonEig :: Symmetry -> [(Epsilon,[Eta])] -> [I.IntMap Int] -> (AnsatzForestEpsilon,Sparse.SparseMatrixXd)
    reduceAnsatzEpsilonEig symL epsL evalM
        | evalM == [] = (M.empty, Sparse.fromList 0 0 [])
        | epsL == [] = (M.empty, Sparse.fromList 0 0 [])
        | otherwise = (finalForest, finalMat)
            where
                epsM = epsMap
                (ans1,rDat1,restEpsL) = mk1stRankDataEpsilonEig symL epsL epsM evalM
                (finalForest, (_,finalMat)) = foldl' (addOrDiscardEpsilonEig symL epsM evalM) (ans1,rDat1) restEpsL 

    --construct a basis ansatz forest 

    getEtaForestEigIO :: Int -> Symmetry -> [I.IntMap Int] -> IO (AnsatzForestEta,Sparse.SparseMatrixXd)
    getEtaForestEigIO ord sym [] = return (EmptyForest, Sparse.fromList 0 0 []) 
    getEtaForestEigIO ord sym evalMs
        | allEtaLists == [] = return (EmptyForest, Sparse.fromList 0 0 []) 
        | otherwise = reduceAnsatzEtaEigIO sym allEtaLists evalMs
            where
                allInds = getEtaInds [1..ord] sym
                allEtaLists = map mkEtaList allInds

    getEtaForestEig :: Int -> Symmetry -> [I.IntMap Int] -> (AnsatzForestEta,Sparse.SparseMatrixXd)
    getEtaForestEig ord sym [] = (EmptyForest, Sparse.fromList 0 0 []) 
    getEtaForestEig ord sym evalMs 
        | allEtaLists == [] = (EmptyForest, Sparse.fromList 0 0 []) 
        | otherwise = reduceAnsatzEtaEig sym allEtaLists evalMs
            where
                allInds = getEtaInds [1..ord] sym
                allEtaLists = map mkEtaList allInds

    getEpsForestEigIO :: Int -> Symmetry -> [I.IntMap Int] -> IO (AnsatzForestEpsilon,Sparse.SparseMatrixXd)
    getEpsForestEigIO ord sym [] = return (M.empty, Sparse.fromList 0 0 []) 
    getEpsForestEigIO ord sym evalMs 
        | allEpsLists == [] = return (M.empty, Sparse.fromList 0 0 []) 
        | otherwise = reduceAnsatzEpsilonEigIO sym allEpsLists evalMs
            where
                allInds = getEpsilonInds [1..ord] sym 
                allEpsLists = map mkEpsilonList allInds

    getEpsForestEig :: Int -> Symmetry -> [I.IntMap Int] -> (AnsatzForestEpsilon,Sparse.SparseMatrixXd)
    getEpsForestEig ord sym [] = (M.empty, Sparse.fromList 0 0 []) 
    getEpsForestEig ord sym evalMs 
        | allEpsLists == [] = (M.empty, Sparse.fromList 0 0 []) 
        | otherwise =  reduceAnsatzEpsilonEig sym allEpsLists evalMs
            where
                allInds = getEpsilonInds [1..ord] sym 
                allEpsLists = map mkEpsilonList allInds

    --eta and eps forest combined

    getFullForestEigIO :: Int -> Symmetry -> [I.IntMap Int] -> [I.IntMap Int] -> IO (AnsatzForestEta, AnsatzForestEpsilon, Sparse.SparseMatrixXd, Sparse.SparseMatrixXd)
    getFullForestEigIO ord sym evalMEta evalMEps =
              do
                (etaAns, etaMat) <- getEtaForestEigIO ord sym evalMEta 
                (epsAns',epsMat) <- getEpsForestEigIO ord sym evalMEps
                let epsAns = relabelAnsatzForestEpsilon (1 + (length $ getForestLabels etaAns)) epsAns'
                return (etaAns, epsAns, etaMat, epsMat)

    getFullForestEig :: Int -> Symmetry -> [I.IntMap Int] -> [I.IntMap Int] -> (AnsatzForestEta, AnsatzForestEpsilon, Sparse.SparseMatrixXd, Sparse.SparseMatrixXd)
    getFullForestEig ord sym evalMEta evalMEps = (etaAns, epsAns, etaMat, epsMat)
            where
                (etaAns,etaMat) = getEtaForestEig ord sym evalMEta 
                (epsAns',epsMat) = getEpsForestEig ord sym evalMEps
                epsAns = relabelAnsatzForestEpsilon (1 + (length $ getForestLabels etaAns)) epsAns'

    {--
    Finally we can evaluated the ansatz trees to a contravariant tensor with spacetime indices
    Sym version outputs the fully symmetriized ansatz tensor, this is however expensive, non Sym version computes the non symmetrized ansatz
    tensor, i.e. only 1 representative out of each symmetry equivalence class is non zero. It is important to note that when constracting the non symmetrized
    tensor with another tensor with given symmetry one needs to account for the now missing muliplicities from the symmetries as in the construction of ansätze 
    we used factor less symmetriser functions. 
    --}

    evalToTensSym :: Symmetry -> M.Map [Int] Int -> [(I.IntMap Int, IndTuple n1 0)] -> [(I.IntMap Int, IndTuple n1 0)] -> AnsatzForestEta -> AnsatzForestEpsilon -> STTens n1 0 (AnsVar Rational) 
    evalToTensSym (p,ap,b,c,bc) epsM evalEta evalEps ansEta ansEps = symTens
                where 
                    p' = map (\(x,y) -> (x-1,y-1)) p 
                    ap' = map (\(x,y) -> (x-1,y-1)) ap 
                    b' = map (\(x,y) -> (map (\z -> z-1) x, map (\z' -> z'-1) y) ) b
                    c' = map (\l -> map (\l' -> l' -1) l) c 
                    bc' = map (\l -> map (\k -> map (\m -> m-1) k) l) bc 
                    etaL = evalAllTensorEta epsM evalEta ansEta 
                    epsL = evalAllTensorEpsilon epsM evalEps ansEps 
                    etaL' = map (\(x,indTuple) -> (indTuple, AnsVar $ I.fromList $ map (\(i,r) -> (i,fromIntegral r)) x)) etaL
                    epsL' = map (\(x,indTuple) -> (indTuple, AnsVar $ I.fromList $ map (\(i,r) -> (i,fromIntegral r)) x)) epsL
                    etaRmL = filter (\(_,b) -> b /= AnsVar I.empty) etaL'
                    epsRmL = filter (\(_,b) -> b /= AnsVar I.empty) epsL'
                    tens = (fromListT2 etaRmL) &+ (fromListT2 epsRmL)
                    symTens = foldr cyclicBlockSymATens1 (
                                foldr cyclicSymATens1 (
                                    foldr symBlockATens1 (
                                        foldr aSymATens1 (
                                            foldr symATens1 tens p'
                                            ) ap'
                                        ) b'
                                    ) c'
                                ) bc'  

    evalToTens :: M.Map [Int] Int -> [(I.IntMap Int, IndTuple n1 0)] -> [(I.IntMap Int, IndTuple n1 0)] -> AnsatzForestEta -> AnsatzForestEpsilon -> STTens n1 0 (AnsVar Rational) 
    evalToTens epsM evalEta evalEps ansEta ansEps = tens
                where 
                    etaL = evalAllTensorEta epsM evalEta ansEta 
                    epsL = evalAllTensorEpsilon epsM evalEps ansEps 
                    etaL' = map (\(x,indTuple) -> (indTuple, AnsVar $ I.fromList $ map (\(i,r) -> (i,fromIntegral r)) x)) etaL
                    epsL' = map (\(x,indTuple) -> (indTuple, AnsVar $ I.fromList $ map (\(i,r) -> (i,fromIntegral r)) x)) epsL
                    etaRmL = filter (\(_,b) -> b /= AnsVar I.empty) etaL'
                    epsRmL = filter (\(_,b) -> b /= AnsVar I.empty) epsL'
                    tens = (fromListT2 etaRmL) &+ (fromListT2 epsRmL)
                    
    
   
    --the 2 final functions, constructing the 2 AnsatzForests and the AnsatzTensor (currently the list of symmetry DOFs must be specified by hand -> this can also yield a performance advantage)

    mkAnsatzTensorEigIOSym :: forall (n :: Nat). SingI n => Int -> Symmetry -> [[Int]] -> IO (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 (AnsVar Rational)) 
    mkAnsatzTensorEigIOSym ord symmetries evalL =
              do
                let epsM = epsMap
                let evalLEta = filter isEtaList evalL 
                let evalLEps = filter isEpsilonList evalL 
                let evalLEtaRed = filter (isLorentzEval symmetries) evalLEta
                let evalLEpsRed = filter (isLorentzEval symmetries) evalLEps
                let evalMEtaRed = mkEvalMaps ord evalLEtaRed
                let evalMEpsRed = mkEvalMaps ord evalLEpsRed
                let evalMEtaInds = mkEvalMapsInds ord evalLEta
                let evalMEpsInds = mkEvalMapsInds ord evalLEps
                (ansEta, ansEps, _, _) <- getFullForestEigIO ord symmetries evalMEtaRed evalMEpsRed
                let tens = evalToTensSym symmetries epsM evalMEtaInds evalMEpsInds ansEta ansEps 
                return (ansEta, ansEps, tens)

    mkAnsatzTensorEigSym :: forall (n :: Nat). SingI n => Int -> Symmetry -> [[Int]] -> (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 (AnsVar Rational)) 
    mkAnsatzTensorEigSym ord symmetries evalL = (ansEta, ansEps, tens)
            where
                epsM = epsMap
                evalLEta = filter isEtaList evalL 
                evalLEps = filter isEpsilonList evalL 
                evalLEtaRed = filter (isLorentzEval symmetries) evalLEta
                evalLEpsRed = filter (isLorentzEval symmetries) evalLEps
                evalMEtaRed = mkEvalMaps ord evalLEtaRed
                evalMEpsRed = mkEvalMaps ord evalLEpsRed
                evalMEtaInds = mkEvalMapsInds ord evalLEta
                evalMEpsInds = mkEvalMapsInds ord evalLEps
                (ansEta, ansEps, _, _) = getFullForestEig ord symmetries evalMEtaRed evalMEpsRed
                tens = evalToTensSym symmetries epsM evalMEtaInds evalMEpsInds ansEta ansEps 

    
    --return the tensor without explicit symmetrisation

    mkAnsatzTensorEigIO :: forall (n :: Nat). SingI n => Int -> Symmetry -> [[Int]] -> IO (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 (AnsVar Rational)) 
    mkAnsatzTensorEigIO ord symmetries evalL =
              do
                let epsM = epsMap
                let evalLEta = filter isEtaList evalL 
                let evalLEps = filter isEpsilonList evalL 
                let evalLEtaRed = filter (isLorentzEval symmetries) evalLEta
                let evalLEpsRed = filter (isLorentzEval symmetries) evalLEps
                let evalMEtaRed = mkEvalMaps ord evalLEtaRed
                let evalMEpsRed = mkEvalMaps ord evalLEpsRed
                let evalMEtaInds = mkEvalMapsInds ord evalLEta
                let evalMEpsInds = mkEvalMapsInds ord evalLEps
                (ansEta, ansEps, _, _) <- getFullForestEigIO ord symmetries evalMEtaRed evalMEpsRed
                let tens = evalToTens epsM evalMEtaInds evalMEpsInds ansEta ansEps 
                return (ansEta, ansEps, tens)

    mkAnsatzTensorEig :: forall (n :: Nat). SingI n => Int -> Symmetry -> [[Int]] -> (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 (AnsVar Rational)) 
    mkAnsatzTensorEig ord symmetries evalL = (ansEta, ansEps, tens)
            where
                epsM = epsMap
                evalLEta = filter isEtaList evalL 
                evalLEps = filter isEpsilonList evalL 
                evalLEtaRed = filter (isLorentzEval symmetries) evalLEta
                evalLEpsRed = filter (isLorentzEval symmetries) evalLEps
                evalMEtaRed = mkEvalMaps ord evalLEtaRed
                evalMEpsRed = mkEvalMaps ord evalLEpsRed
                evalMEtaInds = mkEvalMapsInds ord evalLEta
                evalMEpsInds = mkEvalMapsInds ord evalLEps
                (ansEta, ansEps, _, _) = getFullForestEig ord symmetries evalMEtaRed evalMEpsRed
                tens = evalToTens epsM evalMEtaInds evalMEpsInds ansEta ansEps 


    --now we start with the second way 

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
                etaL = evalAllEta epsM evalM ansEta 
                etaVars = getPivots etaL 
                allEtaVars = getForestLabels ansEta
                remVarsEta =  allEtaVars \\ etaVars
                newEtaAns = relabelAnsatzForest 1 $ removeVarsEta remVarsEta ansEta

    reduceLinDepsFastEps :: M.Map [Int] Int -> [I.IntMap Int] -> Symmetry -> AnsatzForestEpsilon -> AnsatzForestEpsilon
    reduceLinDepsFastEps epsM evalM symL ansEps = newEpsAns
            where 
                epsL = evalAllEpsilon epsM evalM ansEps 
                epsVars = getPivots epsL 
                allEpsVars = getForestLabelsEpsilon ansEps
                remVarsEps =  allEpsVars \\ epsVars
                newEpsAns = relabelAnsatzForestEpsilon 1 $ removeVarsEps remVarsEps ansEps

    --final function, fast way of constructing the ansatztrees and the 2 tensors (again the list of symmetry DOFs bust be specified but this can yield a performance advantage)
                
    mkAnsatzTensorFastSym :: forall (n :: Nat). SingI n => Int -> Symmetry -> [[Int]]-> (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 (AnsVar Rational)) 
    mkAnsatzTensorFastSym ord symmetries evalL = (ansEtaRed, ansEpsRed, tens) 
            where
                epsM = epsMap
                evalLEta = filter isEtaList evalL 
                evalLEps = filter isEpsilonList evalL 
                evalLEtaRed = filter (isLorentzEval symmetries) evalLEta
                evalLEpsRed = filter (isLorentzEval symmetries) evalLEps
                evalMEtaRed = mkEvalMaps ord evalLEtaRed
                evalMEpsRed = mkEvalMaps ord evalLEpsRed
                evalMEtaInds = mkEvalMapsInds ord evalLEta
                evalMEpsInds = mkEvalMapsInds ord evalLEps
                ansEta = getEtaForestFast ord symmetries 
                ansEpsilon = getEpsForestFast ord symmetries  
                ansEtaRed = reduceLinDepsFastEta epsM evalMEtaRed symmetries ansEta
                ansEpsRed' = reduceLinDepsFastEps epsM evalMEpsRed symmetries ansEpsilon
                ansEpsRed = relabelAnsatzForestEpsilon (1 + (length $ getForestLabels ansEtaRed)) ansEpsRed'
                tens = evalToTensSym symmetries epsM evalMEtaInds evalMEpsInds ansEtaRed ansEpsRed 

    --and without explicit symmetriization in tens

    mkAnsatzTensorFast :: forall (n :: Nat). SingI n => Int -> Symmetry -> [[Int]]-> (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 (AnsVar Rational)) 
    mkAnsatzTensorFast ord symmetries evalL = (ansEtaRed, ansEpsRed, tens) 
            where
                epsM = epsMap
                evalLEta = filter isEtaList evalL 
                evalLEps = filter isEpsilonList evalL 
                evalLEtaRed = filter (isLorentzEval symmetries) evalLEta
                evalLEpsRed = filter (isLorentzEval symmetries) evalLEps
                evalMEtaRed = mkEvalMaps ord evalLEtaRed
                evalMEpsRed = mkEvalMaps ord evalLEpsRed
                evalMEtaInds = mkEvalMapsInds ord evalLEta
                evalMEpsInds = mkEvalMapsInds ord evalLEps
                ansEta = getEtaForestFast ord symmetries 
                ansEpsilon = getEpsForestFast ord symmetries  
                ansEtaRed = reduceLinDepsFastEta epsM evalMEtaRed symmetries ansEta
                ansEpsRed' = reduceLinDepsFastEps epsM evalMEpsRed symmetries ansEpsilon
                ansEpsRed = relabelAnsatzForestEpsilon (1 + (length $ getForestLabels ansEtaRed)) ansEpsRed'
                tens = evalToTens epsM evalMEtaInds evalMEpsInds ansEtaRed ansEpsRed 


    {--
    The last step consits of computing the evaluation list from the present symmetries. To that end it is important to note
    that for epsilon tensors only index combinations that contain each value 0,...,3 an odd number of times and for eta tensors we need an even number.
    Further note that due to the Lorentz infvariance of such expressions when computing linear dependencies we are free to relabel the coordinate axis,
    i.e. interchange for instance 1 and 0 as this is precisely the effect of a Lorentztransformation (at least up to a sign).
    Computing the eval Lists is actually the most expensive step and we can thus get a huge performance improvement if we explicitly provide the 
    eval maps by and and furthermore only evaluate index combinations that belong different symmetry equivalence classes.  
    --}

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

    --filter one representative of each symmetry equivalence class

    filterPSym :: [Int] -> (Int,Int) -> Bool 
    filterPSym inds (i,j) = (inds !! (i-1)) <= (inds !! (j-1))

    filterASym :: [Int] -> (Int,Int) -> Bool 
    filterASym inds (i,j) = (inds !! (i-1)) < (inds !! (j-1))    

    filterCSym :: [Int] -> [Int] -> Bool 
    filterCSym inds i =  and boolL 
            where 
                getPairs [a,b] = [(a,b)]
                getPairs (x:xs) = (x, head xs) : getPairs xs 
                pairL =  getPairs i 
                boolL = map (filterPSym inds) pairL

    filterBSym :: [Int] -> ([Int],[Int]) -> Bool 
    filterBSym inds ([],[]) = True
    filterBSym inds ((x:xs),(y:ys))
                | xVal < yVal = True 
                | xVal == yVal = filterBSym inds (xs,ys)
                | otherwise = False 
                 where
                    xVal = inds !! (x-1)
                    yVal = inds !! (y-1)

    filterBCSym :: [Int] -> [[Int]] -> Bool 
    filterBCSym inds i =  and boolL 
            where 
                getPairs [a,b] = [(a,b)]
                getPairs (x:xs) = (x, head xs) : getPairs xs 
                pairL =  getPairs i 
                boolL = map (filterBSym inds) pairL

    filterAllSym :: [Int] -> Symmetry -> Bool
    filterAllSym inds (p,ap,b,c,bc) = and (p' ++ ap' ++ c' ++ b' ++ bc')
            where 
                p' = map (filterPSym inds) p
                ap' = map (filterASym inds) ap 
                c' = map (filterCSym inds) c 
                b' = map (filterBSym inds) b 
                bc' = map (filterBCSym inds) bc

    --filter 1 representative out of each equivalence class that is generated by Lorentz transformations

    isLorentzEval :: Symmetry -> [Int] -> Bool
    isLorentzEval sym inds = inds == canonicalL
            where 
                allInds = filterMins $ getAllIndLists inds 
                canonicalL = minimum $ map (canonicalizeList sym) allInds 

    filterMins :: [[Int]] -> [[Int]]
    filterMins l = map fst $ filter (\x -> n == snd x) l' 
            where
                l' = map (\x -> (x,sum x)) l
                n = minimum $ map snd l' 
                
    --create all equivalent ind Lists 

    getAllIndListsMap :: I.IntMap Int -> [I.IntMap Int]
    getAllIndListsMap iMap = map (\x -> I.map ((I.!) x) iMap) allSwaps
             where 
                inds = nub $ I.elems iMap 
                n = length inds
                allSwaps = zipWith (\x y -> I.fromList $ zip x y) (repeat inds) $ permutations [0..n-1]

    getAllIndLists :: [Int] -> [[Int]] 
    getAllIndLists l = map I.elems $ getAllIndListsMap $ I.fromList $ zip [1..] l 

    --need to filter further as the symmetries might mix with the Lorentz filtration 

    canonicalizePair :: (Int,Int) -> I.IntMap Int -> I.IntMap Int 
    canonicalizePair (i,j) iMap 
                | (I.!) iMap i <= (I.!) iMap j = iMap
                | otherwise = I.mapKeys swapKeys iMap 
                where 
                    swapKeys x 
                        | x == i = j
                        | x == j = i 
                        | otherwise = x

    canonicalizeBlockPair :: ([Int],[Int]) -> I.IntMap Int -> I.IntMap Int 
    canonicalizeBlockPair ([i],[j]) iMap 
                | (I.!) iMap i <= (I.!) iMap j = iMap
                | otherwise = I.mapKeys swapKeys iMap 
                where 
                    swapKeys x 
                        | x == i = j
                        | x == j = i 
                        | otherwise = x
    canonicalizeBlockPair (i:is,j:js) iMap 
                | iVal < jVal = iMap
                | iVal > jVal = I.mapKeys (swapBlocks (i:is,j:js)) iMap
                | iVal == jVal = newMap 
                where 
                    iVal = (I.!) iMap i
                    jVal = (I.!) iMap j
                    swapBlocks (m1,m2) x = let m = I.fromList $ (zip m1 m2) ++ (zip m2 m1) 
                                         in  fromMaybe x $ I.lookup x m
                    newMap = canonicalizeBlockPair (is,js) iMap 
                    

    canonicalizeIntMap :: Symmetry -> I.IntMap Int -> I.IntMap Int 
    canonicalizeIntMap (p,ap,b,c,bc) iMap = iMap2
            where 
                allBlocks = b ++ (concat $ map mkBlocksFromBlockCycle bc) 
                allPairs = p ++ ap ++ (concat $ map mkSymsFromCycle c)
                iMap1 = foldr canonicalizePair iMap allPairs 
                iMap2 = foldr canonicalizeBlockPair iMap1 allBlocks 

    canonicalizeList :: Symmetry -> [Int] -> [Int]
    canonicalizeList sym inds = I.elems $ canonicalizeIntMap sym $ I.fromList $ zip [1..] inds

    allList' :: Int -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> [[Int]]
    allList' 1 syms aSyms symBounds aSymBounds = case (symB, aSymB) of
                                          (Just j, Nothing) -> [[k] | k <- [j..3]]
                                          (Nothing, Just j) -> [[k] | k <- [j+1..3]]
                                          (Nothing, Nothing) -> [[0], [1], [2], [3]]
                                          (Just j, Just k) -> [[k] | k <- [max j (k+1) .. 3]]
                where 
                    (symB,aSymB) = (lookup 1 symBounds, lookup 1 aSymBounds)
    allList' i syms aSyms symBounds aSymBounds = concat $ map (\x -> (:) <$> [x] <*> (allList' (i-1) newSyms newASyms (newSymBounds x) (newASymBounds x))) l
                where 
                    (symB,aSymB) = (lookup 1 symBounds, lookup 1 aSymBounds)
                    l' = case (symB, aSymB) of
                        (Just j, Nothing) -> [j..3]
                        (Nothing, Just j) ->  [j+1..3]
                        (Nothing, Nothing) -> [0..3]
                        (Just j, Just k) -> [max j (k+1) .. 3]
                    l = if (isJust newASymB) then filter (<3) l' else l' 
                    newSyms = map (\(x,y) -> (x-1,y-1)) syms
                    newASyms = map (\(x,y) -> (x-1,y-1)) aSyms
                    newSymB = lookup 1 syms 
                    newASymB = lookup 1 aSyms 
                    newSymBounds' = map (\(x,y) -> (x-1,y-1)) symBounds
                    newASymBounds' = map (\(x,y) -> (x-1,y-1)) aSymBounds
                    newSymBounds x' = case newSymB of 
                                          Just j -> (j-1,x') : newSymBounds'
                                          Nothing -> newSymBounds'
                    newASymBounds x' = case newASymB of 
                                           Just j -> (j-1,x') : newASymBounds'
                                           Nothing -> newASymBounds'

    --create all possible index lists by employing the constraints posed by pair symmetries

    allList :: Int -> Symmetry -> [[Int]]
    allList ord (syms,aSyms,_,_,_) =  allList' ord syms aSyms [] []
            
    mkEvalMaps :: Int -> [[Int]] -> [I.IntMap Int] 
    mkEvalMaps i l = map (\x -> I.fromList $ zip [1..i] x) l 

    mkEvalMapsInds :: forall (n :: Nat). SingI n => Int -> [[Int]] -> [(I.IntMap Int, IndTuple n 0)]
    mkEvalMapsInds i l = map (\x -> (I.fromList $ zip [1..i] x, (fromList' $ map toEnum x, Empty))) l 


    --use the above functions to construct ansätze without providing eval lists by hand 

    mkAnsatzTensorEigIOSym' :: forall (n :: Nat). SingI n =>  Int -> Symmetry -> IO (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 (AnsVar Rational)) 
    mkAnsatzTensorEigIOSym' ord symmetries =
              do
                let evalL = filter (\x -> filterAllSym x symmetries) $ allList ord symmetries
                mkAnsatzTensorEigIOSym ord symmetries evalL   

    mkAnsatzTensorEigSym' :: forall (n :: Nat). SingI n =>  Int -> Symmetry -> (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 (AnsVar Rational)) 
    mkAnsatzTensorEigSym' ord symmetries = mkAnsatzTensorEigSym ord symmetries evalL
            where
                evalL = filter (\x -> filterAllSym x symmetries) $ allList ord symmetries

    mkAnsatzTensorFastSym' :: forall (n :: Nat). SingI n => Int -> Symmetry -> (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 (AnsVar Rational)) 
    mkAnsatzTensorFastSym' ord symmetries = mkAnsatzTensorFastSym ord symmetries evalL
            where
                evalL = filter (\x -> filterAllSym x symmetries) $ allList ord symmetries

    --and without explicit symmetrization

    mkAnsatzTensorEigIO' :: forall (n :: Nat). SingI n =>  Int -> Symmetry -> IO (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 (AnsVar Rational)) 
    mkAnsatzTensorEigIO' ord symmetries =
              do
                let evalL = filter (\x -> filterAllSym x symmetries) $ allList ord symmetries
                mkAnsatzTensorEigIO ord symmetries evalL   

    mkAnsatzTensorEig' :: forall (n :: Nat). SingI n =>  Int -> Symmetry -> (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 (AnsVar Rational)) 
    mkAnsatzTensorEig' ord symmetries = mkAnsatzTensorEig ord symmetries evalL
            where
                evalL = filter (\x -> filterAllSym x symmetries) $ allList ord symmetries

    mkAnsatzTensorFast' :: forall (n :: Nat). SingI n => Int -> Symmetry -> (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 (AnsVar Rational)) 
    mkAnsatzTensorFast' ord symmetries = mkAnsatzTensorFast ord symmetries evalL
            where
                evalL = filter (\x -> filterAllSym x symmetries) $ allList ord symmetries
