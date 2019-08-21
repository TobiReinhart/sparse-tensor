-----------------------------------------------------------------------------
-- |
-- Module      :  Math.Tensor.LorentzGenerator
-- Copyright   :  (c) 2019 Tobias Reinhart and Nils Alex
-- License     :  MIT
-- Maintainer  :  tobi.reinhart@fau.de, nils.alex@fau.de
--
-- This module supplements the sparse-tensor package with the functionality of constructing bases of the space of Lorentz invariant tensors of arbitrary rank and symmetry.
--
-- It can be shown that all \( SO(3,1) \) invariant tensors must be given by expressions that are solely composed of the Minkowski metric \(\eta_{ab} \), its inverse \(\eta^{ab} \) and the covariant and contravariant Levi-Civita
-- symbols \( \epsilon_{abcd}\) and \( \epsilon^{abcd} \). Any such an expression can be written as a sum of products of these tensors, with the individual products
-- containing the appropriate number of factors ensuring the required rank of the expression and the sum further enforcing the required symmetry. In the following such an expression is simply called an ansatz.
-- Thus the goal of the following functions is the computation of a set of ansätze of given rank and symmetry that are linear independent and allow one to express any further Lorentz invariant tensor with the same rank and symmetry as appropriate linear combination of them.
--
-- Considering tensors with @4@ contravariant spacetime indices \(T^{abcd} \) that further satisfy the symmetry property \( T^{abcd} = T^{cdab} = T^{bacd} \) as an example, there only exist two linear independent ansätze namely:
--
--          * \( \eta^{ab} \eta^{cd}\)
--          * \( \eta^{c(a} \eta^{b)d} \).
--
-- If the tensors are required to have @6@ contravariant spacetime indices \( Q^{abcdpq} \) and satisfy the symmetry property \(Q^{abcdpq} = Q^{cdabpq} = - Q^{bacdpq} = Q^{abcdqp} \) there exist three linear independent ansätze:
--
--          * \( \eta^{ac}\eta^{bd}\eta^{pq} - \eta^{ad}\eta^{bc}\eta^{pq} \)
--          * \( \eta^{ac}\eta^{bp}\eta^{dq} + \eta^{ac}\eta^{bq}\eta^{dp} - \eta^{bc}\eta^{ap}\eta^{dq} - \eta^{bc}\eta^{aq}\eta^{dp} - \eta^{ad}\eta^{bp}\eta^{cq} - \eta^{ad}\eta^{bq}\eta^{cp} + \eta^{bd}\eta^{ap}\eta^{cq} + \eta^{bd}\eta^{aq}\eta^{cp}  \)
--          * \( \epsilon^{abcd}\eta^{pq} \).
--
-- One can further show that any Lorentz invariant tensor must include in each of its individual products either exactly one or no Levi-Civita symbol. Further there exist no linear dependencies between those ansätze that contain an \(\epsilon^{abcd}\) or \(\epsilon_{abcd}\) and those that do not.
-- Hence the problem actually decouples into two sub problems, the construction of all linear independent ansätze that do not contain an Levi-Civita symbol and the construction of all those linear independent ansätze that do contain exactly one Levi-Civita symbol.
--
--
-- This module specifically defines data types @'AnsatzForestEta'@ and @'AnsatzForestEpsilon'@ that are internally implemented as ordered expression tailored towards linear combinations of the two types of ansätze.
--
-- Currently the computation of ansatz bases is limited to the case where all indices are contravariant spacetime indices.
-- Minor changes should nevertheless also allow the computation of ansatz bases for arbitrary mixed rank spacetime tensors and even bases for tensors that are invariant under the action of any \(\mathrm{SO}(p,q)\), i.e. in arbitrary dimension and for arbitrary signature of the inner product.
-----------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE TupleSections #-}

module Math.Tensor.LorentzGenerator (
-- * Expression Forest Data Types
-- ** Node Types
Eta(..), Epsilon(..), Var(..),
-- ** Forest types
AnsatzForestEpsilon, AnsatzForestEta(..),
-- ** Conversions of AnsatzForests
-- *** List of Branches
flattenForest, flattenForestEpsilon, forestEtaList, forestEpsList, forestEtaListLatex, forestEpsListLatex,
-- *** ASCII drawing
drawAnsatzEta, drawAnsatzEpsilon,
-- ** Utility functions
-- *** Modifying Variables
getForestLabels, getForestLabelsEpsilon,
removeVarsEta, removeVarsEps,
relabelAnsatzForest, relabelAnsatzForestEpsilon,
mapVars, mapVarsEpsilon,
-- *** Ansatz Rank
ansatzRank, ansatzRankEpsilon,
-- *** Saving and Loading
encodeAnsatzForestEta, encodeAnsatzForestEpsilon,
decodeAnsatzForestEta, decodeAnsatzForestEpsilon,
-- * Construction of Ansatz Bases
-- ** The Fast Way
-- | The following functions construct the basis of Lorentz invariant tensors of given rank and symmetry by using an algorithm that is optimized towards
-- fast computation times. This is achieved at the cost of memory swelling of intermediate results.
--
-- The output of each of the following functions is given by a triplet that consists of @('AnsatzForestEta', 'AnsatzForestEpsilon', 'Tensor' 'AnsVarR')@.
-- The @'Tensor'@ is obtained by explicitly providing the the components of the ansätze with individual ansätze given by individual variables of type @'AnsVar'@.
--
mkAnsatzTensorFastSym, mkAnsatzTensorFast, mkAnsatzTensorFastAbs,
mkAnsatzTensorFastSym', mkAnsatzTensorFast',
-- ** The Memory Optimized Way
-- The following functions essentially compute the same results as their __Fast__ counterparts, with the only distinction being that they employ a slightly different
-- algorithm that avoids the problem of intermediate memory swelling and thus yields improved memory usage. All this is achieved at the cost of slightly higher computation times compared to the __Fast__ functions.
mkAnsatzTensorIncrementalSym, mkAnsatzTensorIncremental, mkAnsatzTensorIncrementalAbs,
mkAnsatzTensorIncrementalSym', mkAnsatzTensorIncremental',
-- * Specifying Additional Data
-- ** Symmetry Type
Symmetry,
-- ** Evaluation Lists
-- *** Area Metric
-- | The following provides an example of evaluation lists.
areaList4, areaList6, areaList8, areaList10_1, areaList10_2, areaList12, areaList14_1, areaList14_2,
-- *** Metric
-- | In the documentation of the following further provided exemplary evaluation lists index labels \(A, B, C, ...\) also refers to indices of type @'Ind9'@.
metricList2, metricList4_1, metricList4_2, metricList6_1, metricList6_2, metricList6_3, metricList8_1, metricList8_2,
-- ** Symmetry Lists
-- *** Area Metric
-- | The following are examples of symmetry lists.
symList4, symList6, symList8, symList10_1, symList10_2, symList12, symList14_1, symList14_2,
-- *** Metric
-- | The following are examples of symmetry lists.
metricsymList2, metricsymList4_1, metricsymList4_2, metricsymList6_1, metricsymList6_2, metricsymList6_3, metricsymList8_1, metricsymList8_2,
symList16_1, areaList16_1
) where

import qualified Data.IntMap.Strict as I
import qualified Data.Map.Strict as M
import Data.List (nub, permutations, foldl', (\\), elemIndex, nubBy, sortBy, insert, intersect, union, partition, delete, maximumBy, splitAt)
import Data.Maybe (fromJust, isNothing, fromMaybe, isJust, mapMaybe)
import Control.Parallel.Strategies (parListChunk, rdeepseq, runEval, NFData)
import Data.Serialize (encodeLazy, decodeLazy, Serialize(..))
import GHC.Generics
import qualified Data.ByteString.Lazy as BS (ByteString)
import Codec.Compression.GZip (compress, decompress)
import Data.Either (either)
import Data.Tuple (swap)
import GHC.TypeLits

--LinearAlgebra subroutines

import qualified Numeric.LinearAlgebra.Data as HM
import qualified Numeric.LinearAlgebra as Matrix

import Math.Tensor.Internal.LinearAlgebra (independentColumns)

import Math.Tensor

{--
The first step consist of pre-reducing the index list for the eta and epsilon trees as much as possible.
This is done by using the symmetries in the sense that we try to select exactly one representative out of each class of indices
that are equivalent under the symmetries.
Note that the pre-reduction is not necessary but increases performance.
--}

-- | Type alias to encode the symmetry information. The individual @'Int'@ values label the individual spacetime indices, the @'Symmetry'@ type is the compromised of (SymPairs, ASymPairs, BlockSyms, CyclicSyms, CyclicBlockSyms).
type Symmetry = ( [(Int,Int)] , [(Int,Int)] , [([Int],[Int])] , [[Int]], [[[Int]]] )

addSym :: Symmetry -> Symmetry -> Symmetry
addSym (a,b,c,d,e) (f,g,h,i,j) = (a `union` f, b `union` g, c `union` h, d `union` i, e `union` j)

--constructing the filter list out of the symmetry data for filtering one representative out of each symmetry class

mkFilters :: Symmetry -> [(Int,Int)]
mkFilters (pairs,aPairs,blocks,cycles,blockCycles) = map sortPair $ f1 `union` (f2 `union` (f3 `union` f4))
    where
        sortPair (a,b) = if a < b then (a,b) else (b,a)
        f1 =  pairs ++ aPairs
        f2 = map (\(a,b) -> (head a, head b)) blocks
        f3 = concatMap getPairs cycles
        f4 = concatMap (getPairs . map head) blockCycles

--filter the index lists

filter1Sym :: [Int] -> (Int,Int) -> Bool
filter1Sym l (i,j) = case (iPos,jPos) of
                        (Just i', Just j') ->  i' < j'
                        _ -> True
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
in terms of a cyclic block symmetry.
--}

getExtraSyms1 :: [Int] -> Symmetry -> Symmetry
getExtraSyms1 [] _ = ([],[],[],[],[])
getExtraSyms1 [_] _ = error "cannot get extra syms from singleton index list"
getExtraSyms1 (a:b:xs) (pairs,aPairs,blocks,cycles,blockCycles) = addSym (newPairs, [],  newBlocks, [], []) (getExtraSyms1 xs newSyms)
        where
            allBlocks = blocks ++ concatMap mkBlocksFromBlockCycle blockCycles
            newBlocks' = map (\(x,y) -> unzip $ filter (\(c,d) -> (c,d) /= (a,b)) $ zip x y) allBlocks
            (newBlocks, newPairs') = partition (\(a',_) -> length a' > 1) newBlocks'
            newPairs = map (\([a'],[b']) -> (a',b')) newPairs'
            newSyms = addSym (pairs,aPairs,blocks,cycles,blockCycles) (newPairs, [],  newBlocks, [], [])

mkBlocksFromBlockCycle :: [[Int]] -> [([Int],[Int])]
mkBlocksFromBlockCycle [x,y] = [(x,y)]
mkBlocksFromBlockCycle (x:xs) = l ++ mkBlocksFromBlockCycle xs
        where
            l = map (x,) xs
mkBlocksFromBlockCycle _ = error "cannot make block symmetries from empty block list"

{--
Furthermore distributing a symmetric or antisymmetric pair of indices over 2 etas yields an additional symmetry or anti-symmetry
of the remaining eta indices due to the product structure: for instance consider the a <-> b symmetry,
writing eta[ac] eta[bd] yields an additional c <-> d symmetry. Here it is additionally necessary to include the pair symmetries that are contributed by a given total symmetry
--}


--given one eta, if the eta contains an index from a symmetric or antisymmetric pair return the corresponding second index and the other index of the eta

get2nd :: [Int] -> Symmetry -> (Maybe [(Int,Int)], Maybe [(Int,Int)])
get2nd [a,b] (pairs,aPairs,_,cycles,_) = (sndPairs, sndAPairs)
        where
            allPairs = pairs ++ concatMap mkSymsFromCycle cycles
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
get2nd _ _ = error "given index list contains more or less than two indices"


--find the eta that contains the computed second pair index and return the other indices of this eta

get2ndSyms :: Maybe [(Int,Int)] -> Symmetry -> [[Int]] -> Symmetry
get2ndSyms Nothing syms _ = syms
get2ndSyms (Just i) _ etas = (newPairs,[],[],[],[])
    where
        get2ndInd l (i',j) = mapMaybe (\[a,b] -> if j == a then Just (i',b) else if j == b then Just (i',a) else Nothing) l
        newPairs = concatMap (get2ndInd etas) i

mkSymsFromCycle :: [Int] -> [(Int,Int)]
mkSymsFromCycle [x,y] = [(x,y)]
mkSymsFromCycle (x:xs) = l ++ mkSymsFromCycle xs
        where
            l = map (x,) xs
mkSymsFromCycle _ = error "cannot make syms from empty cycle list"


get2ndASyms :: Maybe [(Int,Int)] -> Symmetry -> [[Int]] -> Symmetry
get2ndASyms Nothing syms _ = syms
get2ndASyms (Just i) _ etas = ([], newAPairs,[],[],[])
    where
        get2ndInd l (i',j) = mapMaybe (\[a,b] -> if j == a then Just (i',b) else if j == b then Just (i',a) else Nothing) l
        newAPairs = concatMap (get2ndInd etas) i

mkEtas :: [Int] -> [[Int]]
mkEtas [] = []
mkEtas [l,k] = [[l,k]]
mkEtas (l:k:ls) = [l,k] : mkEtas ls
mkEtas _  = error "cannot make etas from singleton index list"

--apply to whole ind list

getExtraSyms2 :: [Int] -> Symmetry -> Symmetry
getExtraSyms2 [] syms = syms
getExtraSyms2 (a':b':xs) syms = addSym (getExtraSyms2 xs newSyms) newSyms
        where
            x = [a',b']
            (i,j) = get2nd x syms
            (p,_,_,_,_) = get2ndSyms i syms (mkEtas xs)
            (_,a,_,_,_) = get2ndASyms j syms (mkEtas xs)
            newSyms = addSym (p,a,[],[],[]) syms
getExtraSyms2 _ _ = error "cannot get extra syms from singleton index list"

--compute all extra symmetries

getAllExtraSyms :: [Int] -> Symmetry -> Symmetry
getAllExtraSyms etas syms = allSyms2
            where
                allSyms1 = addSym (getExtraSyms1 etas syms) syms
                allSyms2 = addSym (getExtraSyms2 etas allSyms1) allSyms1


getAllIndsEta :: [Int] -> [(Int,Int)] -> [[Int]]
getAllIndsEta [a,b] _ = [[a,b]]
getAllIndsEta (x:xs) aSyms = concatMap res firstEta
        where
            firstEta = mapMaybe (\y -> if (x,y) `notElem` aSyms then Just ([x,y],delete y xs) else Nothing) xs
            res (a,b) = (++) a <$> getAllIndsEta b aSyms
getAllIndsEta _ _ = error "empty index list"

filterEta :: [Int] -> Symmetry -> [(Int,Int)] -> Bool
filterEta inds (p1,ap1,b1,c1,cb1) filters = filterSym inds totFilters && isNonZero
        where
            (p2,ap2,b2,c2,cb2) = getAllExtraSyms inds (p1,ap1,b1,c1,cb1)
            extrafilters = mkFilters (p2,ap2,b2,c2,cb2)
            totFilters = filters `union` extrafilters
            etas = map (\[a,b] -> (a,b)) $ mkEtas inds
            isNonZero = null $ etas `intersect` union ap1 ap2

--construct a pre-reduced list of eta indices

getEtaInds :: [Int] -> Symmetry -> [[Int]]
getEtaInds [] _ = [[]]
getEtaInds inds (p,ap,b,c,bc) = filter (\x -> filterEta x (p,ap,b,c,bc) filters1) allInds
        where
            filters1 = mkFilters (p,ap,b,c,bc)
            allInds = getAllIndsEta inds ap

{--
Now we proceed in the same fashion for the epsilon ind list.
Here we can actually from the very beginning prevent some linear dependencies from occurring by noting that due to certain symmetries
certain expressions involving epsilon only differ by an expression that is antisymmetric in 5 or more indices and hence vanishes
we restrict to the simplest case: two antisymmetric pairs with a block symmetry, i.e. an area block

we can use the following observations :
    as we want to construct a basis it suffices to pick representatives of the different symmetry orbits module anti-sym in (>4) indices
        1) whenever 3 indices of one are metric are contracted against an epsilon we can actually express the tensor as one with 4 area indices contracted against epsilon
        2) all tensors with 2 area indices contracted against one epsilon can be expressed as tensors with the first 2 area indices contracted against epsilon
        3) tensors with a maximum of 1 epsilon contraction per area metric can be expressed by those with at least one 2 area contraction
--}

--get all possible epsilon inds that are allowed under the above considerations

getAllIndsEpsilon :: [Int] -> Symmetry  -> [[Int]]
getAllIndsEpsilon inds (p,ap,bs,cyc,_)  = [ [a,b,c,d] | a <- [1..numInds-3], b <- [a+1..numInds-2], c <- [b+1..numInds-1], d <- [c+1..numInds],
                                     not (isSym p [a,b,c,d]) && not (is3Area areaBlocks [a,b,c,d]) && isValid2Area areaBlocks [a,b,c,d]
                                      && not (is1Area areaBlocks [a,b,c,d]) && not (isSymCyc cyc [a,b,c,d]) ]
                where
                    numInds = length inds
                    blocks2 = filter (\x -> length (fst x) == 2)  bs
                    areaBlocks = map (uncurry (++)) $ filter (\([a,b],[c,d]) -> (a,b) `elem` ap && (c,d) `elem` ap) blocks2
                    isSym [] _ = False
                    isSym [(a,b)] [i,j,k,l] = length ([a,b] `intersect` [i,j,k,l]) == 2
                    isSym (x:xs) [i,j,k,l]
                        | isSym [x] [i,j,k,l] = True
                        | otherwise = isSym xs [i,j,k,l]
                    isSym _ _ = error "expected four indices"
                    isSymCyc [] _ = False
                    isSymCyc [l'] [i,j,k,l] = length (l' `intersect` [i,j,k,l]) >= 2
                    isSymCyc (x:xs) [i,j,k,l]
                        | isSymCyc [x] [i,j,k,l] = True
                        | otherwise = isSymCyc xs [i,j,k,l]
                    isSymCyc _ _ = error "expected four indices"
                    is3Area [] _ = False
                    is3Area [[a,b,c,d]] [i,j,k,l] = length ([a,b,c,d] `intersect` [i,j,k,l]) == 3
                    is3Area (x:xs) [i,j,k,l]
                        | is3Area [x] [i,j,k,l] = True
                        | otherwise = is3Area xs [i,j,k,l]
                    is3Area _ _ = error "expected four indices"
                    isValid2Area [] _ = True
                    isValid2Area [[a,b,c,d]] [i,j,k,l]
                        | length inter == 2 = inter == [a,b]
                        | otherwise = True
                         where
                            inter = [a,b,c,d] `intersect` [i,j,k,l]
                    isValid2Area (x:xs) [i,j,k,l]
                        | isValid2Area [x] [i,j,k,l] = isValid2Area xs [i,j,k,l]
                        | otherwise = False
                    isValid2Area _ _ = error "expected four indices"
                    is1Area [] _ = False
                    is1Area list [i,j,k,l] = maximum (map (length . ([i,j,k,l] `intersect`)) list) == 1
                    is1Area _ _ = error "expected four indices"

--a 2-block symmetry with the respectively first indices at an epsilon yields an additional anti-symmetry (note that we did not include higher block anti-symmetries)

getExtraASymsEps :: [Int] -> Symmetry -> Symmetry
getExtraASymsEps eps (_,_,blo,_,cb) = ([],newASyms, [], [], [])
        where
            allBlocks = blo ++ concatMap mkBlocksFromBlockCycle cb
            blocks2 = filter (\(a,_) -> length a == 2) allBlocks
            newASyms = mapMaybe (\([i,j],[k,l]) -> if length ([i,k] `intersect` eps) == 2 then Just (j,l) else if length ([j,l] `intersect` eps) == 2  then Just (i,k) else Nothing) blocks2

getEpsilonInds :: [Int] -> Symmetry -> [[Int]]
getEpsilonInds inds sym = allIndsRed
        where
            epsInds = getAllIndsEpsilon inds sym
            allInds = concat $ filter (not . null) $ map (\x -> map (x ++) $ getEtaInds (inds \\ x) (addSym sym (getExtraASymsEps x sym)) )epsInds
            filters = mkFilters sym
            allIndsRed = filter (\x -> let symEps = addSym (getExtraASymsEps (take 4 x) sym) sym
                                           symEta = addSym symEps (getAllExtraSyms (drop 4 x) symEps)
                                           newFilters = union filters (mkFilters symEta)
                                        in filterSym x newFilters) allInds

{--
Expressions containing sums of products of epsilon and eta with unknown variables are encoded as trees with nodes being given by
epsilons and etas and leafs being given by the variables
--}

--eta and epsilon types for the tree representing a sum of products of these tensors

-- | Data type that represents the individual \(\eta^{ab}\) tensor. The indices are labeled not by characters but by integers.
data Eta = Eta {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show, Read, Eq, Ord, Generic, Serialize, NFData)

-- | Data type that represents the individual \(\epsilon^{abcd}\) tensor. The indices are labeled not by characters but by integers.
data Epsilon = Epsilon {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show, Read, Eq, Ord, Generic, Serialize, NFData)

-- | Data type that represents variables that multiply the individual ansätze to form a general linear combination. The 2nd @'Int'@ argument labels the variables the first @'Int'@ is a factor that multiplies the variable.
data Var = Var {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show, Read, Eq, Ord, Generic, Serialize, NFData )

sortList :: Ord a => [a] -> [a]
sortList = foldr insert []

sortEta :: Eta -> Eta
sortEta (Eta x y) = Eta (min x y) (max x y)
{-# INLINEABLE sortEta #-}

sortEpsilon :: Epsilon -> Epsilon
sortEpsilon (Epsilon i j k l) = Epsilon i' j' k' l'
         where
            [i',j',k',l'] = sortList [i,j,k,l]

getEpsSign :: Epsilon -> Int
getEpsSign (Epsilon i j k l) = (-1) ^ length (filter (==True) [j>i,k>i,l>i,k>j,l>j,l>k])
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

-- | Data type that represents a general linear combination of ansätze that involve no \(\epsilon^{abcd}\).
data AnsatzForestEta = ForestEta (M.Map Eta AnsatzForestEta)| Leaf !Var | EmptyForest  deriving (Show, Read, Eq, Generic, Serialize)

-- | Data type that represents a general linear combination of ansätze that involve one \(\epsilon^{abcd}\) in each individual product.
type AnsatzForestEpsilon = M.Map Epsilon AnsatzForestEta

--save and load forests as bytestrings

-- | Encode an @'AnsatzForestEta'@ employing the @'Serialize'@ instance.
encodeAnsatzForestEta :: AnsatzForestEta -> BS.ByteString
encodeAnsatzForestEta = compress . encodeLazy

-- | Encode an @'AnsatzForestEpsilon'@ employing the @'Serialize'@ instance.
encodeAnsatzForestEpsilon :: AnsatzForestEpsilon -> BS.ByteString
encodeAnsatzForestEpsilon = compress . encodeLazy

-- | Decode an @'AnsatzForestEta'@ employing the @'Serialize'@ instance.
decodeAnsatzForestEta :: BS.ByteString -> AnsatzForestEta
decodeAnsatzForestEta bs = either error id $ decodeLazy $ decompress bs

-- | Decode an @'AnsatzForestEpsilon'@ employing the @'Serialize'@ instance.
decodeAnsatzForestEpsilon :: BS.ByteString -> AnsatzForestEpsilon
decodeAnsatzForestEpsilon bs = either error id $ decodeLazy $ decompress bs

--map a function over the nodes of the AnsatzTree (map over the tensors eta and epsilon)

mapNodes :: (Eta -> Eta) -> AnsatzForestEta -> AnsatzForestEta
mapNodes _ EmptyForest = EmptyForest
mapNodes f (ForestEta m) = ForestEta $ M.mapKeys f . M.map (mapNodes f) $ m
mapNodes _ (Leaf x) = Leaf x

mapNodesEpsilon :: (Epsilon -> Epsilon) -> AnsatzForestEpsilon -> AnsatzForestEpsilon
mapNodesEpsilon = M.mapKeys

--map over the vars, i.e. the leafs of the tree

-- | Map a general function over all variables that are contained in the @'AnsatzForestEta'@.
mapVars :: (Var -> Var) -> AnsatzForestEta -> AnsatzForestEta
mapVars _ EmptyForest = EmptyForest
mapVars f (Leaf var) = Leaf (f var)
mapVars f (ForestEta m) = ForestEta $ M.map (mapVars f) m

-- | Map a general function over all variables that are contained in the @'AnsatzForestEpsilon'@.
mapVarsEpsilon :: (Var -> Var) -> AnsatzForestEpsilon -> AnsatzForestEpsilon
mapVarsEpsilon f = M.map (mapVars f)

--relabel and remove Vars in the Forest

getLeafVals :: AnsatzForestEta -> [Var]
getLeafVals (Leaf var) = [var]
getLeafVals (ForestEta m) = rest
        where
            rest = concatMap getLeafVals $ M.elems m
getLeafVals EmptyForest = []

getLeafValsEpsilon :: AnsatzForestEpsilon -> [Var]
getLeafValsEpsilon m = concatMap getLeafVals $ M.elems m

getVarLabels :: Var -> Int
getVarLabels (Var _ j) = j

-- | Return a list of the labels of all variables that are contained in the @'AnsatzForestEta'@.
getForestLabels :: AnsatzForestEta -> [Int]
getForestLabels ans = nub $ map getVarLabels $ getLeafVals ans

-- | Return a list of the labels of all variables that are contained in the @'AnsatzForestEpsilon'@.
getForestLabelsEpsilon :: AnsatzForestEpsilon -> [Int]
getForestLabelsEpsilon m = nub $ map getVarLabels $ getLeafValsEpsilon m

-- | Return the rank, i.e. the number of different variables that is contained in the @'AnsatzForestEta'@.
ansatzRank :: AnsatzForestEta -> Int
ansatzRank ans = length $ getForestLabels ans

-- | Return the rank, i.e. the number of different variables that is contained in the @'AnsatzForestEpsilon'@.
ansatzRankEpsilon :: AnsatzForestEpsilon -> Int
ansatzRankEpsilon ans = length $ getForestLabelsEpsilon ans


relabelVar :: (Int -> Int) -> Var -> Var
relabelVar f (Var i j) = Var i (f j)

-- | Shift the variable labels of all variables that are contained in the @'AnsatzForestEta'@ by the amount specified.
relabelAnsatzForest :: Int -> AnsatzForestEta -> AnsatzForestEta
relabelAnsatzForest i ans = mapVars update ans
        where
            vars = getForestLabels ans
            relabMap = I.fromList $ zip vars [i..]
            update = relabelVar ((I.!) relabMap)

-- | Remove the branches with variable label contained in the argument @'Int'@ list from the @'AnsatzForestEta'@.
removeVarsEta :: [Int] -> AnsatzForestEta -> AnsatzForestEta
removeVarsEta vars (Leaf (Var i j))
            | j `elem` vars = EmptyForest
            | otherwise = Leaf (Var i j)
removeVarsEta vars (ForestEta m) = ForestEta $ M.filter (/= EmptyForest) $ M.map (removeVarsEta vars) m
removeVarsEta _ EmptyForest = EmptyForest

-- | Shift the variable labels of all variables that are contained in the @'AnsatzForestEpsilon'@ by the amount specified.
relabelAnsatzForestEpsilon :: Int -> AnsatzForestEpsilon -> AnsatzForestEpsilon
relabelAnsatzForestEpsilon i ans = if ans == M.empty then M.empty else mapVarsEpsilon update ans
        where
            vars = getForestLabelsEpsilon ans
            relabMap = I.fromList $ zip vars [i..]
            update = relabelVar ((I.!) relabMap)

-- | Remove the branches with variable label contained in the argument @'Int'@ list from the @'AnsatzForestEpsilon'@.
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
            newLeafVal = addVars var1 var2
addForests (ForestEta m1) (ForestEta m2)
        | M.null newMap = EmptyForest
        | otherwise = ForestEta newMap
         where
            newMap = M.filter (/= EmptyForest) $ M.unionWith addForests m1 m2
addForests _ _ = error "cannot add Leaf and Forest"

addForestsEpsilon :: AnsatzForestEpsilon -> AnsatzForestEpsilon -> AnsatzForestEpsilon
addForestsEpsilon m1 m2 = M.filter (/= EmptyForest) $ M.unionWith addForests m1 m2

addList2Forest :: AnsatzForestEta -> ([Eta],Var) -> AnsatzForestEta
addList2Forest EmptyForest x = mkForestFromAscList x
addList2Forest (Leaf var1) ([], var2)
        | isZeroVar newLeafVal = EmptyForest
        | otherwise = Leaf newLeafVal
        where
            newLeafVal = addVars var1 var2
addList2Forest (ForestEta m1) (x:xs, var) = ForestEta $ M.insertWith (\_ a2 -> addList2Forest a2 (xs, var)) x newVal m1
         where
            newVal = mkForestFromAscList (xs,var)
addList2Forest (ForestEta f) ([], _) = ForestEta f
addList2Forest (Leaf _) _ = error "cannot add something to Leaf"

addList2ForestEpsilon :: AnsatzForestEpsilon -> (Epsilon,[Eta],Var) -> AnsatzForestEpsilon
addList2ForestEpsilon m (eps,eta,var) = M.insertWith (\_ a2 -> addList2Forest a2 (eta, var)) eps newVal m
     where
        newVal = mkForestFromAscList (eta,var)

--flatten Forest to AscList consisting of the several Branches

-- | Flatten an @'AnsatzForestEta'@ to a list that contains the individual branches.
flattenForest :: AnsatzForestEta -> [([Eta],Var)]
flattenForest EmptyForest = []
flattenForest (Leaf var) = [([],var)]
flattenForest (ForestEta m) = concat l
        where
            mPairs = M.assocs m
            l = fmap (\(k,v) -> map (\(i,j) -> (insert k i, j)) $ flattenForest v) mPairs

-- | Flatten an @'AnsatzForestEpsilon'@ to a list that contains the individual branches.
flattenForestEpsilon :: AnsatzForestEpsilon -> [(Epsilon,[Eta],Var)]
flattenForestEpsilon m = concat l
            where
                mPairs = M.assocs m
                l = fmap (\(k,v) -> map (\(i,j) -> (k, i, j)) $ flattenForest v) mPairs

--draw the forests as ASCII picture

drawEtaTree :: Eta -> AnsatzForestEta -> [String]
drawEtaTree (Eta i j) (Leaf (Var a b)) =  ["(" ++ show i ++  "," ++ show j ++ ") * (" ++ show a ++ ") * x[" ++ show b ++ "]"]
drawEtaTree (Eta i j) (ForestEta m) = lines ("(" ++ show i ++ "," ++ show j ++ ")") ++ drawSubTrees m
        where
            drawSubTrees x
                | x == M.empty = []
                | M.size x == 1 = let [(a,b)] = M.assocs x in  "|" : shift "`---- " "   " (drawEtaTree a b)
                | otherwise =  let  (a,b) = head $ M.assocs x in "|" : shift "+---- " "|  " (drawEtaTree a b) ++ drawSubTrees (M.delete a x)
            shift first other = zipWith (++) (first : repeat other)
drawEtaTree _ EmptyForest = []

drawEpsilonTree :: Epsilon -> AnsatzForestEta -> [String]
drawEpsilonTree (Epsilon i j k l) (Leaf (Var a b)) = ["(" ++ show i ++ "," ++ show j ++ "," ++ show k ++ "," ++ show l ++ ") * (" ++ show a ++ ") * x[" ++ show b ++ "]"]
drawEpsilonTree (Epsilon i j k l) (ForestEta m) = lines ("(" ++ show i ++ "," ++ show j ++ "," ++ show k ++ "," ++ show l ++ ")") ++ drawSubTrees m
        where
            drawSubTrees x
                | x == M.empty = []
                | M.size x == 1 = let [(a,b)] = M.assocs x in  "|" : shift "`---- " "   " (drawEtaTree a b)
                | otherwise =  let  (a,b) = head $ M.assocs x in "|" : shift "+---- " "|  " (drawEtaTree a b) ++ drawSubTrees (M.delete a x)
            shift first other = zipWith (++) (first : repeat other)
drawEpsilonTree _ EmptyForest = []

-- | Returns an ASCII drawing of the @'AnsatzForestEta'@ in the fashion explained in "Data.Tree".
-- The ansatz \( x_1 \cdot 8 \{ \eta^{ac}\eta^{bd}\eta^{pq} - \eta^{ad}\eta^{bc}\eta^{pq} \} + x_2 \cdot 2 \{\eta^{ac}\eta^{bp}\eta^{dq} + \eta^{ac}\eta^{bq}\eta^{dp} - \eta^{bc}\eta^{ap}\eta^{dq} - \eta^{bc}\eta^{aq}\eta^{dp} - \eta^{ad}\eta^{bp}\eta^{cq} - \eta^{ad}\eta^{bq}\eta^{cp} + \eta^{bd}\eta^{ap}\eta^{cq} + \eta^{bd}\eta^{aq}\eta^{cp} \} \) is drawn to
--
-- > (1,3)
-- > |
-- > +---- (2,4)
-- > |  |
-- > |  `---- (5,6) * (8) * x[1]
-- > |
-- > +---- (2,5)
-- > |  |
-- > |  `---- (4,6) * (2) * x[2]
-- > |
-- > `---- (2,6)
-- >    |
-- >    `---- (4,5) * (2) * x[2]
-- >
-- > (1,4)
-- > |
-- > +---- (2,3)
-- > |  |
-- > |  `---- (5,6) * (-8) * x[1]
-- > |
-- > +---- (2,5)
-- > |  |
-- > |  `---- (3,6) * (-2) * x[2]
-- > |
-- > `---- (2,6)
-- >    |
-- >    `---- (3,5) * (-2) * x[2]
-- >
-- > (1,5)
-- > |
-- > +---- (2,3)
-- > |  |
-- > |  `---- (4,6) * (-2) * x[2]
-- > |
-- > `---- (2,4)
-- >    |
-- >    `---- (3,6) * (2) * x[2]
-- >
-- > (1,6)
-- > |
-- > +---- (2,3)
-- > |  |
-- > |  `---- (4,5) * (-2) * x[2]
-- > |
-- > `---- (2,4)
-- >    |
-- >    `---- (3,5) * (2) * x[2]
drawAnsatzEta :: AnsatzForestEta -> String
drawAnsatzEta (Leaf (Var a b)) = show a ++ "x[" ++ show b ++ "]"
drawAnsatzEta (ForestEta m) = unlines $ map (\(x,y) -> unlines $ drawEtaTree x y) $ M.assocs m
drawAnsatzEta EmptyForest = []

-- | Returns an ASCII drawing of the @'AnsatzForestEpsilon'@ in the fashion explained in "Data.Tree".
-- The ansatz \( x_3 \cdot 16 \epsilon^{abcd}\eta^{pq} \) is drawn as:
--
-- > (1,2,3,4)
-- > |
-- > `---- (5,6) * (16) * x[3]
drawAnsatzEpsilon :: AnsatzForestEpsilon -> String
drawAnsatzEpsilon m
        | M.size m == 0 = []
        | otherwise = unlines $ map (\(x,y) -> unlines $ drawEpsilonTree x y) $ M.assocs m

--get one representative for each Var Label

-- | Return one representative, i.e. one individual product for each of the basis ansätze in an @'AnsatzForestEta'@. The function thus returns the contained individual ansätze without
-- their explicit symmetrization.
forestEtaList :: AnsatzForestEta -> [[Eta]]
forestEtaList f = map fst fList''
        where
            fList = flattenForest f
            fList' = sortBy (\(_, Var _ y1 ) (_, Var _ y2) -> compare y1 y2) fList
            fList'' = nubBy (\(_, Var x1 y1 ) (_, Var x2 y2) -> if x1 == 0 || x2 == 0 then error "zeros!!" else y1 == y2) fList'

-- | Return one representative, i.e. one individual product for each of the basis ansätze in an @'AnsatzForestEpsilon'@. The function thus returns the contained individual ansätze without
-- their explicit symmetrization.
forestEpsList :: AnsatzForestEpsilon -> [(Epsilon,[Eta])]
forestEpsList f = map (\(a,b,_) -> (a,b)) fList''
        where
            fList = flattenForestEpsilon f
            fList' = sortBy (\(_, _, Var _ y1 ) (_, _,  Var _ y2) -> compare y1 y2) fList
            fList'' = nubBy (\(_, _, Var x1 y1 ) (_, _, Var x2 y2) -> if x1 == 0 || x2 == 0 then error "zeros!!" else y1 == y2) fList'

--output in latex format

mkEtasLatex :: String -> Eta -> String
mkEtasLatex inds (Eta i j) = "\\eta^{" ++ etaI : etaJ : "}"
        where
            (etaI,etaJ) = (inds !! (i-1), inds !! (j-1)  )

-- | Outputs the @'forestEtaList'@ in \( \LaTeX \) format. The @'String'@ argument is used to label the individual indices.
forestEtaListLatex :: AnsatzForestEta -> String -> Char -> String
forestEtaListLatex f inds var =  tail $ concat etaL''
        where
            etaL = sortBy (\(_, Var _ y1 ) (_, Var _ y2) -> compare y1 y2) $ flattenForest f
            etaL' = nubBy (\(_, Var x1 y1 ) (_, Var x2 y2) -> if x1 == 0 || x2 == 0 then error "zeros!!" else y1 == y2) etaL
            etaL'' = map (\(a,Var _ y) -> "+" ++ var : "_{" ++ show y ++ "}\\cdot" ++ concatMap (mkEtasLatex inds) a) etaL'

mkEpsLatex :: String -> Epsilon -> String
mkEpsLatex inds (Epsilon i j k l) =  "\\epsilon^{" ++ epsi : epsj : epsk : epsl : "}"
        where
            (epsi, epsj, epsk, epsl) = (inds !! (i-1), inds !! (j-1), inds !! (k-1), inds !! (l-1))

-- | Outputs the @'forestEpsList'@ in \( \LaTeX \) format. The @'String'@ argument is used to label the individual indices.
forestEpsListLatex :: AnsatzForestEpsilon -> String -> Char -> String
forestEpsListLatex f inds var = tail $ concat epsL''
        where
            epsL = sortBy (\(_, _, Var _ y1 ) (_, _, Var _ y2) -> compare y1 y2) $ flattenForestEpsilon f
            epsL' = nubBy (\(_, _, Var x1 y1 ) (_, _, Var x2 y2) -> if x1 == 0 || x2 == 0 then error "zeros!!" else y1 == y2) epsL
            epsL'' = map (\(a,b,Var _ y) -> "+" ++ var : "_{" ++ show y ++ "}\\cdot" ++ mkEpsLatex inds a ++ concatMap (mkEtasLatex inds) b) epsL'

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
            swapF = I.fromList $ zip x y ++ zip y x

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
            perms = map (I.fromList . zip inds) $ tail $ permutations inds

cyclicSymForestEps :: [Int] -> AnsatzForestEpsilon -> AnsatzForestEpsilon
cyclicSymForestEps inds ans = foldr (\y x -> addForestsEpsilon x $ swapBlockLabelFEps y ans ) ans perms
        where
            perms = map (I.fromList . zip inds) $ tail $ permutations inds

cyclicBlockSymForestEta :: [[Int]] -> AnsatzForestEta -> AnsatzForestEta
cyclicBlockSymForestEta inds ans = foldr (\y x -> addForests x $ swapBlockLabelFEta y ans ) ans perms
        where
            perms = map (I.fromList . zip (concat inds) . concat) $ tail $ permutations inds

cyclicBlockSymForestEps :: [[Int]] -> AnsatzForestEpsilon-> AnsatzForestEpsilon
cyclicBlockSymForestEps inds ans = foldr (\y x -> addForestsEpsilon x $ swapBlockLabelFEps y ans ) ans perms
        where
            perms = map (I.fromList . zip (concat inds) . concat) $ tail $ permutations inds

--general symmetrizer function

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
mkEtaList x = Eta a b : mkEtaList rest
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
isElem [] (Leaf _) = True
isElem _ EmptyForest = False
isElem (x:xs) (ForestEta m) = case mForest of
                                Just forest -> xs `isElem` forest
                                _           -> False
            where
                mForest = M.lookup x m
isElem _ (Leaf _) = error "cannot lookup eta in Leaf"
isElem [] (ForestEta _) = error "cannot look for eta in forest when no eta is specified"

isElemEpsilon :: (Epsilon, [Eta]) -> AnsatzForestEpsilon -> Bool
isElemEpsilon (eps,l) m = case mForest of
                            Just forest -> l `isElem` forest
                            _           -> False
            where
                mForest = M.lookup eps m

--reduce a list of possible ansätze w.r.t the present symmetries, no numerical evaluation

reduceAnsatzEta' :: Symmetry -> [([Eta],Var)] -> AnsatzForestEta
reduceAnsatzEta' sym = foldl' addOrRem' EmptyForest
        where
            addOrRem' f ans = if isElem (fst ans) f then f else addForests f (symAnsatzForestEta sym $ mkForestFromAscList ans)

reduceAnsatzEpsilon' :: Symmetry -> [(Epsilon, [Eta], Var)] -> AnsatzForestEpsilon
reduceAnsatzEpsilon' sym = foldl' addOrRem' M.empty
        where
            addOrRem' f (x,y,z) = if isElemEpsilon (x,y) f then f else addForestsEpsilon f (symAnsatzForestEps sym $ mkForestFromAscListEpsilon (x,y,z))

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
that occur due to implicit anti-symmetries in 5 or more indices.
--}

--evaluate the nodes, i.e. eta and epsilon

evalNodeEta :: I.IntMap Int -> Eta -> Maybe Int
evalNodeEta iMap (Eta x y)
            | a == b && a == 0 = Just (-1)
            | a == b = Just 1
            | otherwise = Nothing
             where
                [a,b] = [(I.!) iMap x, (I.!) iMap y]

evalNodeEpsilon :: I.IntMap Int -> Epsilon -> Maybe Int
evalNodeEpsilon iMap (Epsilon w x y z) = M.lookup l epsMap
             where
                l = [(I.!) iMap w, (I.!) iMap x, (I.!) iMap y, (I.!) iMap z]

epsMap :: M.Map [Int] Int
epsMap = M.fromList $ map (\x@[i,j,k,l] -> (x, epsSign i j k l)) $ permutations [0,1,2,3]
            where
               epsSign i j k l = (-1) ^ length (filter (==True) [j>i,k>i,l>i,k>j,l>j,l>k])

--basic tree eval function

evalAnsatzForestEta :: I.IntMap Int -> AnsatzForestEta -> I.IntMap Int
evalAnsatzForestEta _ (Leaf (Var x y)) = I.singleton y x
evalAnsatzForestEta evalM (ForestEta m) = M.foldlWithKey' foldF I.empty m
            where
                foldF b k a = let nodeVal = evalNodeEta evalM k
                              in if isNothing nodeVal then b
                                 else I.unionWith (+) (I.map (fromJust nodeVal *) (evalAnsatzForestEta evalM a)) b
evalAnsatzForestEta _ EmptyForest = I.empty

evalAnsatzForestEpsilon :: I.IntMap Int -> AnsatzForestEpsilon -> I.IntMap Int
evalAnsatzForestEpsilon evalM = M.foldlWithKey' foldF I.empty
            where
                foldF b k a = let nodeVal = evalNodeEpsilon evalM k
                              in if isNothing nodeVal then b
                                 else I.unionWith (+) (I.map (fromJust nodeVal *) (evalAnsatzForestEta evalM a)) b

--for a single Ansatz we do not need the IntMap to keep track of the VarLabels -> eval to a number

eval1AnsatzForestEta :: I.IntMap Int -> AnsatzForestEta -> Int
eval1AnsatzForestEta _ (Leaf (Var x _)) = x
eval1AnsatzForestEta evalM (ForestEta m) = M.foldlWithKey' foldF 0 m
            where
                foldF b k a = let nodeVal = evalNodeEta evalM k
                              in if isNothing nodeVal then b
                                 else  b + (fromJust nodeVal * eval1AnsatzForestEta evalM a)
eval1AnsatzForestEta _ EmptyForest = 0

eval1AnsatzForestEpsilon :: I.IntMap Int -> AnsatzForestEpsilon -> Int
eval1AnsatzForestEpsilon evalM = M.foldlWithKey' foldF 0
            where
                foldF b k a = let nodeVal = evalNodeEpsilon evalM k
                              in if isNothing nodeVal then b
                                else  b + (fromJust nodeVal * eval1AnsatzForestEta evalM a)

--eval a given 1Var ansatz to a row vector -> HMatrix Indices start at 0 !!

mkVecList :: (Foldable t, NFData a1, Real a1) =>
             (a2 -> Maybe ((Int, Int), a1)) -> [a2] -> t a3 -> Maybe (HM.Matrix Double)
mkVecList mkAns dofList evalM = if null l
                                then Nothing
                                else Just $ HM.assoc (1,n) 0 $ map (fmap (\x -> fromRational $ toRational x / toRational maxVal)) l
    where
            l' = mapMaybe mkAns dofList
            l = runEval $ parListChunk 500 rdeepseq l'
            lVals = map (\((_,_),z) -> z) l
            maxVal = maximum lVals
            n = length evalM

evalAnsatzEtaVecListIncremental :: [I.IntMap Int] -> AnsatzForestEta -> Maybe (HM.Matrix Double)
evalAnsatzEtaVecListIncremental _ EmptyForest = Nothing
evalAnsatzEtaVecListIncremental evalM f = mkVecList mkAns dofList evalM
        where
            dofList = zip [0..] evalM
            mkAns (i,j) = let ansVal = eval1AnsatzForestEta j f
                          in if ansVal == 0 then Nothing else Just ((0,i), ansVal)

evalAnsatzEpsilonVecListIncremental :: [I.IntMap Int] -> AnsatzForestEpsilon -> Maybe (HM.Matrix Double)
evalAnsatzEpsilonVecListIncremental evalM f  = if f == M.empty then Nothing else mkVecList mkAns dofList evalM
        where
            dofList = zip [0..] evalM
            mkAns (i,j) = let ansVal = eval1AnsatzForestEpsilon j f
                          in if ansVal == 0 then Nothing else Just ((0,i), ansVal)

--eval a given Forest for all inds

type AssocsList a = [([(Int,Int)],a)]

type AssocsListAbs a = [([(Int,Int)],Int,a)]


evalAllEta :: [I.IntMap Int] -> AnsatzForestEta -> [[(Int,Int)]]
evalAllEta [] _ = []
evalAllEta _ EmptyForest = []
evalAllEta evalMs f = l'
            where
                l = map (\x -> filter (\(_,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEta x f) evalMs
                l' = runEval $ parListChunk 500 rdeepseq l

evalAllTensorEta :: (NFData a) => [(I.IntMap Int, a)] -> AnsatzForestEta -> AssocsList a
evalAllTensorEta [] _ = []
evalAllTensorEta _ EmptyForest = []
evalAllTensorEta evalMs f = l'
            where
                l = map (\(x,z) -> (filter (\(_,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEta x f,z)) evalMs
                l' = runEval $ parListChunk 500 rdeepseq l

evalAllEpsilon :: [I.IntMap Int] -> AnsatzForestEpsilon -> [[(Int,Int)]]
evalAllEpsilon [] _ = []
evalAllEpsilon evalMs f = if f == M.empty then [] else l'
            where
                l = map (\x -> filter (\(_,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEpsilon x f) evalMs
                l' = runEval $ parListChunk 500 rdeepseq l

evalAllTensorEpsilon :: (NFData a) => [(I.IntMap Int, a)] -> AnsatzForestEpsilon -> AssocsList a
evalAllTensorEpsilon [] _ = []
evalAllTensorEpsilon evalMs f = if f == M.empty then [] else l'
            where
                l = map (\(x,z) -> ( filter (\(_,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEpsilon x f,z)) evalMs
                l' = runEval $ parListChunk 500 rdeepseq l

evalAllTensorEtaAbs :: (NFData a) => [(I.IntMap Int, Int, a)] -> AnsatzForestEta -> AssocsListAbs a
evalAllTensorEtaAbs [] _ = []
evalAllTensorEtaAbs _ EmptyForest = []
evalAllTensorEtaAbs evalMs f = l'
            where
                l = map (\(x,y,z) -> (filter (\(_,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEta x f, y,z)) evalMs
                l' = runEval $ parListChunk 500 rdeepseq l

evalAllTensorEpsilonAbs :: (NFData a) => [(I.IntMap Int, Int, a)] -> AnsatzForestEpsilon -> AssocsListAbs a
evalAllTensorEpsilonAbs [] _ = []
evalAllTensorEpsilonAbs evalMs f = if f == M.empty then [] else l'
            where
                l = map (\(x,y,z) -> ( filter (\(_,b) -> b /= 0) $ I.assocs $ evalAnsatzForestEpsilon x f, y,z)) evalMs
                l' = runEval $ parListChunk 500 rdeepseq l


{--
Now there are two ways how we can proceed in removing the linear dependencies and thus constructing a basis:

1) the memory optimized way, constructing a lin indep tree from the very beginning
   the first step is to check whether a given Ansatz is element of the span of the previous ansätze and therefore can be discarded

2)  the second way is constructing a given Ansatz by first reducing only algebraically, and later on evaluating the whole forest
    to a matrix and reducing the matrix numerically.

We start with the first way.
--}

type RankDataIncremental = (HM.Matrix Double, HM.Matrix Double)

getVarNrIncremental :: RankDataIncremental -> Int
getVarNrIncremental = HM.rows . snd

--check in each step if the new ansatz vector is linear dependant w.r.t. the ansatz vectors obtained previously

checkNumericLinDepIncremental :: RankDataIncremental -> Maybe (HM.Matrix Double) -> Maybe RankDataIncremental
checkNumericLinDepIncremental (lastMat, lastFullMat) (Just newVec)
            | rk < maxRank = Nothing
            | otherwise = Just (newMat, newAnsatzMat)
             where
                newVecTrans = HM.tr newVec
                scalar = newVec Matrix.<> newVecTrans
                prodBlock = lastFullMat Matrix.<> newVecTrans
                prodBlockTrans = HM.tr prodBlock
                newMat = HM.fromBlocks [[lastMat,        prodBlock],
                                        [prodBlockTrans, scalar   ]]
                rk = Matrix.rank newMat
                maxRank = min (HM.cols newMat) (HM.rows newMat)
                newAnsatzMat = lastFullMat HM.=== newVec
checkNumericLinDepIncremental _ Nothing = Nothing

--in each step add the new AnsatzVector to the forest iff it is lin indep of the previous vectors

{-
alreadyPresentIO n total rDat
    = putStrLn $ progress n total ++ " : " ++ "already present, not added, ansatz rank is " ++ show (getVarNrIncremental rDat)
notPresentNotAddedIO n total rDat
    = putStrLn $ progress n total ++ " : " ++ "not present, linearly dependent, not added, ansatz rank is " ++ show (getVarNrIncremental rDat)
notPresentAddedIO n total rDat
    = putStrLn $ progress n total ++ " : " ++ "not present, linearly independent, added, ansatz rank is " ++ show (getVarNrIncremental rDat)
progress n total
    = show n ++ " of " ++ show total
-}

getNewRDat :: [I.IntMap Int] -> AnsatzForestEta -> RankDataIncremental -> Maybe RankDataIncremental
getNewRDat evalM newAns rDat = newRDat
    where
                newVec = evalAnsatzEtaVecListIncremental evalM newAns
                newRDat = checkNumericLinDepIncremental rDat newVec

getNewRDatEps :: [I.IntMap Int] -> AnsatzForestEpsilon -> RankDataIncremental -> Maybe RankDataIncremental
getNewRDatEps evalM newAns rDat = newRDat
    where
                newVec = evalAnsatzEpsilonVecListIncremental evalM newAns
                newRDat = checkNumericLinDepIncremental rDat newVec

getNewAns :: Symmetry -> [Eta] -> RankDataIncremental -> AnsatzForestEta
getNewAns symList etaList rDat = symAnsatzForestEta symList $ mkForestFromAscList (etaList,Var 1 (getVarNrIncremental rDat + 1))

getNewAnsEps :: Symmetry -> Epsilon -> [Eta] -> RankDataIncremental -> AnsatzForestEpsilon
getNewAnsEps symList epsList etaList rDat = symAnsatzForestEps symList $ mkForestFromAscListEpsilon (epsList,etaList,Var 1 (getVarNrIncremental rDat + 1))

{-
addOrDiscardEtaIncrementalIO :: Symmetry -> Int -> [I.IntMap Int] -> (AnsatzForestEta, RankDataIncremental) -> (Int, [Eta]) -> IO (AnsatzForestEta, RankDataIncremental)
addOrDiscardEtaIncrementalIO symList len evalM (ans,rDat) (num, etaL)
            | isElem etaL ans = do
                                    alreadyPresentIO num len rDat
                                    return (ans,rDat)
            | otherwise = case newRDat of
                               Nothing          -> do
                                                    notPresentNotAddedIO num len rDat
                                                    return (ans,rDat)
                               Just newRDat'    -> do
                                                    notPresentAddedIO num len newRDat'
                                                    return (sumAns,newRDat')
             where
                newAns = getNewAns symList etaL rDat
                newRDat = getNewRDat evalM newAns rDat
                sumAns = addForests ans newAns
-}

addOrDiscardEtaIncremental :: Symmetry -> [I.IntMap Int] -> (AnsatzForestEta, RankDataIncremental) -> [Eta] -> (AnsatzForestEta, RankDataIncremental)
addOrDiscardEtaIncremental symList evalM (ans,rDat) etaL
            | isElem etaL ans = (ans,rDat)
            | otherwise = case newRDat of
                               Nothing          -> (ans,rDat)
                               Just newRDat'    -> (sumAns,newRDat')
             where
                newAns = getNewAns symList etaL rDat
                newRDat = getNewRDat evalM newAns rDat
                sumAns = addForests ans newAns


{-
addOrDiscardEpsilonIncrementalIO :: Symmetry -> Int -> [I.IntMap Int] -> (AnsatzForestEpsilon, RankDataIncremental) -> (Int,(Epsilon,[Eta])) -> IO (AnsatzForestEpsilon, RankDataIncremental)
addOrDiscardEpsilonIncrementalIO symList len evalM (ans,rDat) (num,(epsL,etaL))
            | isElemEpsilon (epsL,etaL) ans = do
                                    alreadyPresentIO num len rDat
                                    return (ans,rDat)
            | otherwise = case newRDat of
                               Nothing          -> do
                                                    notPresentNotAddedIO num len rDat
                                                    let r = getVarNrIncremental rDat

                                                    return (ans,rDat)
                               Just newRDat'    -> do
                                                    notPresentAddedIO num len newRDat'
                                                    return (sumAns,newRDat')
             where
                newAns = getNewAnsEps symList epsL etaL rDat
                newRDat = getNewRDatEps evalM newAns rDat
                sumAns = addForestsEpsilon ans newAns
-}

addOrDiscardEpsilonIncremental :: Symmetry -> [I.IntMap Int] -> (AnsatzForestEpsilon, RankDataIncremental) -> (Epsilon,[Eta]) -> (AnsatzForestEpsilon, RankDataIncremental)
addOrDiscardEpsilonIncremental symList evalM (ans,rDat) (epsL,etaL)
            | isElemEpsilon (epsL,etaL) ans = (ans,rDat)
            | otherwise = case newRDat of
                               Nothing          -> (ans,rDat)
                               Just newRDat'    -> (sumAns,newRDat')
             where
                newAns = getNewAnsEps symList epsL etaL rDat
                newRDat = getNewRDatEps evalM newAns rDat
                sumAns = addForestsEpsilon ans newAns


--construct the RankData from the first nonzero Ansatz

{-
mk1stRankDataEtaIncrementalIO :: Symmetry -> Int -> [(Int,[Eta])] -> [I.IntMap Int] -> IO (AnsatzForestEta,RankDataIncremental,[(Int,[Eta])])
mk1stRankDataEtaIncrementalIO symL numEta etaL evalM =
        do
            putStrLn $ show (fst $ head etaL) ++ " of " ++ show numEta
            let newAns = symAnsatzForestEta symL $ mkForestFromAscList (snd $ head etaL,Var 1 1)
            let newVec = evalAnsatzEtaVecListIncremental evalM newAns
            let restList = tail etaL
            case newVec of
                                Nothing         -> if null restList then return (EmptyForest ,(HM.matrix 0 [], HM.matrix 0 []),[]) else mk1stRankDataEtaIncrementalIO symL numEta restList evalM
                                Just newVec'    -> return (newAns, (newMat, newVec'), restList)
                                    where
                                        newVecTrans = HM.tr newVec'
                                        newMat = newVec' Matrix.<> newVecTrans
-}

mk1stRankDataEtaIncremental :: Symmetry -> [[Eta]] -> [I.IntMap Int] -> (AnsatzForestEta,RankDataIncremental,[[Eta]])
mk1stRankDataEtaIncremental symL etaL evalM = output
        where
            newAns = symAnsatzForestEta symL $ mkForestFromAscList (head etaL,Var 1 1)
            newVec = evalAnsatzEtaVecListIncremental evalM newAns
            restList = tail etaL
            output = case newVec of
                                Nothing         -> if null restList then (EmptyForest,(HM.matrix 0 [], HM.matrix 0 []),[]) else mk1stRankDataEtaIncremental symL restList evalM
                                Just newVec'    -> (newAns, (newMat, newVec'), restList)
                                    where
                                        newVecTrans = HM.tr newVec'
                                        newMat = newVec' Matrix.<> newVecTrans


mk1stRankDataEpsilonIncremental :: Symmetry -> [(Epsilon,[Eta])] -> [I.IntMap Int] -> (AnsatzForestEpsilon,RankDataIncremental,[(Epsilon,[Eta])])
mk1stRankDataEpsilonIncremental symL epsL evalM = output
        where
            newAns = symAnsatzForestEps symL $ mkForestFromAscListEpsilon (fst $ head epsL, snd $ head epsL,Var 1 1)
            newVec = evalAnsatzEpsilonVecListIncremental evalM newAns
            restList = tail epsL
            output = case newVec of
                                Nothing         -> if null restList then (M.empty,(HM.matrix 0 [], HM.matrix 0 []),[]) else mk1stRankDataEpsilonIncremental symL restList evalM
                                Just newVec'    -> (newAns,(newMat, newVec'), restList)
                                    where
                                        newVecTrans = HM.tr newVec'
                                        newMat = newVec' Matrix.<> newVecTrans


--finally reduce the ansatzList (IO versions print the current status for longer computations will follow with the next versions)


reduceAnsatzEtaIncremental :: Symmetry -> [[Eta]] -> [I.IntMap Int] -> (AnsatzForestEta, HM.Matrix Double)
reduceAnsatzEtaIncremental symL etaL evalM
        | null evalM = (EmptyForest, HM.matrix 0 [])
        | null etaL = (EmptyForest, HM.matrix 0 [])
        | otherwise = (finalForest, finalMat)
            where
                (ans1,rDat1,restEtaL) = mk1stRankDataEtaIncremental symL etaL evalM
                (finalForest, (_,finalMat)) = foldl' (addOrDiscardEtaIncremental symL evalM) (ans1,rDat1) restEtaL

reduceAnsatzEpsilonIncremental :: Symmetry -> [(Epsilon,[Eta])] -> [I.IntMap Int] -> (AnsatzForestEpsilon, HM.Matrix Double)
reduceAnsatzEpsilonIncremental symL epsL evalM
    | null evalM = (M.empty, HM.matrix 0 [])
    | null epsL = (M.empty, HM.matrix 0 [])
    | otherwise = (finalForest, finalMat)
        where
            (ans1,rDat1,restEpsL) = mk1stRankDataEpsilonIncremental symL epsL evalM
            (finalForest, (_,finalMat)) = foldl' (addOrDiscardEpsilonIncremental symL evalM) (ans1,rDat1) restEpsL

--construct a basis ansatz forest

getEtaForestIncremental :: Int -> Symmetry -> [I.IntMap Int] -> (AnsatzForestEta, HM.Matrix Double)
getEtaForestIncremental _ _ [] = (EmptyForest, HM.matrix 0 [])
getEtaForestIncremental ord sym evalMs
    | null allEtaLists = (EmptyForest, HM.matrix 0 [])
    | otherwise = reduceAnsatzEtaIncremental sym allEtaLists evalMs
        where
            allInds = getEtaInds [1..ord] sym
            allEtaLists = map mkEtaList allInds

getEpsForestIncremental :: Int -> Symmetry -> [I.IntMap Int] -> (AnsatzForestEpsilon, HM.Matrix Double)
getEpsForestIncremental _ _ [] = (M.empty, HM.matrix 0 [])
getEpsForestIncremental ord sym evalMs
    | null allEpsLists = (M.empty, HM.matrix 0 [])
    | otherwise =  reduceAnsatzEpsilonIncremental sym allEpsLists evalMs
        where
            allInds = getEpsilonInds [1..ord] sym
            allEpsLists = map mkEpsilonList allInds

--eta and eps forest combined

getFullForestIncremental :: Int -> Symmetry -> [I.IntMap Int] -> [I.IntMap Int] -> (AnsatzForestEta, AnsatzForestEpsilon, HM.Matrix Double, HM.Matrix Double)
getFullForestIncremental ord sym evalMEta evalMEps = (etaAns, epsAns, etaMat, epsMat)
        where
            (etaAns,etaMat) = getEtaForestIncremental ord sym evalMEta
            (epsAns',epsMat) = getEpsForestIncremental ord sym evalMEps
            epsAns = relabelAnsatzForestEpsilon (1 + length (getForestLabels etaAns)) epsAns'

{--
Finally we can evaluated the ansatz trees to a contravariant tensor with spacetime indices
Sym version outputs the fully symmetrized ansatz tensor, this is however expensive, non Sym version computes the non symmetrized ansatz
tensor, i.e. only 1 representative out of each symmetry equivalence class is non zero. It is important to note that when contracting the non symmetrized
tensor with another tensor with given symmetry one needs to account for the now missing multiplicities from the symmetries as in the construction of ansätze
we used factor less symmetrizer functions.
--}

evalToTensSym :: Symmetry -> [(I.IntMap Int, IndTupleST n1 0)] -> [(I.IntMap Int, IndTupleST n1 0)] -> AnsatzForestEta -> AnsatzForestEpsilon -> STTens n1 0 AnsVarR
evalToTensSym (p,ap,b,c,bc) evalEta evalEps ansEta ansEps = symT
            where
                p' = map (\(x,y) -> (x-1,y-1)) p
                ap' = map (\(x,y) -> (x-1,y-1)) ap
                b' = map (\(x,y) -> (map (\z -> z-1) x, map (\z' -> z'-1) y) ) b
                c' = map (map (subtract 1)) c
                bc' = map (map (map (subtract 1))) bc
                tens = evalToTens evalEta evalEps ansEta ansEps
                symT = foldr cyclicBlockSymATens1 (
                            foldr cyclicSymATens1 (
                                foldr symBlockATens1 (
                                    foldr aSymATens1 (
                                        foldr symATens1 tens p'
                                        ) ap'
                                    ) b'
                                ) c'
                            ) bc'

evalToTens :: [(I.IntMap Int, IndTupleST n1 0)] -> [(I.IntMap Int, IndTupleST n1 0)] -> AnsatzForestEta -> AnsatzForestEpsilon -> STTens n1 0 AnsVarR
evalToTens evalEta evalEps ansEta ansEps = tens
            where
                etaL = evalAllTensorEta evalEta ansEta
                epsL = evalAllTensorEpsilon evalEps ansEps
                etaL' = map (\(x,indTuple) -> (indTuple, AnsVar $ I.fromList $ map (\(i,r) -> (i,SField $ fromIntegral r)) x)) etaL
                epsL' = map (\(x,indTuple) -> (indTuple, AnsVar $ I.fromList $ map (\(i,r) -> (i,SField $ fromIntegral r)) x)) epsL
                etaRmL = filter (\(_,AnsVar b) -> not $ I.null b) etaL'
                epsRmL = filter (\(_,AnsVar b) -> not $ I.null b) epsL'
                tens = fromListT2 etaRmL &+ fromListT2 epsRmL

--eval to abstract tensor type taking into account possible block symmetries and multiplicity of the ansätze

evalToTensAbs :: [(I.IntMap Int, Int, [IndTupleAbs n1 0 n2 0 n3 0])] -> [(I.IntMap Int, Int, [IndTupleAbs n1 0 n2 0 n3 0])] -> AnsatzForestEta -> AnsatzForestEpsilon -> ATens n1 0 n2 0 n3 0 AnsVarR
evalToTensAbs evalEta evalEps ansEta ansEps = fromListT6 etaRmL &+ fromListT6 epsRmL
            where
                etaL = evalAllTensorEtaAbs evalEta ansEta
                epsL = evalAllTensorEpsilonAbs evalEps ansEps
                etaL' = map (\(x,mult,indTuple) -> (indTuple, AnsVar $ I.fromList $ map (\(i,r) -> (i,fromIntegral $ r*mult)) x)) etaL
                epsL' = map (\(x,mult,indTuple) -> (indTuple, AnsVar $ I.fromList $ map (\(i,r) -> (i,fromIntegral $ r*mult)) x)) epsL
                etaRmL = filter (\(_,AnsVar b) -> not $ I.null b) $ concatMap (\(x,y) -> zip x (repeat y)) etaL'
                epsRmL = filter (\(_,AnsVar b) -> not $ I.null b) $ concatMap (\(x,y) -> zip x (repeat y)) epsL'

--the 2 final functions, constructing the 2 AnsatzForests and the AnsatzTensor (currently the list of symmetry DOFs must be specified by hand -> this can also yield a performance advantage)

mkEvalMap :: Int -> [Int] -> I.IntMap Int
mkEvalMap i = I.fromList . zip [1..i]

mkEvalMaps :: [[Int]] -> [I.IntMap Int]
mkEvalMaps l = let s = length (head l) in map (mkEvalMap s) l

mkEvalMapsInds :: forall (n :: Nat). KnownNat n => [[Int]] -> [(I.IntMap Int, IndTupleST n 0)]
mkEvalMapsInds l = let s = length (head l) in map (\x -> (mkEvalMap s x, (fromListUnsafe $ map toEnum x, Empty))) l

mkAllEvalMaps :: forall (n :: Nat). KnownNat n => Symmetry -> [[Int]] -> ([I.IntMap Int], [I.IntMap Int], [(I.IntMap Int, IndTupleST n 0)], [(I.IntMap Int, IndTupleST n 0)])
mkAllEvalMaps sym l = (evalMEtaRed, evalMEpsRed, evalMEtaInds, evalMEpsInds)
        where
            evalLEta = filter isEtaList l
            evalLEps = filter isEpsilonList l
            evalLEtaRed = filter (isLorentzEval sym) evalLEta
            evalLEpsRed = filter (isLorentzEval sym) evalLEps
            evalMEtaRed = mkEvalMaps evalLEtaRed
            evalMEpsRed = mkEvalMaps evalLEpsRed
            evalMEtaInds = mkEvalMapsInds evalLEta
            evalMEpsInds = mkEvalMapsInds evalLEps


mkAllEvalMapsAbs :: Symmetry -> [([Int], Int, [IndTupleAbs n1 0 n2 0 n3 0])] -> ([I.IntMap Int], [I.IntMap Int], [(I.IntMap Int, Int, [IndTupleAbs n1 0 n2 0 n3 0])], [(I.IntMap Int, Int, [IndTupleAbs n1 0 n2 0 n3 0])])
mkAllEvalMapsAbs sym l = (evalMEtaRed, evalMEpsRed, evalMEtaInds, evalMEpsInds)
        where
            (headList,_,_) = head l
            ord = length headList
            evalLEta = filter (\(x,_,_) -> isEtaList x) l
            evalLEps = filter (\(x,_,_) -> isEpsilonList x) l
            evalLEtaRed = map (\(a,_,_) -> a) $ filter (\(x,_,_) -> isLorentzEval sym x) evalLEta
            evalLEpsRed = map (\(a,_,_) -> a) $ filter (\(x,_,_) -> isLorentzEval sym x) evalLEps
            evalMEtaRed = mkEvalMaps evalLEtaRed
            evalMEpsRed = mkEvalMaps evalLEpsRed
            evalMEtaInds = map (\(x,y,z) -> (mkEvalMap ord x, y, z)) evalLEta
            evalMEpsInds = map (\(x,y,z) -> (mkEvalMap ord x, y, z)) evalLEps

-- | The function is similar to @'mkAnsatzTensorFastSym'@ yet it uses an algorithm that prioritizes memory usage over fast computation times.
mkAnsatzTensorIncrementalSym :: forall (n :: Nat). KnownNat n => Int -> Symmetry -> [[Int]] -> (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 AnsVarR)
mkAnsatzTensorIncrementalSym ord symmetries evalL = (ansEta, ansEps, tens)
        where
            (evalMEtaRed, evalMEpsRed, evalMEtaInds, evalMEpsInds) = mkAllEvalMaps symmetries evalL
            (ansEta, ansEps, _, _) = getFullForestIncremental ord symmetries evalMEtaRed evalMEpsRed
            tens = evalToTensSym symmetries evalMEtaInds evalMEpsInds ansEta ansEps

-- | The function is similar to @'mkAnsatzTensorFast'@ yet it uses an algorithm that prioritizes memory usage over fast computation times.
mkAnsatzTensorIncremental :: forall (n :: Nat). KnownNat n => Int -> Symmetry -> [[Int]] -> (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 AnsVarR)
mkAnsatzTensorIncremental ord symmetries evalL = (ansEta, ansEps, tens)
        where
            (evalMEtaRed, evalMEpsRed, evalMEtaInds, evalMEpsInds) = mkAllEvalMaps symmetries evalL
            (ansEta, ansEps, _, _) = getFullForestIncremental ord symmetries evalMEtaRed evalMEpsRed
            tens = evalToTens evalMEtaInds evalMEpsInds ansEta ansEps

-- | The function is similar to @'mkAnsatzTensorFastAbs'@ yet it uses an algorithm that prioritizes memory usage over fast computation times.
mkAnsatzTensorIncrementalAbs :: Int -> Symmetry -> [([Int], Int, [IndTupleAbs n1 0 n2 0 n3 0])] -> (AnsatzForestEta, AnsatzForestEpsilon, ATens n1 0 n2 0 n3 0 AnsVarR)
mkAnsatzTensorIncrementalAbs ord symmetries evalL = (ansEta, ansEps, tens)
        where
            (evalMEtaRed, evalMEpsRed, evalMEtaInds, evalMEpsInds) = mkAllEvalMapsAbs symmetries evalL
            (ansEta, ansEps, _, _) = getFullForestIncremental ord symmetries evalMEtaRed evalMEpsRed
            tens = evalToTensAbs evalMEtaInds evalMEpsInds ansEta ansEps


--now we start with the second way

assocsToMat :: [[(Int,Int)]] -> HM.Matrix Double
assocsToMat l = HM.assoc (m,n) 0 l'
    where
        l' = concat $ zipWith (\r z -> map (\(x,y) -> ((z-1, x-1), fromIntegral y)) r) l [1..]
        sparse = M.fromList l'
        m = maximum (map (\((x,_),_) -> x) l') + 1
        n = maximum (map (\((_,x),_) -> x) l') + 1

--filter the lin. dependant vars from the Assocs List

getPivots :: [[(Int,Int)]]  -> [Int]
getPivots matList = map (1+) pivots
        where
            mat       = assocsToMat matList
            pivots    = independentColumns mat

--reduce linear deps in the ansätze

reduceLinDepsFastEta :: [I.IntMap Int] -> AnsatzForestEta -> AnsatzForestEta
reduceLinDepsFastEta evalM ansEta = newEtaAns
        where
            etaL = evalAllEta evalM ansEta
            etaVars = getPivots etaL
            allEtaVars = getForestLabels ansEta
            remVarsEta =  allEtaVars \\ etaVars
            newEtaAns = relabelAnsatzForest 1 $ removeVarsEta remVarsEta ansEta

reduceLinDepsFastEps :: [I.IntMap Int] -> AnsatzForestEpsilon -> AnsatzForestEpsilon
reduceLinDepsFastEps evalM ansEps = newEpsAns
        where
            epsL = evalAllEpsilon evalM ansEps
            epsVars = getPivots epsL
            allEpsVars = getForestLabelsEpsilon ansEps
            remVarsEps =  allEpsVars \\ epsVars
            newEpsAns = relabelAnsatzForestEpsilon 1 $ removeVarsEps remVarsEps ansEps

--final function, fast way of constructing the ansatz trees and the 2 tensors (again the list of symmetry DOFs bust be specified but this can yield a performance advantage)

mkAnsatzFast :: Int -> Symmetry -> [I.IntMap Int] -> [I.IntMap Int] -> (AnsatzForestEta, AnsatzForestEpsilon)
mkAnsatzFast ord symmetries evalMEtaRed evalMEpsRed = (ansEtaRed, ansEpsRed)
        where
            ansEta = getEtaForestFast ord symmetries
            ansEpsilon = getEpsForestFast ord symmetries
            ansEtaRed = reduceLinDepsFastEta evalMEtaRed ansEta
            ansEpsRed' = reduceLinDepsFastEps evalMEpsRed ansEpsilon
            ansEpsRed = relabelAnsatzForestEpsilon (1 + length (getForestLabels ansEtaRed)) ansEpsRed'

-- | The function computes all linear independent ansätze that have rank specified by the first integer argument and further satisfy the symmetry specified by the @'Symmetry'@ value.
-- The additional argument of type @[['Int']]@ is used to provide the information of all (by means of the symmetry at hand) independent components of the ansätze.
-- Explicit examples how this information can be computed are provided by the functions for @'areaList4'@, ... and also by @'metricList2'@, ... .
-- The output is given as spacetime tensor @'STTens'@ and is explicitly symmetrized.
mkAnsatzTensorFastSym :: forall (n :: Nat). KnownNat n => Int -> Symmetry -> [[Int]]-> (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 AnsVarR)
mkAnsatzTensorFastSym ord symmetries evalL = (ansEta, ansEps, tens)
        where
            (evalMEtaRed, evalMEpsRed, evalMEtaInds, evalMEpsInds) = mkAllEvalMaps symmetries evalL
            (ansEta, ansEps) = mkAnsatzFast ord symmetries evalMEtaRed evalMEpsRed
            tens = evalToTensSym symmetries evalMEtaInds evalMEpsInds ansEta ansEps

--and without explicit symmetrization in tens

-- | This function provides the same functionality as @'mkAnsatzTensorFast'@ but without explicit symmetrization of the result. In other words from each symmetrization sum only the first
-- summand is returned. This is advantageous as for large expressions explicit symmetrization might be expensive and further is sometime simply not needed as the result might for instance be contracted against
-- a symmetric object, which thus enforces the symmetry, in further steps of the computation.
mkAnsatzTensorFast :: forall (n :: Nat). KnownNat n => Int -> Symmetry -> [[Int]]-> (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 AnsVarR)
mkAnsatzTensorFast ord symmetries evalL = (ansEta, ansEps, tens)
        where
            (evalMEtaRed, evalMEpsRed, evalMEtaInds, evalMEpsInds) = mkAllEvalMaps symmetries evalL
            (ansEta, ansEps) = mkAnsatzFast ord symmetries evalMEtaRed evalMEpsRed
            tens = evalToTens evalMEtaInds evalMEpsInds ansEta ansEps

--eval to abstract tensor

-- | This function provides the same functionality as @'mkAnsatzTensorFast'@ but returns the result as tensor of type @'ATens' 'AnsVarR'@. This is achieved by explicitly providing not only
-- the list of individual index combinations but also their representation using more abstract index types as input. The input list consists of triplets where the first element
-- as before labels the independent index combinations, the second element labels the corresponding multiplicity under the present symmetry. The multiplicity simply encodes how many different combinations of spacetime indices
-- correspond to the same abstract index tuple. The last element of the input triplets labels the individual abstract index combinations that then correspond to the provided spacetime indices. If some of the initial symmetries
-- are still present when using abstract indices this last element might consists of more then one index combination. The appropriate value that is retrieved from the two ansatz forests is then written to each of the provided index combinations.
mkAnsatzTensorFastAbs :: Int -> Symmetry -> [([Int], Int, [IndTupleAbs n1 0 n2 0 n3 0])] -> (AnsatzForestEta, AnsatzForestEpsilon, ATens n1 0 n2 0 n3 0 AnsVarR)
mkAnsatzTensorFastAbs ord symmetries evalL = (ansEta, ansEps, tens)
        where
            (evalMEtaRed, evalMEpsRed, evalMEtaInds, evalMEpsInds) = mkAllEvalMapsAbs symmetries evalL
            (ansEta, ansEps) = mkAnsatzFast ord symmetries evalMEtaRed evalMEpsRed
            tens = evalToTensAbs evalMEtaInds evalMEpsInds ansEta ansEps


{--
The last step consists of computing the evaluation list from the present symmetries. To that end it is important to note
that for epsilon tensors only index combinations that contain each value 0,...,3 an odd number of times and for eta tensors we need an even number.
Further note that due to the Lorentz invariance of such expressions when computing linear dependencies we are free to relabel the coordinate axis,
i.e. interchange for instance 1 and 0 as this is precisely the effect of a Lorentz transformation (at least up to a sign).
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

getPairs :: [a] -> [(a, a)]
getPairs [a,b] = [(a,b)]
getPairs (x:xs) = (x, head xs) : getPairs xs
getPairs _ = error "invalid index combination"

filterCSym :: [Int] -> [Int] -> Bool
filterCSym inds i =  and boolL
        where
            pairL =  getPairs i
            boolL = map (filterPSym inds) pairL

filterBSym :: [Int] -> ([Int],[Int]) -> Bool
filterBSym _ ([],[]) = True
filterBSym inds (x:xs,y:ys)
            | xVal < yVal = True
            | xVal == yVal = filterBSym inds (xs,ys)
            | otherwise = False
             where
                xVal = inds !! (x-1)
                yVal = inds !! (y-1)
filterBSym _ _ = error "cannot non-empty list w.r.t. empty symmetries"

filterBCSym :: [Int] -> [[Int]] -> Bool
filterBCSym inds i =  and boolL
        where
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
            allSwaps = map ((\x y -> I.fromList $ zip x y) inds) $ permutations [0..n-1]

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
                swapBlocks (m1,m2) x = let m = I.fromList $ zip m1 m2 ++ zip m2 m1
                                     in  fromMaybe x $ I.lookup x m
                newMap = canonicalizeBlockPair (is,js) iMap
canonicalizeBlockPair _ _ = error "invalid index combination"


canonicalizeIntMap :: Symmetry -> I.IntMap Int -> I.IntMap Int
canonicalizeIntMap (p,ap,b,c,bc) iMap = iMap2
        where
            allBlocks = b ++ concatMap mkBlocksFromBlockCycle bc
            allPairs = p ++ ap ++ concatMap mkSymsFromCycle c
            iMap1 = foldr canonicalizePair iMap allPairs
            iMap2 = foldr canonicalizeBlockPair iMap1 allBlocks

canonicalizeList :: Symmetry -> [Int] -> [Int]
canonicalizeList sym inds = I.elems $ canonicalizeIntMap sym $ I.fromList $ zip [1..] inds

allList' :: Int -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> [[Int]]
allList' 1 _ _ symBounds aSymBounds = case (symB, aSymB) of
                                      (Just j, Nothing) -> [[k] | k <- [j..3]]
                                      (Nothing, Just j) -> [[k] | k <- [j+1..3]]
                                      (Nothing, Nothing) -> [[0], [1], [2], [3]]
                                      (Just j, Just k) -> [[k'] | k' <- [max j (k+1) .. 3]]
            where
                (symB,aSymB) = (lookup 1 symBounds, lookup 1 aSymBounds)
allList' i syms aSyms symBounds aSymBounds = concatMap (\x -> (:) <$> [x] <*> allList' (i-1) newSyms newASyms (newSymBounds x) (newASymBounds x)) l
            where
                (symB,aSymB) = (lookup 1 symBounds, lookup 1 aSymBounds)
                l' = case (symB, aSymB) of
                    (Just j, Nothing) -> [j..3]
                    (Nothing, Just j) ->  [j+1..3]
                    (Nothing, Nothing) -> [0..3]
                    (Just j, Just k) -> [max j (k+1) .. 3]
                l = if isJust newASymB then filter (<3) l' else l'
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

--use the above functions to construct ansätze without providing eval lists by hand

-- | The function is similar to @'mkAnsatzTensorFastSym''@ yet it uses an algorithm that prioritizes memory usage over fast computation times.
mkAnsatzTensorIncrementalSym' :: forall (n :: Nat). KnownNat n =>  Int -> Symmetry -> (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 AnsVarR)
mkAnsatzTensorIncrementalSym' ord symmetries = mkAnsatzTensorIncrementalSym ord symmetries evalL
        where
            evalL = filter (`filterAllSym` symmetries) $ allList ord symmetries

-- | Provides the same functionality as @'mkAnsatzTensorFastSym'@ with the difference that the list of independent index combinations is automatically computed form the present symmetry.
-- Note that this yields slightly higher computation costs.
mkAnsatzTensorFastSym' :: forall (n :: Nat). KnownNat n => Int -> Symmetry -> (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 AnsVarR)
mkAnsatzTensorFastSym' ord symmetries = mkAnsatzTensorFastSym ord symmetries evalL
        where
            evalL = filter (`filterAllSym` symmetries) $ allList ord symmetries

--and without explicit symmetrization

-- | The function is similar to @'mkAnsatzTensorFast''@ yet it uses an algorithm that prioritizes memory usage over fast computation times.
mkAnsatzTensorIncremental' :: forall (n :: Nat). KnownNat n =>  Int -> Symmetry -> (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 AnsVarR)
mkAnsatzTensorIncremental' ord symmetries = mkAnsatzTensorIncremental ord symmetries evalL
        where
            evalL = filter (`filterAllSym` symmetries) $ allList ord symmetries

-- | Provides the same functionality as @'mkAnsatzTensorFast'@ with the difference that the list of independent index combinations is automatically computed form the present symmetry.
-- Note that this yields slightly higher computation costs.
mkAnsatzTensorFast' :: forall (n :: Nat). KnownNat n => Int -> Symmetry -> (AnsatzForestEta, AnsatzForestEpsilon, STTens n 0 AnsVarR)
mkAnsatzTensorFast' ord symmetries = mkAnsatzTensorFast ord symmetries evalL
        where
            evalL = filter (`filterAllSym` symmetries) $ allList ord symmetries

--abstract tensor evaluation lists

--finally the lists for the evaluation

--triangle maps converting from abstract indices to spacetime indices

trianMapArea :: I.IntMap [Int]
trianMapArea = I.fromList $ zip [1..21] list
        where
            list = [ [a,b,c,d] | a <- [0..2], b <- [a+1..3], c <- [a..2], d <- [c+1..3], isAreaSorted a b c d]

trianMap2 :: I.IntMap [Int]
trianMap2 = I.fromList $ zip [1..10] list
        where
            list = [ [p,q] | p <- [0..3], q <- [p..3]]

isAreaSorted :: Int -> Int -> Int -> Int -> Bool
isAreaSorted a b c d
         | a < c || (a == c && b <= d) = True
         | otherwise = False

--computing the multiplicities that result from the use of the area metric inter twiner

areaMult :: [Int] -> Int
areaMult [a,b,c,d]
         | a == c && b == d = 4
         | otherwise = 8
areaMult _ = error "expected four indices"

iMult2 :: [Int] -> Int
iMult2 [p,q] = if p == q then 1 else 2
iMult2 _ = error "expected two indices"

--Area metric eval lists

-- | Evaluation list for \(a^A \).
areaList4 :: [([Int], Int, [IndTupleAbs 1 0 0 0 0 0])]
areaList4 = list
      where
          trianArea = trianMapArea
          list = [ let a' = (I.!) trianArea a in (a', areaMult a', [(singletonInd (Ind20 $ a-1), Empty, Empty, Empty, Empty, Empty)]) | a <- [1..21] ]

-- | Evaluation list for \(a^{AI} \).
areaList6 :: [([Int], Int, [IndTupleAbs 1 0 1 0 0 0])]
areaList6 = list
      where
          trian2 = trianMap2
          trianArea = trianMapArea
          list = [ let (a',i') = ((I.!) trianArea a, (I.!) trian2 i) in  (a' ++ i', areaMult a' * iMult2 i', [(singletonInd (Ind20 $ a-1), Empty, singletonInd (Ind9 $ i-1), Empty, Empty, Empty)]) | a <- [1..21], i <- [1..10]]

-- | Evaluation list for \(a^{A B}\). Note that also when using the abstract indices this ansatz still features the \( A \leftrightarrow B \) symmetry.
areaList8 :: [([Int], Int, [IndTupleAbs 2 0 0 0 0 0])]
areaList8 = list
      where
          trianArea = trianMapArea
          list = [ let (a',b') = ((I.!) trianArea a, (I.!) trianArea b) in  (a' ++ b', areaMult a' * areaMult b', map (\[_a,_b] -> (Append (Ind20 $ _a-1) $ singletonInd (Ind20 $ _b-1), Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b] )  | a <- [1..21], b <- [a..21]]

-- | Evaluation list for \(a^{Ap Bq}\). Note that also when using the abstract indices this ansatz still features the \( (Ap) \leftrightarrow (Bq) \) symmetry.
areaList10_1 :: [([Int], Int, [IndTupleAbs 2 0 0 0 2 0])]
areaList10_1 = list
      where
          trianArea = trianMapArea
          list = [ let (a',b') = ((I.!) trianArea a, (I.!) trianArea b) in  (a' ++ p : b' ++ [q], areaMult a' * areaMult b', map (\[[_a,_p],[_b,_q]] -> (Append (Ind20 $ _a-1) $ singletonInd (Ind20 $ _b-1), Empty, Empty, Empty, Append (Ind3 _p) $ singletonInd (Ind3 _q), Empty)) $ nub $ permutations [[a,p],[b,q]]) | a <- [1..21], b <- [a..21], p <- [0..3], q <- [0..3],  not (a==b && p>q)]

-- | Evaluation list for \(a^{ABI} \).
areaList10_2 :: [([Int], Int, [IndTupleAbs 2 0 1 0 0 0])]
areaList10_2 = list
      where
          trian2 = trianMap2
          trianArea = trianMapArea
          list = [ let (a',b',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trian2 i) in  (a' ++ b' ++ i', areaMult a' * areaMult b' * iMult2 i', [ (Append (Ind20 $ a-1) $ singletonInd (Ind20 $ b-1), Empty, singletonInd (Ind9 $ i-1), Empty, Empty, Empty)] ) | a <- [1..21], b <- [1..21], i <- [1..10] ]

-- | Evaluation list for \(a^{ABC} \).  Note that also when using the abstract indices this ansatz still features the symmetry under arbitrary permutations of \( ABC\).
areaList12 ::  [([Int], Int, [IndTupleAbs 3 0 0 0 0 0])]
areaList12 = list
      where
          trianArea = trianMapArea
          list = [ let (a',b',c') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c) in  (a' ++ b' ++ c', areaMult a' * areaMult b' * areaMult c', map (\[_a,_b,_c] -> (Append (Ind20 $ _a-1) $ Append (Ind20 $ _b-1) $ singletonInd (Ind20 $ _c-1), Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b,c] )| a <- [1..21], b <- [a..21], c <- [b..21] ]

--AI:BJ
areaList12_1 ::  [([Int], Int, [IndTupleAbs 2 0 2 0 0 0])]
areaList12_1 = list
      where
          trian2 = trianMap2
          trianArea = trianMapArea
          list = [ let (a',i',b',j') = ((I.!) trianArea a, (I.!) trian2 i, (I.!) trianArea b, (I.!) trian2 j) in  (a' ++ i' ++ b' ++ j' , areaMult a' * areaMult b' * iMult2 i' * iMult2 j', map (\[[_a,_i],[_b,_j]] ->  (Append (Ind20 $ _a-1) $ singletonInd (Ind20 $ _b-1), Empty, Append (Ind9 $ _i-1) $ singletonInd (Ind9 $ _j-1), Empty, Empty, Empty)) $ nub $ permutations [[a,i],[b,j]] ) | a <- [1..21], b <- [a..21], i <- [1..10], j <- [1..10], not (a==b && i>j) ]

-- | Evaluation list for \(a^{ABp Cq}\). Note that also when using the abstract indices this ansatz still features the \( (Bp) \leftrightarrow (Cq) \) symmetry.
areaList14_1 :: [([Int], Int, [IndTupleAbs 3 0 0 0 2 0])]
areaList14_1 = list
      where
          trianArea = trianMapArea
          list = [ let (a',b',c') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c) in  (a' ++ b' ++ p : c' ++ [q], areaMult a' * areaMult b' * areaMult c', map (\[[_b,_p],[_c,_q]] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ _b-1) $ singletonInd (Ind20 $ _c-1), Empty, Empty, Empty, Append (Ind3 _p) $ singletonInd (Ind3 _q), Empty)) $ nub $ permutations [[b,p],[c,q]]) | a <- [1..21], b <- [1..21], c <- [b..21], p <- [0..3], q <- [0..3], not (b==c && p>q) ]

-- | Evaluation list for \(a^{A B C I}\). Note that also when using the abstract indices this ansatz still features the \( (A) \leftrightarrow (B) \) symmetry.
areaList14_2 :: [([Int], Int, [IndTupleAbs 3 0 1 0 0 0])]
areaList14_2 = list
      where
          trian2 = trianMap2
          trianArea = trianMapArea
          list = [ let (a',b',c',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i) in ( a' ++ b' ++ c' ++ i', areaMult a' * areaMult b' * areaMult c' * iMult2 i', map (\[_a,_b] -> (Append (Ind20 $ _a-1) $ Append (Ind20 $ _b-1) $ singletonInd (Ind20 $ c-1), Empty, singletonInd (Ind9 $ i-1), Empty, Empty, Empty)) $ nub $ permutations [a,b] ) | a <- [1..21], b <- [a..21], c <- [1..21], i <- [1..10] ]

--Ap:Bq:CI
areaList16_1 :: [([Int], Int, [IndTupleAbs 3 0 1 0 2 0])]
areaList16_1 = list
      where
          trian2 = trianMap2
          trianArea = trianMapArea
          list = [ let (a',b',c',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i) in (a' ++ p : b' ++ q : c' ++ i' , areaMult a' * areaMult b' * areaMult c' * iMult2 i', map (\[[_a,_p],[_b,_q]] -> (Append (Ind20 $ _a-1) $ Append (Ind20 $ _b-1) $ singletonInd (Ind20 $ c-1), Empty, singletonInd (Ind9 $ i-1), Empty, Append (Ind3 _p) $ singletonInd (Ind3 _q), Empty)) $ nub $ permutations [[a,p],[b,q]]) | a <- [1..21], b <- [a..21], c <- [1..21], i <- [1..10], p <- [0..3], q <- [0..3], not (a==b && p>q) ]

--A:BI:CJ
areaList16_2 :: [([Int], Int, [IndTupleAbs 3 0 2 0 0 0])]
areaList16_2 = list
      where
          trian2 = trianMap2
          trianArea = trianMapArea
          list = [let (a',b',c',i', j') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i, (I.!) trian2 j) in  (a' ++ b' ++ i' ++ c' ++ j', areaMult a' * areaMult b' * areaMult c' * iMult2 i' * iMult2 j', map (\[[_b,_i],[_c,_j]] -> (Append (Ind20 $ a-1) $ Append (Ind20 $ _b-1) $ singletonInd (Ind20 $ _c-1), Empty, Append (Ind9 $ _i-1) $ singletonInd (Ind9 $ _j-1), Empty, Empty, Empty) ) $ nub $ permutations [[b,i],[c,j]])| a <- [1..21], b <- [1..21], c <- [b..21], i <- [1..10], j <- [1..10], not (b==c && i>j)]

--AI:BJ:CK
areaList18 :: [([Int], Int, [IndTupleAbs 3 0 3 0 0 0])]
areaList18 = list
      where
          trian2 = trianMap2
          trianArea = trianMapArea
          list = [ let (a',b',c',i', j', k') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trian2 i, (I.!) trian2 j, (I.!) trian2 k) in  (a' ++ i' ++ b' ++ j' ++ c' ++ k', areaMult a' * areaMult b' * areaMult c' * iMult2 i' * iMult2 j' * iMult2 k', map (\[[_a,_i],[_b,_j],[_c,_k]] -> (Append (Ind20 $ _a-1) $ Append (Ind20 $ _b-1) $ singletonInd (Ind20 $ _c-1), Empty, Append (Ind9 $ _i-1) $ Append (Ind9 $ _j-1) $ singletonInd (Ind9 $ _k-1), Empty, Empty, Empty) ) $ nub $ permutations [[a,i],[b,j],[c,k]]) | a <- [1..21], b <- [a..21], c <- [b..21], i <- [1..10], j <- [1..10], not (a==b && i>j), k <- [1..10], not (b==c && j>k) ]

--order 4

--A:B:C_D
areaList16 ::  [([Int], Int, [IndTupleAbs 4 0 0 0 0 0])]
areaList16 = list
      where
          trianArea = trianMapArea
          list = [ let (a',b',c', d') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d) in  (a' ++ b' ++ c' ++ d', areaMult a' * areaMult b' * areaMult c' * areaMult d', map (\[_a,_b,_c,_d] -> (Append (Ind20 $ _a-1) $ Append (Ind20 $ _b-1) $ Append (Ind20 $ _c-1) $ singletonInd (Ind20 $ _d-1), Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b,c,d] )| a <- [1..21], b <- [a..21], c <- [b..21], d <- [c..21] ]

--A:B:C:DI
areaList18_2 ::  [( [Int], Int, [IndTupleAbs 4 0 1 0 0 0])]
areaList18_2 = list
      where
          trian2 = trianMap2
          trianArea = trianMapArea
          list = [ let (a',b',c',d',i') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d, (I.!) trian2 i) in  (a' ++ b' ++ c'++d'++i', areaMult a' * areaMult b' * areaMult c' * areaMult d' * iMult2 i', map (\[_a,_b,_c] -> (Append (Ind20 $ _a-1) $ Append (Ind20 $ _b-1) $ Append (Ind20 $ _c-1) (singletonInd (Ind20 $ d-1)), Empty, singletonInd (Ind9 $ i-1), Empty, Empty, Empty) ) $ nub $ permutations [a,b,c] ) | a <- [1..21], b <- [a..21], c <- [b..21], d <- [1..21], i <- [1..10] ]

--A:B:Cp:Dq
areaList18_3 ::  [([Int], Int, [IndTupleAbs 4 0 0 0 2 0])]
areaList18_3 = list
      where
          trianArea = trianMapArea
          list = [ let (a',b',c',d') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d) in  (a' ++ b' ++ c'++ p : d'++[q], areaMult a' * areaMult b' * areaMult c' * areaMult d', map ( \(_a,_b,_c,_p,_d,_q) -> (Append (Ind20 $ _a-1) $ Append (Ind20 $ _b-1) $ Append (Ind20 $ _c-1) (singletonInd (Ind20 $ _d-1)), Empty, Empty, Empty, Append (Ind3 _p) (singletonInd (Ind3 _q)), Empty) ) $ nub [(a,b,c,p,d,q),(b,a,c,p,d,q),(a,b,d,q,c,p),(b,a,d,q,c,p)] ) | a <- [1..21], b <- [a..21], c <- [1..21], d <- [c..21], p <- [0..3], q <- [0..3] , not (c == d && p > q) ]

--order 5

areaList20 ::  [( [Int], Int, [IndTupleAbs 5 0 0 0 0 0])]
areaList20 = list
      where
          trianArea = trianMapArea
          list = [ let (a',b',c', d', e') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c, (I.!) trianArea d, (I.!) trianArea e) in  (a' ++ b' ++ c' ++ d' ++ e', areaMult a' * areaMult b' * areaMult c' * areaMult d' * areaMult e', map (\[_a,_b,_c,_d,_e] -> (Append (Ind20 $ _a-1) $ Append (Ind20 $ _b-1) $ Append (Ind20 $ _c-1) $ Append (Ind20 $ _d-1) $ singletonInd (Ind20 $ _e-1), Empty, Empty, Empty, Empty, Empty)) $ nub $ permutations [a,b,c,d,e] )| a <- [1..21], b <- [a..21], c <- [b..21], d <- [c..21], e <- [d..21] ]

--for the kinetic Ansätze for the Rom calculations -> extra symmetry

--Ap:Bq
areaList10Rom :: [( [Int], Int, [IndTupleAbs 2 0 0 0 2 0])]
areaList10Rom = list
      where
          trianArea = trianMapArea
          list = [ let (a',b') = ((I.!) trianArea a, (I.!) trianArea b) in  (a' ++ p : b' ++ [q], areaMult a' * areaMult b', map (\[_a,_p,_b,_q] -> (Append (Ind20 $ _a-1) $ singletonInd (Ind20 $ _b-1), Empty, Empty, Empty, Append (Ind3 _p) $ singletonInd (Ind3 _q), Empty)) $ nub [[a,p,b,q], [a,q,b,p], [b,p,a,q], [b,q,a,p]]) | a <- [1..21], b <- [a..21], p <- [0..3], q <- [p..3]]

--Ap:Bq:C

areaList14Rom :: [( [Int], Int, [IndTupleAbs 3 0 0 0 2 0])]
areaList14Rom = list
      where
          trianArea = trianMapArea
          list = [ let (a',b',c') = ((I.!) trianArea a, (I.!) trianArea b, (I.!) trianArea c) in  (a' ++ p : b' ++ q : c' , areaMult a' * areaMult b' * areaMult c', map (\[[_a,_p],[_b,_q]] -> (Append (Ind20 $ _a-1) $ Append (Ind20 $ _b-1) $ singletonInd (Ind20 $ c-1), Empty, Empty, Empty, Append (Ind3 _p) $ singletonInd (Ind3 _q), Empty)) $ nub $ permutations [[a,p],[b,q]]) | a <- [1..21], b <- [a..21], c <- [1..21], p <- [0..3], q <- [0..3], not (a==b && p>q) ]


--now the same for the metric ansätze


-- | Evaluation list for \(a^{A} \).
metricList2 :: [( [Int], Int, [IndTupleAbs 0 0 1 0 0 0])]
metricList2 = list
      where
          trianMetric = trianMap2
          list = [ let a' = (I.!) trianMetric a in (a', iMult2 a', [(Empty, Empty, singletonInd (Ind9 $ a-1), Empty, Empty, Empty)]) | a <- [1..10] ]


--(first metric indices)
-- | Evaluation list for \(a^{AI} \).
metricList4_1 :: [( [Int], Int, [IndTupleAbs 0 0 2 0 0 0])]
metricList4_1 =  list
      where
          trianMetric = trianMap2
          list = [ let (a',i') = ((I.!) trianMetric a, (I.!) trianMetric i) in (a'++i', iMult2 a' * iMult2 i', [(Empty, Empty, Append (Ind9 $ a-1) (singletonInd (Ind9 $ i-1)), Empty, Empty, Empty)]) | a <- [1..10], i <- [1..10] ]


-- | Evaluation list for \(a^{A B}\). Note that also when using the abstract indices this ansatz still features the \( A \leftrightarrow B \) symmetry.
metricList4_2 :: [( [Int], Int, [IndTupleAbs 0 0 2 0 0 0])]
metricList4_2 = list
      where
          trianMetric = trianMap2
          list = [ let (a',b') = ((I.!) trianMetric a, (I.!) trianMetric b) in  (a' ++ b', iMult2 a' * iMult2 b', map (\[_a,_b] -> (Empty, Empty, Append (Ind9 $ _a-1) $ singletonInd (Ind9 $ _b-1), Empty, Empty, Empty)) $ nub $ permutations [a,b] )  | a <- [1..10], b <- [a..10]]


-- | Evaluation list for \(a^{Ap Bq}\). Note that also when using the abstract indices this ansatz still features the \( (Ap) \leftrightarrow (Bq) \) symmetry.
metricList6_1 :: [( [Int], Int, [IndTupleAbs 0 0 2 0 2 0])]
metricList6_1 = list
      where
          trianMetric = trianMap2
          list = [ let (a',b') = ((I.!) trianMetric a, (I.!) trianMetric b) in  (a' ++ p : b' ++ [q], iMult2 a' * iMult2 b', map (\[[_a,_p],[_b,_q]] -> (Empty, Empty, Append (Ind9 $ _a-1) $ singletonInd (Ind9 $ _b-1), Empty, Append (Ind3 _p) $ singletonInd (Ind3 _q), Empty)) $ nub $ permutations [[a,p],[b,q]]) | a <- [1..10], b <- [a..10], p <- [0..3], q <- [0..3],  not (a==b && p>q)]


-- | Evaluation list for \(a^{ABI} \).
metricList6_2 :: [( [Int], Int, [IndTupleAbs 0 0 3 0 0 0])]
metricList6_2 = list
      where
          trianMetric = trianMap2
          list = [ let (a',b',i') = ((I.!) trianMetric a, (I.!) trianMetric b, (I.!) trianMetric i) in  (a' ++ b' ++ i', iMult2 a' * iMult2 b' * iMult2 i', [ (Empty, Empty, Append (Ind9 $ a-1) $ Append (Ind9 $ b-1) $ singletonInd (Ind9 $ i-1), Empty, Empty, Empty)] ) | a <- [1..10], b <- [1..10], i <- [1..10] ]


-- | Evaluation list for \(a^{ABC} \).  Note that also when using the abstract indices this ansatz still features the symmetry under arbitrary permutations of \( ABC\).
metricList6_3 ::  [( [Int], Int, [IndTupleAbs 0 0 3 0 0 0])]
metricList6_3 = list
      where
          trianMetric = trianMap2
          list = [ let (a',b',c') = ((I.!) trianMetric a, (I.!) trianMetric b, (I.!) trianMetric c) in  (a' ++ b' ++ c', iMult2 a' * iMult2 b' * iMult2 c', map (\[_a,_b,_c] -> (Empty, Empty, Append (Ind9 $ _a-1) $ Append (Ind9 $ _b-1) $ singletonInd (Ind9 $ _c-1), Empty, Empty, Empty)) $ nub $ permutations [a,b,c] )| a <- [1..10], b <- [a..10], c <- [b..10] ]


-- | Evaluation list for \(a^{ABp Cq}\). Note that also when using the abstract indices this ansatz still features the \( (Bp) \leftrightarrow (Cq) \) symmetry.
metricList8_1 :: [( [Int], Int, [IndTupleAbs 0 0 3 0 2 0])]
metricList8_1 = list
      where
          trianMetric = trianMap2
          list = [ let (a',b',c') = ((I.!) trianMetric a, (I.!) trianMetric b, (I.!) trianMetric c) in  (a' ++ b' ++ p : c' ++ [q], iMult2 a' * iMult2 b' * iMult2 c', map (\[[_b,_p],[_c,_q]] -> (Empty, Empty, Append (Ind9 $ a-1) $ Append (Ind9 $ _b-1) $ singletonInd (Ind9 $ _c-1), Empty, Append (Ind3 _p) $ singletonInd (Ind3 _q), Empty)) $ nub $ permutations [[b,p],[c,q]]) | a <- [1..10], b <- [1..10], c <- [b..10], p <- [0..3], q <- [0..3], not (b==c && p>q) ]


-- | Evaluation list for \(a^{A B C I}\). Note that also when using the abstract indices this ansatz still features the \( (A) \leftrightarrow (B) \) symmetry.
metricList8_2 :: [( [Int], Int, [IndTupleAbs 0 0 4 0 0 0])]
metricList8_2 = list
      where
          trianMetric = trianMap2
          list = [ let (a',b',c',i') = ((I.!) trianMetric a, (I.!) trianMetric b, (I.!) trianMetric c, (I.!) trianMetric i) in ( a' ++ b' ++ c' ++ i', iMult2 a' * iMult2 b' * iMult2 c' * iMult2 i', map (\[_a,_b] -> (Empty, Empty, Append (Ind9 $ _a-1) $ Append (Ind9 $ _b-1) $ Append (Ind9 $ c-1) $ singletonInd (Ind9 $ i-1), Empty, Empty, Empty)) $ nub $ permutations [a,b] ) | a <- [1..10], b <- [a..10], c <- [1..10], i <- [1..10] ]

--symLists for the ansätze

-- | Symmetry list for @'areaList4'@.
symList4 :: Symmetry
symList4 = ([], [(1,2),(3,4)], [([1,2],[3,4])], [], [])

-- | Symmetry list for @'areaList6'@.
symList6 :: Symmetry
symList6 = ([(5,6)], [(1,2),(3,4)], [([1,2],[3,4])], [], [])

-- | Symmetry list for @'areaList8'@.
symList8 :: Symmetry
symList8 = ([], [(1,2),(3,4),(5,6),(7,8)], [([1,2],[3,4]),([5,6],[7,8]),([1,2,3,4],[5,6,7,8])], [], [])

-- | Symmetry list for @'areaList10_1'@.
symList10_1 :: Symmetry
symList10_1 = ([], [(1,2),(3,4),(6,7),(8,9)], [([1,2],[3,4]),([6,7],[8,9]),([1,2,3,4,5],[6,7,8,9,10])], [], [])

-- | Symmetry list for @'areaList10_2'@.
symList10_2 :: Symmetry
symList10_2 = ([(9,10)], [(1,2),(3,4),(5,6),(7,8)], [([1,2],[3,4]),([5,6],[7,8])], [], [])

-- | Symmetry list for @'areaList12'@.
symList12 :: Symmetry
symList12 = ([], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12])], [], [[[1,2,3,4],[5,6,7,8],[9,10,11,12]]])

symList12_1 :: Symmetry
symList12_1 = ([(5,6),(11,12)], [(1,2),(3,4),(7,8),(9,10)], [([1,2],[3,4]),([7,8],[9,10]),([1,2,3,4,5,6],[7,8,9,10,11,12])], [], [])

-- | Symmetry list for @'areaList14_1'@.
symList14_1 :: Symmetry
symList14_1 = ([], [(1,2),(3,4),(5,6),(7,8),(10,11),(12,13)], [([1,2],[3,4]),([5,6],[7,8]),([10,11],[12,13]),([5,6,7,8,9],[10,11,12,13,14])], [], [])

-- | Symmetry list for @'areaList14_2'@.
symList14_2 :: Symmetry
symList14_2 = ([(13,14)], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12]),([1,2,3,4],[5,6,7,8])], [], [])

symList16_1 :: Symmetry
symList16_1 = ([(15,16)], [(1,2),(3,4),(6,7),(8,9),(11,12),(13,14)], [([1,2],[3,4]),([6,7],[8,9]),([11,12],[13,14]),([1,2,3,4,5],[6,7,8,9,10])], [], [])

symList16_2 :: Symmetry
symList16_2 = ([(9,10),(15,16)], [(1,2),(3,4),(5,6),(7,8),(11,12),(13,14)], [([1,2],[3,4]),([5,6],[7,8]),([11,12],[13,14]),([5,6,7,8,9,10],[11,12,13,14,15,16])], [], [])

symList18 :: Symmetry
symList18 = ([(5,6),(11,12),(17,18)], [(1,2),(3,4),(7,8),(9,10),(13,14),(15,16)], [([1,2],[3,4]),([7,8],[9,10]),([13,14],[15,16])], [], [[[1,2,3,4,5,6],[7,8,9,10,11,12],[13,14,15,16,17,18]]])

--order 4

symList16 :: Symmetry
symList16 = ([], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12),(13,14),(15,16)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12]),([13,14],[15,16])], [], [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]])

symList18_2 :: Symmetry
symList18_2 = ([(17,18)], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12),(13,14),(15,16)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12]),([13,14],[15,16])], [], [[[1,2,3,4],[5,6,7,8],[9,10,11,12]]])

symList18_3 :: Symmetry
symList18_3 = ([], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12),(14,15),(16,17)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12]),([14,15],[16,17]),([1,2,3,4],[5,6,7,8]),([9,10,11,12,13],[14,15,16,17,18])], [], [])

--order 5

symList20 :: Symmetry
symList20 = ([], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12),(13,14),(15,16),(17,18),(19,20)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12]),([13,14],[15,16]),([17,18],[19,20])], [], [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16],[17,18,19,20]]])

--lists for rom ansätze

symList10Rom :: Symmetry
symList10Rom = ([(5,10)], [(1,2),(3,4),(6,7),(8,9)], [([1,2],[3,4]),([6,7],[8,9]),([1,2,3,4],[6,7,8,9])], [], [])

symList14Rom :: Symmetry
symList14Rom = ([], [(1,2),(3,4),(6,7),(8,9),(11,12),(13,14)], [([1,2],[3,4]),([6,7],[8,9]),([11,12],[13,14]),([1,2,3,4,5],[6,7,8,9,10])], [], [])


--extra symLists for the metric ansätze

--A ansatz

-- | Symmetry list for @'metricList2'@.
metricsymList2 :: Symmetry
metricsymList2 = ([(1,2)], [], [], [], [])

--AI ansatz

-- | Symmetry list for @'metricList4_1'@.
metricsymList4_1 :: Symmetry
metricsymList4_1 = ([(1,2),(3,4)], [], [], [], [])


--A:B ansatz

-- | Symmetry list for @'metricList4_2'@.
metricsymList4_2 :: Symmetry
metricsymList4_2 = ([(1,2),(3,4)], [], [([1,2],[3,4])], [], [])


--Ap:Bq ansatz

-- | Symmetry list for @'metricList6_1'@.
metricsymList6_1 :: Symmetry
metricsymList6_1 = ([(1,2),(4,5)], [], [([1,2,3],[4,5,6])], [], [])

--A:BI ansatz

-- | Symmetry list for @'metricList6_2'@.
metricsymList6_2 :: Symmetry
metricsymList6_2 = ([(1,2),(3,4),(5,6)], [], [], [], [])

--A:B:C ansatz

-- | Symmetry list for @'metricList6_3'@.
metricsymList6_3 :: Symmetry
metricsymList6_3 = ([(1,2),(3,4),(5,6)], [], [], [], [[[1,2],[3,4],[5,6]]])

--A:Bp:Cq ansatz

-- | Symmetry list for @'metricList8_1'@.
metricsymList8_1 :: Symmetry
metricsymList8_1 = ([(1,2),(3,4),(6,7)], [], [([3,4,5],[6,7,8])], [], [])

--A:B:CI ansatz

-- | Symmetry list for @'metricList8_2'@.
metricsymList8_2 :: Symmetry
metricsymList8_2 = ([(1,2),(3,4),(5,6),(7,8)], [], [([1,2],[3,4])], [], [])
