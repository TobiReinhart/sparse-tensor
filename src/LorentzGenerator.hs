--Generation of Lorentz invariant basis fromgiven valence and symmetry

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
    getEtaInds, getEpsilonInds, getAllIndsEpsilon, getExtraASymsEps

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

    --LinearAlgebra subroutines

    import qualified Data.Eigen.Matrix as Mat 
    import qualified Data.Eigen.SparseMatrix as Sparse
    import qualified Data.Eigen.LA as Sol 
    import qualified Data.Eigen.SparseLA as SpSol

    --SparseTensor

    import SparseTensor

    --the first step consist of pre-reducing the index list for the eta and epsilon trees as much as possible 
    --this is done by using the symmetries 

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


    --note that writing specific indices from a block symmetry at an eta yields additional symmetries: for instance consider the block symmetry 
    --[ab] <-> [cd] writing eta[ac] yields the new symmetry b <-> d. 

    getExtraSyms1 :: [Int] -> Symmetry -> Symmetry 
    getExtraSyms1 [] syms = ([],[],[],[],[]) 
    getExtraSyms1 (a:b:xs) (pairs,aPairs,blocks,cycles,blockCycles) = addSym (newPairs, [],  newBlocks, [], []) (getExtraSyms1 xs newSyms)  
            where 
                newBlocks' = map (\(x,y) -> unzip $ filter (\(c,d) -> not $ (c,d) == (a,b)) $ zip x y) blocks 
                (newBlocks, newPairs') = partition (\(a,b) -> length a > 1) newBlocks'  
                newPairs = map (\([a],[b]) -> (a,b)) newPairs' 
                newSyms = addSym (pairs,aPairs,blocks,cycles,blockCycles) (newPairs, [],  newBlocks, [], [])

    --furthermore distributing a symmetric or antisymmetric pair over 2 etas yields an additinal symmetry: for instance consider the < <-> b symmetry,
    --writing eta[ac] eta[bd] yields an additional c <-> d symmetry.

    get2nd :: [Int] -> Symmetry -> (Maybe [(Int,Int)], Maybe [(Int,Int)]) 
    get2nd [a,b] (pairs,aPairs,blocks,cycles,blockCycles) = (sndPairs, sndAPairs)
            where 
                aPair = lookup a pairs 
                bPair = lookup b  (map swap pairs) 
                aAPair = lookup a aPairs
                bAPair = lookup b (map swap aPairs)
                sndPairs = case (aPair, bPair) of 
                                (Nothing, Nothing)  ->  Nothing 
                                (Just x, Nothing)   -> Just [(b,x)] 
                                (Nothing, Just y)   -> Just [(a,y)] 
                                (Just x, Just y)    -> Just [(b,x),(a,y)]
                sndAPairs = case (aAPair, bAPair) of 
                                (Nothing, Nothing)  ->  Nothing 
                                (Just x, Nothing)   -> Just [(b,x)] 
                                (Nothing, Just y)   -> Just [(a,y)] 
                                (Just x, Just y)    -> Just [(b,x),(a,y)]


    get2ndSyms :: Maybe [(Int,Int)] -> Symmetry -> [[Int]] -> Symmetry 
    get2ndSyms Nothing syms etas = syms
    get2ndSyms (Just i) (pairs,aPairs,blocks,cycles,blockCycles) etas = (newPairs,[],[],[],[])  
        where 
            get2ndInd l (i,j) = mapMaybe (\[a,b] -> if j == a then Just (i,b) else if j == b then Just (i,a) else Nothing) l
            newPairs = concat $ map (get2ndInd etas) i 


    get2ndASyms :: Maybe [(Int,Int)] -> Symmetry -> [[Int]] -> Symmetry 
    get2ndASyms Nothing syms etas = syms
    get2ndASyms (Just i) (pairs,aPairs,blocks,cycles,blockCycles) etas = ([], newAPairs,[],[],[])  
        where 
            get2ndInd l (i,j) = mapMaybe (\[a,b] -> if j == a then Just (i,b) else if j == b then Just (i,a) else Nothing) l
            newAPairs = concat $ map (get2ndInd etas) i 


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
    getEtaInds inds (p,ap,b,c,bc) = filter (\x -> filterEta x (p,ap,b,c,bc) filters1) allInds 
            where 
                filters1 = mkFilters (p,ap,b,c,bc) 
                allInds = getAllIndsEta inds ap 

    --now we proceed in the same fashion for the epsilon ind list

    --here we can actually from the very beggining prevent some linear dependencies from occuring by noting that due to certain symmetries 
    --certain expressions involving epsilon only differ by an expression that is antisymmetric in 5 or more indices and hence vanishes

    --we restrict to the simplest case: two antisymmetric pairs with a block symmetry, i.e. an area block

    {-we can use the following observations :
        as we want to contstruct a basis it suffices to pick representatives of the different symmetry orbits module anti-sym in (>4) indices
            1) whenever 3 indices of one are metric are contracted against an epsilon we can actually express the tensor as one with 4 area indices contracted against epsilon
            2) all tensors with 2 area indices contracted against one epsilon can be expressed as tensors with the first 2 area indices contracted against epsilon 
            3) tensors with a maximum of 1 epsilon contraction per area metric can be exprerssed by those with at least one 2 area contraction 
    -}

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

    getExtraASymsEps :: [Int] -> Symmetry -> Symmetry 
    getExtraASymsEps eps (p,ap,blo,cyc,cb) = ([],newASyms, [], [], [])
            where 
                blocks2 = filter (\(a,b) -> length a == 2) blo 
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
                
