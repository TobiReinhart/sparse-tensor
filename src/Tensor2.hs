



module Tensor2 (
    Ind, Index, Tensor,
    triangleMap2, triangleMap3, triangleMapArea,
    intAIB, mkEqnSparseIntAIB, interArea, interMetric, flatArea, flatAreaST, epsilon, eta, interEqn1_2, interEqn1_3,
    interI_2, interJ_2, interI_Area, interJ_Area, intTest5, tensorProductNumeric, delta_20, intTest5List, tensorProductWith, tensorTranspose
    

) where

    --use hash map ?

    import qualified Data.Map.Strict as M
    import Data.Maybe
    import Data.Foldable
    import Data.List
    import Data.List.Split
    --import GHC.Generics
    --import Data.Hashable
    

    type Ind = [Int] 

    type Index = (Ind, Ind, Ind, Ind ,Ind ,Ind ,Ind ,Ind) 

    type Tensor a = M.Map Index a

    {-
    delta_20 :: Tensor Rational
    delta_20 = M.fromList l
                    where
                        l = [(([a],[a]),1) | a <- [0..20]]

    combineInd :: Ind -> Ind -> Ind 
    combineInd (i) (j) = (i ++ j)

    combineIndex :: Index -> Index -> Index
    combineIndex ((a1,b1)) ((a2,b2)) = (combineInd a1 a2, combineInd b1 b2)


    tensorProductNumeric :: (Num a, Eq a) => Tensor a -> Tensor a -> Tensor a
    tensorProductNumeric map1 map2 = newMap
                where 
                    pairs1 = M.assocs $ M.filter (/=0) map1 
                    pairs2 = M.assocs $ M.filter (/=0) map2
                    combineF = \(a,b) (c,d) -> (combineIndex a c, (*) b d)
                    newMap = M.fromDistinctAscList $ combineF <$> pairs1 <*> pairs2

    intTest5 :: Tensor Rational -> Tensor Rational 
    intTest5 tens = tensorProductNumeric tens $ tensorProductNumeric tens $ tensorProductNumeric tens $ tensorProductNumeric tens tens

    tensorProductList :: (Num a, Eq a) => [(Index, a)] -> [(Index, a)] -> [(Index,a)]
    tensorProductList l1 l2 =  l3
            where
                combineF = \(a,b) (c,d) -> (combineIndex a c, (*) b d)
                l3 = combineF <$> l1 <*> l2

    intTest5List :: [(Index,Rational)] -> [(Index,Rational)]
    intTest5List l = tensorProductList l $ tensorProductList l $ tensorProductList l $ tensorProductList l l
                where
                    l = [(([a],[a]),1) | a <- [0..20]]

    -}




    
    getValInd :: Ind -> Int -> Int
    getValInd seq i = seq !! i

    sortInd :: Ind -> Ind 
    sortInd = sort 

    indSign2 :: Ind -> Int
    indSign2 ind
        | i < j = 1
        | i == j = 0
        | i > j = -1
         where
            i = getValInd ind 0
            j = getValInd ind 1
                

    getRangeList :: Int -> Int -> [Ind]
    getRangeList i r
        | i == 0 = [[]]
        | i == 1 = [[a]| a <- [0..r]]
        | otherwise = [ a : b | a <- [0.. r], b <- getRangeList (i-1) r]  

    --there is the problem

    swapPosInd :: (Int,Int) -> (Ind) -> (Ind)
    swapPosInd (0,i) l = swapHead i l
    swapPosInd (i,j) l = part1 ++ (swapHead (j-i) part2)
            where
                (part1,part2) = splitAt i l  

    swapHead :: Int -> Ind -> Ind
    swapHead 1 (x:xs) = (head xs) : x : (tail xs)
    swapHead i (x:xs) = (head rest) : y ++ x : (tail rest)
            where 
                (y,rest) = splitAt (i-1) xs
    

    swapBlockPosInd :: ([Int],[Int]) -> (Ind) -> (Ind)
    swapBlockPosInd (i,j) s 
        | length i /= length j = error "only blocks with the same lenght can be symmetrized"
        | otherwise = foldr swapPosInd s pairList
            where
                pairList = zip i j


    delInd :: Int -> Ind -> Ind
    delInd 0 l = tail l 
    delInd i l = (head l) : (delInd (i-1) $ tail l)

    insInd :: Int -> Int -> Ind -> Ind 
    insInd i x l =  l1 ++ x : l2
            where 
                (l1,l2) = splitAt i l

    repInd ::Int -> Int -> Ind -> Ind 
    repInd 0 x l = x : (tail l) 
    repInd i x l = (head l) : (repInd (i-1) x $ tail l)

    combineInd :: Ind -> Ind -> Ind 
    combineInd = (++)


    contractionInd :: (Int,Int) -> (Ind, Ind) -> (Ind, Ind)
    contractionInd (i,j) (ind1, ind2) = (delInd i ind1, delInd j ind2)


    indexList :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Index
    indexList a1 b1 c1 d1 e1 f1 g1 h1 = (a2,b2,c2,d2,e2,f2,g2,h2)
        where 
            a2 = a1
            b2 = b1
            c2 = c1
            d2 = d1
            e2 = e1
            f2 = f1
            g2 = g1
            h2 = h1

    actOnIndex :: Int -> (a -> a) -> (a,a,a,a,a,a,a,a) -> (a,a,a,a,a,a,a,a)
    actOnIndex i funct (a,b,c,d,e,f,g,h) 
            | i == 1 = (funct a, b, c, d, e, f, g, h)
            | i == 2 = (a, funct b, c, d, e, f, g, h)
            | i == 3 = (a, b, funct c, d, e, f, g, h)
            | i == 4 = (a, b, c, funct d, e, f, g, h)
            | i == 5 = (a, b, c, d, funct e, f, g, h)
            | i == 6 = (a, b, c, d, e, funct f, g, h)
            | i == 7 = (a, b, c, d, e, f, funct g, h)
            | i == 8 = (a, b, c, d, e, f, g, funct h)
            | otherwise = error "specified index position must be an int between 1 and 8"


    swapPosIndex :: Int -> (Int,Int) -> Index -> Index 
    swapPosIndex i inds = actOnIndex i (swapPosInd inds)
            
            
    swapBlockPosIndex :: Int -> ([Int],[Int]) -> Index -> Index 
    swapBlockPosIndex i inds = actOnIndex i (swapBlockPosInd inds)


    combineIndex :: Index -> Index -> Index
    combineIndex (a1,b1,c1,d1,e1,f1,g1,h1) (a2,b2,c2,d2,e2,f2,g2,h2) = (a1++a2, b1++b2, c1++c2, d1++d2, e1++e2, f1++f2, g1++g2, h1++h2)
        --(combineInd a1 a2, combineInd b1 b2, combineInd c1 c2, combineInd d1 d2, combineInd e1 e2, combineInd f1 f2, combineInd g1 g2, combineInd h1 h2)


    isContractionInd :: (Int,Int) -> Ind -> Ind  -> Bool
    isContractionInd (i,k) s1 s2 = val1 == val2
            where 
                val1 = s1 !! i
                val2 = s2 !! k


    isContractionIndex :: Int -> (Int,Int) -> Index -> Bool
    isContractionIndex i pair (a,b,c,d,e,f,g,h)
                | i == 1 = isContractionInd pair a b
                | i == 2 = isContractionInd pair c d
                | i == 3 = isContractionInd pair e f
                | i == 4 = isContractionInd pair g h
                | otherwise = error "wrong index type to contract" 

    delContractionIndex_20 :: (Int,Int) -> Index -> Index 
    delContractionIndex_20 (i,j) (a,b,c,d,e,f,g,h) = (delInd i a, delInd j b, c, d, e, f, g, h)

    delContractionIndex_19 :: (Int,Int) -> Index -> Index
    delContractionIndex_19 (i,j) (a,b,c,d,e,f,g,h) = (a, b, delInd i c, delInd j d, e, f, g, h)

    delContractionIndex_9 :: (Int,Int) -> Index -> Index
    delContractionIndex_9 (i,j) (a,b,c,d,e,f,g,h) = (a, b, c, d, delInd i e, delInd j f, g, h)

    delContractionIndex_3 :: (Int,Int) -> Index -> Index
    delContractionIndex_3 (i,j) (a,b,c,d,e,f,g,h) = (a, b, c, d, e, f, delInd i g, delInd j h)


    checkInd :: Ind -> Int -> Int -> Bool
    checkInd s i val = (s !! i) == val  

    mkTensorFromList :: [(Index, a)] -> Tensor a
    mkTensorFromList = M.fromList 

    type Rank = (Int, Int, Int, Int, Int, Int, Int, Int)

    tensorIndList :: Rank -> [Index]
    tensorIndList (r1,r2,r3,r4,r5,r6,r7,r8) = list
     where 
        list = [ (y1, y2, y3, y4, y5, y6, y7, y8) | y1 <- (getRangeList r1 20), y2 <- (getRangeList r2 20), y3 <- (getRangeList r3 19), y4 <- (getRangeList r4 19),
         y5 <- (getRangeList r5 9), y6 <- (getRangeList r6 9), y7 <- (getRangeList r7 3), y8 <- (getRangeList r8 3)]


    mkTensorfromF :: (Num a, Eq a) => Rank -> (Index -> a) -> Tensor a
    mkTensorfromF rank f =  M.filter ( /= 0) $ M.fromList (zip indList valueList)
            where 
                indList = tensorIndList rank 
                valueList = map f indList

    getVal :: Num a => Tensor a -> Index -> a
    getVal map1 ind = fromMaybe 0 $ M.lookup ind map1


    tensorSMult :: Num a => a -> Tensor a -> Tensor a 
    tensorSMult a = fmap ( (*) a)

    tensorAdd :: (Num a, Eq a) => Tensor a -> Tensor a -> Tensor a
    tensorAdd map1 map2 = M.filter (/= 0) $ M.unionWith (+) map1 map2 

    tensorSub :: (Num a, Eq a) => Tensor a -> Tensor a -> Tensor a
    tensorSub t1 t2 = tensorAdd t1 (tensorSMult (-1) t2)

    tensorTranspose :: Int -> (Int,Int) -> Tensor a -> Tensor a
    tensorTranspose i pair map1 =  M.mapKeys (swapPosIndex i pair) map1 

    tensorBlockTranspose :: Int -> ([Int],[Int]) -> Tensor a -> Tensor a
    tensorBlockTranspose i pair map1 =  M.mapKeys (swapBlockPosIndex i pair) map1 

    tensorProductWith :: (a -> b -> c) -> Tensor a -> Tensor b -> Tensor c
    tensorProductWith f map1 map2 = newMap
                where
                    pairs1 = M.assocs map1 
                    pairs2 = M.assocs map2
                    combineF = \(a,b) (c,d) -> (combineIndex a c, f b d)
                    newMap = M.fromDistinctAscList $ combineF <$> pairs1 <*> pairs2

    
    tensorProductNumeric :: (Num a, Eq a) => Tensor a -> Tensor a -> Tensor a
    tensorProductNumeric map1 map2 = newMap
                where 
                    pairs1 = M.assocs $ M.filter (/=0) map1 
                    pairs2 = M.assocs $ M.filter (/=0) map2
                    combineF = \(a,b) (c,d) -> (combineIndex a c, (*) b d)
                    newMap = M.fromDistinctAscList $ combineF <$> pairs1 <*> pairs2

    tensorProductList :: (Num a, Eq a) => [(Index, a)] -> [(Index, a)] -> [(Index,a)]
    tensorProductList l1 l2 =  l3
            where
                combineF = \(a,b) (c,d) -> (combineIndex a c, (*) b d)
                l3 = combineF <$> l1 <*> l2

    
    {-
    tensorProductNumericF :: (Num a, Eq a) => Tensor a -> (Index ,a) -> Tensor a 
    tensorProductNumericF map1 (ind, r) = newMap
                where 
                    combineF = combineIndex ind
                    newMap = M.map ((*) r) $ M.mapKeysMonotonic combineF map1


    tensorProductNumeric :: (Num a, Eq a) => Tensor a -> Tensor a -> Tensor a
    tensorProductNumeric map1 map2 = newMap
                where 
                    e = M.empty
                    f = tensorProductNumericF map1  
                    newMap = M.foldrWithKey (\k v b -> M.union (f (k,v)) b) e map2 
    -}
    
    tensorContractWith_20 :: (Int,Int) -> (a -> a -> a) -> Tensor a -> Tensor a
    tensorContractWith_20 pair f map1 = map2 
                    where 
                        mapFilt = M.filterWithKey (\k _ -> isContractionIndex 1 pair k) map1 
                        map2 = M.mapKeysWith f (delContractionIndex_20 pair) mapFilt

    tensorContractWith_19 :: (Int,Int) -> (a -> a -> a) -> Tensor a -> Tensor a
    tensorContractWith_19 pair f map1 = map2 
                    where 
                        mapFilt = M.filterWithKey (\k _ -> isContractionIndex 2 pair k) map1 
                        map2 = M.mapKeysWith f (delContractionIndex_19 pair) mapFilt
                    
    tensorContractWith_9 :: (Int,Int) -> (a -> a -> a) -> Tensor a -> Tensor a
    tensorContractWith_9 pair f map1 = map2 
                    where 
                        mapFilt = M.filterWithKey (\k _ -> isContractionIndex 3 pair k) map1 
                        map2 = M.mapKeysWith f (delContractionIndex_9 pair) mapFilt
                    
    tensorContractWith_3 :: (Int,Int) -> (a -> a -> a) -> Tensor a -> Tensor a
    tensorContractWith_3 pair f map1 = map2 
                    where 
                        mapFilt = M.filterWithKey (\k _ -> isContractionIndex 4 pair k) map1 
                        map2 = M.mapKeysWith f (delContractionIndex_3 pair) mapFilt

    delta_3 :: Tensor Rational
    delta_3 = mkTensorfromF (0,0,0,0,0,0,1,1) delta_3F
                    where
                        delta_3F ([], [], [], [], [], [], (i : []), (j : []))
                            | i == j = 1
                            | otherwise = 0 

    delta_9 :: Tensor Rational
    delta_9 = mkTensorfromF (0,0,0,0,1,1,0,0) delta_9F
                    where
                        delta_9F ([], [], [], [], i : [], j : [], [], [])
                            | i == j = 1
                            | otherwise = 0
                            
    delta_19 :: Tensor Rational
    delta_19 = mkTensorfromF (0,0,1,1,0,0,0,0) delta_19F
                    where
                        delta_19F ([], [], i : [], j : [], [], [], [], [])
                            | i == j = 1
                            | otherwise = 0

    delta_20 :: Tensor Rational
    delta_20 = mkTensorfromF (1,1,0,0,0,0,0,0) delta_20F
                    where
                        delta_20F (i : [], j : [], [], [], [], [], [], [])
                            | i == j = 1
                            | otherwise = 0


    symIndList :: Enum a => Int -> Int -> [[a]]
    symIndList n j 
            | n <= toEnum 0 = error "wrong number of indices"
            | n == 1 = [ [a] | a <- [toEnum 0.. toEnum j] ]
            | otherwise = [ a ++ [b] | a <- (symIndList (n-1) j), b <- [(last a)..toEnum j] ] 


    triangleMap2 :: (Enum a, Enum b, Ord a) =>  M.Map ([a]) b
    triangleMap2 = M.fromList $ zip (symIndList 2 3) [toEnum 0..]

    triangleMap3 :: (Enum a, Enum b, Ord a) =>  M.Map ([a]) b
    triangleMap3 = M.fromList $ zip (symIndList 3 3) [toEnum 0..]

    interF_I2 :: M.Map Ind Int -> Index -> Rational
    interF_I2 map1 (_,_,_,_,x,_,_,y) 
                | indI == xVal = 1
                | otherwise = 0
                 where 
                    indI = (M.!) map1 $ sortInd y
                    xVal = getValInd x 0


    jMult2 :: Ind -> Rational
    jMult2 ind 
            | i == j = 1
            | otherwise = 1/2
             where 
                i = getValInd ind 0
                j = getValInd ind 1

    interF_J2 :: M.Map Ind Int -> Index -> Rational
    interF_J2 map1 (_,_,_,_,_,x,y,_) 
                | indI == xVal = mult
                | otherwise = 0
                 where 
                    indI = (M.!) map1 $ sortInd y
                    xVal = getValInd x 0
                    mult = jMult2 y 
            
    symF_I2 :: M.Map Ind Int -> Index -> Rational
    symF_I2 map1 (_,_,_,_,x,_,_,y) 
                | indI == xVal = mult
                | otherwise = 0
                 where 
                    indI = (M.!) map1 $ sortInd y
                    xVal = getValInd x 0
                    mult = jMult2 y 


    aSymF_I2 :: M.Map Ind Int -> Index -> Rational
    aSymF_I2 map1 (_,_,_,_,x,_,_,y) 
                | indI == xVal = sign 
                | otherwise = 0
                 where 
                    indI = (M.!) map1 $ sortInd y
                    xVal = getValInd x 0
                    sign = fromIntegral $ indSign2 y 



    interF_I3 :: M.Map Ind Int -> Index -> Rational
    interF_I3 map1 (_,_,x,_,_,_,_,y) 
                | indI == xVal = 1
                | otherwise = 0
                 where 
                    indI = (M.!) map1 $ sortInd y
                    xVal = getValInd x 0


    jMult3 :: Ind -> Rational
    jMult3 ind 
                | i == j && j == k = 1
                | i == j || j == k || i == k = 1/3
                | otherwise = 1/6
                 where 
                    i = getValInd ind 0
                    j = getValInd ind 1
                    k = getValInd ind 2

    interF_J3 :: M.Map Ind Int -> Index -> Rational
    interF_J3 map1 (_,_,_,x,_,_,y,_) 
                | indI == xVal = mult
                | otherwise = 0
                 where 
                    indI = (M.!) map1 $ sortInd y
                    xVal = getValInd x 0
                    mult = jMult3 y 

            
    symF_I3 :: M.Map Ind Int -> Index -> Rational
    symF_I3 map1 (_,_,x,_,_,_,_,y) 
                | indI == xVal = mult
                | otherwise = 0
                 where 
                    indI = (M.!) map1 $ sortInd y
                    xVal = getValInd x 0
                    mult = jMult3 y 


    interI_2 :: M.Map Ind Int -> Tensor Rational
    interI_2 map1 = mkTensorfromF (0,0,0,0,1,0,0,2) (interF_I2 map1) 

    interJ_2 :: M.Map Ind Int -> Tensor Rational
    interJ_2 map1 = mkTensorfromF (0,0,0,0,0,1,2,0) (interF_J2 map1) 

    symI_2 :: M.Map Ind Int -> Tensor Rational
    symI_2 map1 = mkTensorfromF (0,0,0,0,1,0,0,2) (symF_I2 map1) 

    aSymI_2 :: M.Map Ind Int -> Tensor Rational
    aSymI_2 map1 = mkTensorfromF (0,0,0,0,1,0,0,2) (aSymF_I2 map1) 

    interI_3 :: M.Map Ind Int -> Tensor Rational
    interI_3 map1 = mkTensorfromF (0,0,1,0,0,0,0,3) (interF_I3 map1) 

    interJ_3 :: M.Map Ind Int -> Tensor Rational
    interJ_3 map1 = mkTensorfromF (0,0,0,1,0,0,3,0) (interF_J3 map1) 

    symI_3 :: M.Map Ind Int -> Tensor  Rational
    symI_3 map1 = mkTensorfromF (0,0,1,0,0,0,0,3) (symF_I3 map1) 

    areaDofList :: (Enum a, Eq a, Ord a) => [[a]]
    areaDofList = [[a,b,c,d] | a <- [toEnum 0..toEnum 2], b <- [succ a .. toEnum 3], c <- [a..toEnum 2], d <- [succ c.. toEnum 3], not $ a == c && b > d  ]

    triangleMapArea :: (Enum a, Enum b, Ord a) =>  M.Map ([a]) b
    triangleMapArea = M.fromList $ zip (areaDofList) [toEnum 0..]

    jMultArea :: Ind -> Rational
    jMultArea ind 
                | a == c && b == d = 1/4
                | otherwise = 1/8
                 where 
                    a = getValInd ind 0
                    b = getValInd ind 1
                    c = getValInd ind 2
                    d = getValInd ind 3

    areaSign :: Ind -> Rational
    areaSign s 
        | xor (a<b) (c<d) = -1
        | otherwise = 1
        where
            xor True False = True
            xor False True = True
            xor True True = False
            xor False False = False
            [a,b,c,d] = toList s 

    isZeroArea :: Ind -> Bool
    isZeroArea s 
            | a == b || c == d = True
            | otherwise = False
            where
                [a,b,c,d] = toList s 


    canonicalizeArea :: Ind -> (Ind,Rational)
    canonicalizeArea s = (newS, sign)
        where
            [s1,s2] = sort $ map sort $ toList $ chunksOf 2 s 
            sign = areaSign s
            newS = s1 ++ s2

    interF_IArea :: M.Map Ind Int -> Index -> Rational
    interF_IArea map1 (x,_,_,_,_,_,_,y) 
                | isZeroArea y = 0
                | indI == xVal = snd sortY
                | otherwise = 0
                 where 
                    sortY = canonicalizeArea y
                    indI = (M.!) map1 $ fst sortY
                    xVal = getValInd x 0


    symF_IArea :: M.Map Ind Int -> Index -> Rational
    symF_IArea map1 (x,_,_,_,_,_,_,y) 
                | isZeroArea y = 0
                | indI == xVal = snd sortY * (jMultArea (fst sortY))
                | otherwise = 0
                 where 
                    sortY = canonicalizeArea y
                    indI = (M.!) map1 $ fst sortY
                    xVal = getValInd x 0

    interF_JArea :: M.Map Ind Int -> Index -> Rational
    interF_JArea map1 (_,x,_,_,_,_,y,_) 
                | isZeroArea y = 0
                | indI == xVal = snd sortY * (jMultArea (fst sortY))
                | otherwise = 0
                 where 
                    sortY = canonicalizeArea y
                    indI = (M.!) map1 $ fst sortY
                    xVal = getValInd x 0


    interI_Area :: M.Map Ind Int -> Tensor Rational
    interI_Area map1 = mkTensorfromF (1,0,0,0,0,0,0,4) (interF_IArea map1) 

    symI_Area :: M.Map Ind Int -> Tensor Rational
    symI_Area map1 = mkTensorfromF (1,0,0,0,0,0,0,4) (symF_IArea map1) 

    interJ_Area :: M.Map Ind Int -> Tensor Rational
    interJ_Area map1 = mkTensorfromF (0,1,0,0,0,0,4,0) (interF_JArea map1) 

    interMetric ::  M.Map Ind Int ->  M.Map Ind Int  -> Tensor Rational 
    interMetric iMap jMap = tensorSMult (-2) $ tensorContractWith_3 (0,0) (+) prod 
            where 
                i = interI_2 iMap
                j = interJ_2 jMap
                prod = tensorProductWith (*) i j 


    interArea ::  M.Map Ind Int ->  M.Map Ind Int  -> Tensor Rational 
    interArea iMap jMap = tensorSMult (-4) $ tensorContractWith_3 (1,1) (+) 
        $ tensorContractWith_3 (2,2) (+) $ tensorContractWith_3 (3,3) (+) prod 
            where 
                i = interI_Area iMap
                j = interJ_Area jMap
                prod = tensorProductWith (*) i j 

    interEqn1_2 :: M.Map Ind Int ->  M.Map Ind Int -> Tensor Rational 
    interEqn1_2 map1Area map2Area = intTotal
                where
                    int1 = tensorProductWith (*) (interArea map1Area map2Area) delta_3 
                    int2 = tensorProductWith (*) ( tensorTranspose 8 (0,1) $ tensorProductWith (*) delta_3 delta_3 ) delta_20
                    intTotal = tensorSub int1 int2 
            
    interEqn1_3 ::  M.Map Ind Int ->  M.Map Ind Int -> M.Map Ind Int ->  M.Map Ind Int -> Tensor Rational
    interEqn1_3 map1Area map2Area map1Metric map2Metric = intTotal 
                where
                    int1 = tensorProductWith (*) (interArea map1Area map2Area) delta_9
                    int2 = tensorProductWith (*) (interMetric map1Metric map2Metric) delta_20
                    intTotal = tensorAdd int1 int2


    eta_F :: Index -> Rational
    eta_F (_,_,_,_,_,_,_,a) 
        | x == y && x == 0 = 1
        | x == y = -1
        | otherwise = 0
            where 
                x = getValInd a 0
                y = getValInd a 1
                
    invEta_F :: Index -> Rational
    invEta_F (_,_,_,_,_,_,a,_) 
        | x == y && x == 0 = 1
        | x == y = -1
        | otherwise = 0
            where 
                x = getValInd a 0
                y = getValInd a 1

    eta :: Tensor Rational
    eta = mkTensorfromF (0,0,0,0,0,0,0,2) eta_F
            

    invEta :: Tensor Rational
    invEta = mkTensorfromF (0,0,0,0,0,0,2,0) invEta_F

    etaAbs :: M.Map Ind Int -> Tensor Rational
    etaAbs mapJ = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) t1
                where 
                    t1 = tensorProductWith (*) eta (interJ_2 mapJ)

    invEtaAbs :: M.Map Ind Int -> Tensor Rational
    invEtaAbs mapI = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) t1
                where 
                     t1 = tensorProductWith (*) invEta (interI_2 mapI)


    permSignN :: Ord a => [a] -> Int
    permSignN [] = 0
    permSignN [a] = 0
    permSignN (x:xs) = (permSignN xs)  + (length $ filter (>x) xs)
    
    permSign :: Ord a => [a] -> Int
    permSign l = (-1)^(permSignN l)

    
    epsilon_F :: Index -> Rational
    epsilon_F (_,_,_,_,_,_,_,x)
                | a == b || a == c || a == d || b == c || b == d || c == d = 0
                | otherwise = fromIntegral $ permSign [a,b,c,d]
                 where
                    a = getValInd x 0
                    b = getValInd x 1
                    c = getValInd x 2
                    d = getValInd x 3

    epsilon :: Tensor Rational
    epsilon = mkTensorfromF (0,0,0,0,0,0,0,4) epsilon_F

    
    flatAreaST :: Tensor Rational
    flatAreaST = tensorSub (tensorSub etaProd1 etaProd2) epsilon 
                where
                        etaProd = tensorProductWith (*) eta eta
                        etaProd1 = tensorTranspose 8 (1,2) etaProd
                        etaProd2 = tensorTranspose 8 (1,3) $ tensorTranspose 8 (2,3) etaProd
        
    flatArea :: M.Map Ind Int -> Tensor Rational
    flatArea map1 = tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorContractWith_3 (2,2) (+) $ tensorContractWith_3 (3,3) (+) prod
                where
                        prod = tensorProductWith (*) flatAreaST $ interJ_Area map1

    
    --test this for an int cond

    intAIB :: M.Map Ind Int ->  M.Map Ind Int -> M.Map Ind Int ->  M.Map Ind Int -> Tensor Rational 
    intAIB map1Area map2Area map1Metric map2Metric = tensorSub tens tensTrans  
            where
                intArea = interArea map1Area map2Area
                intMetric = interMetric map1Metric map2Metric
                flatA = flatArea map2Area
                flatIntA = tensorContractWith_20 (0,1) (+) $ tensorProductWith (*) intArea flatA 
                int3 = interEqn1_3 map1Area map2Area map1Metric map2Metric 
                block1 = tensorProductWith (*) delta_20 $! tensorProductWith (*) delta_20 $! tensorProductWith (*) delta_9 delta_3 
                block2 = tensorProductWith (*) intArea $! tensorProductWith (*) delta_20 delta_9
                block3 = tensorProductWith (*) delta_20 int3 
                totalBlock = tensorAdd block1 $! tensorAdd block2 block3 
                tens = tensorContractWith_20 (0,2) (+) $! tensorProductWith (*) totalBlock flatIntA 
                tensTrans = tensorTranspose 7 (0,1) $ tensorTranspose 8 (0,1) tens 

    index2SparseIntAIB :: Index -> (Int,Int) 
    index2SparseIntAIB (x1, x2, _, _, x5, x6, x7, x8) = ((c-1)*10*4^4+(j-1)*4^4+(m-1)*4^3+(n-1)*4^2+(r-1)*4+s,(a-1)*21*10+(b-1)*10+i)
                         where 
                             a = 1 + (getValInd x2 0) 
                             b = 1 + (getValInd x2 1) 
                             c = 1 + (getValInd x1 0)
                             i = 1 + (getValInd x6 0)
                             j = 1 + (getValInd x5 0)
                             m = 1 + (getValInd x7 0)
                             n = 1 + (getValInd x8 0)
                             r = 1 + (getValInd x7 1)
                             s = 1 + (getValInd x8 1)

    mkEqnSparseIntAIB :: Tensor Rational -> M.Map (Int,Int) Rational
    mkEqnSparseIntAIB  map1 = M.mapKeys index2SparseIntAIB map1

    intTest5 :: Tensor Rational -> Tensor Rational 
    intTest5 tens = tensorProductNumeric tens $ tensorProductNumeric tens $ tensorProductNumeric tens $ tensorProductNumeric tens tens

    intTest5List :: [(Index,Rational)] -> [(Index,Rational)]
    intTest5List l = tensorProductList l $ tensorProductList l $ tensorProductList l $ tensorProductList l l

    intAIBJC :: M.Map Ind Int ->  M.Map Ind Int -> M.Map Ind Int->  M.Map Ind Int -> Tensor Rational 
    intAIBJC map1Area map2Area map1Metric map2Metric = tensorSub tens tensTrans 
                    where
                        intArea = interArea map1Area map2Area
                        intMetric = interMetric map1Metric map2Metric
                        int3 = interEqn1_3 map1Area map2Area map1Metric map2Metric
                        flatA = flatArea map2Area
                        flatInt = tensorContractWith_20 (0,1) (+) $ tensorProductNumeric intArea flatA 
                        block0 = tensorProductNumeric delta_20 $ tensorProductNumeric delta_20 $ tensorProductNumeric delta_9 $ tensorProductNumeric delta_9 delta_3
                        block0prod = tensorProductNumeric block0 $! flatInt
                        block1 = tensorProductNumeric int3 $ tensorProductNumeric delta_20 delta_9 
                        block1prod = tensorProductNumeric block1 $! flatInt
                        block2prod = tensorTranspose 5 (0,1) $ tensorTranspose 1 (0,1) block1prod
                        block3 = tensorProductNumeric delta_20 $ tensorProductNumeric delta_20 $ tensorProductNumeric delta_9 delta_9 
                        block3prod = tensorProductNumeric block3 $! tensorContractWith_20 (0,1) (+) $ tensorProductNumeric intArea $! flatInt
                        totalBlock1prod = tensorAdd block0prod $ tensorAdd block1prod $ tensorAdd block2prod block3prod 
                        totalBlockTransprod = tensorTranspose 6 (0,1) $ tensorTranspose 2 (0,1) totalBlock1prod
                        tens = tensorAdd totalBlock1prod totalBlockTransprod
                        tensTrans = tensorTranspose 7 (0,1) $ tensorTranspose 8 (0,1) tens