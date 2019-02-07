

module TensorTreeNumeric (
    deltaList, mkTens, tensorProd, Tensor(..), toListT, fromList, tensorAdd, tensorContract, filterT,
    interI2, interJ2, interI3, interJ3, interIArea, interJArea, interMetric, interArea, triangleMap2, triangleMap3, triangleMapArea, interEqn2, interEqn3,
    intAIB, flatInter, intAIBSeq, intAIBIMap, intAIBSub, toListSubT, tensorProdList, swapHead, removeContractionInd, mkMatrixIndAIB, showTensorFrac,
    mkMatrixIndInter3, mkMatrixIndInterArea, mkMatrixIndInterMetric, canonicalizeArea, areaSign, triangleMap, intAIBJC, mkMatrixIndAIBJC
    
) where

    import Data.Foldable
    import Data.List 
    import Control.Applicative
    import Data.Maybe
    import qualified Data.Map.Strict as M
    import qualified Data.IntMap.Strict as I
    import qualified Data.Sequence as S

    --the idea is to kept the (key,val) list at all times in ascending order -> only map strictly monotonic functions

    data Tensor a = Scalar a | Tensor [(Int,Tensor a)] deriving (Eq, Ord, Show)

    getTensorList :: Tensor a -> [(Int,Tensor a)]
    getTensorList (Tensor x) = x

    instance Functor Tensor where
        fmap f (Scalar a) = Scalar (f a)
        fmap f (Tensor l) = Tensor (map (\(i,t) -> (i,fmap f t)) l)

    --only keeps order of keys if f is strictly monotonic

    toListT :: Tensor a -> [([Int],a)]
    toListT (Scalar x) = [([],x)]
    toListT (Tensor l) = concat $ map (\(i,t) -> appendF i $ toListT t) l
            where
                appendF = \i l2 -> map (\(l,val) -> (i:l,val)) l2

    toListSubT :: Int -> Tensor a -> [([Int],Tensor a)]
    toListSubT (-1) t = [([],t)]
    toListSubT j (Tensor l) = concat $ map (\(i,t) -> appendF i $ toListSubT (j-1) t) l
            where
                appendF = \i l2 -> map (\(l,val) -> (i:l,val)) l2

    toSeqT :: Tensor a -> [(S.Seq Int, a)]
    toSeqT = (map (\(k,v) -> (S.fromList k, v))) . toListT 

    toIMapT :: Tensor a -> [(I.IntMap Int, a)]
    toIMapT = (map (\(k,v) -> (I.fromList $ zip [0..] k, v))) . toListT 

    filterT :: (a -> Bool) -> Tensor a -> Tensor a
    filterT f (Scalar x) = Scalar x
    filterT f (Tensor l) = Tensor $ filter g $ map h l 
                where
                    g (i, Scalar x) = f x 
                    g (i, Tensor x) = True 
                    h (i,k) = (i,filterT f k)

    mkTens :: ([Int],a) -> Tensor a
    mkTens ([],x) = Scalar x
    mkTens (r:rs,x) = Tensor [(r,mkTens (rs,x) )]

    --better to add the ([ind],Tensor) tuple

    mkTensSub :: ([Int], Tensor a) -> Tensor a
    mkTensSub ([],t) = t 
    mkTensSub (r:rs,t) = Tensor [(r,mkTensSub (rs,t))]

    insertOrAdd :: (Num a) => ([Int],a) -> Tensor a -> Tensor a
    insertOrAdd ([],x) (Scalar y) = Scalar (x+y)
    insertOrAdd (l, x) (Tensor []) = mkTens (l,x)
    insertOrAdd (l:ls,x) (Tensor (r:rs))
                | l < j = Tensor $ (l,mkTens (ls,x)) : r : rs
                | l == j = Tensor $ (l, insertOrAdd (ls,x) tens) : rs
                | l > j = Tensor $ r : (getTensorList $ insertOrAdd (l:ls,x) $ Tensor rs)
                where
                    (j,tens) = r

    insertOrAddSub :: (Num a) => ([Int], Tensor a) -> Tensor a -> Tensor a
    insertOrAddSub ([],t) s = tensorAdd t s 
    insertOrAddSub (l, t) (Tensor []) = mkTensSub (l,t)
    insertOrAddSub (l:ls,x) (Tensor (r:rs))
                | l < j = Tensor $ (l,mkTensSub (ls,x)) : r : rs
                | l == j = Tensor $ (l, insertOrAddSub (ls,x) tens) : rs
                | l > j = Tensor $ r : (getTensorList $ insertOrAddSub (l:ls,x) $ Tensor rs)
                where
                    (j,tens) = r
                    
    fromList :: (Num a) => [([Int],a)] -> Tensor a
    fromList (x:[]) = mkTens x 
    fromList (x:xs) = foldr insertOrAdd (mkTens x) xs

    fromListSub :: (Num a) => [([Int],Tensor a)] -> Tensor a
    fromListSub  (l:[]) = mkTensSub l 
    fromListSub (x:xs) = foldr insertOrAddSub (mkTensSub x) xs

    fromSeq :: (Num a) => [(S.Seq Int,a)] -> Tensor a 
    fromSeq = fromList . (map (\(k,v) -> (toList k, v))) 

    fromIMap :: (Num a) => [(I.IntMap Int,a)] -> Tensor a 
    fromIMap = fromList . (map (\(k,v) -> (I.elems k, v))) 
    
    tensorAdd :: (Num a) => Tensor a -> Tensor a -> Tensor a
    tensorAdd (Scalar x) (Scalar y) = Scalar (x+y)
    tensorAdd (Tensor []) (Tensor y) = Tensor y
    tensorAdd (Tensor x) (Tensor []) = Tensor x 
    tensorAdd (Tensor (x:xs)) (Tensor (y:ys)) 
                    | i1 < i2 = Tensor (x : (getTensorList $ tensorAdd (Tensor xs) (Tensor (y:ys))))
                    | i1 == i2 = Tensor $ (i1, tensorAdd t1 t2) : (getTensorList $ tensorAdd (Tensor xs) (Tensor ys))
                    | i1 > i2 =  Tensor (y : (getTensorList $ tensorAdd (Tensor (x:xs)) (Tensor ys)))
                     where
                        (i1,t1) = x
                        (i2,t2) = y
    
    tensorSub :: (Num a) => Tensor a -> Tensor a -> Tensor a
    tensorSub t1 t2 = tensorAdd t1 (fmap ((*) $ -1) t2)

    --append the 2nd tensor to the right of the first tensor -> fast for t1 beeing smaller than t2 

    tensorProd :: (Num a) => Tensor a -> Tensor a -> Tensor a
    tensorProd (Scalar x) t = fmap ((*) x) t
    tensorProd (Tensor l) t = Tensor $ map (\(i,v) -> (i,tensorProd v t)) l

    --without list seems to be faster

    tensorProdList :: (Num a) => [Tensor a] -> Tensor a
    tensorProdList l = foldr tensorProd (head l) (tail l)

    tensorTranspose :: (Num a) => (Int,Int) -> Tensor a -> Tensor a
    tensorTranspose (0,j) t = fromList $ map (swapHead j) $ toListT t
    tensorTranspose (i,j) (Tensor l) = Tensor $ map (\(k,v) -> (k, tensorTranspose (i-1,j-1) v)) l

    tensorTransposeSub :: (Num a) => (Int,Int) -> Tensor a -> Tensor a 
    tensorTransposeSub (0,j) t = fromListSub $ map (swapHead j) $ toListSubT j t
    tensorTransposeSub (i,j) (Tensor l) = Tensor $ map (\(k,v) -> (k, tensorTransposeSub (i-1,j-1) v)) l


    tensorTransposeSeq :: (Num a) => (Int,Int) -> Tensor a -> Tensor a
    tensorTransposeSeq (0,j) t = fromSeq $ map (swapHeadSeq j) $ toSeqT t
    tensorTransposeSeq (i,j) (Tensor l) = Tensor $ map (\(k,v) -> (k, tensorTranspose (i-1,j-1) v)) l

    tensorTransposeIMap :: (Num a) => (Int,Int) -> Tensor a -> Tensor a
    tensorTransposeIMap (0,j) t = fromIMap $ map (swapHeadIMap j) $ toIMapT t
    tensorTransposeIMap (i,j) (Tensor l) = Tensor $ map (\(k,v) -> (k, tensorTranspose (i-1,j-1) v)) l


    --swap the head of the IndList with the element at the specified position

    swapHead :: Int -> ([Int],a) -> ([Int],a)
    swapHead 1 (x:xs,v) = ((head xs) : x : (tail xs),v)
    swapHead i (x:xs,v) = ((head rest) : y ++ x : (tail rest),v)
            where 
                (y,rest) = splitAt (i-1) xs

    swapHeadSeq :: Int -> (S.Seq Int, a) -> (S.Seq Int ,a)
    swapHeadSeq i (s,v) = (S.update i x1 $ S.update 0 x2 s ,v)
        where 
            x1 = fromJust $ S.lookup 0 s
            x2 = fromJust $ S.lookup i s

    swapHeadIMap :: Int -> (I.IntMap Int, a) -> (I.IntMap Int, a)
    swapHeadIMap i (m,v) = ((I.insert i f1) . (I.insert 0 f2) $ m , v) 
            where
                f1 = (I.!) m 0
                f2 = (I.!) m i

    --filter the contraction indices 

    isContractionInd :: Int -> ([Int],a) -> Bool
    isContractionInd i ((x:xs),y) = x == (xs !! (i-1))

    isContractionIndSeq :: Int -> (S.Seq Int, a) -> Bool
    isContractionIndSeq i (s,v) = (S.index s 0) == (S.index s i)

    isContractionIndIMap :: Int -> (I.IntMap Int, a) -> Bool
    isContractionIndIMap i (m,v) = ((I.!) m 0) == ((I.!) m i)

    removeContractionInd :: Int -> ([Int],a) -> ([Int],a)
    removeContractionInd i ((x:xs),y) = (l1 ++ (tail l2),y)
                where 
                    (l1,l2) = splitAt (i-1) xs

    removeContractionIndSeq :: Int -> (S.Seq Int, a) -> (S.Seq Int, a)
    removeContractionIndSeq i (s,v) = (S.deleteAt i s, v)

    removeContractionIndIMap :: Int -> (I.IntMap Int, a) -> (I.IntMap Int, a)
    removeContractionIndIMap i (m,v) = (I.mapKeysMonotonic (\k -> if k <= i then k else k-1) $ I.delete i m, v)

    tensorContract :: (Num a) => (Int,Int) -> Tensor a -> Tensor a
    tensorContract (0,j) t = fromList $ map (removeContractionInd j) $ filter (isContractionInd j) $ toListT t
    tensorContract (i,j) (Tensor l) = Tensor (map (\(k,v) -> (k, tensorContract (i-1,j-1) v)) l)

    tensorContractSub :: (Num a) => (Int,Int) -> Tensor a -> Tensor a
    tensorContractSub (0,j) t = fromListSub $ map (removeContractionInd j) $ filter (isContractionInd j) $ toListSubT j t
    tensorContractSub (i,j) (Tensor l) = Tensor (map (\(k,v) -> (k, tensorContractSub (i-1,j-1) v)) l)

    tensorContractSeq :: (Num a) => (Int,Int) -> Tensor a -> Tensor a
    tensorContractSeq (0,j) t = fromSeq $ map (removeContractionIndSeq j) $ filter (isContractionIndSeq j) $ toSeqT t
    tensorContractSeq (i,j) (Tensor l) = Tensor (map (\(k,v) -> (k, tensorContractSeq (i-1,j-1) v)) l)

    tensorContractIMap :: (Num a) => (Int,Int) -> Tensor a -> Tensor a
    tensorContractIMap (0,j) t = fromIMap $ map (removeContractionIndIMap j) $ filter (isContractionIndIMap j) $ toIMapT t
    tensorContractIMap (i,j) (Tensor l) = Tensor (map (\(k,v) -> (k, tensorContractIMap (i-1,j-1) v)) l)

    

    deltaList :: Int -> [([Int],Rational)]
    deltaList i = [([a,a],1) | a <- [0..i]]

    triangleMap2 :: M.Map [Int] Int
    triangleMap2 = M.fromList $ zip [ [a,b] | a <- [0..3], b <- [a..3] ] [1..]

    triangleMap3 :: M.Map [Int] Int
    triangleMap3 = M.fromList $ zip [ [a,b,c] | a <- [0..3], b <- [a..3], c <- [b..3] ]  [1..]

    triangleMapArea :: M.Map [Int] Int
    triangleMapArea = M.fromList $ zip [[a,b,c,d] | a <- [0..2], b <- [a+1..3], c <- [a..2], d <- [c+1..3], not $ a == c && b > d ] [1..]
    
    jMult2 :: [Int] -> Rational 
    jMult2 [a,b] 
            | a == b = 1
            | otherwise = 1/2

    jMult3 :: [Int] -> Rational
    jMult3 [a,b,c] 
            | i == 1 = 1
            | i == 2 = 1/3
            | otherwise = 1/6
             where 
                i = length $ nub [a,b,c]

    jMultArea :: [Int] -> Rational
    jMultArea [a,b,c,d] 
                | a == c && b == d = 1/4 
                | otherwise = 1/8

    isZeroArea :: [Int] -> Bool 
    isZeroArea [a,b,c,d] = a == b || c == d 

    areaSign :: [Int] -> Rational 
    areaSign [a,b,c,d] = s1 * s2
                 where
                    s1 = pairSign a b
                    s2 = pairSign c d
                    pairSign x y = if x < y then 1 else -1

    canonicalizeArea :: [Int] -> ([Int],Rational)
    canonicalizeArea [a,b,c,d] = ([a',b',c',d'],s)
            where
                s = areaSign [a,b,c,d]
                [[a',b'],[c',d']] = sort $ map sort [[a,b],[c,d]]

    --indices are [^I,_b,_c]
    interI2 :: M.Map [Int] Int -> Tensor Rational
    interI2 trian2 = fromList $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [[a,b,c] | a <- [0..9], b <- [0..3], c <- [0..3]]
                f [a,b,c] 
                    | a == ((M.!) trian2 $ sort [b,c]) = 1 
                    | otherwise = 0 
    --indices are [_I,^b,^c]
    interJ2 :: M.Map [Int] Int -> Tensor Rational
    interJ2 trian2 = fromList $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [[a,b,c] | a <- [0..9], b <- [0..3], c <- [0..3]]
                f [a,b,c] 
                    | a == ((M.!) trian2 $ sort [b,c]) = jMult2 [b,c]
                    | otherwise = 0 
    --indices are [^K,_b,_c,_d]
    interI3 :: M.Map [Int] Int -> Tensor Rational
    interI3 trian3 = fromList $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [[a,b,c,d] | a <- [0..19], b <- [0..3], c <- [0..3], d <- [0..3]]
                f [a,b,c,d] 
                    | a == ((M.!) trian3 $ sort [b,c,d]) = 1 
                    | otherwise = 0 
    --indices are [_K,^b,^c,^d]
    interJ3 :: M.Map [Int] Int -> Tensor Rational
    interJ3 trian3 = fromList $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = [[a,b,c,d] | a <- [0..19], b <- [0..3], c <- [0..3], d <- [0..3]]
                f [a,b,c,d] 
                    | a == ((M.!) trian3 $ sort [b,c,d]) = jMult2 [b,c,d]
                    | otherwise = 0 
    --indices are [^A,_b,_c,_d,_e]
    interIArea :: M.Map [Int] Int -> Tensor Rational 
    interIArea trianArea = fromList $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = filter (\[a,b,c,d,e] -> (not $ isZeroArea [b,c,d,e])) [[a,b,c,d,e] | a <- [0..20], b <- [0..3], c <- [0..3], d <- [0..3], e <- [0..3]]
                f [a,b,c,d,e] 
                    | a == ((M.!) trianArea $ fst $ canonicalizeArea [b,c,d,e]) = snd $ canonicalizeArea [b,c,d,e]
                    | otherwise = 0
    --indices are [_A,^b,^c,^d,^e]
    interJArea :: M.Map [Int] Int -> Tensor Rational 
    interJArea trianArea = fromList $ filter (\(i,k) -> k /= 0) $ map (\x -> (x,f x)) inds
            where
                inds = filter (\[a,b,c,d,e] -> (not $ isZeroArea [b,c,d,e])) [[a,b,c,d,e] | a <- [0..20], b <- [0..3], c <- [0..3], d <- [0..3], e <- [0..3]]
                f [a,b,c,d,e] 
                    | a == ((M.!) trianArea cArea) = (jMultArea cArea) * s
                    | otherwise = 0
                     where 
                        (cArea,s) = canonicalizeArea [b,c,d,e]
    --indices are [^I,_n,_J,^m]
    interMetric :: M.Map [Int] Int -> Tensor Rational
    interMetric trian2 = fmap ((*) (-2)) $ tensorContract (2,5) prod
            where
                t1 = interI2 trian2 
                t2 = interJ2 trian2 
                prod = tensorProd t1 t2 
    --indices are [^A,_n,_B,^m]
    interArea :: M.Map [Int] Int -> Tensor Rational
    interArea trianArea = fmap ((*) (-4)) $ tensorContract (2,5) $ tensorContract (3,7) $ tensorContract (4,9) prod
            where
                t1 = interIArea trianArea 
                t2 = interJArea trianArea 
                prod = tensorProd t1 t2 
     --indices are [^A,_n,_B,^m,^p,_q]
    interEqn2 :: M.Map [Int] Int -> Tensor Rational
    interEqn2 trianArea = tensorSub int1 int2
            where
                delta3 = fromList $ deltaList 3
                delta20 = fromList $ deltaList 20
                intArea = interArea trianArea
                int1 = tensorProd intArea delta3
                int2 = tensorTranspose (3,4) $ tensorTranspose (1,2) $ tensorProd delta20 $ tensorProd delta3 delta3 

    --indices are [^A,_n,_B,^m,^J,_I]
    interEqn3 :: M.Map [Int] Int -> M.Map [Int] Int -> Tensor Rational
    interEqn3 trian2 trianArea = tensorAdd int1 int2
            where
                delta9 = fromList $ deltaList 9
                delta20 = fromList $ deltaList 20
                intMetric = interMetric trian2 
                intArea = interArea trianArea
                int1 = tensorProd intArea delta9 
                int2 = tensorTranspose (0,4) $ tensorTranspose (2,5) $ tensorProd intMetric delta20

    flatArea :: Tensor Rational 
    flatArea = fromList [([0],-1),([5],-1),([6],-1),([9],1),([11],-1),([12],-1),([15],1),([18],1),([20],1)]

    --indices are [_n,_B,^m]
    flatInter :: M.Map [Int] Int -> Tensor Rational
    flatInter trianArea = tensorContract (0,4) prod
            where
                intArea = interArea trianArea 
                prod = tensorProd intArea flatArea 

    --indices are 
    intAIB :: M.Map [Int] Int -> M.Map [Int] Int -> Tensor Rational
    intAIB trian2 trianArea = tensorSub prod tensorTrans
            where
                delta3 = fromList $ deltaList 3
                delta9 = fromList $ deltaList 9
                delta20 = fromList $ deltaList 20
                intArea = interArea trianArea 
                int3 = interEqn3 trian2 trianArea
                flatInt = flatInter trianArea 
                block1 = tensorTranspose (1,2) $ tensorProd delta20 $ tensorProd delta3 $ tensorProd delta9 delta20 
                block2 = tensorProd intArea $ tensorProd delta9 delta20 
                block3 = tensorProd int3 delta20 
                totalBlock = tensorAdd block1 $ tensorAdd block2 block3
                prod = tensorContract (0,9) $ tensorProd totalBlock flatInt 
                tensorTrans = tensorTranspose (0,7) $ tensorTranspose (2,8) $ prod


    intAIBSub :: M.Map [Int] Int -> M.Map [Int] Int -> Tensor Rational
    intAIBSub trian2 trianArea = tensorSub prod tensorTrans
            where
                delta3 = fromList $ deltaList 3
                delta9 = fromList $ deltaList 9
                delta20 = fromList $ deltaList 20
                intArea = interArea trianArea 
                int3 = interEqn3 trian2 trianArea
                flatInt = flatInter trianArea 
                block1 = tensorTransposeSub (1,2) $ tensorProd delta20 $ tensorProd delta3 $ tensorProd delta9 delta20 
                block2 = tensorProd intArea $ tensorProd delta9 delta20 
                block3 = tensorProd int3 delta20 
                totalBlock = tensorAdd block1 $ tensorAdd block2 block3
                prod = tensorContractSub (0,9) $ tensorProd totalBlock flatInt 
                tensorTrans = tensorTransposeSub (0,7) $ tensorTransposeSub (2,8) $ prod


    intAIBSeq :: M.Map [Int] Int -> M.Map [Int] Int -> Tensor Rational
    intAIBSeq trian2 trianArea = tensorSub prod tensorTrans
            where
                delta3 = fromList $ deltaList 3
                delta9 = fromList $ deltaList 9
                delta20 = fromList $ deltaList 20
                intArea = interArea trianArea 
                int3 = interEqn3 trian2 trianArea
                flatInt = flatInter trianArea 
                block1 = tensorTransposeSeq (7,8) $ tensorProd delta20 $ tensorProd delta3 $ tensorProd delta9 delta20 
                block2 = tensorProd intArea $ tensorProd delta9 delta20 
                block3 = tensorProd int3 delta20 
                totalBlock = tensorAdd block1 $ tensorAdd block2 block3
                prod = tensorContractSeq (0,9) $ tensorProd totalBlock flatInt 
                tensorTrans = tensorTransposeSeq (0,7) $ tensorTransposeSeq (2,8) $ prod
        
    intAIBIMap :: M.Map [Int] Int -> M.Map [Int] Int -> Tensor Rational
    intAIBIMap trian2 trianArea = tensorSub prod tensorTrans
            where
                delta3 = fromList $ deltaList 3
                delta9 = fromList $ deltaList 9
                delta20 = fromList $ deltaList 20
                intArea = interArea trianArea 
                int3 = interEqn3 trian2 trianArea
                flatInt = flatInter trianArea 
                block1 = tensorTransposeIMap (1,2) $ tensorProd delta20 $ tensorProd delta3 $ tensorProd delta9 delta20 
                block2 = tensorProd intArea $ tensorProd delta9 delta20 
                block3 = tensorProd int3 delta20 
                totalBlock = tensorAdd block1 $ tensorAdd block2 block3
                prod = tensorContractIMap (0,9) $ tensorProd totalBlock flatInt 
                tensorTrans = tensorTransposeIMap (0,7) $ tensorTransposeIMap (2,8) $ prod

    mkMatrixIndAIB :: [Int] -> (Int,Int)
    mkMatrixIndAIB [n,a,m,j,i,c,b,s,r] = ((c)*10*4^4+(j)*4^4+(m)*4^3+(n)*4^2+(r)*4+s,(a)*21*10+(b)*10+i+1) 

    mkMatrixIndInter3 :: [Int] -> (Int,Int)
    mkMatrixIndInter3 [a,n,b,m,j,i] = (1,a*400*21^2+b*400*21+j*400+i*40+m*4+n+1)

    mkMatrixIndInterArea :: [Int] -> (Int,Int)
    mkMatrixIndInterArea [a,n,b,m] = (1,a*21*16+b*16+m*4+n+1)

    mkMatrixIndInterMetric :: [Int] -> (Int,Int)
    mkMatrixIndInterMetric [i,n,j,m] = (1,i*10*16+j*16+m*4+n+1)

    showTensorFrac :: (Show a, Num a, Eq a) => ([Int] -> (Int,Int)) -> Tensor a -> String
    showTensorFrac f t = unlines l2
                        where
                            l1 = toListT $ filterT (/= 0) t 
                            l2 = map (\(x,y) -> let (i, j) = f x
                                                in show i ++ " " ++ show j ++ " " ++ show y) l1

            

    intAIBJC :: M.Map [Int] Int -> M.Map [Int] Int -> Tensor Rational 
    intAIBJC trian2 trianArea = tensorSub tens tensTrans
            where
                delta20 = fromList $ deltaList 20
                delta9 = fromList $ deltaList 9
                delta3 = fromList $ deltaList 3 
                intArea = interArea trianArea
                flatInt = flatInter trianArea
                int3 = tensorTranspose (3,5) $ tensorTranspose (2,4) $ tensorTranspose (1,2) $ interEqn3 trian2 trianArea
                block1 = tensorProd delta20 $ tensorProd delta20 $ tensorProd delta9 $ tensorProd delta9 delta3 
                block2 = tensorProd delta20 $ tensorProd delta9 int3 
                block3 = tensorTranspose (7,9) $ tensorTranspose (6,8) $ tensorTranspose (5,7) $ tensorTranspose (4,6) $ tensorTranspose (3,7) $
                         tensorTranspose (2,6) $ tensorProd int3 $ tensorProd delta20 delta9 
                sumBlock = tensorAdd block1 $ tensorAdd block2 block3 
                prod = tensorProd sumBlock flatInt
                block0 = tensorProd delta20 $ tensorProd delta9 $ tensorProd delta20 $ tensorProd delta9 $ tensorTranspose (2,3) $ tensorTranspose (1,2) $ tensorContract (0,1) $ tensorProd intArea flatInt
                tens = tensorAdd block0 prod 
                tensTrans = tensorTranspose (9,12) $ tensorTranspose (8,10) $ tens 

    mkMatrixIndAIBJC :: M.Map [Int] Int -> [Int] -> (Int,Int)
    mkMatrixIndAIBJC trianMap [e,a,k,i,f,b,l,j,m,n,r,c,s] = (e*21*10^2*4^4+f*10^2*4^4+k*10*4^4+l*4^4+m*4^3+r*4^2+n*4+s+1,316+ div (315*316) 2 + x)
                where
                    x = (M.!) trianMap [c,105+a',105+b']
                    [a',b'] = [min ((a-1)*10+i) ((b-1)*10+j) , max ((a-1)*10+i) ((b-1)*10+j)]

    triangleMap2P :: Int -> M.Map [Int] Int 
    triangleMap2P i = M.fromList $ zip j k
                    where
                        j = [ [a,b] | a <- [1..i], b <- [a..i] ]
                        k = [1..]

    triangleMap3P :: Int -> M.Map [Int] Int
    triangleMap3P i = M.fromList $ zip j k
                    where
                        j = [ [a,b,c] | a <- [1..i], b <- [a..i], c <- [b..i] ]
                        k = [1..]

    triangleMap :: Int -> M.Map [Int] Int
    triangleMap i = M.union (triangleMap2P i) (triangleMap3P i)
