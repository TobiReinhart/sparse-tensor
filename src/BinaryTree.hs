module BinaryTree (
    BiTree(..), foldrTreeWithKey, toAscList, fromAscList, fromAscListWithLength, fromListTree, balanceTree, isBalancedTree, maxDepth, minDepth, 
    insertTree, insertTreeWith, insertTreeWithKey, insertTreeWithKeyMaybe, insertTreeWithMaybe, filterTree, isValidTree, lookupTree,
    unionTreeWithKey, unionTreeWith, unionTree, unionTreeWithKeyMaybe, unionTreeWithMaybe, treeSize, balanceFactor, mapKeysTree, mapElemsTree, insertTreeWithSwapped,
    mapElemsWithKeyTree

) where

    import Data.Foldable
    import Data.List
    import Data.Maybe

    data BiTree a b = BiTree a b (BiTree a b) (BiTree a b) | Leaf a b | EmptyTree deriving (Eq, Ord, Show)

    getNodeVal :: BiTree a b -> (a,b)
    getNodeVal (Leaf a b) = (a,b)
    getNodeVal (BiTree a b _ _) = (a,b)

    treeSize :: BiTree a b -> Rational 
    treeSize EmptyTree = 0 
    treeSize (Leaf _ _) = 1
    treeSize (BiTree _ _ left right) = 1 + (treeSize left) + (treeSize right)

    noFalseEmpty :: BiTree a b -> Bool
    noFalseEmpty (BiTree a b EmptyTree EmptyTree) = False
    noFalseEmpty t = True 

    instance Functor (BiTree a) where
        fmap f EmptyTree = EmptyTree
        fmap f (Leaf x y)  = Leaf x (f y) 
        fmap f (BiTree x y left right) = BiTree x (f y) (fmap f left) (fmap f right)

    instance Foldable (BiTree a) where
        foldMap f EmptyTree = mempty 
        foldMap f (Leaf x y) = f y
        foldMap f (BiTree x y left right) = foldMap f left `mappend` f y `mappend` foldMap f right

    foldrTreeWithKey :: (a -> b -> c -> c) -> c -> BiTree a b -> c 
    foldrTreeWithKey f x EmptyTree = x 
    foldrTreeWithKey f x (Leaf a b) = f a b x 
    foldrTreeWithKey f x (BiTree a b left right) = foldrTreeWithKey f (f a b $ foldrTreeWithKey f x right) left 

    toAscList :: BiTree a b -> [(a,b)]
    toAscList = foldrTreeWithKey (\ a b c -> (a,b) : c ) [] 

    toAscListWithLength :: BiTree a b -> ([(a,b)], Int)
    toAscListWithLength = foldrTreeWithKey (\ a b (c1,c2) -> ((a,b) : c1, c2 +1) ) ([],0) 

    fromAscListWithLength :: Int -> [(a,b)] -> BiTree a b
    fromAscListWithLength _ [] = EmptyTree 
    fromAscListWithLength _ [(x,y)] = Leaf x y
    fromAscListWithLength s l = BiTree x1 x2 (fromAscListWithLength s1 l1) (fromAscListWithLength s2 $ tail l2)
            where
                s1 = quot s 2 
                s2 = s - s1 -1 
                (l1,l2) = splitAt s1 l 
                (x1,x2) = head l2 

    fromAscList :: [(a,b)] -> BiTree a b 
    fromAscList l = fromAscListWithLength (length l) l

    --from unsorted list -> sort list first

    fromListTree :: Ord a => [(a,b)] -> BiTree a b
    fromListTree = fromAscList . sortOn fst 

    balanceTree :: Ord a => BiTree a b -> BiTree a b 
    balanceTree t = fromAscListWithLength s l
                where
                    (l,s) = toAscListWithLength t

    maxDepth :: BiTree a b -> Rational 
    maxDepth EmptyTree = 0 
    maxDepth (Leaf _ _) = 1 
    maxDepth (BiTree _  _ left right) = 1 + (max (maxDepth left) (maxDepth right))

    minDepth :: BiTree a b -> Rational 
    minDepth EmptyTree = 0 
    minDepth (Leaf _ _) = 1 
    minDepth (BiTree _ _ left right) = 1 + (min (minDepth left) (minDepth right))

    balanceFactor ::BiTree a b -> Rational 
    balanceFactor t = ((maxDepth t) - (minDepth t))

    isBalancedTree :: BiTree a b -> Bool 
    isBalancedTree t = ((maxDepth t) - (minDepth t)) <= 1

    insertTree :: Ord a => a -> b -> BiTree a b -> BiTree a b 
    insertTree = insertTreeWithKey (\ _ _ c -> c)

    insertTreeWith :: Ord a => (b -> b -> b) -> a -> b -> BiTree a b -> BiTree a b
    insertTreeWith f = insertTreeWithKey (\ _ x y -> f x y)

    insertTreeWithKey :: Ord a => (a -> b -> b -> b) -> a -> b -> BiTree a b -> BiTree a b
    insertTreeWithKey f a b  EmptyTree = Leaf a b 
    insertTreeWithKey f a b (Leaf x y) 
                | a < x = BiTree x y (Leaf a b) EmptyTree 
                | a == x = Leaf a (f a b y)
                | otherwise = BiTree x y EmptyTree (Leaf a b) 
    insertTreeWithKey f a b (BiTree x y left right) 
                | a < x = BiTree x y (insertTreeWithKey f a b left) right 
                | a == x = BiTree x (f x b y) left right  
                | otherwise = BiTree x y left (insertTreeWithKey f a b right)

    --only insert if f yields a just value -> if f yileds nothing (e.g. zeros in addition) whole subtree is deleted


    insertTreeWithMaybe :: Ord a => (b -> b -> Maybe b) -> a -> b -> BiTree a b -> BiTree a b
    insertTreeWithMaybe f = insertTreeWithKeyMaybe (\ _ x y -> f x y)


    insertTreeWithKeyMaybe :: Ord a => (a -> b -> b -> Maybe b) -> a -> b -> BiTree a b -> BiTree a b 
    insertTreeWithKeyMaybe f a b  EmptyTree = Leaf a b 
    insertTreeWithKeyMaybe f a b (Leaf x y) 
                | a < x = BiTree x y (Leaf a b) EmptyTree 
                | a == x = if isJust val then Leaf x (fromJust val) else EmptyTree 
                | otherwise = BiTree x y EmptyTree (Leaf a b)
                 where
                    val = f a b y 
    insertTreeWithKeyMaybe f a b (BiTree x y left right) 
                | a < x = BiTree x y (insertTreeWithKeyMaybe f a b left) right 
                | a == x = if isJust val then BiTree x (fromJust val) left right else removeTopNode (BiTree x y left right)
                | otherwise = BiTree x y left (insertTreeWithKeyMaybe f a b right)
                 where
                    val = f a b y 

    insertTreeWithSwapped :: Ord a => (c -> b) -> (c -> b -> b) -> a -> c -> BiTree a b -> BiTree a b 
    insertTreeWithSwapped f g a c EmptyTree = Leaf a (f c)
    insertTreeWithSwapped f g a c (Leaf x y)
                | a < x = BiTree x y (Leaf a (f c)) EmptyTree 
                | a == x = Leaf a (g c y)
                | otherwise = BiTree x y EmptyTree (Leaf a (f c)) 
    insertTreeWithSwapped f g a c (BiTree x y left right) 
                | a < x = BiTree x y (insertTreeWithSwapped f g a c left) right 
                | a == x = BiTree x (g c y) left right  
                | otherwise = BiTree x y left (insertTreeWithSwapped f g a c right)

    removeTopNode :: BiTree a b -> BiTree a b 
    removeTopNode (Leaf _ _) = EmptyTree 
    removeTopNode (BiTree _ _ EmptyTree t) = t
    removeTopNode (BiTree _ _ t EmptyTree) = t
    removeTopNode (BiTree _ _ left right) = BiTree (fst t1) (snd t1) left t2 
                    where
                        (t1,t2) = lookUpDeleteMin right

    lookUpDeleteMin :: BiTree a b -> ((a,b), BiTree a b)
    lookUpDeleteMin (Leaf x y) = ((x,y), EmptyTree)
    lookUpDeleteMin (BiTree x y EmptyTree right) = ((x,y), right) 
    lookUpDeleteMin (BiTree x y left right) = (t1,BiTree x y (t2) right)
                    where
                        (t1,t2) = lookUpDeleteMin left 

    lookupTree :: Ord a => a -> BiTree a b -> Maybe b 
    lookupTree k EmptyTree = Nothing 
    lookupTree k (Leaf a b) 
                | a == k = Just b 
                | otherwise = Nothing 
    lookupTree k (BiTree a b left right) 
                | a == k = Just b 
                | k < a = lookupTree k left 
                | otherwise = lookupTree k right 

    mapElemsTree :: (b -> b) -> BiTree a b -> BiTree a b
    mapElemsTree = fmap  

    mapElemsWithKeyTree :: (a -> b -> b) -> BiTree a b -> BiTree a b
    mapElemsWithKeyTree f EmptyTree = EmptyTree
    mapElemsWithKeyTree f (Leaf x y)  = Leaf x (f x y) 
    mapElemsWithKeyTree f (BiTree x y left right) = BiTree x (f x y) (mapElemsWithKeyTree f left) (mapElemsWithKeyTree f right)


    --does not rebalance or resort tree -> only makes sense for monotonic maps

    mapKeysTree :: (a -> a) -> BiTree a b -> BiTree a b 
    mapKeysTree f EmptyTree = EmptyTree 
    mapKeysTree f (Leaf a b) = Leaf (f a) b 
    mapKeysTree f (BiTree a b left right) = BiTree (f a) b (mapKeysTree f left) (mapKeysTree f right)

    mapAllTree :: (a -> a) -> (b -> b) -> BiTree a b -> BiTree a b 
    mapAllTree f g EmptyTree = EmptyTree
    mapAllTree f g (Leaf a b) =  Leaf (f a) (g b)
    mapAlltree f g (BiTree a b left right) = BiTree (f a) (g b) (mapAllTree f g left) (mapAllTree f g right)

    filterTree :: (b -> Bool) -> BiTree a b -> BiTree a b 
    filterTree f t = fromAscList $ filter (f.snd) $ toAscList t
                        

    isValidTree :: Ord a => BiTree a b -> Bool 
    isValidTree EmptyTree = True 
    isValidTree (Leaf _ _) = True
    isValidTree (BiTree x y left right) = isValidTree left && isValidTree right && leftOrd left && rightOrd right && noFalseEmpty (BiTree x y left right)
            where
                leftOrd EmptyTree = True 
                leftOrd t = and $ map (\a -> x > fst a) $ toAscList t 
                rightOrd EmptyTree = True 
                rightOrd t = and $ map (\a -> x < fst a) $ toAscList t 

    unionTree :: Ord a => BiTree a b -> BiTree a b -> BiTree a b
    unionTree = unionTreeWithKey (\ _ _ b -> b)

    unionTreeWith :: Ord a => (b -> b -> b) -> BiTree a b -> BiTree a b -> BiTree a b
    unionTreeWith f = unionTreeWithKey (\ _ a b -> f a b)

    unionTreeWithKey :: Ord a => (a -> b -> b -> b) -> BiTree a b -> BiTree a b -> BiTree a b 
    unionTreeWithKey f t EmptyTree = t 
    unionTreeWithKey f EmptyTree t = t 
    unionTreeWithKey f (Leaf x y) t = insertTreeWithKey f x y t 
    unionTreeWithKey f t (Leaf x y) = insertTreeWithKey f x y t 
    unionTreeWithKey f (BiTree a b left1 right1) (BiTree x y left2 right2) 
                | a == x = BiTree a (f a b y) (unionTreeWithKey f left1 left2) (unionTreeWithKey f right1 right2) 
                | a < x = unionTreeWithKey f right1 $ BiTree x y (unionTreeWithKey f (BiTree a b left1 EmptyTree) left2) right2 
                | otherwise = unionTreeWithKey f left1 $ BiTree x y left2 (unionTreeWithKey f (BiTree a b EmptyTree right1) right2)

    unionTreeWithKeyMaybe :: Ord a => (a -> b -> b -> Maybe b) -> BiTree a b -> BiTree a b -> BiTree a b 
    unionTreeWithKeyMaybe f t EmptyTree = t 
    unionTreeWithKeyMaybe f EmptyTree t = t 
    unionTreeWithKeyMaybe f (Leaf x y) t = insertTreeWithKeyMaybe f x y t 
    unionTreeWithKeyMaybe f t (Leaf x y) = insertTreeWithKeyMaybe f x y t 
    unionTreeWithKeyMaybe f (BiTree a b left1 right1) (BiTree x y left2 right2) 
                | a == x = if isJust val then BiTree a (fromJust val) (unionTreeWithKeyMaybe f left1 left2) (unionTreeWithKeyMaybe f right1 right2)
                           else removeTopNode $ BiTree x y (unionTreeWithKeyMaybe f left1 left2) (unionTreeWithKeyMaybe f right1 right2)
                | a < x = unionTreeWithKeyMaybe f right1 $ BiTree x y (unionTreeWithKeyMaybe f (BiTree a b left1 EmptyTree) left2) right2 
                | otherwise = unionTreeWithKeyMaybe f left1 $ BiTree x y left2 (unionTreeWithKeyMaybe f (BiTree a b EmptyTree right1) right2)
                    where
                        val = f a b y 

    unionTreeWithMaybe :: Ord a => (b -> b -> Maybe b) -> BiTree a b -> BiTree a b -> BiTree a b
    unionTreeWithMaybe f = unionTreeWithKeyMaybe (\ _ a b -> f a b)

    
    

    

    

            

    
    

    