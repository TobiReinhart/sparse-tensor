 --pushes type stuff to kind stuff (prefixed with ')
 {-# LANGUAGE DataKinds #-}
 --matching on type constructors
 {-# LANGUAGE GADTs #-}
 --kind signature
 {-# LANGUAGE KindSignatures #-}
 --type family definitions
 {-# LANGUAGE TypeFamilies #-}
 {-# LANGUAGE UndecidableInstances #-}
 --infix type plus and mult
 {-# LANGUAGE TypeOperators #-}

 {-# LANGUAGE ScopedTypeVariables #-}
 {-# LANGUAGE TypeApplications #-}



module Index (
    Ind 

) where


    import Data.List
    import qualified Data.Sequence as S
    import Numeric.Natural 
    import GHC.TypeNats
    import Data.Proxy
    import Data.Maybe
    

    --length indexed sequence data type

    data Ind (n::Nat) a = UnsafemkInd (S.Seq a) deriving Show

    --we need a smart (value-) constructor (fromList ?)

    mkInd :: forall n a. KnownNat n => (S.Seq a) -> Ind n a
    mkInd l 
            | S.length l == nat = UnsafemkInd l
            | otherwise = error "list has wrong number of elems"
                where
                    nat = fromIntegral $ natVal (Proxy @n)

    --now we can write all the functions we need for the indices

    --start with the symmetrizers 

    swapPosSeq :: (Int,Int) -> (S.Seq a) -> (S.Seq a)
    swapPosSeq (i,j) s = S.update j x1 $ S.update i x2 s 
            where 
                x1 = fromJust $ S.lookup i s
                x2 = fromJust $ S.lookup j s

    swapBlockPosSeq :: ([Int],[Int]) -> (S.Seq a) -> (S.Seq a)
    swapBlockPosSeq (i,j) s 
            | length i /= length j = error "only blocks with the same lenght can be symmetrized"
            | otherwise = foldr swapPosSeq s pairList
                where
                    pairList = zip i j

    --now the same for Inds (no need for safe constructors as the length is always unchanged)

    swapPosInd :: KnownNat n => (Int, Int) -> Ind n a -> Ind n a 
    swapPosInd pair (UnsafemkInd s) = UnsafemkInd $ swapPosSeq pair s 
    
    swapBlockPosInd :: KnownNat n => ([Int],[Int]) -> Ind n a -> Ind n a
    swapBlockPosInd pair (UnsafemkInd s) = UnsafemkInd $ swapBlockPosSeq pair s 

    --now the cyclic symmetries

    --only works if the two [Int]s have the same values stored (e.g the second one is a permutation of the first one)
    update1Cycle :: ([Int],[Int]) -> (S.Seq a) -> (S.Seq a)
    update1Cycle (i,j) s = foldr updateS s updateList 
            where 
                updateList = zip i $ map (fromJust.((S.!?) s)) j
                updateS = \x -> S.update (fst x) (snd x) 

    cyclicSwapSeq :: [Int] -> (S.Seq a) -> [(S.Seq a)]
    cyclicSwapSeq l s = s : ( map (\x -> update1Cycle x s) cList )
            where 
                perm = tail $ permutations l 
                cList = zip (repeat l) perm 

    cyclicSwapInd :: KnownNat n => [Int] -> Ind n a -> [Ind n a]
    cyclicSwapInd l (UnsafemkInd s) = map UnsafemkInd $ cyclicSwapSeq l s

    --these are all symmetrizers that we need!

    --removing and inserting of an element is already implemented in Data.Seq (push this to Ind)

    delInd :: KnownNat n => Int -> Ind (n+1) a -> Ind n a
    delInd i (UnsafemkInd s) = mkInd $ S.deleteAt i s

    --if the insert position is to high the elem is inserted at the end

    insInd :: KnownNat n => Int -> a -> Ind n a -> Ind (n+1) a
    insInd i x (UnsafemkInd s) = UnsafemkInd $ S.insertAt i x s 

    repInd :: KnownNat n => Int -> a -> Ind n a -> Ind n a 
    repInd i x (UnsafemkInd s) = UnsafemkInd $ S.update i x s 

    --now the contraction Index (Index after contraction)

    contractionInd :: (KnownNat n, KnownNat m) => (Int,Int) -> (Ind (n+1) a, Ind (m+1) a) -> (Ind n a, Ind m a)
    contractionInd (i,j) (ind1, ind2) = (delInd i ind1, delInd j ind2)

    --to proceedfurther we need to define the concrete data types for the 8 different indices needed

    --spacetimeindeices take values from 0 to 3

    data Uind_3 = U0_3 | U1_3 | U2_3 | U3_3 deriving (Enum, Eq, Ord, Show)

    data Lind_3 = L0_3 | L1_3 | L2_3 | L3_3 deriving (Enum, Eq, Ord, Show)

    --symmetric 2nd derivative indices have values from 0 to 9

    data Uind_9 = U0_9 | U1_9 | U2_9 | U3_9 | U4_9 | U5_9 | U6_9 | U7_9 | U8_9 | U9_9 deriving (Enum, Eq, Ord, Show)

    data Lind_9 = L0_9 | L1_9 | L2_9 | L3_9 | L4_9 | L5_9 | L6_9 | L7_9 | L8_9 | L9_9 deriving (Enum, Eq, Ord, Show)

    --symmetric 3rd derivative indices have values from 0 to 10

    data Uind_19 = U0_19 | U1_19 | U2_19 | U3_19 | U4_19 | U5_19 | U6_19 | U7_19 | U8_19 | U9_19 | U10_19 | U11_19 | U12_19 | U13_19 | U14_19 | U15_19 | U16_19 | U17_19 | U18_19 | U19_19 deriving (Enum, Eq, Ord, Show)

    data Lind_19 = L0_19 | L1_19 | L2_19 | L3_19 | L4_19 | L5_19 | L6_19 | L7_19 | L8_19 | L9_19 | L10_19 | L11_19 | L12_19 | L13_19 | L14_19 | L15_19 | L16_19 | L17_19 | L18_19 | L19_19 deriving (Enum, Eq, Ord, Show)

    --AreaMetric DOF indices have values form 0 to 20

    data Uind_20 = U0_20 | U1_20 | U2_20 | U3_20 | U4_20 | U5_20 | U6_20 | U7_20 | U8_20 | U9_20 | U10_20 | U11_20 | U12_20 | U13_20 | U14_20 | U15_20 | U16_20 | U17_20 | U18_20 | U19_20 | U20_20 deriving (Enum, Eq, Ord, Show)

    data Lind_20 = L0_20 | L1_20 | L2_20 | L3_20 | L4_20 | L5_20 | L6_20 | L7_20 | L8_20 | L9_20 | L10_20 | L11_20 | L12_20 | L13_20 | L14_20 | L15_20 | L16_20 | L17_20 | L18_20 | L19_20 | L20_20 deriving (Enum, Eq, Ord, Show)

    
    --now use these types to construct an index type

    type Uinds_3 (n::Nat) = Ind n Uind_3
    
    type Linds_3 (n::Nat) = Ind n Lind_3
    
    type Uinds_9 (n::Nat) = Ind n Uind_9

    type Linds_9 (n::Nat) = Ind n Lind_9

    type Uinds_19 (n::Nat) = Ind n Uind_19

    type Linds_19 (n::Nat) = Ind n Lind_19 

    type Uinds_20 (n::Nat) = Ind n Uind_20

    type Linds_20 (n::Nat) = Ind n Lind_20

    --now the general Index data types

    type Index (n1::Nat) (n2::Nat) (n3::Nat) (n4::Nat) (n5::Nat) (n6::Nat) (n7::Nat) (n8::Nat) =  
        (Uinds_3 n1, Linds_3 n2, Uinds_9 n3, Linds_9 n4, Uinds_19 n5, Linds_19 n6, Uinds_20 n7, Linds_20 n8) 

    --we need constructors for Index

    indexList :: (KnownNat n1, KnownNat n2, KnownNat n3, KnownNat n4, KnownNat n5, KnownNat n6, KnownNat n7, KnownNat n8) => 
        [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Index n1 n2 n3 n4 n5 n6 n7 n8
    indexList a1 b1 c1 d1 e1 f1 g1 h1 = (a2,b2,c2,d2,e2,f2,g2,h2)
            where 
                a2 = mkInd $ S.fromList $ map toEnum a1
                b2 = mkInd $ S.fromList $ map toEnum b1
                c2 = mkInd $ S.fromList $ map toEnum c1
                d2 = mkInd $ S.fromList $ map toEnum d1
                e2 = mkInd $ S.fromList $ map toEnum e1
                f2 = mkInd $ S.fromList $ map toEnum f1
                g2 = mkInd $ S.fromList $ map toEnum g1
                h2 = mkInd $ S.fromList $ map toEnum h1

    --we need to push all functions for Ind to the IndexLevel (must do all cases individually)

    swapPosIndex :: (KnownNat n1, KnownNat n2, KnownNat n3, KnownNat n4, KnownNat n5, KnownNat n6, KnownNat n7, KnownNat n8) =>
               Int -> (Int,Int) -> (Index n1 n2 n3 n4 n5 n6 n7 n8) -> Index n1 n2 n3 n4 n5 n6 n7 n8
    swapPosIndex i j (a,b,c,d,e,f,g,h) 
                | i == 1 = (swapPosInd j a, b, c, d, e, f, g, h)
                | i == 2 = (a, swapPosInd j b, c, d, e, f, g, h)
                | i == 3 = (a, b, swapPosInd j c, d, e, f, g, h)
                | i == 4 = (a, b, c, swapPosInd j d, e, f, g, h)
                | i == 5 = (a, b, c, d, swapPosInd j e, f, g, h)
                | i == 6 = (a, b, c, d, e, swapPosInd j f, g, h)
                | i == 7 = (a, b, c, d, e, f, swapPosInd j g, h)
                | i == 8 = (a, b, c, d, e, f, g, swapPosInd j h)
                | otherwise = error "specified index position must be an int between 1 and 8"

                
    swapBlockPosIndex :: (KnownNat n1, KnownNat n2, KnownNat n3, KnownNat n4, KnownNat n5, KnownNat n6, KnownNat n7, KnownNat n8) =>
               Int -> ([Int],[Int]) -> (Index n1 n2 n3 n4 n5 n6 n7 n8) -> Index n1 n2 n3 n4 n5 n6 n7 n8
    swapBlockPosIndex i j (a,b,c,d,e,f,g,h) 
                | i == 1 = (swapBlockPosInd j a, b, c, d, e, f, g, h)
                | i == 2 = (a, swapBlockPosInd j b, c, d, e, f, g, h)
                | i == 3 = (a, b, swapBlockPosInd j c, d, e, f, g, h)
                | i == 4 = (a, b, c, swapBlockPosInd j d, e, f, g, h)
                | i == 5 = (a, b, c, d, swapBlockPosInd j e, f, g, h)
                | i == 6 = (a, b, c, d, e, swapBlockPosInd j f, g, h)
                | i == 7 = (a, b, c, d, e, f, swapBlockPosInd j g, h)
                | i == 8 = (a, b, c, d, e, f, g, swapBlockPosInd j h)
                | otherwise = error "specified index position must be an int between 1 and 8"

    
    cyclicSwapIndex :: (KnownNat n1, KnownNat n2, KnownNat n3, KnownNat n4, KnownNat n5, KnownNat n6, KnownNat n7, KnownNat n8) =>
                Int -> [Int] -> (Index n1 n2 n3 n4 n5 n6 n7 n8) -> [Index n1 n2 n3 n4 n5 n6 n7 n8]
    cyclicSwapIndex i j (a,b,c,d,e,f,g,h) 
                 | i == 1 = map (\x -> (x,b,c,d,e,f,g,h)) (cyclicSwapInd j a)
                 | i == 2 = map (\x -> (a,x,c,d,e,f,g,h)) (cyclicSwapInd j b)
                 | i == 3 = map (\x -> (a,b,x,d,e,f,g,h)) (cyclicSwapInd j c)
                 | i == 4 = map (\x -> (a,b,c,x,e,f,g,h)) (cyclicSwapInd j d)
                 | i == 5 = map (\x -> (a,b,c,d,x,f,g,h)) (cyclicSwapInd j e)
                 | i == 6 = map (\x -> (a,b,c,d,e,x,g,h)) (cyclicSwapInd j f)
                 | i == 7 = map (\x -> (a,b,c,d,e,f,x,h)) (cyclicSwapInd j g)
                 | i == 8 = map (\x -> (a,b,c,d,e,f,g,x)) (cyclicSwapInd j h)
                 | otherwise = error "specified index position must be an int between 1 and 8"

--there are several other functions we might need to add later






    