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






module TensorInt (
    
) where
    import Index
    import qualified Data.IntMap.Strict as I
    import Numeric.Natural 
    import GHC.TypeNats
    import Data.Proxy
    import Data.Maybe
    import Data.Foldable
    import qualified Data.Sequence as S

    --convert index to "hash" key starting from 1 to the max possible number of indices of the given type

    index2Int :: forall n1 n2 n3 n4 n5 n6 n7 n8.
                (KnownNat n1, KnownNat n2, KnownNat n3, KnownNat n4, KnownNat n5, KnownNat n6, KnownNat n7, KnownNat n8) =>
                 Index n1 n2 n3 n4 n5 n6 n7 n8 -> Int
    index2Int (Index ind1 ind2 ind3 ind4 ind5 ind6 ind7 ind8) = a * (ind2Int ind1) + b * (ind2Int ind2) + c * (ind2Int ind3) + d * (ind2Int ind4)
        + e * (ind2Int ind5) + f * (ind2Int ind6) + g * (ind2Int ind7) + (ind2Int ind8) + 1
            where
                nat1 = fromIntegral $ natVal (Proxy @n1)
                nat2 = fromIntegral $ natVal (Proxy @n2)
                nat3 = fromIntegral $ natVal (Proxy @n3)
                nat4 = fromIntegral $ natVal (Proxy @n4)
                nat5 = fromIntegral $ natVal (Proxy @n5)
                nat6 = fromIntegral $ natVal (Proxy @n6)
                nat7 = fromIntegral $ natVal (Proxy @n7)
                nat8 = fromIntegral $ natVal (Proxy @n8)
                a = maxNrIndex (0,nat2,nat3,nat4,nat5,nat6,nat7,nat8)
                b = maxNrIndex (0,0,nat3,nat4,nat5,nat6,nat7,nat8)
                c = maxNrIndex (0,0,0,nat4,nat5,nat6,nat7,nat8)
                d = maxNrIndex (0,0,0,0,nat5,nat6,nat7,nat8)
                e = maxNrIndex (0,0,0,0,0,nat6,nat7,nat8)
                f = maxNrIndex (0,0,0,0,0,0,nat7,nat8)
                g = maxNrIndex (0,0,0,0,0,0,0,nat8)



    ind2Int :: forall n a. (KnownNat n, Enum a) => Ind n a -> Int 
    ind2Int (UnsafemkInd s) = S.foldlWithIndex f 0 s
                where
                    nat = fromIntegral $ natVal (Proxy @n)
                    f = \b i a -> b + (fromEnum a)*nat^i

    maxNrIndex :: (Int,Int,Int,Int,Int,Int,Int,Int) -> Int
    maxNrIndex (a,b,c,d,e,f,g,h) = 4^h + 4^h * 4^g + 4^h * 4^g * 10^f + 4^h * 4^g * 10^f * 10^e + 4^h * 4^g * 10^f * 10^e * 19^d
         + 4^h * 4^g * 10^f * 10^e * 19^d * 19^c + 4^h * 4^g * 10^f * 10^e * 19^d * 19^c * 21^b + 4^h * 4^g * 10^f * 10^e * 19^d * 19^c * 21^b * 21^a 


    --and the inverse


    int2List :: Int -> Int -> Int -> [Int]
    int2List range 1 nr
                | nr <= range = [nr-1]
                | otherwise = error "number does not fir ind type"
    int2List range len nr
                | i > (range-1) = error "range and length do not fit nr"
                | otherwise = i : (int2List range (len-1) j)
                 where
                    (i,j) = quotRem nr (range^(len-1))
            
