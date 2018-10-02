--this module contains the functions that we need to construct the equivariance equations

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

module EquivarinaceEqns (

) where

    import Index
    import Tensor
    import BasicTensors
    import Ivar
    import Pde



    --these functions are specified for are metric (21 dofs)

    index2Sparse1 :: Index 0 1 0 0 0 0 1 1 -> (Int,Int) 
    index2Sparse1 (_, x2, _, _, _, _, x7, x8) = ((m-1)*4+n,a)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    --the V_Ai i index always comes after the delta_mn n index (order of indices is important !)

    --check this later on when we have extracted the equations

    index2Sparse2 :: Index 0 1 0 0 0 0 1 2 -> (Int,Int) 
    index2Sparse2 (_, x2, _, _, _, _, x7, x8) = ((m-1)*4+n,21+(a-1)*4+i)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             i = 1 + (fromEnum $ getValInd x8 1)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    index2Sparse3 :: Index 0 1 0 0 0 1 1 1 -> (Int,Int) 
    index2Sparse3 (_, x2, _, _, _, x6, x7, x8) = ((m-1)*4+n,21*5+(a-1)*10+i)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             i = 1 + (fromEnum $ getValInd x6 0)
                             m = 1 + (fromEnum $ getValInd x7 0)
                             n = 1 + (fromEnum $ getValInd x8 0)

    index2Sparse4 :: Index 0 1 0 0 1 0 0 2 -> (Int,Int) 
    index2Sparse4 (_, x2, _, _, x5, _, _, x8) = ((j-1)*4+n,21+(a-1)*4+i)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             j = 1 + (fromEnum $ getValInd x5 0)
                             i = 1 + (fromEnum $ getValInd x8 1)
                             n = 1 + (fromEnum $ getValInd x8 0)

    index2Sparse5 :: Index 0 1 0 0 1 1 0 1 -> (Int,Int) 
    index2Sparse5 (_, x2, _, _, x5, x6, _, x8) = ((j-1)*4+n,21*5+(a-1)*10+i)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             j = 1 + (fromEnum $ getValInd x5 0)
                             i = 1 + (fromEnum $ getValInd x6 1)
                             n = 1 + (fromEnum $ getValInd x8 0)

    index2Sparse6 :: Index 0 1 1 0 0 1 0 1 -> (Int,Int) 
    index2Sparse6 (_, x2, x3, _, _, x6, _, x8) = ((j-1)*4+n,21*5+(a-1)*10+i)
                         where 
                             a = 1 + (fromEnum $ getValInd x2 0)
                             j = 1 + (fromEnum $ getValInd x3 0)
                             i = 1 + (fromEnum $ getValInd x6 1)
                             n = 1 + (fromEnum $ getValInd x8 0)
