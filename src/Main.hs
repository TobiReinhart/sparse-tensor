
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

{-# LANGUAGE BangPatterns #-}


{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Main (
 main
) where
    
    {-
    
    import Index
    import Tensor
    import Ivar
    import Pde
    import BasicTensors
    import EquivarianceEqns 
    import Ansatz
    import Symmetrize
    import qualified Data.Map.Strict as M 
    import qualified Data.IntMap.Strict as I
    import qualified Data.Sequence as S
    import Numeric.Natural 
    import GHC.TypeNats
    import EquivarianceMetric
    import System.Random.TF
    import System.Random.TF.Gen
    import System.Random.TF.Instances 
    import Integrabillity
    import Perturbation
    import AnsatzEqns2
    import Order1Int
    import Order2Int
    import Order3Int
    import qualified Data.Map.Strict as M
    import PerturbationTree2
    import Data.Tree
    import Data.Functor
    import Data.List
   
    import qualified Tensor2 as Tens 
    
    

    import qualified TensorTreeNumeric as Tree

    --import Control.DeepSeq
    -}

    import TensorTreeNumeric2 
    import TensorTreeIntCond
    import qualified Tensor2 as T
    import qualified Data.Map as M
    import Data.List
    import Data.Maybe
    import qualified BinaryTree as Bin 
    import qualified TensorTreeNumeric3 as Tree3 
    import PerturbationTree2
    import qualified Data.IntMap.Strict as I
    import qualified Data.Matrix as HasMat
    import qualified Data.Vector as Vec


    
    
     

    main = do

        {-
        
        let map1Area = M.mapKeys mkInd triangleMapArea :: M.Map (Linds_3 4) Uind_20
        let map2Area = M.mapKeys mkInd triangleMapArea :: M.Map (Uinds_3 4) Lind_20

        let map1Metric = M.mapKeys mkInd triangleMap2 :: M.Map (Linds_3 2) Uind_9  
        let map2Metric = M.mapKeys mkInd triangleMap2 :: M.Map (Uinds_3 2) Lind_9 

        

        let mapInter3 = M.mapKeys mkInd triangleMap3 :: M.Map (Linds_3 3) Uind_19


        let eqn1 = eqn1_1 map1Area map2Area :: Tensor 0 1 0 0 0 0 1 1 (Ivar Rational)
        let eqn2 = eqn1_2 map1Area map2Area :: Tensor 0 1 0 0 0 0 1 2 (Ivar Rational)
        let eqn3 = eqn1_3 map1Metric map2Metric map1Area map2Area :: Tensor 0 1 0 0 0 1 1 1 (Ivar Rational)

        let eqn4 = eqn2_2 map1Metric map1Area map2Area :: Tensor 0 1 0 0 1 0 0 2 (Ivar Rational)
        let eqn5 = eqn2_3 map1Metric map2Metric map1Area map2Area :: Tensor 0 1 0 0 1 1 0 1 (Ivar Rational)

        let eqn6 = eqn3_3 mapInter3 map2Metric map1Area map2Area :: Tensor 0 1 1 0 0 1 0 1 (Ivar Rational)

        let eqnConst = eqn1_4

        let eqn1Sparse = mkEqn1Sparse eqn1 
        let eqn2Sparse = mkEqn2Sparse eqn2
        let eqn3Sparse = mkEqn3Sparse eqn3
        let eqn4Sparse = mkEqn4Sparse eqn4
        let eqn5Sparse = mkEqn5Sparse eqn5
        let eqn6Sparse = mkEqn6Sparse eqn6
        let eqnConstSparse = mkEqnConstSparse eqnConst

        let totalEqn = M.unions [eqn1Sparse, eqn2Sparse, eqn3Sparse, eqn4Sparse, eqn5Sparse, eqn6Sparse, eqnConstSparse]

        let eqn1Flat = mkEqn1Sparse $ eqn1_1Flat map1Area map2Area

        let eqn2Flat = mkEqn4Sparse $ eqn2_2Flat map1Metric map1Area map2Area

        let eqn3Flat = mkEqn6Sparse $ eqn3_3Flat mapInter3 map2Metric map1Area map2Area 

        let eqnConstFlat = mkEqnConstSparseFlat eqnConst 

        let totalFlatEqn = M.unions [eqn1Flat,eqn2Flat,eqn3Flat,eqnConstFlat] 

        --writeFile "PdeHaskellFlat.txt" $ showEqnsFlat totalFlatEqn 

        --construct the metric eqns

        let eqn1M = eqn1_1M map1Metric map2Metric :: Tensor 0 0 0 0 0 1 1 1 (Ivar Rational)
        let eqn2M = eqn1_2M map1Metric map2Metric :: Tensor 0 0 0 0 0 1 1 2 (Ivar Rational)
        let eqn3M = eqn1_3M map1Metric map2Metric :: Tensor 0 0 0 0 0 2 1 1 (Ivar Rational)

        let eqn4M = eqn2_2M map1Metric map2Metric :: Tensor 0 0 0 0 1 1 0 2 (Ivar Rational)
        let eqn5M = eqn2_3M map1Metric map2Metric :: Tensor 0 0 0 0 1 2 0 1 (Ivar Rational)

        let eqn6M = eqn3_3M mapInter3 map1Metric map2Metric :: Tensor 0 0 1 0 0 2 0 1 (Ivar Rational)

        let eqnConst = eqn1_4

        let eqn1SparseM = mkEqn1SparseM eqn1M 
        let eqn2SparseM = mkEqn2SparseM eqn2M
        let eqn3SparseM = mkEqn3SparseM eqn3M
        let eqn4SparseM = mkEqn4SparseM eqn4M
        let eqn5SparseM = mkEqn5SparseM eqn5M
        let eqn6SparseM = mkEqn6SparseM eqn6M
        let eqnConstSparse = mkEqnConstSparse eqnConst

        let totalEqnM = M.unions [eqn1SparseM, eqn2SparseM, eqn3SparseM, eqn4SparseM, eqn5SparseM, eqn6SparseM, eqnConstSparse]

        gen <- newTFGen 

        let randList = randomRs (-10000,10000) gen :: [Int]

        let randMap = I.fromList $ zip [1..315] randList

        let pde = mkPdefromTens totalEqn 

        let trian = triangleMap 315 

        let mults = mkAllMultInds 315

        let pdeProlonged = prolongPdeAll mults pde

        let pdeTotal = combinePdesIvar pde pdeProlonged

        --now the same for the metric

        let randMapM = I.fromList$ zip [1..150] randList

        let pdeM = mkPdefromTens totalEqnM

        let trianM = triangleMap 150

        let multsM = mkAllMultInds 150

        let pdeProlongedM = prolongPdeAll multsM pdeM

        let pdeTotalM = combinePdes pdeM pdeProlongedM

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/MetricPdeCorrected2.txt" $ print2Maple 150 trianM  pdeM

        let tens = tensorContractWith_9 (0,0) addIvar $ tensorContractWith_9 (1,1) addIvar $ tensorProductWith sMultIvar (tensorProductWith (*) (invEtaAbs map1Metric) (invEtaAbs map1Metric)) ivar3M

        let eqn6New = mkEqn6SparseMPulled $ tensorContractWith_3 (0,0) (+) $ tensorContractWith_3 (1,1) (+) $ tensorContractWith_3 (3,2) (+) 
             $ tensorProductWith (*) (interJ_2 map2Metric) $ tensorProductWith (*) (interJ_2 map2Metric) (interI_3 mapInter3)

        let pulledEqn3 = mkPdefromTens eqn6New

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/PulledEqn3_2.txt" $ printConstPde pulledEqn3

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/PMetricNew.txt" $ evalPdeRand 150 trianM randMapM pdeTotalM

        let flatAreaT = flatArea map2Area

        let flatAreaM = flatAreaMap map2Area 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/PdeHaskellFlatNew1.txt" $ showEqnsFlat totalFlatEqn 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/PdeHaskellFlatNew2.txt" $  evalPdeRand 315 trian flatAreaM pde

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/PdeHaskellFlatProlonged.txt" $  evalPdeRand 315 trian flatAreaM pdeTotal 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/PdeHaskellAreaRand.txt" $  evalPdeRand 315 trian randMap pde

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/PdeHaskell3New.txt" $ showEqns totalEqn 

        let eqnSD = mkPdefromTens $ M.union eqn1Sparse eqnConstSparse

        let multsSD = mkAllMultInds 21

        let trianSD = triangleMap 21
         
        let eqnSDProlonged = combinePdes eqnSD $ prolongPdeAllBlock1 multsSD eqnSD

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Block1ProlongedVars.txt" $  print2Maple 21 trianSD eqnSDProlonged 

        --now the integrabillity conditions

        let int1 = mkEqnSparseCond1 $ intCond1 map1Area map2Area map1Metric map2Metric 

        let cond1Zero = mkEqnSparseCond1 $ intCond1Zero map1Area map2Area map1Metric map2Metric 

        let cond1RelFac = mkEqnSparseCond1 $ intCond1RelFac map1Area map2Area map1Metric map2Metric 

        let int1noFactor = mkEqnSparseCond1 $ intCond1noFactor map1Area map2Area map1Metric map2Metric 

        let totalInt1 = M.union eqn3Flat int1 

        let totalInt1noFactor = M.union eqn3Flat int1noFactor 

        let intZero =  mkEqnSparseCond1Zero $ int1Zero map1Area map2Area map1Metric 

        let intTest =  mkEqnSparseCond1Zero $ int1Test map1Area map2Area map1Metric 


        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Integrabillity1Zero.txt" $  showEqnsFlat intZero

        let test = mkEqnSparseBlock1Eta $ tensorContractWith_3 (1,0) (+) $ tensorProductWith (*) (eqn1_1Flat map1Area map2Area) invEta  

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Integrabillity2Zero.txt" $  showEqnsFlat eqn2Flat

        let int2 = mkEqnSparseCond2 $ intCond2 map1Area map2Area map1Metric 
        
        let totalInt2 = M.union eqn2Flat int2 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Integrabillity2.txt" $  showEqnsFlat totalInt2

        let int3 = mkEqnSparseCond3 $ intCond3 map1Area map2Area map1Metric

        let totalInt3 = M.union eqn1Flat int3 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Integrabillity3.txt" $  showEqnsFlat totalInt3

        let totalEqn1 = M.unions [totalFlatEqn,totalInt3,totalInt2,totalInt1]

        let projector = mkEqnSparseProjector $ projectorMatrix map1Area map1Metric

        let projector2 = mkEqnSparseProjector2 $ projectorMatrix2 map1Area 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/FlatTotal.txt" $  showEqnsFlatFrac totalFlatEqn

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/PorMat.txt" $  showEqnsFlatFrac projector

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Flat3Sym.txt" $  showEqnsFlat eqn3Flat

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/AllInts.txt" $  showEqnsFlat totalEqn1

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Projector2.txt" $  showEqnsFlatFrac projector2

        let int2_1 = mkEqnSparseintCond2_1 $ intCond2_1 map1Area map2Area map1Metric map2Metric 

        let intCondIndList = concat . map (\i -> zip (repeat i) [i..21] ) $ [1..21]

        let trianintCond = M.fromList $ zip intCondIndList [1..]

        let int2_1SymbolPure = mkEqnSparseintCond2_1SymbolPure trianintCond $ intCond2_1SymbolPure map1Area map2Area map1Metric map2Metric 

        let int2_1SymbolRed = mkEqnSparseintCond2_1SymbolRed trianintCond $ intCond2_1SymbolRed map1Area map2Area map1Metric map2Metric 

        let int2_1SymbolRedWrong = mkEqnSparseintCond2_1SymbolRed trianintCond $ intCond2_1SymbolRedWrong map1Area map2Area map1Metric map2Metric 
        
        let int2_1Symbol = mkEqnSparseintCond2_1Symbol trianintCond $ intCond2_1Symbol map1Area map2Area map1Metric map2Metric 
        
        let int2_1SymbolRedFull = mkEqnSparseintCond2_1SymbolRedFull  $ intCond2_1SymbolRed map1Area map2Area map1Metric map2Metric 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/intCond2_1Symbol_3.txt" $  showEqnsFlatFrac int2_1Symbol

        let dens1Metric = mkEqnSparseDens1 $ densityEqnMetric1 map1Metric map2Metric

        let dens2Metric = mkEqnSparseDens2 $ densityEqnMetric2 map1Metric map2Metric

        let dens1Area = mkEqnSparseDens1Area $ densityEqnArea1 map1Area map2Area

        let dens2Area = mkEqnSparseDens2Area $ densityEqnArea2 map1Area map2Area

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/AreaDens1.txt" $  showEqnsFlatFrac dens1Area

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/AreaDens2.txt" $  showEqnsFlatFrac dens2Area

        let areaM = mkEqnSparseAreaM $ areaMetricMetric1 map1Area map2Area 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/AreaM1.txt" $  showEqnsFlatFrac areaM

        let areaMintCond = mkEqnSparseAreaMintCond $ areaMetricMetric1intCond map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/AreaM1intCond.txt" $  showEqnsFlatFrac areaMintCond

        --try the ansatz generator

        let ansatz = getAllInds [(1,3),(1,2),(3,4),(5,7),(5,6),(7,8),(9,11),(9,10),(11,12),(13,14),(1,5),(5,9)] [] [(1,3),(3,5),(5,7),(7,9),(9,11),(11,13),(1,2),(3,4),(5,6),(7,8),(9,10),(11,12),(13,14)]

        let ansatz3 = getAllIndsLabel "abcd" [(1,3),(1,2),(3,4)] [] [(1,2),(3,4)]

        let ans = map mkAns ansatz 

        let ansatz2 = getAllInds [(1,3),(1,2),(3,4),(5,7),(5,6),(7,8),(9,10),(1,5)] [] [(1,3),(1,2),(3,5),(3,4),(5,7),(5,6),(7,9),(7,8),(9,10)]

        let symLabel2 = ([(9,10)], [(1,2),(3,4),(5,6),(7,8)], [([1,2],[3,4]),([5,6],[7,8])], [], []) 

        let symPos2 = ([(1,2),(3,4),(5,6),(7,8),(9,10)], [], [], [], [[[1,2],[3,4],[5,6],[7,8],[9,10]]])

        let ans2 = map mkAns ansatz2 

        let symAns2 = symAnsSet ([],[],[],[],[]) symLabel2 ans2

        let inds2 = getRepInds "abcdefghpq" symAns2

        let pertAns = map mkPertAns ansatz2

        let symPertAns = symAnsSetPert symLabel2 pertAns 

        let symPertAnsInds = getRepIndsPert "abcdefghpq" symPertAns

        let ansatz4 = getAllInds [(1,2),(1,3),(3,4),(5,6)] [] [(1,2),(1,3),(3,4),(3,5),(5,6)]

        let ansatz4Pert = map mkPertAns ansatz4

        let ansatz4Sym = symAnsSetPert ([(5,6)],[(1,2),(3,4)],[([1,2],[3,4])], [], []) ansatz4Pert

        let ansatz4SymInds = getRepIndsPert "abcdpq" ansatz4Sym

        let pertAns1 = map mkPertAns ansatz

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/FlatTest1.txt" $ showEqnsFlat $ M.union eqn1Flat eqnConstFlat

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/FlatTest2.txt" $ showEqnsFlat eqn2Flat

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/FlatTest3.txt" $ showEqnsFlat eqn3Flat

        let intTest2 = mkEqnSparseintCond2_1SymbolRedFull  $ int1Test2 map1Area map2Area map1Metric  

        let intTest3 = mkEqnSparseintCond2_1SymbolRedFull  $ int1Test3 map1Area map2Area map1Metric  

        
        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/IntCond1Test5.txt" $  showEqnsFlatFrac intTest2

        let flatMetric = mkEqnSparseflatMetricInter $ flatMetricInter map1Metric map2Metric 

        let flatMetricPro = mkEqnSparseflatMetricInterProlong $ flatMetricInterProlong map1Metric map2Metric

        let flatMetricProUnSym = mkEqnSparseflatMetricInterProlong $ tensorProductWith (*) delta_9 $ flatMetricInter map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/flatMetricInter.txt" $  showEqnsFlatFrac flatMetric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/flatMetricINterProlong.txt" $  showEqnsFlatFrac flatMetricPro

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/flatMetricINterProlongUnSym.txt" $  showEqnsFlatFrac flatMetricProUnSym

        let rankDef1 = mkEqnSparseintRankDef1 $ intRankDef1 map1Area map2Area map1Metric map2Metric 

        let rankDef2 = mkEqnSparseintCond2_1SymbolRedFull $ intRankDef2 map1Area map2Area map1Metric map2Metric

        let rankDef3 = mkEqnSparseintRankDef3 $ intRankDef3 map1Area map2Area map1Metric map2Metric

        let rankDef5 = mkEqnSparseintRankDef3 $ intRankDef5 map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/RankDef5.txt" $  showEqnsFlatFrac rankDef5

        let intComp = mkEqnSparseintCondComp $ intCondComp map1Area map2Area map1Metric map2Metric 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/RankDef5.txt" $  showEqnsFlatFrac intComp

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/int1_31_12.txt" $  showEqnsFlatFrac totalInt3

        let intCondSymmetrized = mkEqnSparseintCondSym $ intCondSym map1Area map2Area map1Metric map2Metric

        let intCompZero = mkEqnSparseintCondComp $ intCondCompZero map1Area map2Area map1Metric map2Metric 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/int2_1_9_1_19.txt" $  showEqnsFlatFrac int2_1

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/intCondCompZero_9_1_19.txt" $  showEqnsFlatFrac intCompZero

        let intOrd2 = mkEqnSparseintCondOrd2 $ intCondOrd2 map1Area map2Area map1Metric map2Metric 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/intCondOrd2_9_1_19.txt" $ showEqnsFlatFrac  intOrd2 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/intCond1_9_1_19.txt" $ showEqnsFlatFrac  int1 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/PdeHaskellFlatProlonged10_1_19.txt" $  evalPdeRand 315 trian flatAreaM pdeTotal 

        let int2_1New = mkEqnSparseintCond2_1New trian $ intCond2_1 map1Area map2Area map1Metric map2Metric 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/AnsatzEqn10er10_1_19.txt" $  showEqnsFlat int2_1New 

        let intCompNew = mkEqnSparseintCondCompNew trian $ intCondComp map1Area map2Area map1Metric map2Metric 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/intCondOrd2New_9_1_19.txt" $ showEqnsFlat  intCompNew

        let intCompNoSym = mkEqnSparseintCondCompNoSym $ intCondCompNoSym map1Area map2Area map1Metric map2Metric 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/intCondCompNoSym_10_1_19.txt" $ showEqnsFlat  intCompNoSym

        let ansAB = mkEqnSparseAnsatzAB trian $ ansatzAB map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/ansatzAB_11_1_19.txt" $ showEqnsFlat  ansAB

        let ansABb = mkEqnSparseAnsatzABb trian $ ansatzABb map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/ansatzABb_11_1_19.txt" $ showEqnsFlat  ansABb

        let ansAaBb = mkEqnSparseAnsatzAaBb trian $ ansatzAaBb map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/ansatzAaBb_11_1_19.txt" $ showEqnsFlat  ansAaBb

        let ansAIB = mkEqnSparseAnsatzAIB trian $ ansatzAIB map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/ansatzAIB_11_1_19.txt" $ showEqnsFlat  ansAIB

        let ansAaBI = mkEqnSparseAnsatzAaBI trian $ ansatzAaBI map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/ansatzAaBI_11_1_19.txt" $ showEqnsFlat  ansAaBI

        let ansAIBJ = mkEqnSparseAnsatzAIBJ trian $ ansatzAIBJ map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/ansatzAIBJ_11_1_19.txt" $ showEqnsFlat  ansAIBJ

        let newInt2 = mkEqnSparseintCond2NoSym trian $ intCond2NoSym map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/intCond_11_1_19.txt" $ showEqnsFlat  newInt2

        let rem = mkEqnSparseRemoveAIB trian $ removeAIB 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/removeAIB_11_1_19.txt" $ showEqnsFlat  rem

        let internoFac = mkEqnSparseinterMat $ inter4noFactor map1Area map2Area 

        let interFac = mkEqnSparseinterMat $ inter4Factor map1Area map2Area 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/interMatnoFac_11_1_19.txt" $ showEqnsFlatFrac  internoFac

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/interMatFac_11_1_19.txt" $ showEqnsFlatFrac  interFac

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/IntCond1.txt" $  showEqnsFlat int1

        let internoFac6 = mkEqnSparseinter6Mat $ inter6noFactor map1Area map2Area map1Metric map2Metric

        let interFac6 = mkEqnSparseinter6Mat $ inter6Factor map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/inter6MatnoFac_11_1_19.txt" $ showEqnsFlatFrac  internoFac6

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/inter6MatFac_11_1_19.txt" $ showEqnsFlatFrac  interFac6

        let intFirstOrder = mkEqnSparsefirstOrder $ intCondfirstOrder map1Area map2Area map1Metric map2Metric 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/int3_12_1_19.txt" $ showEqnsFlatFrac  int3

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/intFirstOrderTest_12_1_19.txt" $ showEqnsFlatFrac  intFirstOrder

        ---now the 2nd order integrabillity conditions 

        --start with the A:B conditions

        let ansatzCondAB = mkEqnSparseAnsatzAB2 $ ansatzAB2 map1Area map2Area map1Metric map2Metric

        let intCondAB1 = mkEqnSparseIntAB $ intAB1 map1Area map2Area map1Metric map2Metric

        let intCondAB2 = mkEqnSparseIntAB $ intAB2 map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/2ndOrderInt/ansatzAB_12_1_19.txt" $ showEqnsFlatFrac  ansatzCondAB

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/2ndOrderInt/int1AB_12_1_19.txt" $ showEqnsFlatFrac  intCondAB1

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/2ndOrderInt/int2AB_12_1_19.txt" $ showEqnsFlatFrac  intCondAB2

        let ansatzCondABb = mkEqnSparseAnsatzABb2 $ ansatzABb2 map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/2ndOrderInt/ansatzABb_13_1_19.txt" $ showEqnsFlatFrac  ansatzCondABb

        let intCondABI = mkEqnSparseIntAIB $ intAIB map1Area map2Area map1Metric map2Metric

        let ansatzCondAIB = mkEqnSparseAnsatzAIB2 $ ansatzAIB2 map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/2ndOrderInt/intABI_13_1_19.txt" $ showEqnsFlatFrac  intCondABI

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/2ndOrderInt/ansatzABI_13_1_19.txt" $ showEqnsFlatFrac  ansatzCondAIB

        let intCondABIsym = mkEqnSparseIntAIBsym $ intAIBsym map1Area map2Area map1Metric map2Metric

        let intCondABIsymRed = mkEqnSparseIntAIBsym $ intAIBsymRed map1Area map2Area map1Metric map2Metric

        let intCondABIsymZero = evalTensorVals $ intAIBsymZero map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/2ndOrderInt/intABISym_13_1_19.txt" $ showEqnsFlatFrac  intCondABIsym

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/2ndOrderInt/intABISymZero_13_1_19.txt" $ show intCondABIsymZero

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/2ndOrderInt/intABISymRed_13_1_19.txt" $ showEqnsFlatFrac  intCondABIsymRed

        let intCondAI = mkEqnSparseIntAI $ intAI map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/2ndOrderInt/intAI_13_1_19.txt" $ showEqnsFlatFrac intCondAI

        let ansatzAI = mkEqnSparseAnsatzAI2 $ ansatzAI2 map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/2ndOrderInt/ansatzAI_13_1_19.txt" $ showEqnsFlatFrac ansatzAI

        let ansatzCondABC = mkEqnSparseAnsatzABC2 $ ansatzABC2 map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/3rdOrderInt/ansatzABC2_14_1_19.txt" $ showEqnsFlatFrac ansatzCondABC

        let intCondABC = mkEqnSparseIntABC2 $ intABC map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/3rdOrderInt/intABC_14_1_19.txt" $ showEqnsFlatFrac intCondABC

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/2ndOrderInt/ansatzAB4_12_1_19.txt" $ showEqnsFlat intCondAB2 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/3rdOrderInt/intABC_14_1_19.txt" $ showEqnsFlatFrac intCondABC

        --writeFile "intABC_14_1_19.txt" $ showEqnsFlatFrac intCondABC 

        let ansatzCondAaBC = mkEqnSparseAnsatzAaBC $ ansatzAaBC map1Area map2Area map1Metric map2Metric

        --writeFile "intAaBC_14_1_19.txt" $ showEqnsFlat ansatzCondAaBC 

        let ansatzCondAIBC = mkEqnSparseAnsatzAIBC $ ansatzAIBC map1Area map2Area map1Metric map2Metric

        --writeFile "intAIBC_14_1_19.txt" $ showEqnsFlat ansatzCondAIBC 

        let intCondAIBC = mkEqnSparseintAIBC $ intAIBC map1Area map2Area map1Metric map2Metric

        --writeFile "intAIBC_15_1_19.txt" $ showEqnsFlat intCondAIBC 

        let ansatzCondAaBbC = mkEqnSparseAnsatzAaBbC $ ansatzAaBbC map1Area map2Area map1Metric map2Metric

        --writeFile "ansatzAaBbC_15_1_19.txt" $ showEqnsFlat ansatzCondAaBbC 

        let intCondAaBbC = mkEqnSparseintAaBbC $ intAaBbC2 map1Area map2Area map1Metric map2Metric

        --writeFile "intAaBbC2_15_1_19.txt" $ showEqnsFlat intCondAaBbC 

        let ansatzCondAIBbC = mkEqnSparseAnsatzAIBbC $ ansatzAIBbC map1Area map2Area map1Metric map2Metric

        --writeFile "ansatzAIBbC_15_1_19.txt" $ showEqnsFlat ansatzCondAIBbC 

        let ansatzCondA = mkEqnSparseAnsatzA $ ansatzA map1Area map2Area map1Metric map2Metric

        let ansatzCondAa = mkEqnSparseAnsatzAa $ ansatzAa map1Area map2Area map1Metric map2Metric

        let ansatzCondAI = mkEqnSparseAnsatzAI $ ansatzAI3 map1Area map2Area map1Metric map2Metric

        --writeFile "/cip/austausch/cgg/Ansatz/ansatzA_16_1_19.txt" $ showEqnsFlatMatLab ansatzCondA

        --writeFile "/cip/austausch/cgg/Ansatz/ansatzAa_16_1_19.txt" $ showEqnsFlatMatLab ansatzCondAa

        --writeFile "/cip/austausch/cgg/Ansatz/ansatzAI_16_1_19.txt" $ showEqnsFlatMatLab ansatzCondAI

        --writeFile "/cip/austausch/cgg/topSorts18SymmetrizedEpsilon.txt" $ show ans18SymEpsilon

        --writeFile "/cip/austausch/cgg/epsilonTopSorts.txt" $ show ansatz18SymEps 

        let prolong1AI = mkEqnSparseprolongation1AI_AI $ prolongation1AI_AI map1Area map2Area map1Metric map2Metric

        let prolong1ACK = mkEqnSparseprolongation1AI_ACK $ prolongation1AI_ACK map1Area map2Area map1Metric map2Metric

        let totalProlong1 = M.unionWith (+) prolong1AI prolong1ACK

        --writeFile "/cip/austausch/cgg/prolongEqn1.txt" $ showEqnsFlatMatLab totalProlong1

        let prolong2AaBb = mkEqnSparseprolongation2AaBb $ prolongation2AaBb map1Area map2Area map1Metric map2Metric

        let prolong2AaBbC = mkEqnSparseprolongation2AaBbC $ prolongation2AaBbC map1Area map2Area map1Metric map2Metric

        let totalProlong2 = M.unionWith (+) prolong2AaBb prolong2AaBbC

        --writeFile "/cip/austausch/cgg/prolongEqn2AaBb.txt" $ showEqnsFlatMatLab totalProlong2

        let intCondABTrian = mkEqnSparseIntABTrian trian $ intAB1 map1Area map2Area map1Metric map2Metric

        --writeFile "/cip/austausch/cgg/intCondABTrian.txt" $ showEqnsFlatMatLab intCondABTrian

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/ansatzA_18_1_19.txt" $ showEqnsFlat int3

        let intCond1A = mkEqnSparseint1A $ int1A map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/intA_18_1_19.txt" $ showEqnsFlat intCond1A

        let eqn1SparseNew = M.union eqn1Sparse eqnConstSparse

        let pde1New = mkPdefromTens eqn1SparseNew

        let trian1New = triangleMap 21 

        let mults1New = mkAllMultInds 21

        let pdeProlonged1New_1 = prolongPdeAll mults1New pde1New

        let pdeTotal1NewOrd2 =  combinePdesIvar pde1New pdeProlonged1New_1

        let pdeProlonged1New_2 = prolongPdeAll mults1New pdeProlonged1New_1

        let pdeTotal1NewOrd3 =  combinePdesIvar pde1New $ combinePdesIvar pdeProlonged1New_2 pdeProlonged1New_1

        let sys1NewOrd3 = evalPdeRand 21 trian1New flatAreaM pdeTotal1NewOrd3

        let sys1NewOrd2 = evalPdeRand 21 trian1New flatAreaM pdeTotal1NewOrd2

        let sys1NewOrd3Maple = print2MaplePde pdeTotal1NewOrd3


        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/pdeOrd1Maple_18_1_19.txt" $ print2MaplePde pde1New

        let ansABSol = mkEqnSparseAnsatzABSolo trian1New $ ansatzAB map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/ansatzABFac_18_1_19.txt" $ showEqnsFlat ansABSol

        let intCondABTrian2 = mkEqnSparseIntABTrian trian1New $ intAB1 map1Area map2Area map1Metric map2Metric

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Ansatz/intABFac_18_1_19.txt" $ showEqnsFlat intCondABTrian2


        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/PdeHaskellFlatProlonged2.txt" $  evalPdeRand 315 trian flatAreaM pdeTotal 

        --print $ M.size $ getPdeMap pdeTotal1NewOrd3
        
        --print $ M.size $ M.mapKeys (\(x,y) -> (x,multInd2MatrixNr y 21 trian1New)) $ getPdeMap pdeTotal1NewOrd3

        --writeFile "/cip/austausch/cgg/Ansatz/ansAB_18_1_19.txt" $ showEqnsFlatMatLab  ansAB

        --writeFile "/cip/austausch/cgg/Ansatz/ansABb_18_1_19.txt" $ showEqnsFlatMatLab  ansABb

        --writeFile "/cip/austausch/cgg/Ansatz/ansAaBb_18_1_19.txt" $ showEqnsFlatMatLab  ansAaBb

        --writeFile "/cip/austausch/cgg/Ansatz/ansAIB_18_1_19.txt" $ showEqnsFlatMatLab  ansAIB

        --writeFile "/cip/austausch/cgg/Ansatz/ansAaBI_18_1_19.txt" $ showEqnsFlatMatLab  ansAaBI

        --writeFile "/cip/austausch/cgg/Ansatz/ansAIBJ_18_1_19.txt" $ showEqnsFlatMatLab  ansAIBJ

        --writeFile "/cip/austausch/cgg/Ansatz/intABI_18_1_19.txt" $ showEqnsFlatMatLab  newInt2

        --writeFile "/cip/austausch/cgg/Ansatz/intAB_18_1_19.txt" $ showEqnsFlatMatLab  intCondABTrian

        let trianNew = triangleMap 315

        let ansatzCondAaBbCT = mkEqnSparseAnsatzAaBbCTrian trianNew $ ansatzAaBbC map1Area map2Area map1Metric map2Metric

        let intCondAaBbCT = mkEqnSparseintAaBbCTrian trianNew $ intAaBbC2 map1Area map2Area map1Metric map2Metric

        --writeFile "/cip/austausch/cgg/Ansatz/ansatzAaBbC_18_1_19.txt" $ showEqnsFlatMatLab  ansatzCondAaBbCT

        --writeFile "/cip/austausch/cgg/Ansatz/intAaBbC_18_1_19.txt" $ showEqnsFlatMatLab  intCondAaBbCT

        --putStr $ printForest etaTrees16

        --print $ getVarsForest epsTrees16

        --print $ length $ getVarsForest epsTrees16

        --print $ getVarsForest epsTrees14 

        --print $ length $ getVarsForest epsTrees14


        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Tree10.txt" $ printForest epsTrees14

        --putStr $ concat $ map drawTree.(map (fmap show)) $ mkForest [(1,2),(2,3),(5,6),(6,8)] [] 

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Eps.txt"  $ show   $ getAllIndsEpsilon [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]

        --putStr $ printForest etaTrees18 

        --print $ delta_20

        let pdeProlongedOrd3 = prolongPdeAll mults pdeProlonged

        let totalPdeOrd3 = combinePdesIvar pde $ combinePdesIvar pdeProlonged pdeProlongedOrd3

        let pdeOrd3Eval = evalPdeRand 315 trian flatAreaM totalPdeOrd3

        --writeFile "/cip/austausch/cgg/pdeOrd2.txt" $ evalPdeRand 315 trian flatAreaM pdeTotal

        let pdeOrd2 = evalPdeRand 315 trian flatAreaM pdeTotal

        --putStr  pdeOrd2
        

        let filter18 = [(1,2),(3,4),(1,3),(1,5),(5,6),(7,8),(5,7),(5,9),(9,10),(11,12),(9,11),(13,14),(15,16),(17,18)] 

        let sym18 = ([(13,14),(15,16),(17,18)], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12])], [], [[[1,2,3,4,13,14],[5,6,7,8,15,16],[9,10,11,12,17,18]]])

        let filter14 = [(1,2),(3,4),(1,3),(1,5),(5,6),(7,8),(5,7),(9,10),(11,12),(9,11)] 

        let sym14 = ([], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12]),([1,2,3,4,13],[5,6,7,8,14])], [], [])

        let epsAnsatz14 = getEpsForest [1..14] filter14 1 sym14 

        let epsAnsatz18 = getEpsForest [1..18] filter18 1 sym18

        let testForest = mkForestFromAscList $ [Eta 1 2, Eta 3 4, Eta 5 6, Eta 7 8 , Var 1 1]

        let testForestSym = symAnsatzForestEta ([],[],[],[[1,2,3,4,5]],[]) testForest

        --putStr $ unlines $ printAnsatz $ mapNodes showAnsatzNode testForestSym
        
        --print $ length $ getForestLabels epsAnsatz18 

        let intCondABI = mkEqnSparseIntAIB $ intAIB map1Area map2Area map1Metric map2Metric

        let intCondAIBJC = mkEqnSparseintAIBJCTrian trian $ intAIBJC map1Area map2Area map1Metric map2Metric

        writeFile "/cip/austausch/cgg/intAIBJC.txt" $ showEqnsFlatMatLab intCondAIBJC

        -}

        {-
        let map1Area = M.mapKeys mkInd triangleMapArea :: M.Map (Linds_3 4) Uind_20
        let map2Area = M.mapKeys mkInd triangleMapArea :: M.Map (Uinds_3 4) Lind_20

        let map1Metric = M.mapKeys mkInd triangleMap2 :: M.Map (Linds_3 2) Uind_9  
        let map2Metric = M.mapKeys mkInd triangleMap2 :: M.Map (Uinds_3 2) Lind_9 

        let trianI_3 = M.mapKeys mkInd triangleMap3 :: M.Map (Linds_3 3) Uind_19
        let trianJ_3 = M.mapKeys mkInd triangleMap3 :: M.Map (Uinds_3 3) Lind_19


        let interI2 = inter_I2Map map1Metric
        let interJ2 = inter_J2Map map2Metric

        let interI3 = inter_I3Map trianI_3
        let interJ3 = inter_J3Map trianJ_3

        let interIArea = inter_IAreaMap map1Area
        let interJArea = inter_JAreaMap map2Area

        let intMetric = interMetric interI2 interJ2
        let intArea = interArea interIArea interJArea

        let int2 = interEqn1_2 intArea
        let int3 = interEqn1_3 intMetric intArea

        let flatArea = flatAreaMap interJArea $ flatAreaSTMap epsilonMap

        let flatInter = flatInterMap flatArea intArea

        let intCondAIB = intAIB intArea intMetric flatInter int3

        let vals = evalFullTensor intCondAIB 

        print vals 
        
        let map1Area = M.mapKeys mkInd triangleMapArea :: M.Map (Linds_3 4) Uind_20
        let map2Area = M.mapKeys mkInd triangleMapArea :: M.Map (Uinds_3 4) Lind_20

        let map1Metric = M.mapKeys mkInd triangleMap2 :: M.Map (Linds_3 2) Uind_9  
        let map2Metric = M.mapKeys mkInd triangleMap2 :: M.Map (Uinds_3 2) Lind_9 

        let trianI_3 = M.mapKeys mkInd triangleMap3 :: M.Map (Linds_3 3) Uind_19
        let trianJ_3 = M.mapKeys mkInd triangleMap3 :: M.Map (Uinds_3 3) Lind_19

        let intArea = interArea map1Area map2Area

        let int3 = interEqn1_3 map1Area map2Area map1Metric map2Metric

        let flatInt = flatInter map1Area map2Area  

        let intMetric = interMetric map1Metric map2Metric

        let intCondAIBNew = intAIBNew intArea intMetric flatInt int3
        
        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/TensorMap.txt"  $ show  intCondAIBNew

        

        let delta20T = delta_20 

        let delta20List = M.assocs delta20T

        let testInt = intTest5 delta20T 

        let l = intTest5List delta20List
        
--        let m = foldl (\m (k, v) -> force $ M.insert k v m) M.empty testIntList

        print l

        print l
        
        
        

        
        let delta20 = fromList $ deltaList 20 

        let test = fmap ((*) 2) delta20

        let trian2 = triangleMap2

        let intMetric = interMetric trian2

        let trianArea = triangleMapArea 

        let intArea = interArea trianArea

        let test2 = interEqn3 trian2 trianArea 

        let test3 = intAIB trian2 trianArea

        let test4 = flatInter trianArea

        let test5 = tensorProd delta20 $ tensorProd delta20 $ tensorProd delta20 $ tensorProd delta20 delta20  

        --writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/Int3Tree.txt"  $ showTensorFracMaple mkMatrixIndAIB test3

        --print $ map snd $ toListT intArea

        --print $ toListT $ interJArea trianArea

        --let trianMap = triangleMap 315 

        --let intCondAIBJC = intAIBJC trian2 trianArea 

        --writeFile "/cip/austausch/cgg/intAIBJC.txt" $ showTensorFrac (mkMatrixIndAIBJC  trianMap) intCondAIBJC

        let test6 = Tensor [(2,Scalar 3), (1, Scalar 5)]
        
        print $ isValid test6

        

        let trian2 = Tens.triangleMap2

        let trianA = Tens.triangleMapArea

        let intAIBTens = M.mapKeysMonotonic index2List $ getMap $ intAIB map1Area map2Area map1Metric map2Metric 

        let intAIBTens2 = Tens.intAIB trianA trianA trian2 trian2

        let diff1 = M.difference intAIBTens intAIBTens2
        
        let diff2 = M.difference intAIBTens2 intAIBTens

        let symDiff = M.union diff1 diff2 

        --print $ nub $ M.assocs symDiff

        let intArea = M.mapKeysMonotonic index2List $ getMap $ interArea map1Area map2Area 

        let intArea2 = Tens.interArea trianA trianA 

        let intMetric = M.mapKeysMonotonic index2List $ getMap $ interMetric map1Metric map2Metric 

        let intMetric2 = Tens.interMetric trian2 trian2 

        let intI2 = Tens.interJ_2 trian2  

        let interEqn1_2Tens = Tens.interEqn1_2 trianA trianA

        let areaFlat = M.filter (/= 0) $  M.mapKeysMonotonic index2List $ getMap $ intAIB map1Area map2Area  map1Metric map2Metric

        let areaFlat2 = Tens.intAIB trianA trianA trian2 trian2

        let trian2Tree = Tree.triangleMap2

        let trianATree = Tree.triangleMapArea

        let intMetricTree = Tree.interMetric trian2Tree

        let intAreaTree = Tree.interArea trianATree

        let intEqn1_2Tree = Tree.interEqn2 trianATree

        let i1 = sort $ Tree.toListT intEqn1_2Tree

        let i2 = sort $ map (\(a,b) -> (Tens.swapPosInd (2,3)  $ Tens.swapPosInd (1,3) $ Tens.index2List a,b)) $ M.assocs interEqn1_2Tens
        
        putStr $ unlines $ map show $ zip i1 i2 

        print $ i1 == i2

        

        let map1Area = trianMapAreaI 
        let map2Area = trianMapAreaJ
        let map1Metric = trianMapI2
        let map2Metric = trianMapJ2

        let map1AreaTree3 = Tree3.trianMapAreaI 
        let map2AreaTree3 = Tree3.trianMapAreaJ
        let map1MetricTree3 = Tree3.trianMapI2
        let map2MetricTree3 = Tree3.trianMapJ2

        let mapAreaT = T.triangleMapArea :: M.Map [Int] Int
        let mapMetricT = T.triangleMap2 :: M.Map [Int] Int

        let intM = interMetric map1Metric map2Metric 

        --print $ map snd $ toListShow intM 

        let intMT = T.interMetric mapMetricT mapMetricT 

        let intMTree3 = Tree3.interMetric map1MetricTree3 map2MetricTree3 

        let intI2 = interI2 map1Metric 

        let intI2Tree3 = Tree3.interI2 map1MetricTree3

        --print $ M.elems intMT 

        --print $ (M.elems intMT) == (map snd $ toListShow intM) 

        let intA = interArea map1Area map2Area

        let intATree3 = Tree3.interArea map1AreaTree3 map2AreaTree3

        let intAT = T.interArea mapAreaT mapAreaT 

        --print $ (M.elems intAT) == (map snd $ toListShow intA) 

        let int2 = interEqn2 map1Area map2Area

        let int2T = T.interEqn1_2 mapAreaT mapAreaT 

        let int2_1 = tensorProd intA delta3

        let int2_1Tree3 = Tree3.tensorProd intATree3 Tree3.delta3

        let int2Tree3 = Tree3.interEqn2 map1AreaTree3 map2AreaTree3
        
        let int2_2 = tensorProd (tensorTransL3 (0,1) $ tensorProd delta3 delta3 ) delta20

        let int2_2Tree3 = Tree3.tensorProd (Tree3.tensorTransL3 (0,1) $ Tree3.tensorProd Tree3.delta3 Tree3.delta3 ) Tree3.delta20

        let int2_1T =  T.tensorProductNumeric intAT T.delta_3 

        let int2_2T = T.tensorProductNumeric  (T.tensorTranspose 8 (0,1) $ T.tensorProductNumeric T.delta_3 T.delta_3 ) T.delta_20

        let test10 =  tensorTransL3 (0,1) $ tensorProd delta3 delta3

        let test11 = T.tensorTranspose 8 (0,1) $ T.tensorProductNumeric T.delta_3 T.delta_3

        let test12 = tensorProd  (tensorProd test10 delta3) delta3

        let test13 = T.tensorProductNumeric ( T.tensorProductNumeric test11 T.delta_3) T.delta_3

        let test14 = tensorTransL3 (0,1) $ tensorProd delta3 $ tensorProd delta3 delta3

        let intCond = intAIB map1Metric map2Metric map1Area map2Area 

        let intCondT = T.intAIB mapAreaT mapAreaT mapMetricT mapMetricT

        let int3 = interEqn3 map1Metric map2Metric map1Area map2Area 

        let int3T = T.interEqn1_3 mapAreaT mapAreaT mapMetricT mapMetricT 



        --print $ zip ( map (truncate) (M.elems int2T) ) (map (truncate) $ (map snd $ toListShow int2) )

        --print $ toListShow $ tensorProd delta3 delta3  

        --print $ map snd $ toListShow $ tensorTransL3 (0,1) $ tensorProd delta3 delta3 
        
        --print $ M.elems $ T.tensorTranspose 8 (0,1) $ T.tensorProductNumeric T.delta_3 T.delta_3

        --print $ (map snd $ toListShow $ tensorProd (tensorTransL3 (0,1) $ tensorProd delta3 delta3) delta20) == (M.elems $ T.tensorProductNumeric (T.tensorTranspose 8 (0,1) $ T.tensorProductNumeric T.delta_3 T.delta_3) T.delta_20)

        
        --print $ length l1
        --print $ length l2
        
        let l3 = toListT intCond

        let l4 = filter (\(a,b) -> b /= 0) $ toListT intCond

        let ansAB = ansatzAB map1Metric map2Metric map1Area map2Area 

        let trianM = triangleMap 315 

        let ansABString = M.assocs $ M.fromListWith (+) $ map ((index2SparseAnsatzAB trianM)) $ toListShow ansAB 

        let ansatzABT = T.ansatzAB mapAreaT mapAreaT mapMetricT mapMetricT 

        let l1 = filter (\(a,b) -> b /= 0) $ sort ( map (\(x,y) -> (x,truncate y)) $ toListShowIndex $ int2)
        let l2 = filter (\(a,b) -> b /= 0) $ sort $ map (\(x,y) -> (x,truncate y)) $ M.toAscList $ int2T
        
        --putStr $ unlines ansABString

        let ansABSym = ansatzABSym map1Metric map2Metric map1Area map2Area 

        let ansABStringSym = sort $ mapMaybe ((index2SparseAnsatzABSym trianM)) $ toListShow ansABSym 

  --      print $ ansABStringSym == ansABString

    --    putStr $ unlines $ map show $ zip ansABStringSym ansABString
        --print $ length $ ansABStringSym

        let t = zip  [ [a,b,c] | a <- [1..30], b <- [a..30], c <- [b..30] ] [1..]

        let t2 = zip [1..] [1..10000000]

        let map2 = Bin.fromAscList t2 

        let map3 = M.fromAscList t2 


        --putStr $ unlines $ map show $ zip l1 l2

        let ansAaBb = ansatzAaBb map1Metric map2Metric map1Area map2Area 


        let l3 = filter (\(a,b) -> b /= 0) $ sort ( map (\(x,y) -> (x,truncate y)) $ toListShowIndex $ ansAaBb)

        --print $ Bin.lookupTree 90 map2

        let prod =  Tree3.tensorContr3 (0,0) Tree3.delta3
        let prod2 = tensorContr3 (0,0) delta3

        --print $ ( map (\(x,y) -> (x,Tree3.toListL3 y)) $ Tree3.toListU3 Tree3.delta3)

        --print $ map (\(x,y) -> (x,toListL3 y)) $ toListU3 delta3

        let list1 =  ( map (\(a,Tree3.TensorL3 b) -> Bin.treeSize b) $  (\(Tree3.TensorU9 m) -> Bin.toAscList m) intI2Tree3)
        
        let list2 =  ( map (\(a,TensorL3 b) -> length b) $  (\(TensorU9 m) -> m) intI2)

        --print $ M.assocs map1Metric

        --print $ M.assocs map1MetricTree3

        let list3 = [((Tree3.Empty, Tree3.Empty, Tree3.Empty, Tree3.Empty, (Tree3.Append a Tree3.Empty), Tree3.Empty, Tree3.Empty, (Tree3.Append b $ Tree3.Append c Tree3.Empty)),1) | a <- [toEnum 0..toEnum 2], b <- [toEnum 0..toEnum 2], c <- [toEnum 0..toEnum 2]]

        let tens1 = Tree3.fromListT list3 

        let tens2 = map Tree3.mkTens list3

        let intAIBTree3 = Tree3.intAIB map1MetricTree3 map2MetricTree3 map1AreaTree3 map2AreaTree3

        let int3Tree3 = Tree3.interEqn3 map1MetricTree3 map2MetricTree3 map1AreaTree3 map2AreaTree3

        let flatIntTree3 = Tree3.flatInter map1AreaTree3 map2AreaTree3

        let flatIntTree2 = flatInter map1Area map2Area

        let intAIBTree2 = intAIB map1Metric map2Metric map1Area map2Area

        --print $ ( Tree3.toListShow intAIBTree3) == ( toListShow intAIBTree2)

        --print $ Tree3.toListShow intAIBTree3

        --print $ toListShow intAIBTree2

        let ansAaBb = ansatzAaBb map1Metric map2Metric map1Area map2Area 

        let ansAIBC = ansatzAIBC map1Metric map2Metric map1Area map2Area 

        let ansAaBbTree3 = Tree3.ansatzAaBb map1MetricTree3 map2MetricTree3 map1AreaTree3 map2AreaTree3

        let ansAIBCTree3 = Tree3.ansatzAIBC map1MetricTree3 map2MetricTree3 map1AreaTree3 map2AreaTree3

        print $ (Tree3.toListShow ansAaBbTree3) == (toListShow ansAaBb)

        -}

        --filter area metric symmetries

        let filter14 = [(1,2),(3,4),(1,3),(1,5),(5,6),(7,8),(5,7),(9,10),(11,12),(9,11)] 

        let filter14Eta = [(1,2),(1,3),(3,4),(1,5),(5,6),(7,8),(5,7),(5,9),(9,10),(11,12),(9,11)] 

        let sym14 = ([], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12]),([1,2,3,4,13],[5,6,7,8,14])], [], [])

        let epsAnsatz14 = getEpsForest [1..14] filter14 1 sym14

        let etaAnsatz14 = relabelAnsatzForest $ getEtaForest [1..14] filter14Eta 1 sym14

        let filter10 = [(1,2),(2,3),(3,4),(5,6),(7,8),(5,7),(9,10),(7,9)] 

        let filter10Eta = [(1,2),(1,3),(3,4),(3,5),(5,6),(7,8),(5,7),(9,10),(7,9)] 

        let sym10 = ([], [(1,2),(3,4),(5,6),(7,8)], [([1,2],[3,4]),([5,6],[7,8]),([1,2,3,4,9],[5,6,7,8,10])], [], [])

        let testSym = ([], [], [([1,2],[3,4])], [], [])

        let epsAnsatz10 = relabelAnsatzForest $ getEpsForest [1..10] filter10 1 sym10

        let etaAnsatz10 = relabelAnsatzForest $ getEtaForest [1..10] filter10Eta 1 sym10

        let epsList = mkEpsilonList (Var 1 1) [1,2,3,4,5,6,7,8,9,10]

        let filter18Eps = [(1,2),(2,3),(3,4),(5,6),(7,8),(5,7),(7,9),(9,10),(11,12),(9,11),(13,14),(11,13),(15,16),(13,15),(15,17),(17,18)] 

        let filter18Eta = [(1,2),(1,3),(3,4),(3,5),(5,6),(7,8),(5,7),(7,9),(9,10),(11,12),(9,11),(13,14),(11,13),(15,16),(13,15),(15,17),(17,18)] 
        
        let sym18 = ([(13,14),(15,16),(17,18)], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12])], [], [[[1,2,3,4,13,14],[5,6,7,8,15,16],[9,10,11,12,17,18]]])

        let eps18 = getEpsForest [1..18] filter18Eps 1 sym18

        let eta18 = getEtaForest [1..18] filter18Eta 1 sym18

        --writeFile "/cip/austausch/cgg/epsTree18.txt" $ show $ relabelAnsatzForest eps18

        --writeFile "/cip/austausch/cgg/etaTree18.txt" $ show $ relabelAnsatzForest eta18

        let epsM = epsMap 

        let ansatz4 = relabelAnsatzForest $ getEpsForest [1..4] filterList4 1 symList4 

        let ansatz4Eta = relabelAnsatzForest $ getEtaForest [1..4] filterList4 1 symList4 

        let evalList4 = areaEvalMap4 

        let ans4 = evalAllList epsM evalList4 ansatz4

        let ans4Eta = evalAllList epsM evalList4 ansatz4Eta

        let ansatz6 = relabelAnsatzForest $ getEpsForest [1..6] filterList6 1 symList6 

        let ansatz6Eta = relabelAnsatzForest $ getEtaForest [1..6] filterList6 1 symList6 

        let evalList6 = areaEvalMap6 

        let ans6 = evalAllList epsM evalList6 ansatz6

        let ans6Eta = evalAllList epsM evalList6 ansatz6Eta

        let ansatz8 = relabelAnsatzForest $ getEpsForest [1..8] filterList8 1 symList8

        let ansatz8Eta = relabelAnsatzForest $ getEtaForest [1..8] filterList8 1 symList8

        let evalList8 = areaEvalMap8 

        let ans8 = evalAllList epsM evalList8 ansatz8

        let ans8Eta = evalAllList epsM evalList8 ansatz8Eta

        let ansatz10_1 = relabelAnsatzForest $ getEpsForest [1..10] filterList10_1 1 symList10_1  

        let ansatz10_1Eta = relabelAnsatzForest $ getEtaForest [1..10] filterList10_1 1 symList10_1  

        let evalList10_1 = areaEvalMap10_1 

        let ans10_1 = evalAllList epsM evalList10_1 ansatz10_1

        let ans10_1Eta = evalAllList epsM evalList10_1 ansatz10_1Eta

        let ansatz10_2 = relabelAnsatzForest $ getEpsForest [1..10] filterList10_2 2 symList10_2 

        let ansatz10_2Eta = relabelAnsatzForest $ getEtaForest [1..10] filterList10_2 2 symList10_2 
        
        let evalList10_2 = areaEvalMap10_2 

        let ans10_2 = evalAllList epsM evalList10_2 ansatz10_2

        let ans10_2Eta = evalAllList epsM evalList10_2 ansatz10_2Eta

        let ansatz12 = relabelAnsatzForest $ getEpsForest [1..12] filterList12 1 symList12

        let ansatz12Eta = relabelAnsatzForest $ getEtaForest [1..12] filterList12 1 symList12

        let evalList12 = areaEvalMap12 

        let ans12 = evalAllList epsM evalList12 ansatz12

        let ans12Eta = evalAllList epsM evalList12 ansatz12Eta 

        let ansatz12_1 = relabelAnsatzForest $ getEpsForest [1..12] filterList12_1 1 symList12_1

        let ansatz12_1Eta = relabelAnsatzForest $ getEtaForest [1..12] filterList12_1 1 symList12_1

        let evalList12_1 = areaEvalMap12_1

        let ans12_1 = evalAllList epsM evalList12_1 ansatz12_1

        let ans12_1Eta = evalAllList epsM evalList12_1 ansatz12_1Eta

        let ansatz14_1 = relabelAnsatzForest $ getEpsForest [1..14] filterList14_1 1 symList14_1  

        let ansatz14_1Eta = relabelAnsatzForest $ getEtaForest [1..14] filterList14_1 1 symList14_1  

        let evalList14_1 = areaEvalMap14_1 

        let ans14_1 = evalAllList epsM evalList14_1 ansatz14_1

        let ans14_1Eta = evalAllList epsM evalList14_1 ansatz14_1Eta

        let ansatz14_2 = relabelAnsatzForest $ getEpsForest [1..14] filterList14_2 2 symList14_2 

        let ansatz14_2Eta = relabelAnsatzForest $ getEtaForest [1..14] filterList14_2 2 symList14_2 
        
        let evalList14_2 = areaEvalMap14_2 

        let ans14_2 = evalAllList epsM evalList14_2 ansatz14_2

        let ans14_2Eta = evalAllList epsM evalList14_2 ansatz14_2Eta

        let ansatz16_1 = relabelAnsatzForest $ getEpsForest [1..16] filterList16_1 1 symList16_1  

        let ansatz16_1Eta = relabelAnsatzForest $ getEtaForest [1..16] filterList16_1 1 symList16_1  

        let evalList16_1 = areaEvalMap16_1 

        let ans16_1 = evalAllList epsM evalList16_1 ansatz16_1

        let ans16_1Eta = evalAllList epsM evalList16_1 ansatz16_1Eta

        let ansatz16_2 = relabelAnsatzForest $ getEpsForest [1..16] filterList16_2 2 symList16_2 

        let ansatz16_2Eta = relabelAnsatzForest $ getEtaForest [1..16] filterList16_2 2 symList16_2 
        
        let evalList16_2 = areaEvalMap16_2 

        let ans16_2 = evalAllList epsM evalList16_2 ansatz16_2

        let ans16_2Eta = evalAllList epsM evalList16_2 ansatz16_2Eta

        let ansatz18 = relabelAnsatzForest $ getEpsForest [1..18] filterList18 1 symList18

        let ansatz18Eta = relabelAnsatzForest $ getEtaForest [1..18] filterList18 1 symList18

        let evalList18 = areaEvalMap18 

        let ans18 = evalAllList epsM evalList18 ansatz18 :: [([(Int,Rational)],Int)]

        let ans18Eta = evalAllList epsM evalList18 ansatz18Eta :: [([(Int,Rational)],Int)]

        writeFile "/cip/austausch/cgg/eta18List.txt" $ show ans18Eta 

        print "eta done!"

        writeFile "/cip/austausch/cgg/epsilon18List.txt" $ show ans18


       


        

       
        