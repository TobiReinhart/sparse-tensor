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

module Main (
 main
) where

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
    import Order2Int
    import Order3Int
     

    main = do

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

        let pdeTotal = combinePdes pde pdeProlonged

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

        let symLabel = ([(13,14)], [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12)], [([1,2],[3,4]),([5,6],[7,8]),([9,10],[11,12])], [], []) 

        let symPos = ([(1,2),(3,4),(5,6),(7,8),(9,10),(11,12),(13,14)], [], [], [], [[[1,2],[3,4],[5,6],[7,8],[9,10],[11,12],[13,14]]])

        let ans = map mkAns ansatz 

        let symAns = symAnsSet ([],[],[],[],[]) symLabel ans

        let inds = getRepInds "abcdefghijklpq" symAns

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

        let pertAns1Sym = symAnsSetPert symLabel pertAns1

        let pertAns1SymInds = getRepIndsPert "abcdefghijklpq" pertAns1Sym

        let vals = evalFullAns areaEvalMap14 [pertAns1Sym !! 0]

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

        writeFile "intAaBbC_15_1_19.txt" $ showEqnsFlat ansatzCondAaBbC 








