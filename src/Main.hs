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

        let int2_1Symbol = mkEqnSparseintCond2_1Symbol trianintCond $ intCond2_1Symbol map1Area map2Area map1Metric map2Metric 

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

        writeFile "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2Data/AnsatzTest2.txt"  $ inds


        
        