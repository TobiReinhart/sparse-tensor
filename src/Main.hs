{-# LANGUAGE DataKinds #-}

module Main (
 main
) where

import PerturbationTree2_3
import TensorTreeNumeric4_2
import FlatTensorEquations

import qualified Data.ByteString.Lazy as BS
import qualified Data.Eigen.SparseMatrix as Sparse
import qualified Data.Eigen.LA as LA

main =
    do
       ans20' <- BS.readFile "ansatz/ans20.dat.gz"
       let ans20 = decodeTensor ans20' :: ATens 5 0 0 0 0 0 AnsVar

       ans16' <- BS.readFile "ansatz/ans16.dat.gz"
       let ans16 = decodeTensor ans16' :: ATens 4 0 0 0 0 0 AnsVar

       let (_,_,ans12) = mkAnsatzTensorFast 12 filterList12 symList12 areaList12IndsEta areaList12IndsEps
       let (_,_,ans8) = mkAnsatzTensorFast 8 filterList8 symList8 areaList8IndsEta areaList8IndsEps
       let (_,_,ans4) = mkAnsatzTensorFast 4 filterList4 symList4 areaList4IndsEta areaList4IndsEps

       let r4 = tensorRank ans4
       let r8 = tensorRank ans8
       let r12 = tensorRank ans12
       let r16 = tensorRank ans16
       let r20 = tensorRank ans20

       let ans4S  = shiftLabels6 (r8 + r12 + r16 + r20) ans4
       let ans8S  = shiftLabels6 (r12 + r16 + r20)      ans8
       let ans12S = shiftLabels6 (r16 + r20)            ans12
       let ans16S = shiftLabels6 r20                    ans16
       let ans20S =                                     ans20

       let eq1 = eqn1     ans4
       let eq2 = eqn1A    ans4  ans8
       let eq3 = eqn1AB   ans8  ans12
       let eq4 = eqn1ABC  ans12 ans16
       let eq5 = eqn1ABCD ans16 ans20

       let sys1 = singletonTList eq1
       let sys2 = eq2 &> sys1
       let sys3 = eq3 &> sys2
       let sys4 = eq4 &> sys3
       let sys5 = eq5 &> sys4

       let sys1Rank = LA.rank LA.FullPivLU $ Sparse.toMatrix $ toEMatrix6 sys1
       let sys2Rank = LA.rank LA.FullPivLU $ Sparse.toMatrix $ toEMatrix6 sys2
       let sys3Rank = LA.rank LA.FullPivLU $ Sparse.toMatrix $ toEMatrix6 sys3
       let sys4Rank = LA.rank LA.FullPivLU $ Sparse.toMatrix $ toEMatrix6 sys4
       let sys5Rank = LA.rank LA.FullPivLU $ Sparse.toMatrix $ toEMatrix6 sys5

       let sym1 = eqn1     ans4
       let sym2 = eqn1A    ZeroTensor ans8
       let sym3 = eqn1AB   ZeroTensor ans12
       let sym4 = eqn1ABC  ZeroTensor ans16
       let sym5 = eqn1ABCD ZeroTensor ans20

       let sym1Rank = tensorRank sym1
       let sym2Rank = tensorRank sym2
       let sym3Rank = tensorRank sym3
       let sym4Rank = tensorRank sym4
       let sym5Rank = tensorRank sym5

       putStrLn $ "# of vars in ansatz 4  : " ++ (show r4)
       putStrLn $ "# of vars in ansatz 8  : " ++ (show r8)
       putStrLn $ "# of vars in ansatz 12 : " ++ (show r12)
       putStrLn $ "# of vars in ansatz 16 : " ++ (show r16)
       putStrLn $ "# of vars in ansatz 20 : " ++ (show r20)
       putStrLn ""
       putStrLn $ "dim of prolongation 0  : " ++ (show (r4 - sys1Rank))
       putStrLn $ "dim of prolongation 1  : " ++ (show (r4 + r8 - sys2Rank))
       putStrLn $ "dim of prolongation 2  : " ++ (show (r4 + r8 + r12 - sys3Rank))
       putStrLn $ "dim of prolongation 3  : " ++ (show (r4 + r8 + r12 + r16 - sys4Rank))
       putStrLn $ "dim of prolongation 4  : " ++ (show (r4 + r8 + r12 + r16 + r20 - sys5Rank))
       putStrLn ""
       putStrLn $ "dim of symbol 0        : " ++ (show (r4 - sym1Rank))
       putStrLn $ "dim of symbol 1        : " ++ (show (r8 - sym2Rank))
       putStrLn $ "dim of symbol 2        : " ++ (show (r12 - sym3Rank))
       putStrLn $ "dim of symbol 3        : " ++ (show (r16 - sym4Rank))
       putStrLn $ "dim of symbol 4        : " ++ (show (r20 - sym5Rank))