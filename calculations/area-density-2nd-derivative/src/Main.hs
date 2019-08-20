{-#LANGUAGE DataKinds#-}

import Math.Tensor
import Math.Tensor.LorentzGenerator
import Math.Tensor.Examples.Gravity
import Math.Tensor.Examples.Gravity.DiffeoSymEqns
import Math.Tensor.Examples.Gravity.Schwarzschild

import Data.Ratio

import qualified Data.IntMap.Strict as I

main = do

  let ans0 = fromListT6' [(([],[],[],[],[],[]), AnsVar $ I.fromList [(1,1)])] :: ATens 0 0 0 0 0 0 AnsVarR
  let (eta4,eps4,ans4) = mkAnsatzTensorFastAbs 4 symList4 areaList4 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 1 0 0 0 0 0 AnsVarR)
  let (eta6,eps6,ans6) = mkAnsatzTensorFastAbs 6 symList6 areaList6 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 1 0 1 0 0 0 AnsVarR)
  let (eta8,eps8,ans8) = mkAnsatzTensorFastAbs 8 symList8 areaList8 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 2 0 0 0 0 0 AnsVarR)
  let (eta10_1,eps10_1,ans10_1) = mkAnsatzTensorFastAbs 10 symList10_1 areaList10_1 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 2 0 0 0 2 0 AnsVarR)
  let (eta10_2,eps10_2,ans10_2) = mkAnsatzTensorFastAbs 10 symList10_2 areaList10_2 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 2 0 1 0 0 0 AnsVarR)
  let (eta12,eps12,ans12) = mkAnsatzTensorFastAbs 12 symList12 areaList12 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 3 0 0 0 0 0 AnsVarR)
  let evalL = map (\(x,_,_) -> x) areaList14_1
  let (eta14_1,eps14_1,ans14_1) = mkAnsatzTensorFast 14 symList14_1 evalL :: (AnsatzForestEta, AnsatzForestEpsilon, STTens 14 0 AnsVarR)
  let (eta14_2,eps14_2,ans14_2) = mkAnsatzTensorFastAbs 14 symList14_2 areaList14_2 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 3 0 1 0 0 0 AnsVarR)
  let (_,_,ans14_1') = mkAnsatzTensorIncremental 14 symList14_1 evalL :: (AnsatzForestEta, AnsatzForestEpsilon, STTens 14 0 AnsVarR)
  let (_,_,ans14_2') = mkAnsatzTensorIncrementalAbs 14 symList14_2 areaList14_2 :: (AnsatzForestEta, AnsatzForestEpsilon, ATens 3 0 1 0 0 0 AnsVarR)

  let zeroT = fromListT6' [(([],[],[],[],[],[]), AnsVar $ I.fromList [(1,0)])] :: ATens 0 0 0 0 0 0 AnsVarR

  putStrLn "=== construct epsilon ansatz AI and draw the ansatz forest ==="
  putStr $ unlines $ map ("-->" ++) $ lines $ drawAnsatzEpsilon eps6

  putStrLn ""
  putStrLn "=== calculate ansatz with 14 spacetime indices and print rank (should be 110) ==="

  print $ tensorRank2' ans14_1

  putStrLn ""
  putStrLn "=== calculate ansatz with 14 spacetime indices and print rank (should be 110), incremental method ==="

  print $ tensorRank2' ans14_1'

  putStrLn ""
  putStrLn "=== calculate ansatz with 3 area indices and one two-index and print rank (should be 75) ==="
  print $ tensorRank6' ans14_2

  putStrLn ""
  putStrLn "=== calculate ansatz with 3 area indices and one two-index and print rank (should be 75), incremental method ==="
  print $ tensorRank6' ans14_2'

  putStrLn ""
  putStrLn "=== calculate einstein tensor for schwarzschild metric and evaluate at spacetime point (should be ZeroTensor) ==="
  print $ evalSec (einstein (2::Double)) [3,3,3,3]

  putStrLn ""
  putStrLn "=== encode ansatz tensor as ByteString ==="
  let bs = encodeTensor ans4
  print bs

  putStrLn ""
  putStrLn "=== decode ansatz tensor from ByteString (should give an error, because of wrong type) ==="
  let t' = decodeTensor bs :: Either String (STTens 0 1 (SField Double))
  print t'

  putStrLn ""
  putStrLn "=== decode ansatz tensor from ByteString (should succeed) ==="
  let t'' = decodeTensor bs :: Either String (ATens 1 0 0 0 0 0 AnsVarR)
  print t''

  putStrLn ""
  putStrLn "=== construct statically-typed index list from untyped list ==="
  let il = fromList [1,2,3,4,5] :: Maybe (IndList 5 Int)
  print il

  putStrLn ""
  putStrLn "=== construct statically-typed index list from untyped list of wrong length (should give an error) ==="
  let il' = fromList [1,2,3,4,5] :: Maybe (IndList 7 Int)
  print il'

  putStrLn ""
  putStrLn "=== construct statically-typed index list from untyped list of wrong length (should give an error) ==="
  let il'' = fromList [1,2,3,4,5] :: Maybe (IndList 1 Int)
  print il''
