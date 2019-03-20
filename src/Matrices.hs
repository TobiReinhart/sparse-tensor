module Matrices (writeMatrices) where

import AnsatzMatrices
import DiffeoMatrices
import IntMatrices

ansaetze :: [(String, String)]
ansaetze = [("ansatzA.dat",     showMatLab ansatzAMat),
            ("ansatzAa.dat",    showMatLab ansatzAaMat),
            ("ansatzAI.dat",    showMatLab ansatzAIMat),
            ("ansatzAB.dat",    showMatLab ansatzABMat),
            ("ansatzABb.dat",   showMatLab ansatzABbMat),
            ("ansatzABJ.dat",   showMatLab ansatzABJMat),
            ("ansatzAaBb.dat",  showMatLab ansatzAaBbMat),
            ("ansatzAaBJ.dat",  showMatLab ansatzAaBJMat),
            ("ansatzAIBJ.dat",  showMatLab ansatzAIBJMat)]

diffeos :: [(String, String)]
diffeos = [("diffeo_0_0.dat",    showMatLab diffeo_0_0Mat),
           ("diffeo_0_1.dat",    showMatLab diffeo_0_1Mat),
           ("diffeo_0_2.dat",    showMatLab diffeo_0_2Mat),
           ("diffeo_1_0_0.dat",  showMatLab diffeo_1_0_0Mat),
           ("diffeo_1_0_2.dat",  showMatLab diffeo_1_0_2Mat),
           ("diffeo_1_1_1.dat",  showMatLab diffeo_1_1_1Mat),
           ("diffeo_1_2_0.dat",  showMatLab diffeo_1_2_0Mat),
           ("diffeo_1_2_2.dat",  showMatLab diffeo_1_2_2Mat)]

ints :: [(String, String)]
ints = [("intABJ.dat", showMatLab intABJMat)]

writeMat :: String -> String -> IO ()
writeMat name mat = writeFile name $ "1 50086 0\n" ++ mat

writeMatrices :: IO ()
writeMatrices = sequence_ $ map (uncurry writeMat) (ansaetze ++ diffeos ++ ints)
