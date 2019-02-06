module Main (main) where

import Tensor2
import qualified Data.Map.Strict as M

main = do
        let trian2 = triangleMap2 :: M.Map Ind Int
        let trian3 = triangleMap3 :: M.Map Ind Int
        let trianArea = triangleMapArea :: M.Map Ind Int
        let intCondAIB = intAIB trianArea trianArea trian2 trian2 
        print intCondAIB
