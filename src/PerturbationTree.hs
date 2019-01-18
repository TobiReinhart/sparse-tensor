module PerturbationTree (


) where

    import Data.List
    import qualified Data.IntMap.Strict as I
    import qualified Data.Sequence as S
    import Data.Foldable
    import Data.Maybe

    type Eta = (Int,Int)

    type Epsilon = (Int,Int,Int,Int)

    type Var = I.IntMap Rational 

    sortEta :: Eta -> Eta 
    sortEta (i,j) = (min i j, max i j)

    sortEpsilon :: Epsilon -> Epsilon
    sortEpsilon (i,j,k,l) = (a,b,c,d)
        where
            [a,b,c,d] = sort [i,j,k,l]

    signEpsilon :: Epsilon -> Int
    signEpsilon (i,j,k,l) = (-1)^(length $  filter (==True) [j>i,k>i,l>i,k>j,l>j,l>k])

    --now we need tree data types for the 2 ansätze
    
    data AnsatzTree = EpsilonNode Epsilon (S.Seq AnsatzTree) |
                          EpsilonLeaf Epsilon Var | 
                          EtaNode Eta (S.Seq AnsatzTree) | 
                          EtaLeaf Eta Var deriving (Show)

    type AnsatzForest = S.Seq AnsatzTree

    mkTreeEta :: Var -> S.Seq Int -> AnsatzTree
    mkTreeEta var seq 
                        | S.length seq == 2 = EtaLeaf (S.index seq 0, S.index seq 1) var  
                        | otherwise = EtaNode (S.index seq 0, S.index seq 1) $ S.singleton (mkTreeEta var (S.drop 2 seq))

    mkTreeEpsilon :: Var -> S.Seq Int -> AnsatzTree
    mkTreeEpsilon var seq 
                        | S.length seq == 4 = EpsilonLeaf (S.index seq 0, S.index seq 1, S.index seq 2, S.index seq 3) var
                        | otherwise = EpsilonNode (S.index seq 0, S.index seq 1, S.index seq 2, S.index seq 3) $ S.singleton (mkTreeEta var   (S.drop 4 seq))


    lookupAnsatz :: AnsatzTree -> AnsatzForest -> Bool
    lookupAnsatz (EtaLeaf eta var) ansF 
                | elem eta (fmap getEta ansF) = True
                | otherwise = False
    lookupAnsatz (EpsilonLeaf eps var) ansF 
                | elem eps (fmap getEpsilon ansF) = True
                | otherwise = False
    lookupAnsatz (EtaNode eta restAns) ansF
                | isJust indEta = and $ fmap (\ a -> lookupAnsatz a (getSubForest $ S.index ansF $ fromJust indEta)) restAns
                | otherwise = False
                where
                    indEta = S.elemIndexL eta $ fmap getEta ansF
    lookupAnsatz (EpsilonNode eps restAns) ansF
                | isJust indEps = and $ fmap (\ a -> lookupAnsatz a (getSubForest $ S.index ansF $ fromJust indEps)) restAns
                | otherwise = False
                where
                    indEps = S.elemIndexL eps $ fmap getEpsilon ansF
{-
    addAnsatz :: AnsatzTree -> AnsatzForest -> AnsatzForest
    addAnsatz (EtaLeaf eta var) ansF 
                | length indsEta > 1 = 
                | otherwise = False
                where
                    indsEta = elemIndices eta $ map getEta ansF
                    subFEta = getSubForest $ ansF !! (indsEta !! 0)
    addAnsatz (EpsilonLeaf eps var) ansF 
                | length indsEps > 1 = True
                | otherwise = False
                where
                    indsEps = elemIndices eps $ map getEpsilon ansF
                    subFEps = getSubForest $ ansF !! (indsEps !! 0)
    addAnsatz (EtaNode eta restAns) ansF
                | length indsEta > 1 = and $ map (\ a -> lookupAnsatz a subFEta) restAns
                | otherwise = False
                where
                    indsEta = elemIndices eta $ map getEta ansF
                    subFEta = getSubForest $ ansF !! (indsEta !! 0)
    addAnsatz (EpsilonNode eps restAns) ansF
                | length indsEps > 1 = and $ map (\ a -> lookupAnsatz a subFEps) restAns
                | otherwise = False
                where
                    indsEps = elemIndices eps $ map getEpsilon ansF
                    subFEps = getSubForest $ ansF !! (indsEps !! 0)

-}
    getEta :: AnsatzTree -> Eta
    getEta (EtaNode i j) = i
    getEta (EtaLeaf i j) = i

    getEpsilon :: AnsatzTree -> Epsilon
    getEpsilon (EpsilonNode i j) = i
    geEpsilon (EpsilonLeaf i j) = i
    
    getSubForest :: AnsatzTree -> AnsatzForest
    getSubForest (EpsilonNode i s) = s
    getSubForest (EtaNode i s) = s
    getSubForest x = error "wrong type of forest"
   
    


    isLeaf :: AnsatzTree -> Bool
    isLeaf (EtaLeaf i j) = True
    isLeaf (EpsilonLeaf i j) = True
    isLeaf x = False
    
    addVarLeaf :: Var -> AnsatzTree -> AnsatzTree
    addVarLeaf var (EtaLeaf i j) = EtaLeaf i (I.unionWith (+) var j)
    addVarLeaf var (EpsilonLeaf i j) = EpsilonLeaf i (I.unionWith (+) var j)
    addVarLeaf var x = error "wrong ansatzTree"

    {-
        
    insertSeqEta :: Var -> AnsatzForest -> S.Seq [Int] -> AnsatzForest 
    insertSeqEta var ans seq 
                        | S.length ans == 0 = mkSeqAnsatzForestEta var seq
                        | seq1 == (getNodeVal ans1) && (isLeaf ans1) = (S.<|) (addVarLeaf var ans1) ansRest  
                        | seq1 == (getNodeVal ans1) = (S.<|) (EtaNode (getNodeVal ans1) (insertSeqEta var (getSubForest ans1) seqRest)) ansRest 
                        | otherwise = (S.<|) ans1 $ insertSeqEta var ansRest seq
                        where
                            ans1 = S.index ans 0
                            seq1 = S.index seq 0
                            ansRest = S.index (S.tails ans ) 1
                            seqRest = S.index (S.tails seq ) 1
    
    mkSeqAnsatzForestEta :: Var -> S.Seq [Int] -> AnsatzForest
    mkSeqAnsatzForestEta var seq 
                            | S.length seq == 1 = S.singleton $ EtaLeaf (S.index seq 0) var  
                            | otherwise = S.singleton $ EtaNode (S.index seq 0) $ mkSeqAnsatzForestEta var (S.index (S.tails seq ) 1) 
    
    -}
    