module PerturbationTree (


) where

    import Data.List
    import qualified Data.IntMap.Strict as I
    import qualified Data.Sequence as S
    import Data.Foldable
    import Data.Maybe
    import Data.Tree 

    type Eta = (Int,Int)

    type Epsilon = (Int,Int,Int,Int)

    type Var = I.IntMap Rational 

    --use the following data type for nodes in Data.Tree !!!

    data AnsatzNode = EpsilonNode Epsilon | EtaNode Eta | EpsilonLeaf Epsilon Var | EtaLeaf Eta Var deriving (Show, Eq)

    sortEta :: Eta -> Eta 
    sortEta (i,j) = (min i j, max i j)

    sortEpsilon :: Epsilon -> Epsilon
    sortEpsilon (i,j,k,l) = (a,b,c,d)
        where
            [a,b,c,d] = sort [i,j,k,l]

    signEpsilon :: Epsilon -> Int
    signEpsilon (i,j,k,l) = (-1)^(length $  filter (==True) [j>i,k>i,l>i,k>j,l>j,l>k])


    mkEtaSeq :: S.Seq Int -> S.Seq Eta 
    mkEtaSeq seq = fmap f seq2
            where 
                seq2 = S.chunksOf 2 seq 
                f = \s -> (S.index s 0, S.index s 1)

    mkEpsilonSeq :: S.Seq Int -> (Epsilon,S.Seq Eta)
    mkEpsilonSeq seq = (eps,etaSeq)
            where 
                eps = (S.index seq 0, S.index seq 1, S.index seq 2, S.index seq 3)
                etaSeq = mkEtaSeq $ S.drop 4 seq 

    --the next step is making a tree from the top Sort Seq

    mkTreeEta :: Var -> S.Seq Eta -> Tree AnsatzNode 
    mkTreeEta var seq 
                | S.length seq == 1 = Node (EtaLeaf (S.index seq 0) var) []  
                | otherwise = Node (EtaNode (S.index seq 0)) [mkTreeEta var (S.drop 1 seq)]

    mkTreeEpsilon :: Var -> (Epsilon,S.Seq Eta) -> Tree AnsatzNode 
    mkTreeEpsilon var (eps,seq) 
                | S.length seq == 0 = Node (EpsilonLeaf eps var) []
                | otherwise = Node (EpsilonNode eps) [mkTreeEta var seq]

    getEta :: AnsatzNode -> Eta
    getEta (EtaNode i ) = i
    getEta (EtaLeaf i j) = i
    getEta x = error "Node is an Epsilon Node!"

    getEpsilon :: AnsatzNode -> Epsilon
    getEpsilon (EpsilonNode i ) = i
    getEpsilon (EpsilonLeaf i j) = i
    getEpsilon x = error "Node is an Eta Node!"

    --the next step is symmetrizing the trees

    multVarNode :: Rational -> AnsatzNode -> AnsatzNode
    multVarNode n (EtaLeaf i var) = EtaLeaf i $ I.map ((*) n) var 
    multVarNode n (EpsilonLeaf i var) = EpsilonLeaf i $ I.map ((*) n) var 
    multVarNode n i = i 


    swapLabelF :: Eq a => (a,a) -> a -> a 
    swapLabelF (x,y) z
            | x == z = y
            | y == z = x
            | otherwise = z 

    swapLabelEta :: (Int,Int) -> Eta -> Eta
    swapLabelEta l (e1,e2) = sortEta (swapLabelF l e1, swapLabelF l e2)

    swapLabelEpsilon :: (Int,Int) -> Epsilon -> (Int,Epsilon)
    swapLabelEpsilon l (e1,e2,e3,e4) = (epsSign,sortEpsilon newEps)
                    where
                        newEps = (swapLabelF l e1, swapLabelF l e2, swapLabelF l e3, swapLabelF l e4)
                        epsSign = signEpsilon newEps


    swapLabelNode :: (Int,Int) -> AnsatzNode -> AnsatzNode
    swapLabelNode j (EtaNode eta) = EtaNode (swapLabelEta j eta)
    swapLabelNode j (EtaLeaf eta var) = EtaLeaf (swapLabelRta j eta) var
    swapLabelNode j (EpsilonNode eps) = EpsilonNode (swapLabelEpsilon j eps)
    



    {-

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

    addAnsatz :: AnsatzTree -> AnsatzForest -> AnsatzForest
    addAnsatz (EtaLeaf eta var) ansF 
                | isJust indEta = S.adjust (addVarLeaf var) $ fromJust indEta
                | otherwise = 
                where
                    indEta = S.elemIndexL eta $ fmap getEta ansF
    addAnsatz (EpsilonLeaf eps var) ansF 
                | isJust indEps = and $ fmap (\ a -> lookupAnsatz a (getSubForest $ S.index ansF $ fromJust indEps)) restAns
                | otherwise = False
                where
                    indEps = S.elemIndexL eps $ fmap getEpsilon ansF

    addAnsatz (EtaNode eta restAns) ansF
                | isJust indEta = and $ fmap (\ a -> lookupAnsatz a (getSubForest $ S.index ansF $ fromJust indEta)) restAns
                | otherwise = False
                where
                    indEta = S.elemIndexL eta $ fmap getEta ansF
    addAnsatz (EpsilonNode eps restAns) ansF
                | isJust indEps = and $ fmap (\ a -> lookupAnsatz a (getSubForest $ S.index ansF $ fromJust indEps)) restAns
                | otherwise = False
                where
                    indEps = S.elemIndexL eps $ fmap getEpsilon ansF


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
    