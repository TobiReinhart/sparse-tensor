module IntEquations where ()

import Intertwiners ()

import TensorTreeNumeric4 (Tensor, VarMap, multVarsMap,
                           tensorProd8, tensorContr20,
                           tensorSMult, tensorProd8With,
                           tensorContrWith20, tensorTransWithU20)
import PertubationTree2_2 ()

mult :: VarMap -> Rational -> VarMap
mult = flip multVarsMap

invAreaDerivativeFlat :: Tensor 2 0 0 0 0 0 0 0 Rational
invAreaDerivativeFlat = contracted
    where
        product    = tensorProd8 inverseDerivativeInt $
                     tensorProd8 flatAreaInvNoEps flatAreaInvNoEps
        contracted = tensorContr20 (2,0) $ tensorContr20 (3,1) product

intEquation :: Tensor 2 0 0 0 0 0 4 0 VarMap
intEquation = result
    where
        ansatz   =
        flatInt1 = tensorContr20 (1,1) $
                   tensorProd8 intCondInt1 flatAreaInvNoEps
        flatInt2 = tensorSMult (-1) $
                   tensorContr20 (0,1) $
                   tensorContr20 (0,2) $
                   tensorProd8 intCondInt1 $
                   tensorProd8 flatAreaNoEps invAreaDerivativeFlat
        block1'  = tensorProdWith8 mult ansatz flatInt1
        block1   = tensorContrWith20 (0,0) addVars block1'
        block2'  = tensorProdWith8 mult ansatz flatInt2
        block2'' = tensorContrWith20 (0,0) addVars block2'
        block2   = tensorTransWithU20 (0,1) addVars block2''
        result   = tensorAddWith8 addVars block1 block2
