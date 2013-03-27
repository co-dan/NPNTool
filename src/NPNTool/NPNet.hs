module NPNTool.NPNet (
  Expr(..), vars, MSExpr(..),
  Labelling, SNet(..), 
  ) where

import NPNTool.PetriNet
import Data.Set (Set)
import Data.MultiSet (MultiSet)
import qualified Data.Foldable as F

-- | Arc expressions for higher level nets
data Expr v c = Var v
              | Const c
              | Plus (Expr v c) (Expr v c)
              deriving (Show,Eq,Ord)
                       
vars :: Expr v c -> [v]
vars e = (vars' e) []
  where vars' (Var v) = (v:)
        vars' (Const _) = id
        vars' (Plus e1 e2) = vars' e1 . vars' e2

newtype MSExpr v c p = MSExpr (MultiSet (Expr v c, p))

instance F.Foldable (MSExpr v c) where 
  foldMap f (MSExpr ms) = F.foldMap (f . snd) ms

type Labelling l = Trans -> Maybe l 

data SNet lab var con = SNet
     { net :: Net PTPlace Trans (MSExpr var con) MultiSet
     , elementNets :: [(PTNet, Labelling lab)] -- element nets together with
                      -- transition labelling functions
     , labelling :: Labelling lab
     , labels :: Set lab
     }  

