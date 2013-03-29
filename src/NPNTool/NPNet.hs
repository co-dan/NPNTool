{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NPNTool.NPNet (
  Expr(..), vars, SArc(..),
  Labelling, SNet(..), ElemNet(..), NPMark(..),
  exprMult
  ) where

import NPNTool.PetriNet
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.Foldable as F
import Data.Monoid

-- | Arc expressions for higher level nets
data Expr v c = Var v
              | Const c
              | Plus (Expr v c) (Expr v c)
              deriving (Show,Eq,Ord)

-- | Mutiply expression by some positive integer
exprMult :: Expr v c -> Int -> Expr v c
exprMult x n = if n == 1 then x else Plus x (exprMult x (n-1))

vars :: Expr v c -> [v]
vars e = (vars' e) []
  where vars' (Var v) = (v:)
        vars' (Const _) = id
        vars' (Plus e1 e2) = vars' e1 . vars' e2

newtype SArc v c p = SArc { unSArc :: Set (Expr v c, p) }
                   deriving (Monoid,Show)
                            
instance F.Foldable (SArc v c) where 
  foldMap f (SArc ms) = F.foldMap (f . snd) ms

type Labelling l = Trans -> Maybe l 

type ElemNet l = (PTNet, Labelling l)
type ElemNetId = Int
                    
newtype NPMark l a = NPMark { unMark :: MultiSet (Either a ElemNetId) }

data SNet lab var con = SNet
     { net :: Net PTPlace Trans (SArc var con) (NPMark lab)
     , elementNets :: [ElemNet lab] -- element nets together with
                      -- transition labelling functions
     , labelling :: Labelling lab
     , labels :: Set lab
     }  

