{-# LANGUAGE GeneralizedNewtypeDeriving,
FlexibleInstances, MultiParamTypeClasses #-}
module NPNTool.NPNet (
  Expr(..), vars, SArc(..),
  Labelling, NPNet(..), SNet, ElemNet, ElemNetId(..),
  NPMark(..),
  exprMult
  ) where

import NPNTool.PetriNet
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import qualified Data.Foldable as F
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap (IntMap)
import Data.Tuple (swap)

-- | Arc expressions for higher level nets
data Expr v c = Var v
              | Const c
              | Plus (Expr v c) (Expr v c)
              deriving (Show,Eq,Ord)

-- | Mutiply expression by some positive integer
exprMult :: Expr v c -> Int -> Expr v c
exprMult x n = if n == 1 then x else Plus x (exprMult x (n-1))

vars :: Expr v c -> [v]
vars e = vars' e []
  where vars' (Var v) = (v:)
        vars' (Const _) = id
        vars' (Plus e1 e2) = vars' e1 . vars' e2

newtype SArc v c p = SArc { unSArc :: Set (Expr v c, p) }
                   deriving (Monoid,Show)
                            
instance F.Foldable (SArc v c) where 
  foldMap f (SArc ms) = F.foldMap (f . snd) ms

type SNet lab var con = Net PTPlace Trans (SArc var con) (NPMark lab con)
type Labelling l = Trans -> Maybe l 
type ElemNet l = (PTNet, Labelling l, PTMark)
newtype ElemNetId = ElemNetId Int deriving (Eq,Ord,Show)

instance Ord v => DynNet (SNet l v Int) PTPlace Trans (NPMark l Int) where
  enabledS sn m = enabledS (toPNet sn) (markTokens m) 
  enabled sn m = enabled (toPNet sn) (markTokens m) 
  fire sn m = tokensToMark . fire (toPNet sn) (markTokens m)


toPNet :: Ord v => SNet l v Int -> PTNet
toPNet sn = Net { places  = places sn
                , trans   = trans sn
                , pre     = toTokens . (pre sn)
                , post    = toTokens . (post sn)
                , initial = markTokens (initial sn)
                }

fromPNet :: Ord v => PTNet -> SNet l v Int
fromPNet pn = Net { places  = places pn
                  , trans   = trans pn
                  , pre     = fromTokens . (pre pn)
                  , post    = fromTokens . (post pn)
                  , initial = undefined
                  }

tokensToMark :: MultiSet PTPlace -> NPMark l Int PTPlace
tokensToMark m = NPMark $ M.fromList $
                 map (\(x,y) -> (x, replicate y (Left 1))) $ MSet.toOccurList m

fromTokens :: (Ord v) => MultiSet PTPlace -> SArc v Int PTPlace
fromTokens = SArc . Set.fromList
             . map (\(pl,n) -> (exprMult (Const 1) n,pl))
             . MSet.toOccurList
  
exprToTokens :: Expr v c -> Int
exprToTokens e = toTokens e 0
  where toTokens (Var _) = (+1)
        toTokens (Const _) = (+1)
        toTokens (Plus e1 e2) = toTokens e1 . toTokens e2

toTokens :: (Ord c, Ord v) => SArc v c PTPlace -> MultiSet PTPlace
toTokens = MSet.fromOccurList . Set.toList .
           Set.map (\(e,pl) -> (pl, exprToTokens e)) . unSArc 

markTokens :: NPMark l c PTPlace -> MultiSet PTPlace
markTokens (NPMark m) =
  MSet.fromList (concatMap (\p -> replicate (length (m M.! p)) p)
                 (M.keys m))

newtype NPMark l c a = NPMark { unMark :: M.Map a [Either c ElemNetId] }
                     deriving (Show)

instance Eq (NPMark l c PTPlace) where
  (==) a b = markTokens a == markTokens b

instance Ord (NPMark l c PTPlace) where
  compare a b = markTokens a `compare` markTokens b
  
data NPNet lab var con = NPNet
     { net :: SNet lab var con
     , elementNets :: IntMap (ElemNet lab) -- element nets together with
                      -- transition labelling functions
     , labelling :: Labelling lab
     , labels :: Set lab
     }  


