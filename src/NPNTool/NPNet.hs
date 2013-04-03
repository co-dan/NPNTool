{-# LANGUAGE GeneralizedNewtypeDeriving,
FlexibleInstances, MultiParamTypeClasses #-}
module NPNTool.NPNet (
  Expr(..), vars, SArc(..),
  Labelling, NPNet(..), SNet, ElemNet, NPMark(..),
  exprMult, showMarking
  ) where

import NPNTool.PetriNet
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import qualified Data.Foldable as F
import Data.Monoid
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

instance Ord v => DynNet (SNet l v Int) PTPlace Trans (NPMark l Int) where
  enabledS sn m = enabledS (toPNet sn) (markTokens sn m) 
  enabled sn m = enabled (toPNet sn) (markTokens sn m) 
  fire sn m = tokensToMark . fire (toPNet sn) (markTokens sn m)

toPNet :: SNet l v Int -> PTNet
toPNet sn = Net { places  = places sn
                , trans   = trans sn
                , pre     = toTokens . (pre sn)
                , post    = toTokens . (post sn)
                , initial = markTokens sn (initial sn)
                }

fromPNet :: Ord v => PTNet -> SNet l v Int
fromPNet pn = Net { places  = places pn
                  , trans   = trans pn
                  , pre     = fromTokens . (pre pn)
                  , post    = fromTokens . (post pn)
                  , initial = undefined
                  }

tokensToMark :: MultiSet PTPlace -> NPMark l Int PTPlace
tokensToMark m = NPMark $ \p ->
  replicate (MSet.occur p m) (Left 1) 

fromTokens :: (Ord v) => MultiSet PTPlace -> SArc v Int PTPlace
fromTokens = SArc . Set.fromList
             . map (\(pl,n) -> (exprMult (Const 1) n,pl))
             . MSet.toOccurList
  
exprToTokens :: Expr v c -> Int
exprToTokens e = toTokens e 0
  where toTokens (Var _) = (+1)
        toTokens (Const _) = (+1)
        toTokens (Plus e1 e2) = toTokens e1 . toTokens e2

toTokens :: SArc v c PTPlace -> MultiSet PTPlace
toTokens = MSet.fromOccurList . Set.toList .
           Set.map (\(e,pl) -> (pl, exprToTokens e)) . unSArc 

markTokens :: SNet l v c -> NPMark l c PTPlace -> MultiSet PTPlace
markTokens sn (NPMark m) =
  MSet.fromList (concatMap (\p -> replicate (length (m p)) p)
                 (Set.toList (places sn)))

newtype NPMark l c a = NPMark { unMark :: a -> [Either c (ElemNet l)] }

data NPNet lab var con = NPNet
     { net :: SNet lab var con
     , elementNets :: [ElemNet lab] -- element nets together with
                      -- transition labelling functions
     , labelling :: Labelling lab
     , labels :: Set lab
     }  

showMarking :: Show con => SNet lab var con -> NPMark lab con PTPlace -> String
showMarking net mark =
  show $ map (map showEN . unMark mark) (Set.toList (places net))
  where
    showEN (Left c) = Left c
    showEN (Right (_,_,m)) = Right m

