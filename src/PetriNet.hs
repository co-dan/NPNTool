{-# LANGUAGE Rank2Types, FlexibleContexts #-}
module PetriNet (
  Net(..), Trans(..), SS,
  PTNet, PTMark, PTTrans,
  enabled, fire,
  reachabilityGraph
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import Data.Monoid
import Data.Graph.Inductive (Gr)
import qualified Data.Graph.Inductive as G
--import Data.Graph.Inductive.NodeMap
import NodeMap
import qualified Data.Foldable as F

data Trans = Trans { name :: String }
           deriving (Eq,Ord)
                      
instance Show Trans where
  show = name

data Net p n m = Net 
               { places :: Set p
               , trans :: Set Trans
               , pre :: Trans -> n p
               , post :: Trans -> n p
               , initial :: m p
               }

type PTNet = Net Int MultiSet MultiSet
type PTMark = MultiSet Int
type PTTrans = Trans


enabled :: PTNet -> PTMark -> PTTrans -> Bool
enabled (Net {pre=pre}) marking =
  (`MSet.isSubsetOf` marking)  . pre

fire :: PTNet -> PTMark -> PTTrans -> PTMark
fire (Net {pre=pre, post=post}) mark t =
  (mark MSet.\\ pre t) <> post t


-- reuse types from StateSpace module

type SS = Gr PTMark PTTrans

--- How to pick an arbitrary M from the set Work?
reachabilityGraph :: PTNet -> SS
reachabilityGraph net = run_ G.empty $ 
                        insMapNodeM (initial net) >> go (Set.singleton (initial net))
  where go work | Set.null work = return ()
                | otherwise     = do
          let m = (head . Set.toList) work 
              work' = Set.delete m work
          work'' <- F.foldrM (act net m) work' (trans net)
          go work'' 
  
act :: G.DynGraph g => 
       PTNet -> PTMark -> PTTrans -> Set PTMark -> NodeMapM PTMark PTTrans g (Set PTMark)
act net m t w =
  if enabled net m t
  then do 
     let m' = fire net m t
     present <- lookupNodeM m'
     w' <- case present of
                Just _ -> return w
                Nothing -> do
                  insMapNodeM m'
                  return (Set.insert m' w)
     insMapEdgeM (m,m',t)
     return w'
  else return w
  
