{-# LANGUAGE Rank2Types, FlexibleContexts #-}
module NPNTool.PetriNet (
  -- * Datatypes
  Net(..), Trans(..), SS,
  PTNet, PTMark, PTTrans, PTPlace,
  -- * General and abstract functions, opertations
  HLArc, LLArc, annotate,
  -- * P/T nets specific functions
  enabled, enabledS,
  fire, fireSequence_, fireSequence,
  reachabilityGraph, reachabilityGraph',
  postP, preP
  ) where

import Prelude
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import Data.Monoid
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Graph.Inductive (Gr)
import qualified Data.Graph.Inductive as G
--import Data.Graph.Inductive.NodeMap
import NPNTool.NodeMap
import qualified Data.Foldable as F

newtype Trans = Trans { name :: String }
           deriving (Eq,Ord)
                      
instance Show Trans where
  show = name

-- | A generalized net datatype  
data Net p t n m = Net 
     { places :: Set p -- ^ A set of places
     , trans :: Set t -- ^ A set of transitions
     , pre :: t -> n p -- ^ The pre function
     , post :: t -> n p -- ^ The post function
     , initial :: m p -- ^ Initial marking
     }

newtype HLArc a p = Arc { unArc :: (MultiSet (a p)) }
type LLArc   = HLArc Identity
type PTNet   = Net PTPlace Trans MultiSet MultiSet
type PTMark  = MultiSet PTPlace
type PTTrans = Trans
type PTPlace = Int

-- | Annotate net using additional functors of place/transition relationship
annotate :: (Functor a, Functor n) => 
              Net p t n m -> (p -> t -> a p) -> (t -> p -> a p) -> Net p t (Compose n a) m
annotate n f g = n { pre = \t -> Compose $ fmap (flip f t) (pre n t) 
                   , post = \t -> Compose $ fmap (g t) (post n t) }

-- | Whether some transition is enabled
enabled :: PTNet -> PTMark -> PTTrans -> Bool
enabled (Net {pre=pre}) marking =
  (`MSet.isSubsetOf` marking)  . pre

-- | Whether a set of transitions is enabled
-- Note that there is different then checking whether each
-- transition in the set is enabled
enabledS :: PTNet -> PTMark -> Set PTTrans -> Bool
enabledS (Net {pre=pre}) marking =
  (`MSet.isSubsetOf` marking) . (F.foldMap pre)

-- | The marking after some transitions is fired
fire :: PTNet -> PTMark -> PTTrans -> PTMark
fire (Net {pre=pre, post=post}) mark t =
  (mark MSet.\\ pre t) <> post t

fireSequence_ :: PTNet -> PTMark -> [PTTrans] -> PTMark
fireSequence_ n = foldl (fire n)

fireSequence :: PTNet -> PTMark -> [PTTrans] -> Maybe PTMark
fireSequence n = F.foldlM (fire' n)
  where fire' n m t = if enabled n m t then Just (fire n m t) else Nothing
        
-- reuse types from StateSpace module

type SS = Gr PTMark PTTrans


-- | The reachability graph of a Petri Net
reachabilityGraph :: PTNet -> SS
reachabilityGraph = snd. reachabilityGraph'          

-- | The reachability graph of a Petri Net together with a 'NodeMap'                    
reachabilityGraph' :: PTNet -> (NodeMap PTMark, SS)
reachabilityGraph' net = snd $ run G.empty $ 
                        insMapNodeM (initial net) >> go (Set.singleton (initial net))
  where go work | Set.null work = return ()
                | otherwise     = do
          let m = (head . Set.toList) work
                  --- Better way to pick an arbitrary M from the set Work?
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
  
-- | Post-transitions of a place
postP :: (Eq p, F.Foldable n) => p -> Net p t n m -> [t]
postP p (Net {trans=trans, pre=preT, post=postT}) = 
  F.foldMap (\x -> if F.any (== p) (preT x) then [x] else []) trans

-- | Pre-transitions of a place  
preP :: (Eq p, F.Foldable n) => p -> Net p t n m -> [t]
preP p (Net {trans=trans, pre=preT, post=postT}) = 
  F.foldMap (\x -> if F.any (== p) (postT x) then [x] else []) trans
