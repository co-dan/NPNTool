{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
{-# LANGUAGE TupleSections, FlexibleContexts #-}
-- | Petri net unfoldings
module NPNTool.Unfoldings (
  -- * Occurence nets and branching processes
  OccurNet, Hom, BProc, toOccurNet, 
  -- * Configurations and cuts
  Conf, Cut, cut, cutMark,
  -- * Orders and relations on nodes of branching processes
  Node, predPred, predR, preds, predTrans, conflict,
  -- * Auxiliary functions 
  pairs, fixedPoint
  ) where

import Prelude hiding (pred)
import NPNTool.PetriNet
import NPNTool.PTConstr  
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import Data.Maybe (maybe)
import Data.Foldable (foldMap, fold)

---- Occurence nets and branching processes

-- | Occurence net
type OccurNet = Net PTPlace PTTrans Set Set
-- | Homomorphism from P/T-net to occurence net                
type Hom = (PTPlace -> PTPlace, PTTrans -> PTTrans)
-- | Branching process
type BProc = (OccurNet, Hom)

toOccurNet :: PTConstr l -> OccurNet
toOccurNet c =  Net { places  = M.keysSet (p c)
                    , trans   = M.keysSet (tin c)
                                `Set.union` M.keysSet (tout c)
                                `Set.union` M.keysSet (tlab c)
                    , pre     = pre'
                    , post    = post'
                    , initial = M.keysSet $ M.filter (>0) (p c)
                    } 
  where pre'  t = maybe Set.empty msetforget (M.lookup t (tin c))
        post' t = maybe Set.empty msetforget (M.lookup t (tout c))
        msetforget = foldMap Set.singleton
        
        
---- Configurations and cuts        
        
-- | A configuration of the branching process        
-- is a set of causally closed and conflict-free events
type Conf = Set PTTrans
type Cut  = Set PTPlace

cut :: OccurNet -> Conf -> Cut
cut n c = (initial n `Set.union` postC)
          Set.\\ preC
  where postC = foldMap (post n) c
        preC = foldMap (pre n) c


cutMark :: Hom -> Set PTPlace -> MultiSet PTPlace
cutMark (h1,_) p = foldMap (MSet.singleton . h1) p


---- Orders and relations on nodes of branching processes

class Node n where
  type CoNode n 
  pred :: OccurNet -> n -> Set (CoNode n)

instance Node PTPlace where
  type CoNode PTPlace = PTTrans
  pred on p = Set.fromList $ preP p on

instance Node PTTrans where
  type CoNode PTTrans = PTPlace
  pred = pre
  
-- | Predecessor nodes of the same type
predPred :: (Ord n, Node n, Node (CoNode n), 
             n ~ CoNode (CoNode n)) 
            => BProc -> n -> Set n
predPred (on,_) = foldMap (pred on) . pred on

-- | Reflexive predecessor relationship
predR :: (Ord n, Node n, Node (CoNode n), 
             n ~ CoNode (CoNode n)) 
         => BProc -> n -> Set n
predR bp p = p `Set.insert` (predPred bp p)

predMany :: (Ord n, Node n, Node (CoNode n), 
             n ~ CoNode (CoNode n)) 
         => BProc -> Set n -> Set n
predMany bp = fold . Set.map (predR bp)

-- | General fixed point funciton
-- the @fix@ from @Data.Function@ only compute Kleene fixed-points
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x = let it = iterate f x
                 in fst $ head $ dropWhile (uncurry (/=)) $ it `zip` (tail it)

-- | Transitive closure of the predecessor relationship
preds :: BProc -> Set PTPlace -> Set PTPlace
preds bp = fixedPoint (predMany bp)         

predTrans :: BProc -> PTPlace -> Set PTTrans
predTrans bp@(on,_) = fixedPoint (predMany bp) . pred on

conflict :: BProc -> PTPlace -> PTPlace -> Bool
conflict bp@(on,_) p1 p2 = any intersectingPred $ filter (uncurry (/=)) pastTrans
  where intersectingPred :: (PTTrans,PTTrans) -> Bool
        intersectingPred (a,b) = not . Set.null
                                 $ Set.intersection (pred on a) (pred on b)
        pastTrans = pairs (Set.toList (predTrans bp p1)) (Set.toList (predTrans bp p2))
        
pairs :: [a] -> [a] -> [(a,a)]        
pairs []     _  = []
pairs _      [] = []
pairs (x:xs) ys = map (x,) ys ++ pairs xs ys

        
