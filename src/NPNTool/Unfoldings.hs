{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
{-# LANGUAGE TupleSections, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- | Petri net unfoldings
module NPNTool.Unfoldings (
  -- * Occurence nets and branching processes
  OccurNet, Hom, BProc, toOccurNet, 
  -- * Configurations and cuts
  Conf, Cut, cut, cutMark,
  -- * Orders and relations on nodes of branching processes
  Node, predPred, predR, preds, predTrans, conflict,
  causalPred, concurrent,
  -- * Unfoldings and prefixes
  posTrans,
  -- * Auxiliary functions 
  pairs, fixedPoint, choose
  ) where

import Prelude hiding (pred)
import NPNTool.PetriNet
import NPNTool.PTConstr  
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import Data.Maybe (maybe, fromJust)
import Data.Foldable (foldMap, fold)
import Data.Monoid
import Data.List
import Control.Applicative

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


cutMark :: Hom -> Cut -> MultiSet PTPlace
cutMark (h1,_) p = foldMap (MSet.singleton . h1) p


---- Orders and relations on nodes of branching processes

class Node n m where
  type CoNode n 
  pred :: m -> n -> Set (CoNode n)

instance Node PTPlace OccurNet where
  type CoNode PTPlace = PTTrans
  pred on p = Set.fromList $ preP p on

instance Node PTTrans OccurNet where
  type CoNode PTTrans = PTPlace
  pred = pre

instance Node PTPlace PTNet where
  type CoNode PTPlace = PTTrans
  pred n p = Set.fromList $ preP p n

instance Node PTTrans PTNet where
  type CoNode PTTrans = PTPlace
  pred n = MSet.toSet . pre n
  
-- | Predecessor nodes of the same type
predPred :: (Ord n, Node n OccurNet, Node (CoNode n) OccurNet, 
             n ~ CoNode (CoNode n)) 
            => BProc -> n -> Set n
predPred (on,_) = foldMap (pred on) . pred on

-- | Reflexive predecessor relationship
predR :: (Ord n, Node n OccurNet, Node (CoNode n) OccurNet, 
             n ~ CoNode (CoNode n)) 
         => BProc -> n -> Set n
predR bp p = p `Set.insert` (predPred bp p)

predMany :: (Ord n, Node n OccurNet, Node (CoNode n) OccurNet, 
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

-- | Whether two places are in conflict
conflict :: BProc -> PTPlace -> PTPlace -> Bool
conflict bp@(on,_) p1 p2 = any intersectingPred $ filter (uncurry (/=)) pastTrans
  where intersectingPred :: (PTTrans,PTTrans) -> Bool
        intersectingPred (a,b) = not . Set.null
                                 $ Set.intersection (pred on a) (pred on b)
        pastTrans = pairs (Set.toList (predTrans bp p1)) (Set.toList (predTrans bp p2))

-- | Whether one place is a causal predecessor of another
causalPred :: BProc -> PTPlace -> PTPlace -> Bool
causalPred bp p1 p2 = p1 `Set.member` (preds bp (Set.singleton p2))

-- | Whether two places are concurrent                      
concurrent :: BProc -> PTPlace -> PTPlace -> Bool
concurrent bp p1 p2 = not $ and
                      [ causalPred bp p1 p2
                      , causalPred bp p2 p1
                      , conflict bp p1 p2 ]

pairs :: [a] -> [a] -> [(a,a)]        
pairs []     _  = []
pairs _      [] = []
pairs (x:xs) ys = map (x,) ys ++ pairs xs ys

        
---- Unfoldings, prefixes

-- | All the ways to choose `k` elements from a list                  
choose :: Int -> [a] -> [[a]]
choose 0 _      = [[]]
choose k []     = []
choose k (x:xs) = map (x:) (choose (k-1) xs)
                  ++ choose k xs

conformingLabels :: Hom -> [PTPlace] -> [PTPlace] -> Bool
conformingLabels (h,_) labels cands =
  let c' = map h cands
  in labels \\ c' == []
     && c' \\ labels == []

-- | Whether a list of places are concurreny
pairwiseCo :: BProc -> [PTPlace] -> Bool
pairwiseCo bp []     = True
pairwiseCo bp (p:ps) = and (map (concurrent bp p) ps)
                       && pairwiseCo bp ps

-- | Whether two transitions are equal under a homomorphism
eqHom :: Hom -> PTTrans -> PTTrans -> Bool
eqHom (_,h) t1 t2 = t1 == h t2

notPresent :: BProc -> PTTrans -> [PTPlace] -> Bool
notPresent bp@(on,h) t ps = all (not . any (eqHom h t). flip postP on) ps

-- | Whether it is possbile to add a transition to a branching process.
-- Return `Nothing` if it's not, returns `Just s` otherwise, where `s` 
-- is a pre-set of the transition which should be added.
posTrans :: BProc -> PTNet -> PTTrans -> Maybe (Set PTPlace)
posTrans bp@(on, h) ptn t = Set.fromList <$> find (pairwiseCo bp) candidates
  where candidates = filter (notPresent bp t) $
                     filter (conformingLabels h (Set.toList labels)) $
                     choose k (Set.toList (places on))
        labels = pred ptn t
        k = Set.size labels

