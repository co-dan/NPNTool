{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
{-# LANGUAGE TupleSections, FlexibleContexts #-}
-- | Petri net unfoldings
module NPNTool.Unfoldings where

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
import qualified Data.Foldable as F  
import Control.Monad (replicateM)

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


mark :: Hom -> Set PTPlace -> MultiSet PTPlace
mark (h1,_) p = foldMap (MSet.singleton . h1) p


---- Orders and relations on nodes of branching processes
                
posExt :: BProc -> PTNet -> Set PTTrans                
posExt (on,p) pt = Set.filter goodEnough (trans pt)
  where goodEnough = const True

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
        
-- let intersectingPred bp (a,b) = not . Set.null $ Set.intersection (predR bp a) (predR bp b)

pairs :: [a] -> [a] -> [(a,a)]        
pairs []     _  = []
pairs _      [] = []
pairs (x:xs) ys = map (x,) ys ++ pairs xs ys

---- Unfolding algorithms        
        
        
-- genTrans :: BProc -> Set PTPlace -> PTNet -> Set PTTrans
-- genTrans (on,h) s' pt = if hasConflict s' then Set.empty
--                         else Set.empty
        
--------------------------------------------------------        

---- Tests        
        
--  filter (uncurry (conflict bp1)) $ pairs (Set.toList (places on1)) (Set.toList (places on1))
--   == []                  

run' :: PTConstrM l a -> (a, PTNet)
run' = flip run new        

testNet :: PTNet
((),testNet) = run' $ do
  [s1,s2,s3,s4] <- replicateM 4 mkPlace
  [t1,t2,t3] <- replicateM 3 mkTrans
  arc t1 s1
  arc s1 t2
  arc t2 s3
  arc s3 t1
  
  arc s2 t2
  arc t2 s4
  arc s4 t3
  arc t3 s2
  return ()

on1 :: OccurNet
on1 = toOccurNet . flip runConstr new $ do
  ps <- replicateM 6 mkPlace 
  ts <- replicateM 3 mkTrans
  arc (ps !! 0) (ts !! 0)
  arc (ps !! 1) (ts !! 0)
  arc (ts !! 0) (ps !! 2)
  arc (ts !! 0) (ps !! 3)
  arc (ps !! 2) (ts !! 1)
  arc (ps !! 3) (ts !! 2)
  arc (ts !! 1) (ps !! 4)
  arc (ts !! 2) (ps !! 5)
  return ()

bp1 :: BProc
bp1 = (on1, undefined)


on2 :: OccurNet
on2 = toOccurNet . flip runConstr new $ do 
  [p1,p2,p3,p4,p5,p6,p7] <- replicateM 7 mkPlace
  [t1,t2,t3,t4,t5] <- replicateM 5 mkTrans
  arc p1 t1
  arc t1 p3
  arc p2 t2
  arc p2 t3
  arc t2 p4
  arc t3 p5
  
  arc p3 t4
  arc p3 t5
  arc p4 t4
  arc p5 t5
  arc t4 p6
  arc t5 p7

bp2 :: BProc
bp2 = (on2, undefined)

