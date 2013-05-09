{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
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
  
-- | Predecessor places of a place  
predPl :: BProc -> PTPlace -> Set PTPlace
predPl (on,_) = foldMap (pred on) . pred on

-- | Reflexive predecessor relationship
predR :: BProc -> PTPlace -> Set PTPlace
predR bp p = p `Set.insert` (predPl bp p)

predPls :: BProc -> Set PTPlace -> Set PTPlace
predPls bp = fold . Set.map (predR bp)

-- | General fixed point funciton
-- the @fix@ from @Data.Function@ only compute Kleene fixed-points
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x = let it = iterate f x
                 in fst $ head $ dropWhile (uncurry (/=)) $ it `zip` (tail it)

-- | Transitive closure of the predecessor relationship
preds :: BProc -> Set PTPlace -> Set PTPlace
preds bp = fixedPoint (predPls bp)         

conflict :: BProc -> PTPlace -> PTPlace -> Bool
conflict (on,p) p1 p2 = any intersectingPred pastPlaces
  where intersectingPred = undefined
        pastPlaces = undefined

---- Unfolding algorithms        
        
        
-- genTrans :: BProc -> Set PTPlace -> PTNet -> Set PTTrans
-- genTrans (on,h) s' pt = if hasConflict s' then Set.empty
--                         else Set.empty
        
--------------------------------------------------------        

---- Tests        
        
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
