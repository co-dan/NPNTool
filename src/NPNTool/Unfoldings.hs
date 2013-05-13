{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
{-# LANGUAGE TupleSections, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Petri net unfoldings
module NPNTool.Unfoldings (
  -- * Occurence nets and branching processes
  Event, Condition, OccurNet, Hom, BProc, toOccurNet,  
  -- * Configurations and cuts
  Conf, Cut, cut, cutMark, localConf,
  -- * Orders and relations on nodes of branching processes
  Node, CoNode, predPred, predR, preds, predTrans, conflict,
  causalPred, concurrent,
  -- * Unfoldings and prefixes
  EventQueue, posTrans, posExt, unfolding,
  -- * Auxiliary functions 
  pairs, fixedPoint, choose,
  BPConstrM, runBPConstrM
  ) where

import Prelude hiding (pred, all, any)
import NPNTool.PetriNet
import NPNTool.PTConstr  
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import Data.Foldable (foldMap, fold, forM_, all, any, find)
import Data.List ((\\), sortBy, nub)
import Data.Ord (comparing)
import Control.Applicative
import Control.Monad (replicateM, replicateM_)
import Control.Monad.State hiding (forM_)
import Data.Monoid ((<>), Monoid)
import Data.Maybe (fromJust, catMaybes, isJust)


---- Occurence nets and branching processes

type Event = PTTrans
type Condition = PTPlace

-- | Occurrence net
type OccurNet = Net Condition Event Set Set
-- | Homomorphism from P/T-net to occurence net                
type Hom = (Condition -> PTPlace, Event -> PTTrans)
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
type Conf = Set Event
type Cut  = Set Condition

cut :: BProc -> Conf -> Cut
cut (n,_) c = (initial n `Set.union` postC)
          Set.\\ preC
  where postC = foldMap (post n) c
        preC = foldMap (pre n) c


cutMark :: BProc -> Cut -> MultiSet PTPlace
cutMark (_,(h1,_)) = foldMap (MSet.singleton . h1)

localConf :: BProc -> Event -> Conf
localConf bp = preds bp . Set.singleton

---- Orders and relations on nodes of branching processes

class Node n m where
  type CoNode n 
  pred :: m -> n -> Set (CoNode n)

instance Node Condition OccurNet where
  type CoNode PTPlace = PTTrans
  pred on p = Set.fromList $ preP p on

instance Node Event OccurNet where
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

-- | General fixed point funciton since 
-- the @fix@ from "Data.Function" only compute Kleene fixed-points
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x = let it = iterate f x
                 in fst $ head $ dropWhile (uncurry (/=)) $ it `zip` (tail it)

-- | Transitive closure of the predecessor relationship
preds :: (Ord n, Node n OccurNet, Node (CoNode n) OccurNet, 
             n ~ CoNode (CoNode n)) 
         => BProc -> Set n -> Set n
preds bp = fixedPoint (predMany bp)         

predTrans :: BProc -> Condition -> Set Event
predTrans bp@(on,_) = fixedPoint (predMany bp) . pred on

-- | Whether two places are in conflict
conflict :: BProc -> Condition -> Condition -> Bool
conflict bp@(on,_) p1 p2 = any intersectingPred $ filter (uncurry (/=)) pastTrans
  where intersectingPred :: (PTTrans,PTTrans) -> Bool
        intersectingPred (a,b) = not . Set.null
                                 $ Set.intersection (pred on a) (pred on b)
        pastTrans = pairs (Set.toList (predTrans bp p1)) (Set.toList (predTrans bp p2))

-- | Whether one place is a causal predecessor of another
causalPred :: BProc -> Condition -> Condition -> Bool
causalPred bp p1 p2 = p1 `Set.member` (preds bp (Set.singleton p2))

-- | Whether two places are concurrent                      
concurrent :: BProc -> Condition -> Condition -> Bool
concurrent bp p1 p2 = not $ and
                      [ causalPred bp p1 p2
                      , causalPred bp p2 p1
                      , conflict bp p1 p2 ]

pairs :: [a] -> [a] -> [(a,a)]        
pairs []     _  = []
pairs _      [] = []
pairs (x:xs) ys = map (x,) ys ++ pairs xs ys

        
---- Unfoldings, prefixes

-- | All the possible ways to choose @k@ elements from a list                  
choose :: Int -> [a] -> [[a]]
choose 0 _      = [[]]
choose k []     = []
choose k (x:xs) = map (x:) (choose (k-1) xs)
                  ++ choose k xs

conformingLabels :: Hom -> [PTPlace] -> [Condition] -> Bool
conformingLabels (h,_) labels cands =
  let c' = map h cands
  in null (labels \\ c')
     && null (c' \\ labels)

-- | Whether a list of places are concurreny
pairwiseCo :: BProc -> [PTPlace] -> Bool
pairwiseCo bp []     = True
pairwiseCo bp (p:ps) = all (concurrent bp p) ps
                       && pairwiseCo bp ps

-- | Whether two transitions are equal under a homomorphism
eqHom :: Hom -> PTTrans -> Event -> Bool
eqHom (_,h) t1 t2 = t1 == h t2

notPresent :: BProc -> PTTrans -> [PTPlace] -> Bool
notPresent bp@(on,h) t = all (not . any (eqHom h t). flip postP on)

hasCompanion :: BProc -> Event -> Bool
hasCompanion bp@(on,_) t =
  any (\t' -> (confSize t' < confSize t) && sameState bp st t') (trans on)
  where st = cutMark bp . cut bp . localConf bp $ t
        confSize = Set.size . localConf bp 
        
sameState :: BProc -> MultiSet PTPlace -> Event -> Bool
sameState bp st e = st == cutMark bp (cut bp (localConf bp e))

-- | Whether it is possbile to add a transition to a branching process.
-- Return @Nothing@ if it's not, returns @Just s@ otherwise, where @s@
-- is a pre-set of the transition which should be added.
posTrans :: BProc -> PTNet -> PTTrans -> Maybe (Set Condition)
posTrans bp@(on, h) ptn t = Set.fromList <$> find (pairwiseCo bp) candidates
  where candidates = filter (notPresent bp t) $
                     filter (conformingLabels h (Set.toList labels)) $
                     choose k (Set.toList (places on))
        labels = pred ptn t
        k = Set.size labels


type HomS = (M.Map Condition PTPlace, M.Map Event PTTrans)
type BPConstrM = StateT HomS (PTConstrM PTTrans)
type EventQueue = [(PTTrans, Set Condition)]

runBPConstrM :: BPConstrM a -> (a, BProc)
runBPConstrM c =
  let ((a,homs), s') = runState (runStateT c (M.empty, M.empty)) new
      hom = toHom homs
      on = toOccurNet s'
  in (a, (on,hom))
  
tell :: (MonadState s m, Monoid s) => s -> m ()
tell x = modify (<> x)

toHom :: HomS -> Hom
toHom (m1,m2) =(fromJust . flip M.lookup m1,
                fromJust . flip M.lookup m2)
         
getBP :: BPConstrM BProc
getBP = do
  on <- lift get
  homS <- get
  return (toOccurNet on, toHom homS)

-- | Adds a place to the homomorphism
homP :: Condition -> PTPlace -> BPConstrM ()
homP p s = tell (M.singleton p s, M.empty)

-- | Adds an event to the homomorphism
homT :: Event -> PTTrans -> BPConstrM ()
homT t1 t2 = do
  tell (M.empty, M.singleton t1 t2)
  lift (label t1 t2)

posExt :: BProc -> PTNet -> Set Event -> EventQueue
posExt bp n cutoff = filter (predTerminal . snd) $
                     catMaybes $
                     map (\t -> (t,) <$> posTrans bp n t) $
                     Set.toList (trans n)
  where predTerminal cs = Set.null $
                          fold (Set.map (pred (fst bp)) cs)
                          `Set.intersection` cutoff
              
-- | One step of unfolding
unfStep :: PTNet -> Set Event -> EventQueue -> BPConstrM (Set Event, EventQueue)
unfStep _ cutoff [] = return (cutoff, [])
unfStep n cutoff ((tl,preT):qs) = do
  t <- lift mkTrans
  homT t tl
  forM_ preT $ \p -> do
    lift $ arc p t
  forM_ (post n tl) $ \p -> do
    p' <- lift mkPlace
    homP p' p
    lift $ arc t p'
  bp <- getBP
  let cutoff' = if hasCompanion bp t
                then t `Set.insert` cutoff
                else cutoff
  let qs' = sortBy (comparing (localConfSize bp)) $
            nub (posExt bp n cutoff' ++ qs)
  return (cutoff', qs')


-- | For debugging purposes
unfSteps :: Int -> PTNet -> Set Event -> EventQueue -> BPConstrM ()
unfSteps 0 _ _ _ = return ()
unfSteps k n cutoff q = do
  let step = unfStep n cutoff q
  (cutoff', q') <- step
  unfSteps (k-1) n cutoff' q'
        
-- | Initial step for the unfolding algorithm
unfInitial :: PTNet -> BPConstrM EventQueue
unfInitial n = do
  forM_ (initial n) $ \p -> do
    p' <- lift mkPlace
    homP p' p
  bp <- getBP
  return $ posExt bp n Set.empty

-- | Run the unfolding algorithm for `k` steps or until it stops
unfolding :: Int -> PTNet -> BPConstrM ()
unfolding k n = do
  q <- unfInitial n
  unfSteps k n Set.empty q

localConfSize :: BProc -> (PTTrans, Set Condition) -> Int
localConfSize bp = Set.size . preds bp . snd

