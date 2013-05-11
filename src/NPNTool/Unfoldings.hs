{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
{-# LANGUAGE TupleSections, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  posTrans, posExt, unfStep, unfSteps, unfolding,
  -- * Auxiliary functions 
  pairs, fixedPoint, choose,
  BPConstrM, runBPConstrM
  ) where

import Prelude hiding (pred)
import NPNTool.PetriNet
import NPNTool.PTConstr  
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import Data.Foldable (foldMap, fold, forM_)
import Data.List
import Data.Ord (comparing)
import Control.Applicative
import Control.Monad (replicateM, replicateM_)
import Control.Monad.State hiding (forM_)
import Data.Monoid ((<>), Monoid)
import Data.Maybe (fromJust, catMaybes)


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

cut :: OccurNet -> Conf -> Cut
cut n c = (initial n `Set.union` postC)
          Set.\\ preC
  where postC = foldMap (post n) c
        preC = foldMap (pre n) c


cutMark :: Hom -> Cut -> MultiSet PTPlace
cutMark (h1,_) = foldMap (MSet.singleton . h1)


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

-- | General fixed point funciton
-- the @fix@ from @Data.Function@ only compute Kleene fixed-points
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

-- | Whether it is possbile to add a transition to a branching process.
-- Return `Nothing` if it's not, returns `Just s` otherwise, where `s` 
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

-- runBPConstrM :: BPConstrM a -> (a, BProc)
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

posExt :: BProc -> PTNet -> EventQueue
posExt bp n = catMaybes $
              map (\t -> (t,) <$> posTrans bp n t) $
              Set.toList (trans n)
              
-- | One step of unfolding
unfStep :: PTNet -> EventQueue -> BPConstrM EventQueue
unfStep _ [] = return []
unfStep n ((tl,preT):qs) = do
  t <- lift mkTrans
  homT t tl
  forM_ preT $ \p -> do
    lift $ arc p t
  forM_ (post n tl) $ \p -> do
    p' <- lift mkPlace
    homP p' p
    lift $ arc t p'
  bp <- getBP
  let qs' = sortBy (comparing (localConfSize bp)) $
            nub (posExt bp n ++ qs)
  return $ qs'


-- | For debugging purposes
unfSteps :: Int -> PTNet -> EventQueue -> BPConstrM ()
unfSteps 0 _ _ = return ()
unfSteps k n q = step >>= unfSteps (k-1) n
  where step = unfStep n q
        
unfInitial :: PTNet -> BPConstrM EventQueue
unfInitial n = do
  forM_ (initial n) $ \p -> do
    p' <- lift mkPlace
    homP p' p
  bp <- getBP
  return $ posExt bp n

unfolding :: Int -> PTNet -> BPConstrM ()
unfolding k n = do
  q <- unfInitial n
  unfSteps k n q

localConfSize :: BProc -> (PTTrans, Set Condition) -> Int
localConfSize bp = Set.size . preds bp . snd

