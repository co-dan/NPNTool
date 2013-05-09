{-# LANGUAGE TypeFamilies, TypeSynonymInstances, RankNTypes #-}
module NPNTool.PTConstr (
  -- * Datatypes
  PTConstr(..), PTConstrM,
  -- * Operations
  new,
  -- * Monadic interface
  run, runL, runConstr,
  mkPlace, insPlace, mkTrans, 
  label, mark, marking,
  inT, outT, inTn, outTn,
  -- * Generalized arcs
  Arc (..), arcn, conn
  ) where

import NPNTool.PetriNet
import NPNTool.NPNet (Labelling)
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import Data.Maybe (fromMaybe)  

-- | Datatype for dynamically constructing Petri Nets  
data PTConstr l =
  PTConstr
  { p     :: M.Map PTPlace Int
  , key   :: Int, keyT :: Int
  , tin   :: M.Map Trans (MultiSet PTPlace)
  , tout  :: M.Map Trans (MultiSet PTPlace)
  , tlab  :: M.Map Trans l
  }
  
type PTConstrM l = State (PTConstr l)

-- | New (empty) 'PTConstr'                   
new :: forall l. PTConstr l                   
new = PTConstr { p = M.empty, key = 1, keyT = 1, tin = M.empty, tout = M.empty, tlab = M.empty }

toPTNet :: forall l. PTConstr l -> PTNet
toPTNet c = Net { places  = M.keysSet (p c)
                , trans   = M.keysSet (tin c)
                            `Set.union` M.keysSet (tout c)
                            `Set.union` M.keysSet (tlab c)
                , pre     = pre'
                , post    = post'
                , initial = MSet.fromMap (p c)
                } 
  where pre'  t = fromMaybe MSet.empty (M.lookup t (tin c))
        post' t = fromMaybe MSet.empty (M.lookup t (tout c))

toLabelling :: PTConstr l -> Labelling l
toLabelling c t = M.lookup t (tlab c)

-- | Creates a new place not yet present in the net
mkPlace :: PTConstrM l PTPlace
mkPlace = do
  c <- get
  let key' = key c
      p'   = p c
  put $ c { p = M.insert key' 0 p', key = key' + 1 }
  return key'

-- | Marks a place with a token
mark :: PTPlace -> PTConstrM l ()
mark place = do
  c <- get
  let p' = p c
  put $ c { p = M.insertWith (+) place 1 p' }

-- | Marks a set of places
marking :: [PTPlace] -> PTConstrM l ()
marking = mapM_ mark

-- | Inserts an existing place in the net
-- Does nothing if the place is already present
insPlace :: PTPlace -> PTConstrM l ()
insPlace newP = do
  c <- get
  let p' = p c
  put $ c { p = M.insertWith (flip const) newP 0 p' }

-- | Creates a new transition not yet present in the net
mkTrans :: PTConstrM l Trans
mkTrans = do
  c <- get
  let keyT' = keyT c
  put $ c { keyT = keyT' + 1 }
  return $ Trans ("t" ++ show keyT')
  
-- | Specifies a label for some transition  
label :: Trans -> l -> PTConstrM l ()
label t lab = do
  c <- get
  let tlab' = tlab c
  put $ c { tlab = M.insert t lab tlab' }
  
-- | Generalized arc class
class Arc k where
  type Co k :: *
  -- | Specifies an arc from some object to its co-object
  arc :: k -> Co k -> PTConstrM l ()
  
-- | Specifies @n@ arcs  
arcn :: Arc k => k -> Co k -> Int -> PTConstrM l ()
arcn a b n = replicateM_ n $ arc a b

-- | Directly connects two places via an intermediate transition
-- Might be useful for specifying workflow nets
conn :: Show l => PTPlace -> PTPlace -> l -> PTConstrM l ()
conn p1 p2 l = do
  t <- mkTrans
  arc p1 t
  arc t p2
  label t l

instance Arc Trans where  
  type Co Trans = PTPlace
  arc = outT

instance Arc PTPlace where  
  type Co PTPlace = Trans
  arc = inT

inT :: PTPlace -> Trans -> PTConstrM l ()
inT p t = do
  c <- get
  let pre' = tin c
  put $ c { tin = M.insertWith MSet.union t (MSet.singleton p) pre'}

outT :: Trans -> PTPlace -> PTConstrM l ()
outT t p = do
  c <- get
  let post' = tout c
  put $ c { tout = M.insertWith MSet.union t (MSet.singleton p) post' }  

inTn :: PTPlace -> Trans -> Int -> PTConstrM l ()  
inTn p t n = replicateM_ n $ inT p t

outTn :: Trans -> PTPlace -> Int -> PTConstrM l ()
outTn t p n = replicateM_ n $ outT t p

-- | Runs a @PTConstrM@ monad and returns a Petri Net together with a result              
run :: PTConstrM l a -> PTConstr l -> (a, PTNet)
run c s =
  let (a, s') = runState c s
  in (a, toPTNet s')

-- | Runs a @PTConstrM@ monad and returns a @PTConstr@ 
runConstr :: PTConstrM l a -> PTConstr l -> PTConstr l
runConstr c s =
  let (a, s') = runState c s
  in s'

-- | Runs a @PTConstrM@ monad and returns a Petri Net together with its labelling
-- and a result
runL :: PTConstrM l a -> PTConstr l -> (a, PTNet, Labelling l)
runL c s =
  let (a, s') = runState c s
  in (a, toPTNet s', toLabelling s')
