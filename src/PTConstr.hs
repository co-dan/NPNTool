{-# LANGUAGE TypeFamilies, TypeSynonymInstances, RankNTypes #-}
module PTConstr (
  PTConstr(..), PTConstrM,
  new, run, runL, mkPlace, insPlace, label,
  inT, outT, inTn, outTn,
  Arc (..), arcn, conn
  ) where

import PetriNet
import NPNet (SNet, Expr(..), Labelling)
import qualified NPNet as NPN
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import Data.Maybe (fromMaybe)  

data PTConstr l =
  PTConstr
  { p     :: Set PTPlace, key :: Int
  , tin   :: M.Map Trans (MultiSet PTPlace)
  , tout  :: M.Map Trans (MultiSet PTPlace)
  , tlab  :: M.Map Trans l
  }
  
type PTConstrM l = State (PTConstr l)

new :: forall l. PTConstr l                   
new = PTConstr { p = Set.empty, key = 1, tin = M.empty, tout = M.empty, tlab = M.empty }

toPTNet :: forall l. PTConstr l -> PTNet
toPTNet c = Net { places  = p c
                , trans   = Set.fromList (M.keys (tin c))
                            `Set.union` Set.fromList (M.keys (tout c))
                , pre     = pre'
                , post    = post'
                , initial = MSet.empty
                } 
  where pre'  t = fromMaybe MSet.empty (M.lookup t (tin c))
        post' t = fromMaybe MSet.empty (M.lookup t (tout c))

toLabelling :: PTConstr l -> Labelling l
toLabelling c t = M.lookup t (tlab c)

mkPlace :: PTConstrM l Int
mkPlace = do
  c <- get
  let key' = key c
      p'   = p c
  put $ c {p = Set.insert key' p', key = key' + 1}
  return key'

insPlace :: PTPlace -> PTConstrM l ()
insPlace newP = do
  c <- get
  let p' = p c
  put $ c {p = Set.insert newP p'}

label :: Trans -> l -> PTConstrM l ()
label t lab = do
  c <- get
  let tlab' = tlab c
  put $ c {tlab = M.insert t lab tlab'}
  
class Arc k where
  type Co k :: *
  arc :: k -> Co k -> PTConstrM l ()

arcn :: Arc k => k -> Co k -> Int -> PTConstrM l ()
arcn a b n = replicateM_ n $ arc a b

conn :: Show l => PTPlace -> PTPlace -> l -> PTConstrM l ()
conn p1 p2 l = do
  t <- getTrans $ Trans ("__t" ++ show l)
  arc p1 t
  arc t p2
  label t l

getTrans :: Trans -> PTConstrM l Trans
getTrans n = do
  c <- get
  if not (M.member n (tlab c))
    then return n
    else getTrans (Trans ((name n) ++ "_"))
  
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

run :: PTConstrM l a -> PTConstr l -> (a, PTNet)
run c s =
  let (a, s') = runState c s
  in (a, toPTNet s')

runL :: PTConstrM l a -> PTConstr l -> (a, PTNet, Labelling l)
runL c s =
  let (a, s') = runState c s
  in (a, toPTNet s', toLabelling s')
