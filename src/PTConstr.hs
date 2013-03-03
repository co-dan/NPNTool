{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
module PTConstr (
  PTConstr(..), PTConstrM,
  new, run, mkPlace,
  inT, outT, inTn, outTn,
  Arc (..), arcn
  ) where

import PetriNet
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import Data.Maybe (fromMaybe)  

data PTConstr =
  PTConstr
  { p    :: Set Int, key :: Int
  , tin  :: M.Map Trans (MultiSet Int)
  , tout :: M.Map Trans (MultiSet Int) 
  }
  
type PTConstrM a = State PTConstr a

new :: PTConstr                   
new = PTConstr { p = Set.empty, key = 1, tin = M.empty, tout = M.empty }

toPTNet :: PTConstr -> PTNet
toPTNet c = Net { places  = p c
                , trans   = Set.fromList (M.keys (tin c))
                            `Set.union` Set.fromList (M.keys (tout c))
                , pre     = pre'
                , post    = post'
                , initial = MSet.empty
                } 
  where pre'  t = fromMaybe MSet.empty (M.lookup t (tin c))
        post' t = fromMaybe MSet.empty (M.lookup t (tout c))


mkPlace :: PTConstrM Int
mkPlace = do
  c <- get
  let key' = key c
      p'   = p c
  put $ c {p = Set.insert key' p', key = key' + 1}
  return key'

class Arc k where
  type Co k :: *
  arc :: k -> Co k -> PTConstrM ()

arcn :: Arc k => k -> Co k -> Int -> PTConstrM ()
arcn a b n = replicateM_ n $ arc a b

instance Arc Trans where  
  type Co Trans = PTPlace
  arc = outT

instance Arc PTPlace where  
  type Co PTPlace = Trans
  arc = inT
  
inT :: PTPlace -> Trans -> PTConstrM ()
inT p t = do
  c <- get
  let pre' = tin c
  put $ c { tin = M.insertWith MSet.union t (MSet.singleton p) pre'}

outT :: Trans -> PTPlace -> PTConstrM ()
outT t p = do
  c <- get
  let post' = tout c
  put $ c { tout = M.insertWith MSet.union t (MSet.singleton p) post' }  

inTn :: PTPlace -> Trans -> Int -> PTConstrM ()  
inTn p t n = replicateM_ n $ inT p t

outTn :: Trans -> PTPlace -> Int -> PTConstrM ()
outTn t p n = replicateM_ n $ outT t p

run :: PTConstrM a -> PTConstr -> (a, PTNet)
run c s =
  let (a, s') = runState c s
  in (a, toPTNet s')
