module PTConstr (
  PTConstr(..), PTConstrM,
  new, run, mkPlace,
  inT, outT, inTn, outTn
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

inT :: Int -> Trans -> PTConstrM ()
inT p t = do
  c <- get
  let pre' = tin c
  put $ c { tin = M.insertWith MSet.union t (MSet.singleton p) pre'}

outT :: Trans -> Int -> PTConstrM ()
outT t p = do
  c <- get
  let post' = tout c
  put $ c { tout = M.insertWith MSet.union t (MSet.singleton p) post' }  

inTn :: Int -> Trans -> Int -> PTConstrM ()  
inTn p t n = (replicateM n $ inT p t) >> return ()

outTn :: Trans -> Int -> Int -> PTConstrM ()
outTn t p n = (replicateM n $ outT t p) >> return ()

run :: PTConstrM a -> PTConstr -> (a, PTNet)
run c s =
  let (a, s') = runState c s
  in (a, toPTNet s')
