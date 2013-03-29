{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module NPNTool.NPNConstr (
    -- * Datatypes
  NPNConstr(..), NPNConstrM,
  -- * Operations
  new,
  -- * Monadic interface
  run, 
  liftPTC,
  mkPlace, mkTrans, label,
  inT, outT,
  -- * Generalized arcs (with expressions)
  ArcExpr (..), arcExpr,
  ) where

import qualified NPNTool.PTConstr as PTC
import NPNTool.PetriNet
import NPNTool.NPNet
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Control.Arrow (second)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import qualified Data.Foldable as F

-- | Datatype for dynamically constructing Nested Petri nets
data NPNConstr l v c =
  NPNConstr
  { p     :: Set PTPlace
  , key   :: Int, keyT :: Int
  , tin   :: M.Map Trans (SArc v c PTPlace)
  , tout  :: M.Map Trans (SArc v c PTPlace)
  , nets  :: Set (ElemNet l)
  , tlab  :: M.Map Trans l
  }

toNPN :: (Ord l, Ord v, Ord c) => NPNConstr l v c -> SNet l v c
toNPN c =
  SNet
  { net = Net { places = p c
              , trans   = M.keysSet (tin c)
                          `Set.union` M.keysSet (tout c)
              , pre = pre'
              , post = post'
              , initial = NPMark (MSet.empty)
              }
  , elementNets = Set.toList (nets c)
  , labelling = toLabelling c
  , labels = Set.fromList (M.elems (tlab c))
  }
  where pre' t = fromMaybe mempty (M.lookup t (tin c))
        post' t = fromMaybe mempty (M.lookup t (tout c))
        toLabelling c t = M.lookup t (tlab c)

type NPNConstrM l v = State (NPNConstr l v Int)

-- | Runs a @NPNConstrM@ monad and returns a NP-net together with a result              
run :: (Ord v, Ord l) => NPNConstrM l v a -> NPNConstr l v Int -> (a, SNet l v Int)
run c s =
  let (a, s') = runState c s
  in (a, toNPN s')     
       
-- | New (empty) 'NPNConstr'                                       
new :: NPNConstr l v c
new = NPNConstr { p    = Set.empty
               , key  = 1, keyT = 1
               , tin  = M.empty, tout = M.empty
               , nets = Set.empty
               , tlab = M.empty
               }

intToExpr :: Int -> Expr v Int
intToExpr x = if x == 1 then Const 1 else Plus (Const 1) (intToExpr (x-1))

toSArc :: Ord v => MultiSet PTPlace -> SArc v Int PTPlace
toSArc =  F.foldMap (SArc . Set.singleton . swap . (second intToExpr)) . MSet.toOccurList

-- | Lift a 'PTC.PTConstrM' constructions to 'NPNConstrM' monad  
liftPTC :: Ord v => PTC.PTConstrM l a -> NPNConstrM l v a
liftPTC ptc = do
  st <- get
  let c = PTC.new { PTC.p = p st
                  , PTC.key = key st, PTC.keyT = keyT st
                  , PTC.tlab = tlab st }
      (res,c') = runState ptc c
      st' = st { p    = PTC.p c'
               , key  = PTC.key c', keyT = PTC.keyT c'
               , tlab = PTC.tlab c'
               , tin  = M.unionWith mappend (tin st) (M.map toSArc (PTC.tin c'))
               , tout = M.unionWith mappend (tout st) (M.map toSArc (PTC.tout c'))
               }
  put st'             
  return res

-- | Creates a new place not yet present in the net  
mkPlace :: Ord v => NPNConstrM l v PTPlace
mkPlace = liftPTC PTC.mkPlace

-- | Creates a new tranisition not yet present in the net          
mkTrans :: Ord v => NPNConstrM l v Trans
mkTrans = liftPTC PTC.mkTrans

-- | Specifies a label for some transition  
label :: Ord v => Trans -> l -> NPNConstrM l v ()
label t = liftPTC . PTC.label t

inT :: Ord v => PTPlace -> Expr v Int -> Trans -> NPNConstrM l v ()
inT p e t = do
  c <- get
  let pre' = tin c
  put $ c { tin = M.insertWith mappend t (SArc (Set.singleton (e,p))) pre' }

outT :: Ord v => Trans -> Expr v Int -> PTPlace -> NPNConstrM l v ()
outT t e p = do
  c <- get
  let post' = tout c
  put $ c { tout = M.insertWith mappend t (SArc (Set.singleton (e,p))) post' }

class PTC.Arc a => ArcExpr a v where
  arcExpr :: a -> Expr v Int -> PTC.Co a -> NPNConstrM l v ()
  arc :: a -> PTC.Co a -> NPNConstrM l v ()
  arc k j = arcExpr k (Const 1) j

instance Ord v => ArcExpr Trans v where
  arcExpr = outT

instance Ord v => ArcExpr PTPlace v where
  arcExpr = inT
