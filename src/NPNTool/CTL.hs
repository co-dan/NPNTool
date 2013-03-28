{-# LANGUAGE FlexibleInstances #-}
-- | A simple CTL model checker
module NPNTool.CTL (
  -- * CTL
  CTL(..),
  -- ** Additional operators
  ef,af,eg,ag,
  -- * Verification
  verifyPT,
  -- * Helper functions
  orElse, andThen
  ) where

import Control.Monad
import Control.Monad.State
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.Graph.Inductive
import NPNTool.PetriNet
-- import NCTL
import NPNTool.StateSpace hiding (SS, succSt)
import NPNTool.NodeMap

-- | CTL datatype  
data CTL =
  CTLFalse
  | CTLTrue
  | CTLAtom (String, PTMark -> Bool)
  | CTLTrans (String, PTTrans -> Bool)
  | CTLNot CTL
  | CTLOr CTL CTL
  | EX CTL
  | EU CTL CTL
  | AU CTL CTL
  deriving (Show,Eq,Ord)
    
instance Ord (PTMark -> Bool) where
  compare _ _ = EQ

instance Eq (PTMark -> Bool) where
  _ == _ = True
  
instance Show (PTMark -> Bool) where
  show _ = "pred"

instance Ord (PTTrans -> Bool) where
  compare _ _ = EQ

instance Eq (PTTrans -> Bool) where
  _ == _ = True

instance Show (PTTrans -> Bool) where
  show _ = "epred"
           
-- | Derived formulae           
ef,af,eg,ag :: CTL -> CTL
ef = EU CTLTrue
af = AU CTLTrue 
eg f = CTLNot $ af (CTLNot f)
ag f = CTLNot $ ef (CTLNot f)             


-- | The usual liftM2 f won't work here, we need a left-biased version
orElse, andThen :: Monad m => m Bool -> m Bool -> m Bool
orElse a b = do
  r <- a
  if r 
    then return True
    else b
         
andThen a b = do
  r <- a
  if r  
    then b
    else return False
         
  
notM :: Monad m => m Bool -> m Bool
notM = liftM not

anyM,allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p = foldM (\a b -> return a `orElse` p b) False
allM p = foldM (\a b -> return a `andThen` p b) True

succSt :: SS -> Obj -> [Obj]
succSt g (Left s) = map toState $ suc g s
succSt g (Right (_,s)) = [toState s]



type Verif = State (M.Map (Obj,CTL) Bool) Bool

-- | CTL verification w/ memoization
verifyPT :: PTNet -> CTL -> (Bool, M.Map (Obj, CTL) Bool)
verifyPT net f = runState (verifyF' f ss (toState s)) M.empty
  where (nm,ss) = reachabilityGraph' net
        (s,_)  = fromJust $ lookupNode nm (initial net)

verifyF' :: CTL -> SS -> Obj -> Verif
verifyF' f ss s = do
  m <- get
  case M.lookup (s,f) m of
    Just x -> return x
    Nothing -> do
      put $ M.insert (s,f) False m
      r <- case s of
        Left st -> verify' f ss s
        Right tr -> undefined --verifyN' f ss (fromJust t) tr
      m <- get
      put $ M.insert (s,f) r m
      return r


verify' :: CTL -> SS -> Obj -> Verif
verify' CTLFalse _ _ = return False
verify' CTLTrue  _ _ = return True
verify' (CTLAtom (n,f)) ss (Left s) = return (f (fromJust (lab ss s)))
verify' (CTLAtom (n,_)) ss (Right _) = error ""
verify' (EX f) ss s =  
  anyM (verifyF' f ss) (succSt ss s)  
verify' (CTLNot f) ss s =
  notM $ verifyF' f ss s
verify' (CTLOr f g) ss s =
  verifyF' f ss s `orElse` verifyF' g ss s
verify' t@(EU f g) ss s =
  verifyF' g ss s `orElse` 
    (verifyF' f ss s `andThen` anyM (verifyF' t ss) (succSt ss s))
verify' t@(AU f g) ss s =            
  verifyF' g ss s `orElse` 
    (verifyF' f ss s `andThen` allM (verifyF' t ss) (succSt ss s))

             
