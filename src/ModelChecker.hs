{-# LANGUAGE TypeOperators,ScopedTypeVariables #-}
module ModelChecker where

import Data.Graph.Inductive
import Data.Maybe (fromJust)
import StateSpace
import NCTL
import Data.Function
import Debug.Trace  
import Data.List
import Data.Map as M
import Control.Monad.State

instance (Eq a, Eq b) => Eq (Gr a b) where
  (==) = equal

-- | Successors of a node
succSt :: SS a -> Node -> [Node]
succSt (g,_,_) = suc g

-- | Helper functions
-- The usual liftM2 f won't work here, we need a left-biased version
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
anyM p l = foldM (\a b -> return a `orElse` p b) False l
allM p l = foldM (\a b -> return a `andThen` p b) True l

-- | Recursive memozied verification function
verifyF :: NCTL -> SS a -> Node -> State (M.Map (Node,NCTL) Bool) Bool
verifyF f ss s = do
  m <- get
  case M.lookup (s,f) m of
    Just x -> return x
    Nothing -> do
      put $ M.insert (s,f) False m
      r <- verify f ss s
      m <- get
      put $ M.insert (s,f) r m
      return r

verify :: NCTL -> SS a -> Node -> State (M.Map (Node,NCTL) Bool) Bool
verify NCTLFalse _ _ = return False
verify NCTLTrue  _ _ = return True
verify (NCTLAtom (n,f)) ss s = return $ f s
verify (EX f) ss s =  
  anyM (verifyF f ss) (succSt ss s)  
verify (NCTLNot f) ss s =
  notM $ verifyF f ss s
verify (NCTLOr f g) ss s =
  verifyF f ss s `orElse` verifyF g ss s
verify t@(EU f g) ss s = do
  verifyF g ss s `orElse` 
    (verifyF f ss s `andThen` anyM (verifyF t ss) (succSt ss s))
verify t@(AU f g) ss s = do           
  verifyF g ss s `orElse` 
    (verifyF f ss s `andThen` allM (verifyF t ss) (succSt ss s))

-- | Evaluate the nCTL formula on a given statespace
evalF :: NCTL -> SS a -> Node -> (Bool,M.Map (Node,NCTL) Bool)
evalF f ss s = runState (verifyF f ss s) M.empty

lfp f a = 
  let fa = iterate f a 
  in head [x | (x,y) <- zip fa (tail fa), x == y]
