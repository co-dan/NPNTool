{-# LANGUAGE TypeOperators,ScopedTypeVariables #-}
-- | A simple NCTL model checker
module NPNTool.ModelChecker where

import Data.Graph.Inductive
import Data.Maybe (fromJust)
import NPNTool.StateSpace
import NPNTool.NCTL
import Data.Function
import Debug.Trace  
import Data.List
import qualified Data.Map as M
import Control.Monad.State

instance (Eq a, Eq b) => Eq (Gr a b) where
  (==) = equal

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
anyM p = foldM (\a b -> return a `orElse` p b) False
allM p = foldM (\a b -> return a `andThen` p b) True

-- | Recursive memozied verification function

-- verifyF :: NCTL -> SS a -> Node -> State (M.Map (Obj,NCTL) Bool) Bool
-- verifyF f ss s = verifyF' f ss (toState s)

type NNCTL = (NCTL, Maybe ET)

type Verif = State (M.Map (Obj,NNCTL) Bool) Bool

verifyF :: NCTL -> SS a -> Obj -> Verif
verifyF x = verifyF' (x,Nothing)

verifyNF :: NCTL -> SS a -> ET -> Edge -> Verif
verifyNF x ss t e = verifyF' (x,Just t) ss (Right e)

verifyF' :: NNCTL -> SS a -> Obj -> Verif
verifyF' g@(f,t) ss s = do
  m <- get
  case M.lookup (s,g) m of
    Just x -> return x
    Nothing -> do
      put $ M.insert (s,g) False m
      r <- case s of
        Left st -> verify f ss s
        Right tr -> verifyN' f ss (fromJust t) tr
      m <- get
      put $ M.insert (s,g) r m
      return r

verify :: NCTL -> SS a -> Obj -> Verif
verify NCTLFalse _ _ = return False
verify NCTLTrue  _ _ = return True
verify (NCTLAtom (n,f)) ss (Left s) = return $ f s
verify (NCTLAtom (n,_)) ss (Right _) = error ""
verify (EX f) ss s =  
  anyM (verifyF f ss) (succSt ss s)  
verify (NCTLNot f) ss s =
  notM $ verifyF f ss s
verify (NCTLOr f g) ss s =
  verifyF f ss s `orElse` verifyF g ss s
verify t@(EU f g) ss s =
  verifyF g ss s `orElse` 
    (verifyF f ss s `andThen` anyM (verifyF t ss) (succSt ss s))
verify t@(AU f g) ss s =            
  verifyF g ss s `orElse` 
    (verifyF f ss s `andThen` allM (verifyF t ss) (succSt ss s))
verify (NMod f) ss s = allM (verifyN f ss) (outEdges ss s) 

verifyN :: NCTL -> SS a -> Obj -> Verif
verifyN _ _ (Left _) = error "Bad object"
verifyN f ss (Right tr) = allM (\e -> verifyN' f ss e tr) (getETokens ss)


verifyN' :: NCTL -> SS a -> ET -> Edge -> Verif
verifyN' (NCTLAtom (n,_)) _ _ _ = error "bad context"
verifyN' (NCTLTrans (n,f)) ss tok e = return (f e) 
                                      `andThen` return (fired ss e tok)
verifyN' (EX f) ss tok e =  
  anyM (verifyNF f ss tok) (succTr ss e)  
verifyN' (NCTLNot f) ss tok e =
  notM $ verifyNF f ss tok e
verifyN' (NCTLOr f g) ss tok e =
  verifyNF f ss tok e `orElse` verifyNF g ss tok e
verifyN' t@(EU f g) ss tok e =            
  verifyNF g ss tok e `orElse` 
    (verifyNF f ss tok e `andThen` anyM (verifyNF t ss tok) (succTr ss e))
verifyN' t@(AU f g) ss tok e =            
  verifyNF g ss tok e `orElse` 
    (verifyNF f ss tok e `andThen` allM (verifyNF t ss tok) (succTr ss e))
                                      

-- | Evaluate the nCTL formula on a given statespace
evalF :: NCTL -> SS a -> Node -> (Bool,M.Map (Obj,NNCTL) Bool)
evalF f ss s = runState (verifyF f ss (toState s)) M.empty

lfp f a = 
  let fa = iterate f a 
  in head [x | (x,y) <- zip fa (tail fa), x == y]
