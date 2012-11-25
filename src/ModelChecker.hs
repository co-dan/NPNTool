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

succSt :: SS a -> Node -> [Node]
succSt = suc

verifyF :: NCTL -> SS a -> Node -> State (M.Map (Node,NCTL) Bool) Bool
verifyF f ss s = do
  m <- get
  --trace ((show f) ++ " @ " ++ (show s)) $ return ()
  --trace (show m) $ return ()
  case M.lookup (s,f) m of
    Just x -> return x
    Nothing -> do
      put $ M.insert (s,f) False m
      r <- verify f ss s
      m <- get
      put $ M.insert (s,f) r m
      return r

-- | Helper functions
-- The usual liftM2 (||) won't work here, we need a left-biased version
orElse, andThen :: Monad m => m Bool -> m Bool -> m Bool
orElse a b = do
  r <- a
  if r 
    then return True
    else b
  
notM :: Monad m => m Bool -> m Bool
notM = liftM not

andThen a b = do
  r <- a
  if r  
    then b
    else return False
         
anyM p l = foldM (\a b -> return a `orElse` p b) False l
allM p l = foldM (\a b -> return a `andThen` p b) True l

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

evalF :: NCTL -> SS a -> Node -> (Bool,M.Map (Node,NCTL) Bool)
evalF f ss s = runState (verifyF f ss s) M.empty

lfp f a = 
  let fa = iterate f a 
  in head [x | (x,y) <- zip fa (tail fa), x == y]

-- verifyF :: (Show a) => (Eq a,Eq b) => Gr [NCTL a b] [NTrans b] -> NCTL a b -> Gr [NCTL a b] [NTrans b]
-- verifyF gr (NCTLFalse) = gr
-- verifyF gr (NCTLAtom a) = gr -- we assume that all node are already tagged with atoms
-- verifyF gr (NCTLNot f) = nmap notf $ verifyF gr f
--   where notf x = if f `elem` x then x else (NCTLNot f):x
-- verifyF gr (NCTLOr f g) = nmap orfg $ verifyF (verifyF gr f) g
--   where orfg x = if (f `elem` x) && (g `elem` x) then (NCTLOr f g):x else x
-- verifyF gr (EX f) = gmap exf grf
--   where 
--     grf = verifyF gr f
--     exf (to,node,labels,from) = 
--       if any ((elem f) . fromJust . lab grf) (suc grf node) then
--                                       (to,node,(EX f):labels,from)
--                                     else (to,node,labels,from)
-- verifyF gr (EU f g) = lfp (checkEU f g) grf
--   where
--     grf = verifyF (verifyF gr g) f
 
-- verifyF gr _ = error "Unimplemented"
     
-- checkEU f g grf = gmap eu grf
--   where    
--     checkElem x = (elem g $ fromJust $ lab grf x) || (elem (EU f g) $ fromJust $ lab grf x)
--     eu (to,node,labels,from) =
--       if not ((EU f g) `elem` labels)
--          && (g `elem` labels 
--          || any checkElem (suc grf node)) then
--         (to,node,(EU f g):labels,from)
--       else (to,node,labels,from)
