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

verify :: NCTL' -> SS a -> Node -> State (M.Map (Node,Int) Bool) Bool
verify NCTLFalse' _ _ = return False
verify NCTLTrue'  _ _ = return True
verify (NCTLAtom' (i,n,f)) ss s = do
  m <- get
  case M.lookup (s,i) m of
    Nothing -> do 
      let r = f s
      put $ M.insert (s,i) r m
      return r
    Just x -> return x
verify (NCTLNot' (i,f)) ss s = do
  m <- get
  case M.lookup (s,i) m of
    Nothing -> do
      r <- liftM (not) $ verify f ss s
      m <- get
      put $ M.insert (s,i) r m
      return r
    Just x -> return x
verify (NCTLOr' (i, f, g)) ss s = do
  m <- get
  case M.lookup (s,i) m of
    Nothing -> do
      r <- liftM2 (||) (verify f ss s) (verify g ss s)
      m <- get
      put $ M.insert (s,i) r m
      return r
    Just x -> return x
verify (EX' (i,f)) ss s = do
  m <- get
  case M.lookup (s,i) m of
    Nothing -> do
      r <- foldM (\a b -> liftM2 (||) (return a) (verify f ss b)) False (succSt ss s)
      m <- get
      put $ M.insert (s,i) r m
      return r
    Just x -> return x  

evalF :: NCTL' -> SS a -> Node -> (Bool,M.Map (Node,Int) Bool)
evalF f ss s = runState (verify f ss s) M.empty

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
