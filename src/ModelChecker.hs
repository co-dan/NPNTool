module ModelChecker where

import Data.Graph.Inductive
import Data.Maybe (fromJust)
import StateSpace
import NCTL
import Data.Function
import Debug.Trace  
import Data.List

verify :: (Show a) => (Eq a) => Gr [a] () -> NCTL a -> Gr [NCTL a] ()
verify g = verifyF $ nmap (map NCTLAtom) g



instance (Eq a, Eq b) => Eq (Gr a b) where
  (==) = equal
    
lfp f a = 
  let fa = iterate f a 
  in head [x | (x,y) <- zip fa (tail fa), x == y]


     
verifyF :: (Show a) => (Eq a) => Gr [NCTL a] () -> NCTL a -> Gr [NCTL a] ()
verifyF gr (NCTLFalse) = gr
verifyF gr (NCTLAtom a) = gr -- we assume that all node are already tagged with atoms
verifyF gr (NCTLNot f) = nmap notf $ verifyF gr f
  where notf x = if f `elem` x then x else (NCTLNot f):x
verifyF gr (NCTLOr f g) = nmap orfg $ verifyF (verifyF gr f) g
  where orfg x = if (f `elem` x) && (g `elem` x) then (NCTLOr f g):x else x
verifyF gr (EX f) = gmap exf grf
  where 
    grf = verifyF gr f
    exf (to,node,labels,from) = 
      if any ((elem f) . fromJust . lab grf) (suc grf node) then
                                      (to,node,(EX f):labels,from)
                                    else (to,node,labels,from)
verifyF gr (EU f g) = lfp (checkEU f g) grf
  where
    grf = verifyF (verifyF gr g) f
 
verifyF gr _ = error "Unimplemented"
     
checkEU f g grf = gmap eu grf
  where    
    checkElem x = (elem g $ fromJust $ lab grf x) || (elem (EU f g) $ fromJust $ lab grf x)
    eu (to,node,labels,from) =
      if not ((EU f g) `elem` labels)
         && (g `elem` labels 
         || any checkElem (suc grf node)) then
        (to,node,(EU f g):labels,from)
      else (to,node,labels,from)
