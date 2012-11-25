module Main (main) where

import ModelChecker
import Data.Graph.Inductive
import NCTL
import StateSpace
import Data.GraphViz
import Data.Graph.Inductive.Graphviz
import qualified Data.Text.Lazy.IO as T
import qualified Data.Map as M

statespace :: SS String
statespace = mkGraph [(1,"a"),(2,"b"),(3,"a"),(4,"c")] 
             [(1,2,()),(2,1,()),(3,1,()),(3,4,())]
a n | n == 1    = True
    | n == 3    = True
    | otherwise = False

formula = NCTLOr' (1, NCTLAtom' (2, "a", a) , EX' (3, NCTLAtom' (2, "a", a)))

main = do
  print $ evalF formula statespace 1
  
