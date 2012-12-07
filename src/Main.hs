module Main (main) where

import ModelChecker
import Data.Graph.Inductive
import NCTL
import StateSpace
import Data.GraphViz
import Data.Graph.Inductive.Graphviz
import qualified Data.Text.Lazy.IO as T
import qualified Data.Map as M
import Data.Maybe (fromJust)

statespace :: SS String
statespace = (gr, ntrmap, tokens)

gr :: Gr String String
gr = mkGraph [(1,"a"),(2,"b"),(3,"a"),(4,"c")] 
     [(1,2,""),(2,1,""),(3,1,""),(3,4,"")]

ntrmap :: M.Map Edge (NTrans String)
ntrmap = M.empty

tokens :: [ET]
tokens = []

labCheck a n = fromJust (lab gr n) == a
atom a = NCTLAtom (a,labCheck a)

formula = eg (NCTLOr (atom "a") (atom "b"))

main = 
  print $ evalF formula statespace 3
  
