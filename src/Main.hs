module Main (main) where

import ModelChecker
import Data.Graph.Inductive
import NCTL
import StateSpace
import Data.GraphViz
import Data.Graph.Inductive.Graphviz
import qualified Data.Text.Lazy.IO as T

statespace :: Gr [Char] ()
statespace = mkGraph [(1,"a"),(2,"b"),(3,"a"),(4,"c")] 
             [(1,2,()),(2,1,()),(3,1,()),(3,4,())]
formula = EU (NCTLAtom 'a') (NCTLAtom 'b')

main = do
  let g = printDotGraph $ visualize $ verify statespace formula
  T.putStr g
  -- let ss = verify statespace formula
  -- putStrLn $ graphviz' ss -- "State_space" (500,500) (15,15) Portrait
