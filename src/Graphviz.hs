module Graphviz (drawPT) where

import PetriNet
import Data.Set (Set)
import qualified Data.Set as S
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS

showSet :: Show a => Set a -> String
showSet = concatMap ((++ "; ") . show) . S.toList
          
showPre :: PTNet -> Trans -> String
showPre (Net {pre=pre}) t =
  concatMap ((++ " -> \"" ++ show t ++ "\"; ") . show) (MS.toList (pre t))
  
showPost :: PTNet -> Trans -> String
showPost (Net {post=post}) t =
  concatMap (\x -> "\"" ++ show t ++ "\" -> " ++ show x ++ "; ") (MS.toList (post t))


drawPT :: PTNet -> String
drawPT net = 
  unlines 
    [ "digraph PT {"
    , "    subgraph place {"
    , "        graph [shape=circle,color=gray];"
    , "        node [shape=circle,fixedsize=true,width=1];"
    , "        " ++ showSet (places net)
    , "    }"
    , "    subgraph transitions {"
    , "        node [shape=rect,height=0.2,width=1];"
    , "        " ++ showSet (trans net)
    , "    }"
    , "    " ++ concatMap (showPre net) (S.toList (trans net))
    , "    " ++ concatMap (showPost net) (S.toList (trans net))
    , "}"]
