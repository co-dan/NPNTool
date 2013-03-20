module Graphviz (drawPT) where

import PetriNet
import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable (Foldable)
import qualified Data.Foldable as F

showSet :: Show a => Set a -> String
showSet = concatMap ((++ "; ") . show) . S.toList
          
showPre :: (Show p, Foldable n) => Net p Trans n m -> Trans -> String
showPre (Net {pre=pre}) t =
  F.concatMap ((++ " -> \"" ++ show t ++ "\"; ") . show) (pre t)
  
showPost :: (Show p, Foldable n) => Net p Trans n m -> Trans -> String
showPost (Net {post=post}) t =
  F.concatMap (\x -> "\"" ++ show t ++ "\" -> " ++ show x ++ "; ") (post t)


drawPT :: (Show p, Foldable n) => Net p Trans n m -> String
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
