-- | Simple module for producing Graphviz diagrams of Petri Nets
module NPNTool.Graphviz (drawPT, drawWithLab,drawBP) where

import NPNTool.PetriNet
import NPNTool.NPNet
import NPNTool.Unfoldings  
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

-- | Draw a P/T-net
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


showLab :: (Show a1, Show a) => (a, a1) -> String
showLab (t, l) = filter (not . (`elem` "\"'")) $ show t ++ "_" ++ show l

showSetLab :: (Show a, Show b) => Set (a,b) -> String
showSetLab = concatMap ((++ "\"; ") . ("\"" ++ ) . showLab) . S.toList
        
showPreLab :: (Show p, Foldable n, Show l) => Net p Trans n m -> (Trans, Maybe l) -> String
showPreLab (Net {pre=pre}) tr@(t,l)  =
  F.concatMap ((++ " -> \"" ++ showLab tr  ++ "\"; ") . show) (pre t)
 
showPostLab :: (Show p, Foldable n, Show l) => Net p Trans n m -> (Trans, Maybe l) -> String
showPostLab (Net {post=post}) tr@(t,l) =
  F.concatMap (\x -> "\"" ++ showLab tr ++ "\" -> " ++ show x ++ "; ") (post t)

-- | Draws a net using the labelling for transitions
drawWithLab :: (Show p, Foldable n, Show l, Ord l) => Net p Trans n m -> Labelling l -> String
drawWithLab net lab =
    unlines 
    [ "digraph PT {"
    , "    subgraph place {"
    , "        graph [shape=circle,color=gray];"
    , "        node [shape=circle,fixedsize=true,width=1];"
    , "        " ++ showSet (places net)
    , "    }"
    , "    subgraph transitions {"
    , "        node [shape=rect,height=0.2,width=1];"
    , "        " ++ showSetLab tr
    , "    }"
    , "    " ++ concatMap (showPreLab net) (S.toList tr)
    , "    " ++ concatMap (showPostLab net) (S.toList tr)
    , "}"]
  where tr = S.map (\x -> (x, lab x)) (trans net)


-- | Draws a branching process
drawBP :: BProc -> String
drawBP bp@(net,(hp,ht)) = unlines 
    [ "digraph PT {"
    , "    subgraph place {"
    , "        graph [shape=circle,color=gray];"
    , "        node [shape=circle,fixedsize=true,width=1];"
    , "        " ++ showSetLab ps
    , "    }"
    , "    subgraph transitions {"
    , "        node [shape=rect,height=0.2,width=1];"
    , "        " ++ showSetLab tr
    , "    }"
    , "    " ++ concatMap (showPreBP bp) (S.toList tr)
    , "    " ++ concatMap (showPostBP bp) (S.toList tr)
    , "}"]
  where tr = S.map (\x -> (x, ht x)) (trans net)
        ps = S.map (\p -> (p, hp p)) (places net)

showPreBP :: BProc -> (PTTrans, PTTrans) -> String
showPreBP (Net {pre=pre},(hp,ht)) tr@(t,tl) =
  F.concatMap (\p -> "\"" ++ showLab (p, hp p) ++ "\" -> \"" ++ showLab tr  ++ "\"; ") (pre t)

showPostBP :: BProc -> (PTTrans, PTTrans) -> String
showPostBP (Net {post=post},(hp,ht)) tr@(t,tl) =
  F.concatMap (\x -> "\"" ++ showLab tr ++ "\" -> \"" ++ showLab (x, hp x) ++ "\"; ") (post t)

