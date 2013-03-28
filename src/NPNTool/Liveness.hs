module NPNTool.Liveness (
  isLive) where

import NPNTool.PetriNet
import Data.Set (Set)
import qualified Data.Set as S

import Data.Graph.Inductive hiding (NodeMap)
import Data.Graph.Inductive.Query.DFS

isTerminal :: Graph gr => gr a b -> [Node] -> Bool
isTerminal g ns = all (not . hasOut . (flip match g)) ns
  where hasOut (Nothing,_) = False
        hasOut (Just (_,_,_,out),_) = any (not . (`elem` ns) . snd) out

everyTrans :: SS -> Set Trans -> [Node] -> Bool
everyTrans ss tr [] = S.null tr
everyTrans ss tr (n:ns) =
  let tr' = foldl (\acc t -> S.delete (snd t) acc) tr (lsuc ss n)
  in everyTrans ss tr' ns

-- | Whether a Petri Net is live (based on its reachability graph)
isLive :: SS -> PTNet -> Bool
isLive ss pn = all (\c -> not (isTerminal ss c) || everyTrans ss (trans pn) c) (scc ss)

  
  
