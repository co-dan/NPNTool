{-# LANGUAGE FlexibleInstances #-}
module StateSpace where

import Data.Graph.Inductive
-- import Data.GraphViz
-- import Data.GraphViz.Attributes
-- import Data.GraphViz.Attributes.Complete
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import NCTL

-- | Statespace
type SS a = (Gr a a, M.Map Edge (NTrans a), [ET])

type ET = Int -- element token
newtype NTrans a = NTrans (a,[ET])

getGraph :: SS a -> Gr a a
getGraph (x,_,_) = x

getNTrans :: SS a -> M.Map Edge (NTrans a)
getNTrans (_,x,_) = x

getETokens :: SS a -> [ET]
getETokens (_,_,x) = x

-- | Has the nested transition in a token fired during the step
fired :: SS a -> Edge -> ET -> Bool
fired (_,m,_) e t = fromMaybe False $ do
  NTrans (_,xs) <- M.lookup e m 
  return $ t `elem` xs

-- | Outgoing edges of a node
outEdges :: SS a -> Obj -> [Obj]
outEdges ss (Left s) = map (\(a,b,_) -> toTrans (a,b)) $ out (getGraph ss) s
outEdges ss (Right (_,s)) = outEdges ss (toState s)

-- | Successors of a node
succSt :: SS a -> Obj -> [Obj]
succSt (g,_,_) (Left s) = map toState $ suc g s
succSt (g,_,_) (Right (_,s)) = [toState s]

-- | Successors of a transition
succTr :: SS a -> Edge -> [Edge]
succTr ss = map fromObj . outEdges ss . toTrans

type Obj = Either Node Edge
toState :: Node -> Obj
toState = Left
toTrans :: Edge -> Obj
toTrans = Right

fromObj :: Obj -> Edge
fromObj (Right e) = e

-- visualize :: (Labellable a) => SS a -> DotGraph Node
-- visualize =
--   setDirectedness graphToDot params
--   where params = nonClusteredParams { 
--           fmtNode = \(_,l) -> [toLabel l]
--           }

-- instance Labellable [NCTL] where
--   toLabelValue = toLabelValue . show 
