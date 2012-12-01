{-# LANGUAGE FlexibleInstances #-}
module StateSpace where

import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import qualified Data.Text as T
import qualified Data.Map as M
import NCTL

type ET = Int -- element token

type SS a = (Gr a a, M.Map Edge (NTrans a), [ET])
newtype NTrans a = NTrans (a,[ET])

-- visualize :: (Labellable a) => SS a -> DotGraph Node
-- visualize =
--   setDirectedness graphToDot params
--   where params = nonClusteredParams { 
--           fmtNode = \(_,l) -> [toLabel l]
--           }

-- instance Labellable [NCTL] where
--   toLabelValue = toLabelValue . show 
