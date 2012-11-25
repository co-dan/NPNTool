{-# LANGUAGE FlexibleInstances #-}
module StateSpace where

import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import qualified Data.Text as T
import NCTL

type SS a = Gr a ()

visualize :: (Labellable a) => SS a -> DotGraph Node
visualize =
  setDirectedness graphToDot params
  where params = nonClusteredParams { 
          fmtNode = \(_,l) -> [toLabel l]
          }

instance Labellable [NCTL] where
  toLabelValue = toLabelValue . show 
