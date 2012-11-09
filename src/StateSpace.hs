{-# LANGUAGE FlexibleInstances #-}
module StateSpace where

import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import qualified Data.Text as T
import NCTL

visualize :: Labellable a => Gr a () -> DotGraph Node
visualize =
  setDirectedness graphToDot params
  where params = nonClusteredParams { 
          fmtNode = \(_,l) -> [toLabel l]
          }

instance (Show a) => Labellable [(NCTL a)] where
  toLabelValue = toLabelValue . show . map nctlToStr
