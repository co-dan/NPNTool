{-# LANGUAGE FlexibleInstances #-}
module StateSpace where

import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import qualified Data.Text as T
import NCTL


newtype SS a b = SS (Gr a b, [Int]) -- state graph + a set of nested tokens 

visualize :: (Ord b, Labellable a) => Gr a [NTrans b] -> DotGraph Node
visualize =
  setDirectedness graphToDot params
  where params = nonClusteredParams { 
          fmtNode = \(_,l) -> [toLabel l]
          }

instance (Show a, Show b) => Labellable [(NCTL a b)] where
  toLabelValue = toLabelValue . show . map nctlToStr
