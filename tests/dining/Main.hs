module Main (main) where

import NPNTool.PTConstr
import NPNTool.NPNConstr (NPNConstrM,arcExpr)  
import qualified NPNTool.NPNConstr as NPC
import NPNTool.NPNet
import NPNTool.PetriNet
import NPNTool.Liveness
import Control.Monad (replicateM,forM)
import Criterion.Main
import Criterion.Config
import Debug.Trace

cfg :: Config
cfg = defaultConfig { cfgTemplate = ljust "./report.tpl"
                    , cfgReport = ljust "./report.html"
                    , cfgSamples = ljust 20
                    , cfgResamples = ljust (20 * 10) }

main = defaultMainWith cfg (return ()) [
  -- bgroup "diningPhils" $ 
  -- map (\x -> bench (show x) $ nf bmark x) [3,5]
  ]


-- Labels  
data ForkLabel = PickR | PickL | Put
               deriving (Show,Eq,Ord)
-- Variables                        
data V = X -- we only need one
       deriving (Show,Eq,Ord)

