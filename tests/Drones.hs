module Drones where

import qualified Test.HUnit as H
import NPNTool.PetriNet
import NPNTool.PTConstr
import NPNTool.NPNConstr (arcExpr, liftPTC, liftElemNet, addElemNet, NPNConstrM)
import qualified NPNTool.NPNConstr as NPC
import NPNTool.Graphviz
import NPNTool.Bisimilarity
import NPNTool.Liveness
import NPNTool.AlphaTrail
import NPNTool.NPNet
import NPNTool.CTL
import Control.Monad
import Data.List
import Data.Maybe

data DroneLabels = Leave | Orbit | Attack
                 deriving (Show,Eq,Ord)
data V = X | Y | Z
       deriving (Show,Ord,Eq)

drone :: PTConstrM DroneLabels ()
drone = do
  [station,air,searching,orbiting,attacking,fin,avoid] <- replicateM 7 mkPlace
  [leave,orbit,startSearch,start,finish,returnBack,tooClose,continue] <- replicateM 8 mkTrans
  label leave Leave
  label orbit Orbit
  label start Attack

  arc station leave
  arc leave air
  arc air startSearch
  arc startSearch searching
  arc searching orbit
  arc orbit orbiting

  arc orbiting tooClose
  arc tooClose avoid
  arc avoid continue
  arc continue orbiting
  
  arc orbiting start
  arc start attacking
  arc attacking finish
  arc finish fin
  arc fin returnBack
  arc returnBack station

  mark station
  return ()

x = Var X
y = Var Y
z = Var Z

droneSystem :: NPNConstrM DroneLabels V [PTPlace]
droneSystem = do
  let n = 3
  basePs <- replicateM n NPC.mkPlace
  leaveTs <- replicateM n NPC.mkTrans
  airPs <- replicateM n NPC.mkPlace
  orbitTs <- replicateM n NPC.mkTrans
  orbitingPs <- replicateM n NPC.mkPlace
  attack <- NPC.mkTrans
  finPs <- replicateM n NPC.mkPlace
  retTs <- replicateM n NPC.mkTrans

  drones <- replicateM n (NPC.liftElemNet drone)

  NPC.label attack Attack
  forM orbitTs (flip NPC.label Orbit)
  forM leaveTs (flip NPC.label Leave)
  
  forM_ (zip basePs drones) $ \(b,d) -> NPC.mark b (Right d)
  forM_ (zip5 basePs leaveTs airPs orbitTs orbitingPs) $ \(b,l,a,o,orbiting) -> do
    arcExpr b x l
    arcExpr l x a
    arcExpr a x o
    arcExpr o x orbiting

  forM_ (zip3 [x,y,z] orbitingPs finPs) $ \(v,orbiting,fin) -> do
    arcExpr orbiting v attack
    arcExpr attack v fin

  forM_ (zip3 finPs retTs basePs) $ \(fin,ret,base) -> do
    arcExpr fin x ret
    arcExpr ret x base

  return basePs

droneNet :: NPNet DroneLabels V Int
bases :: [PTPlace]
(bases,droneNet) = NPC.run (droneSystem) NPC.new



sn = net droneNet
ss = reachabilityGraph sn
((),droneElemNet, droneL) = runL drone new

(atN,atL) = alphaTrail droneNet 1
(nm,rg) = reachabilityGraph' atN

droneTest = H.TestCase $ do
  H.assertBool "Normal agent liveness" $
    isLive (reachabilityGraph droneElemNet) droneElemNet
  H.assertBool "System net liveness" $
    isLive ss sn
  mapM_ (H.assertBool "Agent is m-bisimilar to the alpha-trail net"
        . isJust
        . isMBisim (droneElemNet,droneL)
        . alphaTrail droneNet) 
    bases
  return ()

droneTestXML = H.TestCase $ do
  npn <- runConstr "./test/Drones.npnets"
  
  -- [base,air,orbiting,fin] <- replicateM 4 NPC.mkPlace
  -- [leave,orbit,attack,ret] <- replicateM 4 NPC.mkTrans
  
  -- arcExpr base x leave
  -- arcExpr leave x air
  -- arcExpr air x orbit
  -- arcExpr orbit x orbiting
  -- arcExpr orbiting x attack
  -- arcExpr attack x fin
  -- arcExpr fin x ret
  -- arcExpr ret x base

  -- NPC.label attack Attack

  -- d1 <- NPC.liftElemNet drone
  -- NPC.mark base (Right d1)
  -- return [base]

      -- if (null nodes) then do
      --   trace "sim1" $ return ()
      --   traceShow l $ return ()
      --   traceShow m1 $ traceShow m2 $ return ()
      --  else return ()
