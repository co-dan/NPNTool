module Main (main) where

import NPNTool.PTConstr
import NPNTool.NPNConstr (NPNConstrM,arcExpr)  
import qualified NPNTool.NPNConstr as NPC
import NPNTool.NPNet
import NPNTool.PetriNet
import NPNTool.Liveness
import Control.Monad (replicateM,forM)

bmark x =
  let npnet = diningPhils x
      sn    = net npnet
      ss    = reachabilityGraph sn
  in
   isLive ss sn

-- Labels  
data ForkLabel = PickR | PickL | Put
               deriving (Show,Eq,Ord)
-- Variables                        
data V = X -- we only need one
       deriving (Show,Eq,Ord)

philAgent :: PTConstrM ForkLabel ()
philAgent = do
  [p1,p2,p3] <- replicateM 3 mkPlace
  [pickL,pickR,put] <- replicateM 3 mkTrans
  label pickL PickL
  label pickR PickR
  label put Put
  mark p1
  arc p1 pickR
  arc pickR p2
  arc p2 pickL
  arc pickL p3
  arc p3 put
  arc put p1

lastPhilAgent :: PTConstrM ForkLabel ()
lastPhilAgent = do
  [p1,p2,p3] <- replicateM 3 mkPlace
  [pickL,pickR,put] <- replicateM 3 mkTrans
  label pickL PickL
  label pickR PickR
  label put Put
  mark p1
  arc p1 pickL
  arc pickL p2
  arc p2 pickR
  arc pickR p3
  arc p3 put
  arc put p1

-- returns (Fork_i,PickL_i,Put_i)
phil :: PTConstrM ForkLabel () -> NPNConstrM ForkLabel V (PTPlace,Trans,Trans)
phil agent = do
  [fork,noForkL,noForkR] <- replicateM 3 NPC.mkPlace
  p <- NPC.mkPlace
  [pickL,pickR,put] <- replicateM 3 NPC.mkTrans
  philAgent <- NPC.liftElemNet agent
  NPC.label pickL PickL
  NPC.label pickR PickR
  NPC.label put Put
  NPC.mark fork (Left 1)
  NPC.mark p (Right philAgent)

  forM [pickL,pickR,put] $ \t ->
    arcExpr p (Var X) t >> arcExpr t (Var X) p
  
  NPC.arc fork pickR
  NPC.arc pickR noForkR
  NPC.arc pickL noForkL
  NPC.arc noForkL put
  NPC.arc noForkR put
  NPC.arc put fork
  
  return (fork,pickL,put)


-- Works only for n >= 2
cyclePhils :: Int -> NPNConstrM ForkLabel V ()
cyclePhils n = do
  (fork1,pickL1,put1) <- phil philAgent
  (pickL,put) <- midPhils (n-2) (pickL1,put1)
  (forkLast,pickLLast,putLast) <- phil lastPhilAgent

  NPC.arc put forkLast
  NPC.arc forkLast pickL
  NPC.arc fork1 pickLLast
  NPC.arc putLast fork1
  
midPhils :: Int -> (Trans,Trans) -> NPNConstrM ForkLabel V (Trans,Trans)
midPhils n interf | n == 0    = return interf
                  | otherwise = do
  (pl,put) <- midPhils (n-1) interf
  (f',pl',put') <- phil philAgent
  NPC.arc put f'
  NPC.arc f' pl
  return (pl',put')
  
  
diningPhils :: Int -> NPNet ForkLabel V Int
diningPhils n = snd $ NPC.run (cyclePhils n) NPC.new
