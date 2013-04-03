-- Main test suite
module Main where

import qualified Test.HUnit as H
import System.Exit (exitFailure)

import NPNTool.PetriNet
import NPNTool.PTConstr
import NPNTool.NPNConstr (arcExpr, liftPTC, liftElemNet, addElemNet)
import qualified NPNTool.NPNConstr as NPC
import NPNTool.Graphviz
import NPNTool.Bisimilarity
import NPNTool.Liveness
import NPNTool.NPNet
import NPNTool.CTL
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import Data.Maybe (isJust,isNothing)

import Data.Monoid
import qualified Data.Foldable as F


pn1 :: PTNet
pn1 = Net { places = Set.fromList [1,2,3,4]
          , trans  = Set.fromList [t1,t2]
          , pre    = \(Trans x) -> case x of
               "t1" -> MSet.fromList [1,2]
               "t2"  -> MSet.fromList [1]
          , post   = \(Trans x) -> case x of
               "t1" -> MSet.fromList [3,4]
               "t2"  -> MSet.fromList [2]
          , initial = MSet.fromList [1,1,2,2]
          } 
  where t1 = Trans "t1"
        t2 = Trans "t2"
  
pn2 :: PTNet         
pn2 = Net { places = Set.fromList [1,2]
          , trans  = Set.fromList [t1,t2]
          , pre    = \(Trans x) -> case x of
               "t1" -> MSet.fromList [1]
               "t2" -> MSet.fromList [2]
          , post   = \(Trans x) -> case x of
               "t1" -> MSet.fromList [2]
               "t2" -> MSet.fromList [1]
          , initial = MSet.fromList [1]
          } 
  where t1 = Trans "t1"
        t2 = Trans "t2"

        
run' :: PTConstrM l a -> (a, PTNet)
run' = flip run new        

pn3 :: PTNet
pn3 = snd . run' $ do
  let [t1,t2,t3] = map Trans ["t1", "t2", "t3"]
  [p1,p2] <- replicateM 2 mkPlace
  arc p1 t1
  arc p1 t2
  arc t1 p2
  arc t2 p2
  arc p2 t3
  arc t3 p1
  return ()
  

sn1 :: NPNet String String PTPlace
sn1 = NPNet { net = sn1'
           , elementNets = IM.empty
           , labelling = undefined
           , labels = Set.empty
           }
      
sn1' = Net { places = Set.fromList [1,2,3,4]
           , trans  = Set.fromList [t1]
           , pre    = \(Trans x) -> case x of
                "t1" -> SArc $ Set.fromList [(Plus (Var "x") (Var "y"), 1)]
           , post   = \(Trans x) -> case x of
                "t1" -> SArc $ Set.fromList [(Plus (Var "x") (Var "y"), 2),
                                       (Const 1, 3),
                                       (Var "y", 4)]
           , initial = NPMark (M.fromList [(1,[Left 1])])
          } 
  where t1 = Trans "t1"
        
sn2 :: NPNet String String PTPlace
sn2 = NPNet { net = sn2'
           , elementNets = IM.empty
           , labelling = undefined
           , labels = Set.empty
           }
      
sn2' = Net { places = Set.fromList [1,2,3,4,5,6]
           , trans  = Set.fromList [t1,t2,t3,t4]
           , pre    = \(Trans t) -> case t of
                "t1" -> SArc $ Set.fromList [(x, 1), (y, 2)]
                "t2" -> SArc $ Set.fromList [(x, 3), (y, 4)]
                "t3" -> SArc $ Set.fromList [(x, 5)]                
                "t4" -> SArc $ Set.fromList [(x, 6)]                
           , post   = \(Trans t) -> case t of
                "t1" -> SArc $ Set.fromList [(x, 3), (y, 4)]
                "t2" -> SArc $ Set.fromList [(x, 5), (y, 6)]
                "t3" -> SArc $ Set.fromList [(x, 5), (x, 6)]
                "t4" -> SArc $ Set.fromList [(x, 4)]                
           , initial = NPMark (M.fromList [(1,[Left 1])])
          } 
  where [t1,t2,t3,t4] = map Trans ["t1","t2","t3","t4"]
        x = Var "x" :: Expr String Int
        y = Var "y" :: Expr String Int
  
-- pnQ :: PTNet
-- pnQ = snd . run' $ do
--   let [t1,t2,t3] = map Trans ["AskQ", "Greet", "Exit"]
--   [p1,p2,p3] <- replicateM 3 mkPlace
--   arc p1 t1
--   arc t1 p2
--   arc p2 t2
--   arc t2 p1
--   arc p1 t3
--   return ()

-- testH1 :: HANet IO
-- testH1 = HANet
--   { ptnet = pnQ { initial = MSet.fromList [1] }
--   , actions = greeter . read . name
--   }

-- data Command = AskQ | Greet | Exit  
--              deriving Read
-- greeter :: Command -> IO ()  
-- greeter AskQ = putStrLn "What is your name?" >> getLine >> return ()
-- greeter Greet = putStrLn "Welcome!"
-- greeter Exit = putStrLn "Bye"


--------------------------------------------------

pn4 :: PTNet
l4  :: Labelling String
(_,pn4',l4) = flip runL new $ do
  let [t1,t2] = map (Trans . (++ "t") . show) [1,2]
  [p1,p2,p3,p4] <- replicateM 4 mkPlace
  p2' <- mkPlace
  let t2' = Trans "2t'"
  arc p1 t1
  arc t1 p2'
  arc p2' t2'
  arc t2' p2
  label t2' "a"

  arc p1 (Trans "3t'")
--  arc p3 (Trans "3t'")
  arc (Trans "3t'") p4
  label (Trans "3t'") "a"
          
  -- arc p1 t1
  -- arc t1 p2
  -- label t1 "a"
  arc p3 t2
  arc t2 p4
  label t2 "b"
  return ()
pn4 = pn4' { initial = initp4 }

pn5 :: PTNet
l5  :: Labelling String
(_,pn5',l5) = flip runL new $ do
  let [t1,t2,t3,t4] = map (Trans . (++ "tr") . show ) [1..4]
  [p0,p1,p2,p3,p4] <- replicateM 5 mkPlace
  arc p0 t1
  arc p0 t2
  label t1 "a"
  label t2 "b"
  arc t1 p1
  arc t2 p3

  arc p1 t3
  arc t3 p2
  label t3 "b"
  arc p3 t4
  arc t4 p4
  label t4 "a"
  
  -- arc p2 (Trans "5tr")
  -- label (Trans "5tr") "b"
  -- pp <- mkPlace
  -- arc (Trans "5tr") pp
  return ()

pn5 = pn5' { initial = initp5 }

initp4 :: MSet.MultiSet PTPlace
initp4 = MSet.fromList [1,3]

initp5 :: MSet.MultiSet PTPlace
initp5 = MSet.fromList [1]

test1 = H.assertBool "pn4 should be m-bisimilar to pn5" (isJust (isMBisim (pn4,l4) (pn5,l5)))

pn6 :: PTNet
l6  :: Labelling String
(_,pn6',l6) = flip runL new $ do
  [p1,p2,p3,p4,p5] <- replicateM 5 mkPlace
  conn p1 p2 "a"
  conn p2 p3 "a"
  conn p2 p4 "b"
  conn p1 p5 "a"
  return ()

pn6 = pn6' { initial = MSet.fromList [1] }

test2 = H.assertBool "pn6 should NOT be m-bisimilar to itself" (isNothing (isMBisim (pn6,l6) (pn6,l6)))
                                                       
data L = A | B | C deriving (Show,Eq,Ord)

circ = do
  p2 <- mkPlace
  p3 <- mkPlace
  conn p2 p3 B
  conn p3 p2 A
  return (p2,p3)

(_,pn7',l7) = flip runL new $ do
  p1 <- mkPlace
  (p2,_) <- circ
  conn p1 p2 A
  return ()

pn7 = pn7' { initial = MSet.fromList [1] }
      
(_,pn8',l8) = flip runL new $ do
  p1 <- mkPlace
  t <- mkTrans
  (_,p3) <- circ
  arc p1 t
  arc t p3
  return ()

pn8 = pn8' { initial = MSet.fromList [1] }  

test3 = H.assertBool "pn7 should be m-bisimilar to pn8" (isJust (isMBisim (pn7,l7) (pn8,l8)))


pn9 :: PTNet
(_,pn9',l9) = flip runL new $ do
  [p1,p2,p3,p4] <- replicateM 4 mkPlace
  [t1,t2,t3] <- replicateM 3 mkTrans
  arc p1 t1
  arc t1 p2
  
  arc p2 t2
  arc p2 t3
  arc t2 p3
  arc t3 p4
  label t2 "a"
  label t3 "b"
  return ()
pn9 = pn9' { initial = MSet.fromList [1] }

pn10 :: PTNet
(_,pn10',l10) = flip runL new $ do
  [s1,s2,s3,s4] <- replicateM 4 mkPlace
  [tr1,tr2,tr3,tr4,tr5] <- replicateM 5 mkTrans

  [p1,p2,p3,p4,p5] <- replicateM 5 mkPlace
  [t1,t2,t3,t4] <- replicateM 4 mkTrans

  arc s1 tr1
  arc tr1 s2
  arc s2 tr2
  arc tr2 s1
  
  arc s2 tr3
  arc tr3 p1
  
  arc s2 tr4
  arc tr4 s3
  arc s3 tr5
  arc tr5 s4
  label tr5 "a"
  
  arc p1 t1
  arc t1 p2
  
  arc p2 t2
  arc p2 t3
  arc t2 p3
  label t2 "b"

  arc t3 p4
  arc p4 t4
  arc t4 p5
  label t4 "a"
  return ()

pn10 = pn10' { initial = MSet.fromList [1] }

test4 :: H.Assertion
test4 = H.assertBool "pn9 should NOT be m-bisimilar to pn10"
        (isNothing (isMBisim (pn9,l9) (pn10,l10)))

isInterchangeable :: PTNet -> PTMark -> Set Trans -> Bool
isInterchangeable n m ts =
  getAll $ F.foldMap (All . enabledS n m . (`Set.delete` ts)) ts


mBisimTests = H.TestList [ H.TestLabel "test1" (H.TestCase test1)
                         , H.TestLabel "test2" (H.TestCase test2)
                         , H.TestLabel "test3" (H.TestCase test3) 
                         , H.TestLabel "test4" (H.TestCase test4)]


((a,b,idle),twoProcNet') = run' $ do
  let pA = Trans "PickA"
      pB = Trans "PickB"
      pAB = Trans "PickAB"
      pBA = Trans "PickBA"
  [a,b,waitA,waitB,idle] <- replicateM 5 mkPlace

  arc a    pA
  arc idle pA
  arc pA waitB
  arc waitB pAB
  arc b     pAB
  arc pAB   b
  arc pAB   a
  arc pAB   idle

  arc b    pB
  arc idle pB
  arc pB   waitA
  arc waitA pBA
  arc a     pBA
  arc pBA   a
  arc pBA   idle
  arc pBA   b

  return (a,b,idle)

twoProcNet1 = twoProcNet' { initial = MSet.fromList [a,b,idle] }
twoProcNet2 = twoProcNet' { initial = MSet.fromList [a,b,idle,idle] }              

testLive1 = H.assertBool "twoProcNet1 should be live" (isLive ss1 twoProcNet1)
  where ss1 = reachabilityGraph twoProcNet1

testLive2 = H.assertBool "twoProcNet2 should NOT be live" (not (isLive ss2 twoProcNet2))
  where ss2 = reachabilityGraph twoProcNet2

p2pLive = H.assertBool "snP2P should be live as a sep component"
          (isLive (reachabilityGraph (net snP2P)) (net snP2P))
        
livenessTests = H.TestList
                [ H.TestLabel "Liveness test 1" (H.TestCase testLive1)
                , H.TestLabel "Liveness test 2" (H.TestCase testLive2)
                , H.TestLabel "Liveness test for P2P example 1"
                  (H.TestCase p2pLive)]

dynTests = H.TestList [ H.TestLabel "DynNet test 1" (H.TestCase dynTest1) ]

main = do
  c <- H.runTestTT $ H.TestList [mBisimTests, livenessTests, dynTests]
  when (H.errors c > 0 || H.failures c > 0) exitFailure


data P2PL = StartTransfer | EndTransfer | Reinitialize
          deriving (Show, Eq, Ord)

lSeed, lPeer, lPipe :: Labelling P2PL
enSeed, enPeer, enPipe :: PTNet

(_,enSeed,lSeed) = flip runL new $ do
  [p1,p2,p3] <- replicateM 3 mkPlace
  [t1,t2,t3] <- replicateM 3 mkTrans
  arc p1 t1
  arc t1 p2
  arc p2 t2
  arc t2 p3
  arc p3 t3
  arc t3 p1
  label t1 StartTransfer
  label t2 EndTransfer
  label t3 Reinitialize
  mark p1
  
  
(_,enPeer,lPeer) = flip runL new $ do
  [p4,p5] <- replicateM 2 mkPlace
  [t4,t5] <- replicateM 2 mkTrans
  label t4 StartTransfer
  label t5 EndTransfer
  arc p4 t4
  arc p5 t5
  arc t4 p5
  arc t5 p4
  mark p5

(_,enPipe,lPipe) = flip runL new $ do
  [p6,p7,p8,p9,p10,p11,p12,p13,p14,p15] <- replicateM 10 mkPlace
  [t6,t7,t8,t9,t10,t11,t12] <- replicateM 7 mkTrans
  arc p6 t7
  arc t6 p6

  arc p7 t6
  arc p7 t12
  arc t7 p7
  arc t11 p7

  arc p8 t8
  arc t7 p8

  arc p9 t7
  arc t8 p9

  arc p10 t9
  arc t8 p10

  arc p11 t8
  arc t9 p11

  arc p12 t9
  arc t6 p12

  arc p13 t12
  arc p13 t6
  arc t11 p13
  arc t9 p13

  arc p14 t10
  arc t12 p14

  arc p15 t11
  arc t10 p15

  label t12 EndTransfer
  label t11 StartTransfer
  label t10 Reinitialize

  mark p9
  mark p11
  mark p15
  return ()
  
snP2P :: NPNet P2PL String Int
snP2P = snd . flip NPC.run NPC.new $ do
  [initSeed,initPeer,initPipe,
   finalSeed,finalPeer,finalPipe,
   p22,p23] <- replicateM 8 NPC.mkPlace
  [t14,t15,t16,t17] <- replicateM 4 NPC.mkTrans
  NPC.label t14 StartTransfer
  NPC.label t16 EndTransfer
  NPC.label t15 Reinitialize
  NPC.label t17 Reinitialize
  let x = Var "x"
      y = Var "y"
      z = Var "z"
  arcExpr initSeed x t14
  arcExpr initPeer y t14
  arcExpr initPipe z t14
  arcExpr t14 x finalSeed
  arcExpr t14 y finalPeer
  arcExpr t14 z finalPipe

  arcExpr finalSeed x t16
  arcExpr finalPeer y t16
  arcExpr finalPipe z t16

  arcExpr t16 x p22
  arcExpr t16 z p23
  arcExpr t16 y initPeer

  arcExpr p22 x t15
  arcExpr t15 x initSeed

  arcExpr p23 z t17
  arcExpr t17 z initPipe

  [seed1,seed2,seed3] <- replicateM 3 (addElemNet (enSeed,lSeed,initial enSeed))
  pipe1 <- addElemNet (enPipe,lPipe,initial enPipe)
  [peer1,peer2,peer3,peer4] <- replicateM 4 (addElemNet (enPeer,lPeer,initial enPeer))

  NPC.marks initSeed (map Right [seed1,seed2,seed3])
  NPC.mark initPipe (Right pipe1)
  NPC.marks initPeer (map Right [peer1,peer2,peer3,peer4])

dynTest1 :: H.Assertion
dynTest1 = H.assertBool "t1 should be enabled in snP2P"
           (enabled (net snP2P) (initial (net snP2P)) (Trans "t1"))
           
