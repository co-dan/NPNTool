module Unfoldings where

import qualified Test.HUnit as H

import NPNTool.PetriNet
import NPNTool.PTConstr
import NPNTool.Unfoldings
import qualified Data.Set as Set
import Control.Monad

---- Tests        
        
run' :: PTConstrM l a -> (a, PTNet)
run' = flip run new        

testNet :: PTNet
((),testNet) = run' $ do
  [s1,s2,s3,s4] <- replicateM 4 mkPlace
  [t1,t2,t3] <- replicateM 3 mkTrans
  arc t1 s1
  arc s1 t2
  arc t2 s3
  arc s3 t1
  
  arc s2 t2
  arc t2 s4
  arc s4 t3
  arc t3 s2
  return ()

on1 :: OccurNet
on1 = toOccurNet . flip runConstr new $ do
  ps <- replicateM 6 mkPlace 
  ts <- replicateM 3 mkTrans
  arc (ps !! 0) (ts !! 0)
  arc (ps !! 1) (ts !! 0)
  arc (ts !! 0) (ps !! 2)
  arc (ts !! 0) (ps !! 3)
  arc (ps !! 2) (ts !! 1)
  arc (ps !! 3) (ts !! 2)
  arc (ts !! 1) (ps !! 4)
  arc (ts !! 2) (ps !! 5)
  return ()

bp1 :: BProc
bp1 = (on1, undefined)

testOn1 :: H.Assertion
testOn1 = H.assertBool "no conflicts in on1"
          ((filter (uncurry (conflict bp1)) $ pairs (Set.toList (places on1)) (Set.toList (places on1)))
           == [])

on2 :: OccurNet
on2 = toOccurNet . flip runConstr new $ do 
  [p1,p2,p3,p4,p5,p6,p7] <- replicateM 7 mkPlace
  [t1,t2,t3,t4,t5] <- replicateM 5 mkTrans
  arc p1 t1
  arc t1 p3
  arc p2 t2
  arc p2 t3
  arc t2 p4
  arc t3 p5
  
  arc p3 t4
  arc p3 t5
  arc p4 t4
  arc p5 t5
  arc t4 p6
  arc t5 p7

bp2 :: BProc
bp2 = (on2, undefined)


testOn2_1 :: H.Assertion
testOn2_1 = H.assertBool "p4 and p5 should be in conflict" $
            conflict bp2 4 5

testOn2_2 :: H.Assertion
testOn2_2 = H.assertBool "p6 and p7 should be in conflict" $
            conflict bp2 6 7
            
testOn2_3 :: H.Assertion
testOn2_3 = H.assertBool "p4 and p7 should be in conflict" $
            conflict bp2 4 7
            
testOn2_4 :: H.Assertion
testOn2_4 = H.assertBool "p6 and p5 should be in conflict" $
            conflict bp2 5 6
            
            
on2Tests :: H.Test
on2Tests = H.TestCase $ do
  testOn2_1
  testOn2_2
  testOn2_3 
  testOn2_4

conflictTests :: H.Test
conflictTests = H.TestList [ H.TestLabel "Testing on1" (H.TestCase testOn1)
                           , H.TestLabel "Testing on2" on2Tests ]
  
            
