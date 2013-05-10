module Unfoldings where

import qualified Test.HUnit as H

import NPNTool.PetriNet
import NPNTool.PTConstr
import NPNTool.Unfoldings
import NPNTool.Graphviz
import qualified Data.Set as Set
import Control.Monad
import Data.Monoid
import Data.Maybe
import qualified Data.Map as M

---- Maps

(==>) :: k -> a -> M.Map k a
(==>) = M.singleton

lookupM :: Ord a => M.Map a c -> a -> c
lookupM m = fromJust . flip M.lookup m

homP :: [M.Map PTPlace PTPlace] -> (PTPlace -> PTPlace)
homP = lookupM . mconcat

transify :: Int -> Trans
transify = Trans . ("t"++) . show

homT :: [M.Map Int Int] -> (PTTrans -> PTTrans)
homT m = transify . lookupM (M.mapKeys transify (mconcat m))
       
---- Tests        
        
run' :: PTConstrM l a -> (a, PTNet)
run' = flip run new        

------ BP1 and NET1

net1 :: PTNet
((),net1) = run' $ do
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

h11 = homP [1 ==> 1, 2 ==> 2, 3 ==> 3, 4 ==> 4, 5 ==> 1, 6 ==> 2]
h12 = homT [1 ==> 2, 2 ==> 1, 3 ==> 3]

bp1 :: BProc
bp1 = (on1, (h11, h12))

testOn1 :: H.Assertion
testOn1 = H.assertBool "no conflicts in on1" $
          not (any (uncurry (conflict bp1))
               (pairs (Set.toList (places on1)) (Set.toList (places on1))))

testBp1_1 :: H.Assertion
testBp1_1 = H.assertBool "possible to extend bp1 with t2" $
            isJust (posTrans bp1 net1 (Trans "t2"))

testBp1_2 :: H.Assertion
testBp1_2 = H.assertBool "not possible to extend bp1 with t1 or t3" $
            isNothing (posTrans bp1 net1 (Trans "t1"))
            && isNothing (posTrans bp1 net1 (Trans "t3"))

bp1Tests :: H.Test
bp1Tests = H.TestCase $ do
  testOn1
  testBp1_1
  testBp1_2
          
--------------------------------------------------

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
conflictTests = H.TestList [ H.TestLabel "Testing on1" bp1Tests
                           , H.TestLabel "Testing on2" on2Tests ]
  
            
