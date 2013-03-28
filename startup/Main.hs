module Main (main) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import NPNTool.CTL
import NPNTool.PetriNet

atom :: [Int] -> CTL
atom a = CTLAtom (show (MSet.fromList a), (==MSet.fromList a))

formula :: CTL
formula = ef (CTLOr (atom [2,2,3,4]) (atom [3,3,4,4]))

main = do
  print $ verifyPT pn1 formula
  print $ verifyPT pn2 formula

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