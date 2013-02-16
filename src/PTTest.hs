module PTTest where

import PetriNet
import CTL
import qualified Data.Set as Set

pn1 :: PTNet
pn1 = Net { places = Set.fromList [1,2,3,4]
          , trans  = Set.fromList [t1,t2]
          , pre    = \(Trans x) -> case x of
               "t1" -> [1,2]
               "t2"  -> [1]
          , post   = \(Trans x) -> case x of
               "t1" -> [3,4]
               "t2"  -> [2]
          , initial = [1,1,2,2]
          } 
  where t1 = Trans "t1"
        t2 = Trans "t2"
  
pn2 :: PTNet         
pn2 = Net { places = Set.fromList [1,2]
          , trans  = Set.fromList [t1,t2]
          , pre    = \(Trans x) -> case x of
               "t1" -> [1]
               "t2" -> [2]
          , post   = \(Trans x) -> case x of
               "t1" -> [2]
               "t2" -> [1]
          , initial = [1]
          } 
  where t1 = Trans "t1"
        t2 = Trans "t2"
      

atom a = CTLAtom (show a,\x->x==a)
formula = ef (CTLOr (atom [2,2,3,4]) (atom [3,3,4,4]))
