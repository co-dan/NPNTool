module PTTest where

import PetriNet
import PTConstr
import Control.Monad
import CTL
import qualified Data.Set as Set
import qualified Data.MultiSet as MSet
import Graphviz

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

        
run' :: PTConstrM a -> (a, PTNet)
run' = flip run new        

pn3 :: PTNet
pn3 = snd . run' $ do
  let [t1,t2,t3] = map Trans ["t1", "t2", "t3"]
  [p1,p2] <- replicateM 2 mkPlace
  inT  p1 t1
  inT  p1 t2
  outT t1 p2
  outT t2 p2
  inT  p2 t3
  outT t3 p1
  return ()
  
atom :: [Int] -> CTL
atom a = CTLAtom (show (MSet.fromList a), (==MSet.fromList a))

formula :: CTL
formula = ef (CTLOr (atom [2,2,3,4]) (atom [3,3,4,4]))
