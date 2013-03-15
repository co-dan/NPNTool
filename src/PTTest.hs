module PTTest where

import PetriNet
import PTConstr
import Control.Monad
import CTL
import qualified Data.Set as Set
import qualified Data.MultiSet as MSet
import Graphviz
import NPNet

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
  arc p1 t1
  arc p1 t2
  arc t1 p2
  arc t2 p2
  arc p2 t3
  arc t3 p1
  return ()
  
atom :: [Int] -> CTL
atom a = CTLAtom (show (MSet.fromList a), (==MSet.fromList a))

formula :: CTL
formula = ef (CTLOr (atom [2,2,3,4]) (atom [3,3,4,4]))

sn1 :: SNet String String PTPlace
sn1 = SNet { net = sn1'
           , elementNets = []
           , label = undefined
           , labels = Set.empty
           }
      
sn1' = Net { places = Set.fromList [1,2,3,4]
           , trans  = Set.fromList [t1]
           , pre    = \(Trans x) -> case x of
                "t1" -> MSExpr $ MSet.fromList [(Plus (Var "x") (Var "y"), 1)]
           , post   = \(Trans x) -> case x of
                "t1" -> MSExpr $ MSet.fromList [(Plus (Var "x") (Var "y"), 2),
                                       (Const 1, 3),
                                       (Var "y", 4)]
           , initial = MSet.fromList [1]
          } 
  where t1 = Trans "t1"
        
sn2 :: SNet String String PTPlace
sn2 = SNet { net = sn2'
           , elementNets = []
           , label = undefined
           , labels = Set.empty
           }
      
sn2' = Net { places = Set.fromList [1,2,3,4,5,6]
           , trans  = Set.fromList [t1,t2,t3,t4]
           , pre    = \(Trans t) -> case t of
                "t1" -> MSExpr $ MSet.fromList [(x, 1), (y, 2)]
                "t2" -> MSExpr $ MSet.fromList [(x, 3), (y, 4)]
                "t3" -> MSExpr $ MSet.fromList [(x, 5)]                
                "t4" -> MSExpr $ MSet.fromList [(x, 6)]                
           , post   = \(Trans t) -> case t of
                "t1" -> MSExpr $ MSet.fromList [(x, 3), (y, 4)]
                "t2" -> MSExpr $ MSet.fromList [(x, 5), (y, 6)]
                "t3" -> MSExpr $ MSet.fromList [(x, 5), (x, 6)]
                "t4" -> MSExpr $ MSet.fromList [(x, 4)]                
           , initial = MSet.fromList [1]
          } 
  where [t1,t2,t3,t4] = map Trans ["t1","t2","t3","t4"]
        x = Var "x" :: Expr String Int
        y = Var "y" :: Expr String Int
  
