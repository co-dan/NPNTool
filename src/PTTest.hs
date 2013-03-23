module PTTest where

import PetriNet
import PTConstr
import Control.Monad
import CTL
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import Graphviz
import Bisimilarity
import NPNet
import PNAction

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
  
atom :: [Int] -> CTL
atom a = CTLAtom (show (MSet.fromList a), (==MSet.fromList a))

formula :: CTL
formula = ef (CTLOr (atom [2,2,3,4]) (atom [3,3,4,4]))

sn1 :: SNet String String PTPlace
sn1 = SNet { net = sn1'
           , elementNets = []
           , labelling = undefined
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
           , labelling = undefined
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
  
pnQ :: PTNet
pnQ = snd . run' $ do
  let [t1,t2,t3] = map Trans ["AskQ", "Greet", "Exit"]
  [p1,p2,p3] <- replicateM 3 mkPlace
  arc p1 t1
  arc t1 p2
  arc p2 t2
  arc t2 p1
  arc p1 t3
  return ()

test1 :: HANet IO
test1 = HANet
  { ptnet = pnQ { initial = MSet.fromList [1] }
  , actions = greeter . read . name
  }

data Command = AskQ | Greet | Exit  
             deriving Read
greeter :: Command -> IO ()  
greeter AskQ = putStrLn "What is your name?" >> getLine >> return ()
greeter Greet = putStrLn "Welcome!"
greeter Exit = putStrLn "Bye"


--------------------------------------------------

pn4 :: PTNet
l4  :: Labelling String
(_,pn4,l4) = flip runL new $ do
  let [t1,t2] = map (Trans . (++ "t") . show) [1,2]
  [p1,p2,p3,p4] <- replicateM 4 mkPlace
  arc p1 t1
  arc t1 p2
  label t1 "a"
  arc p3 t2
  arc t2 p4
  label t2 "b"
  return ()

pn5 :: PTNet
l5  :: Labelling String
(_,pn5,l5) = flip runL new $ do
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

initp4 :: MSet.MultiSet PTPlace
initp4 = MSet.fromList [1,3]

initp5 :: MSet.MultiSet PTPlace
initp5 = MSet.fromList [1]

-- runStateT (bisim (pn4,l4) (pn5,l5) (initp4,initp5)) (Set.empty)         

isInterchangeable :: PTNet -> PTMark -> Set Trans -> Bool
isInterchangeable n m ts =
  getAll $ F.foldMap (All . enabledS n m . (`Set.delete` ts)) ts
