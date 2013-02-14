{-# LANGUAGE Rank2Types, FlexibleContexts #-}
module PetriNet (
  Net(..), Trans(..), SS,
  enabled, fire,
  reachabilityGraph) where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Monoid
import Data.Graph.Inductive (Gr)
import qualified Data.Graph.Inductive as G
--import Data.Graph.Inductive.NodeMap
import NodeMap
import qualified Data.Foldable as F

data Trans p = Trans 
               { name :: String
               }
             deriving (Eq,Ord)
                      
instance Show (Trans p) where
  show = name

data Net p n m = Net 
               { places :: Set p
               , trans :: Ord (Trans p) => Set (Trans p)
               , pre :: Trans p -> n p
               , post :: Trans p -> n p
               , initial :: m p
               }

type PTNet = Net Int [] []
type PTMark = [Int]
type PTTrans = Trans Int

sublist :: Eq a => [a] -> [a] -> Bool             
sublist [] _ = True
sublist (x:xs) ys = if x `elem` ys
                    then sublist xs (List.delete x ys)
                    else False

enabled :: PTNet -> PTMark -> PTTrans -> Bool
enabled net@(Net {pre=pre}) marking =
  flip sublist marking . pre

fire :: PTNet -> PTMark -> PTTrans -> PTMark
fire net@(Net {pre=pre, post=post}) mark t =
  (mark List.\\ (pre t)) <> (post t) 


-- reuse types from StateSpace module

type SS = Gr PTMark PTTrans

--- How to pick an arbitrary M from the set Work?
reachabilityGraph :: PTNet -> ((), (NodeMap PTMark, SS))
reachabilityGraph net = run G.empty $ insMapNodeM (initial net) >> go (Set.singleton (initial net))
  where go work | Set.null work = return ()
                | otherwise     = do
          let m = (head . Set.toList) work 
              work' = Set.delete m work
          work'' <- F.foldrM (act net m) work' (trans net)
          go work'' 
  
act :: G.DynGraph g => 
       PTNet -> PTMark -> PTTrans -> (Set PTMark) -> NodeMapM PTMark PTTrans g (Set PTMark)
act net m t w =
  if enabled net m t
  then do 
     let m' = fire net m t
     present <- lookupNodeM m'
     w' <- case present of
                Just _ -> return w
                Nothing -> do
                  insMapNodeM m'
                  return (Set.insert m' w)
     insMapEdgeM (m,m',t)
     return w'
  else return w
  
                      
pn1 :: PTNet
pn1 = Net { places = Set.fromList [1,2,3,4]
          , trans  = Set.fromList [t1]
          , pre    = \(Trans x) -> case x of
               "t1" -> [1,2]
               _  -> []
          , post   = \(Trans x) -> case x of
               "t1" -> [3,4]
               _  -> []
          , initial = [1,2,1,2]
          } 
  where t1 = Trans "t1"
  
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
      
