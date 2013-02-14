{-# LANGUAGE Rank2Types, FlexibleContexts #-}
module PetriNet where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Monoid
import Data.Graph.Inductive (Gr)
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.NodeMap
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

-- reachabilityGraph :: PTNet -> (a, (NodeMap PTMark, Gr PTMark ()))
-- reachabilityGraph net = go (Set.singleton (initial net)) (insMapNodeM (initial net) >> return ())
--   where go work | Set.null work = return ()
--                 | otherwise     = do
--           let m = (head . Set.toList) work 
--               work' = Set.delete m work
--               act a t = 
--                 if enabled net m t
--                 then 
--           insMapNodeM m
--           F.foldrM act (trans m)
  
  
  
              
    
--- How to pick an arbitrary M from the set Work?
reachabilitySet :: PTNet -> ((), (NodeMap PTMark, Gr PTMark ()))
reachabilitySet net = go (Set.singleton (initial net)) (insMapNodeM (initial net) >> return ())
  where go work acc | Set.null work = run G.empty acc
                    | otherwise = 
                      let m = (head . Set.toList) work 
                          work' = Set.delete m work
                          m' = Set.map (fire net m) (Set.filter (enabled net m) (trans net))
                          act t = 
                            if enabled net m t
                            then do
                              let m' = fire net m t
                              insMapNodeM m'
                              return ()
--                              insMapEdgeM (m,m',())
                            else return ()
                          acc' = acc >> F.mapM_ act (trans net)
                      in go (Set.union work' m') acc'
        
                      
pn1 :: PTNet
pn1 = Net { places = Set.fromList [1,2,3,4]
          , trans  = Set.fromList [t1]
          , pre    = \x -> case x of
               t1 -> [1,2]
               _  -> []
          , post   = \x -> case x of
               t1 -> [3,4]
               _  -> []
          , initial = [1,2,1,2]
          } 
  where t1 = Trans "t1"
  
      
