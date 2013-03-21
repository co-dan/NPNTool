module Bisimilarity where

import NPNet
import PetriNet
import Control.Monad.State
import Control.Monad
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Foldable as F

import Debug.Trace

newtype Rel a b = Rel [(a,b)]

member :: (Eq a, Eq b) => a -> b -> Rel a b -> Bool
member a b (Rel r) = (a,b) `elem` r

mBisimilarity :: Eq l => (PTNet, Labelling l) -> (PTNet, Labelling l) -> Bool
mBisimilarity = undefined

bisim :: Eq l =>
         (PTNet, Labelling l) -> (PTNet, Labelling l) ->
         (PTMark, PTMark) -> StateT (Set (PTMark,PTMark)) Maybe Bool
bisim (pt1,l1) (pt2,l2) (m1,m2) = do
  r <- get
  trace ("Checking " ++ show (m1,m2)) $ return ()
  if S.member (m1,m2) r
    then return True
    else let ts1 = F.foldMap (\t -> if enabled pt1 m1 t then [t] else []) $ trans pt1
             ts2 = F.foldMap (\t -> if enabled pt2 m2 t then [t] else []) $ trans pt2
             ts1' = filter (isJust . l1) ts1
             ts2' = filter (isJust . l2) ts2
         in do
           trace ("Ts1' = " ++ show ts1') $ return ()
           trace ("Ts2' = " ++ show ts2') $ return ()
           put $ S.insert (m1,m2) r
           r <- get
           traceShow r $ return ()
           mapM_ (\t -> mapM_ (existBsim l1 l2 t) ts2') ts1'
           mapM_ (\t1 -> mapM_ (\t2 -> existBsim l2 l1 t2 t1) ts2') ts1'
           (return True)
  where
    existBsim l1 l2 t1 t2 = do
      guard (l1 t1 == l2 t2)
      bisim (pt1,l1) (pt2,l2) ((fire pt1 m1 t1), (fire pt2 m2 t2))

groupByLabel :: (Eq l) => Labelling l -> [Trans] -> [[Trans]]
groupByLabel _ []     = []
groupByLabel l (t:ts) = ts1:groupByLabel l ts
  where (ts1,ts2) = partition ((== l t) . l) ts
