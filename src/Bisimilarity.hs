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

isBisim (pn1,l1) (pn2,l2) (m1,m2)  = isJust $ runStateT (bisim (pn1,l1) (pn2,l2) (m1,m2)) (S.empty)         

bisim :: Eq l =>
         (PTNet, Labelling l) -> (PTNet, Labelling l) ->
         (PTMark, PTMark) -> StateT (Set (PTMark,PTMark)) Maybe Bool
bisim (pt1,l1) (pt2,l2) (m1,m2) = do
  r <- get
  if S.member (m1,m2) r
    then return True
    else let ts1 = F.foldMap (\t -> guard (enabled pt1 m1 t) >> return t) $ trans pt1
             ts2 = F.foldMap (\t -> guard (enabled pt2 m2 t) >> return t) $ trans pt2
             (ts1',silentTs1) = partition (isJust . l1) ts1
             (ts2',silentTs2) = partition (isJust . l2) ts2
         in do
           put $ S.insert (m1,m2) r
           mapM_ (\t1 -> msum $ map (existBsim l1 l2 t1) ts2') ts1'
           mapM_ (\t1 -> msum $ map (\t2 -> existBsim l2 l1 t2 t1) ts1') ts2'
           return True
  where
    existBsim l1 l2 t1 t2 = do
      guard (l1 t1 == l2 t2)
      bisim (pt1,l1) (pt2,l2) ((fire pt1 m1 t1), (fire pt2 m2 t2))

groupByLabel :: (Eq l) => Labelling l -> [Trans] -> [[Trans]]
groupByLabel _ []     = []
groupByLabel l (t:ts) = ts1:groupByLabel l ts
  where (ts1,ts2) = partition ((== l t) . l) ts
