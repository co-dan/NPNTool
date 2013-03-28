module NPNTool.Bisimilarity where

import NPNTool.NPNet
import NPNTool.PetriNet
import NPNTool.NodeMap

import Control.Monad.State
import Control.Monad.Reader
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Foldable as F

import Data.Graph.Inductive hiding (NodeMap)
import Data.Graph.Inductive.Query.DFS
import Data.Tree

isBisim :: Eq l => (PTNet, Labelling l) -> (PTNet, Labelling l) -> (PTMark, PTMark) -> Bool
isBisim (pn1,l1) (pn2,l2) (m1,m2) = isJust $ runStateT (bisim (pn1,l1) (pn2,l2) (m1,m2)) (S.empty)

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

findPath :: Eq l => (SS, Labelling l) -> PTNet -> NodeMap PTMark -> l -> PTMark -> [PTMark]
findPath (ss,ll) pt nm l from =
  (concatMap leaves $ findPath' (ss,ll) nm l from) \\ (growPath (pt,ll) [from])

findPath' :: Eq l => (SS, Labelling l) -> NodeMap PTMark -> l -> PTMark -> [Tree PTMark]
findPath' (ss,ll) nm l from = xdffWith (nextNode ll l) lab' [nodeFromLab from] ss
  where nodeFromLab :: PTMark -> Node
        nodeFromLab m = case lookupNode nm m of
          Just (n,_) -> n
          Nothing    -> error "Marking not reachable"
        

leaves :: Tree t -> [t]
leaves (Node x []) = [x]
leaves (Node _ ts) = concatMap leaves ts

context4l' :: Context a b -> Adj b 
context4l' (p,v,_,s) = s++filter ((==v).snd) p

nextNode :: Eq l => Labelling l -> l -> CFun PTMark PTTrans [Node]
nextNode lab l =
  map snd . filter (maybe True (==l) . lab . fst) . context4l'

growPath :: (PTNet, Labelling l) -> [PTMark] -> [PTMark]
growPath (pn,lab) ms = ms ++ 
  concatMap (\m -> map (fire pn m) (filter (shouldFire m) (S.toList (trans pn)))) ms
  where shouldFire m t = enabled pn m t && isNothing (lab t)

-- isMBisim :: Eq l =>
--             (PTNet, Labelling l) -> (PTNet, Labelling l) -> Maybe (Bool, Set (PTMark, PTMark))
isMBisim (pt1,l1) (pt2,l2) =
  runReaderT 
  (runStateT (mBisim (pt1,l1) (pt2,l2) (initial pt1, initial pt2)) S.empty)
  (swap . reachabilityGraph' $ pt1, swap . reachabilityGraph' $ pt2)
  where  swap (a,b) = (b,a)
         
type SM = (SS, NodeMap PTMark)

mBisim :: (Show l, Eq l) =>
          (PTNet, Labelling l) -> (PTNet, Labelling l) ->
          (PTMark, PTMark) -> StateT (Set (PTMark,PTMark)) (ReaderT (SM,SM) Maybe) Bool
mBisim (pt1,l1) (pt2,l2) (m1,m2) = do
  r <- get
  if S.member (m1,m2) r
    then return True
    else let ts1 = F.foldMap (\t -> guard (enabled pt1 m1 t) >> return t) $ trans pt1
             ts1' = filter (isJust . l1) ts1
             ts2 = F.foldMap (\t -> guard (enabled pt2 m2 t) >> return t) $ trans pt2
             ts2' = filter (isJust . l2) ts2
         in do
           put $ S.insert (m1,m2) r
           mapM_ sim1 ts1'
           mapM_ sim2 ts2'
           return True
  where
    sim1 t1 = do
      let l = l1 t1
          m1' = fire pt1 m1 t1
      ((ss1,nm1),(ss2,nm2)) <- ask
      let nodes = growPath (pt2,l2) $ findPath (ss2,l2) pt2 nm2 (fromJust l) m2
      guard (not (null nodes)) -- there exist a path ==> m2'
      mapM_ (\m2' -> mBisim (pt1,l1) (pt2,l2) (m1',m2')) nodes -- all of them are m-bisimilar
    sim2 t2 = do
      let l = l2 t2
          m2' = fire pt2 m2 t2
      ((ss1,nm1),(ss2,nm2)) <- ask
      let nodes = growPath (pt1,l1) $ findPath (ss1,l1) pt1 nm1 (fromJust l) m1
      guard (not (null nodes)) -- there exist a path ==> m1'
      mapM_ (\m1' -> mBisim (pt1,l1) (pt2,l2) (m1',m2')) nodes -- all of them are m-bisimilar
      