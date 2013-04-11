module NPNTool.DFSM where

import NPNTool.NPNet
import NPNTool.PetriNet
import NPNTool.NodeMap

import Control.Arrow
import Control.Monad.State
import Control.Monad.Reader
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Foldable as F

import Data.Graph.Inductive hiding (NodeMap)
import Data.Tree

-- generalized depth-first forest
-- 
type PCFun a b c = Node -> CFun a b c 
xdfWithP :: Graph gr => CFun a b [Node] -> PCFun a b c -> [Node] -> gr a b -> ([Tree c], gr a b)
xdfWithP _ _ []     g             = ([],g)
xdfWithP _ _ _      g | isEmpty g = ([],g)
xdfWithP d f (v:vs) g = case match v g of
                         (Nothing,g1) -> xdfWithP d f vs g1 
                         (Just c,g1)  -> (Node (f v c) ts:ts',g3)
                           where (ts,g2)  = xdfWithP' d f v (d c) g1
                                 (ts',g3) = xdfWithP d f vs g2 

xdfWithP' :: Graph gr => CFun a b [Node] -> PCFun a b c -> Node -> [Node] -> gr a b -> ([Tree c], gr a b)
xdfWithP' _ _ _ []     g             = ([],g)
xdfWithP' _ _ _ _      g | isEmpty g = ([],g)
xdfWithP' d f s (v:vs) g = case match v g of
                            (Nothing,g1) -> xdfWithP' d f s vs g1 
                            (Just c,g1)  -> (Node (f s c) ts:ts',g3)
                              where (ts,g2)  = xdfWithP' d f v (d c) g1
                                    (ts',g3) = xdfWithP' d f s vs g2 



xdffWithP :: Graph gr => CFun a b [Node] -> PCFun a b c -> [Node] -> gr a b -> [Tree c]
xdffWithP d f vs g = fst (xdfWithP d f vs g)
