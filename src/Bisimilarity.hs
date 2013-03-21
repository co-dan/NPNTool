module Bisimilarity where

import NPNet
import PetriNet
import Control.Monad.State
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Foldable as F
  
newtype Rel a b = Rel [(a,b)]

member :: (Eq a, Eq b) => a -> b -> Rel a b -> Bool
member a b (Rel r) = (a,b) `elem` r

mBisimilarity :: Eq l => (PTNet, Labelling l) -> (PTNet, Labelling l) -> Bool
mBisimilarity = undefined

  
  
