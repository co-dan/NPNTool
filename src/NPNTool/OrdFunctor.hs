{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module NPNTool.OrdFunctor where

import qualified Data.Functor as F
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet

newtype OrdF f a = OrdF { unOrdF :: f a }

class OrdFunctor f where
  fmap :: (Ord a, Ord b) => (a -> b) -> f a -> f b
  
instance F.Functor f => OrdFunctor (OrdF f) where 
  fmap f = OrdF . F.fmap f . unOrdF

instance OrdFunctor MultiSet where
  fmap = MSet.map
   
instance OrdFunctor Set where
  fmap = Set.map

