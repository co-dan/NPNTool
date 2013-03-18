{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module OrdFunctor where
import qualified Data.Functor as F
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet


class OrdFunctor f where
  fmap :: (Ord a, Ord b) => (a -> b) -> f a -> f b
  
instance F.Functor f => OrdFunctor f where 
  fmap = F.fmap

instance OrdFunctor MultiSet where
  fmap = MSet.map

instance OrdFunctor Set where
  fmap = Set.map
