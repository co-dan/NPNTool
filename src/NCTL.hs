{-# LANGUAGE FlexibleInstances #-}
module NCTL where

import Data.Graph.Inductive

data NCTL =
  NCTLFalse
  | NCTLTrue
  | NCTLAtom (String, (Node -> Bool))
  | NCTLTrans (String, (Edge -> Bool))
  | NCTLNot NCTL
  | NCTLOr NCTL NCTL
  | EX NCTL
  | EU NCTL NCTL
  | AU NCTL NCTL
  | NMod NCTL
  deriving (Show,Eq,Ord)
    
-- | Derived formulae           

ef f = EU NCTLTrue f
af f = AU NCTLTrue f
eg f = NCTLNot $ af (NCTLNot f)
ag f = NCTLNot $ ef (NCTLNot f)             


instance Eq  (Node -> Bool) where
  _ == _ = True
  
instance Ord (Node -> Bool) where
  compare _ _ = EQ
  
instance Show (Node -> Bool) where
  show _ = "pred"

instance Eq  (Edge -> Bool) where
  _ == _ = True
  
instance Ord (Edge -> Bool) where
  compare _ _ = EQ
  
instance Show (Edge -> Bool) where
  show _ = "epred"
