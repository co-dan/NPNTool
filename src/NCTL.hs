{-# LANGUAGE FlexibleInstances #-}
module NCTL where

import Data.Graph.Inductive

data NCTL =
  NCTLFalse
  | NCTLTrue
  | NCTLAtom (String, (Node -> Bool))
  | NCTLNot NCTL
  | NCTLOr NCTL NCTL
  | EX NCTL
  | EU NCTL NCTL
  | AU NCTL NCTL
  deriving (Show,Eq,Ord)

data NNCTL =
  NNCTLFalse
  | NNCTLTrue
  | NNCTLAtom (String, (Edge -> Bool))
  | NNCTLNot NNCTL
  | NNCTLOr NNCTL NNCTL
  | NEX NNCTL
  | NEU NNCTL NNCTL
  | NAU NNCTL NNCTL
    
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

