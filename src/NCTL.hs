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
    | EU NCTL
    | EG NCTL
    deriving (Show)

data NCTL' =
    NCTLFalse'
    | NCTLTrue'
    | NCTLAtom' (Int, String, (Node -> Bool))
    | NCTLNot' (Int, NCTL')
    | NCTLOr' (Int, NCTL', NCTL')
    | EX' (Int, NCTL')
    | EU' (Int, NCTL')
    | EG' (Int, NCTL')
    deriving (Show,Ord)

instance Eq  (Node -> Bool) where
  _ == _ = True
  
instance Ord (Node -> Bool) where
  compare _ _ = EQ

instance Eq NCTL' where
  NCTLFalse' == NCTLFalse' = True
  NCTLTrue'  == NCTLTrue'  = True
  NCTLAtom' (i,_,_) == NCTLAtom' (j,_,_) = i == j
  NCTLNot' (i,_) == NCTLNot' (j,_) = i == j
  NCTLOr' (i,_,_) == NCTLOr' (j,_,_) = i == j 
  EX' (i,_) == EX' (j,_) = i == j
  _ == _ = False

             
instance Show (Node -> Bool) where
  show _ = "pred"

