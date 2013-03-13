module NPNet where

import PetriNet
import PTConstr
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import Data.Monoid
import qualified Data.Foldable as F
import Data.Maybe (fromJust)
import Data.List (intersect)
import Control.Monad
import Control.Monad.State

import Debug.Trace

data Expr v c = Var v
              | Const c
              | Plus (Expr v c) (Expr v c)
              deriving (Show,Eq,Ord)
                       
vars :: Expr v c -> [v]
vars e = (vars' e) []
  where vars' (Var v) = (v:)
        vars' (Const c) = id
        vars' (Plus e1 e2) = vars' e1 . vars' e2

newtype MSExpr v c p = MSExpr (MultiSet (Expr v c, p))

instance F.Foldable (MSExpr v c) where 
  foldMap f (MSExpr ms) = F.foldMap (f . snd) ms

type Labelling l = Trans -> Either l ()
data SNet lab var con = 
  SNet { net :: Net PTPlace (MSExpr var con) MultiSet
       , elementNets :: [(PTNet, Labelling lab)] -- element nets together with
         -- transition labelling functions
       , label :: Trans -> Either lab ()
       , labels :: Set lab
         -- place typing function
       }

alphaTrail :: Eq v => SNet l v c -> PTPlace -> PTNet
alphaTrail n p = snd $ run (evalStateT (go n) (Set.singleton p,Set.empty)) init
  where init = new { p = Set.singleton 1, key = 2 }
          
go :: Eq v => SNet l v c -> StateT (Set PTPlace,Set PTPlace) PTConstrM [Int]
go n = do
  (work,seen) <- get
  let p = (head . Set.toList) work 
      work' = Set.delete p work
      ppost = postP p (net n)
  x <- forM ppost $ \t -> do
    let (MSExpr pre') = pre (net n) t
        (MSExpr post') = post (net n) t
        (expr,_) = fromJust $ F.find ((==p) . snd) pre'
        vs = vars expr
        ps = filterF (\(e',p') -> not (p' `Set.member` seen) && intersects vs (vars e')) post'
    forM ps $ \(e',p') -> do
      lift $ addP p t
      return p'
  put $ (work, p `Set.insert` seen)
  return $ concat x
  -- return $ go n 
   
addP :: PTPlace -> Trans -> PTConstrM PTPlace
addP p' t = do
  pNew <- mkPlace
  let t' = Trans $ (show t) ++ (show pNew)
  arc p' t'
  arc t' pNew
  return pNew

intersects :: (Eq a) => [a] -> [a] -> Bool   
intersects x y = (x `intersect` y) /= [] 
      
filterF :: F.Foldable f => (a -> Bool) -> f a -> [a]
filterF f = filter f . F.toList 

test :: PTConstrM ()      
test = traceStack "HELLO" (return ())
  

