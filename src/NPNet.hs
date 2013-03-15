module NPNet (
  Expr(..), vars, MSExpr(..),
  Labelling, SNet(..), 
  alphaTrail
  ) where

import PetriNet
import PTConstr
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.Foldable as F
import Data.Maybe (fromJust)
import Data.List (intersect)
import Control.Monad
import Control.Monad.State

data Expr v c = Var v
              | Const c
              | Plus (Expr v c) (Expr v c)
              deriving (Show,Eq,Ord)
                       
vars :: Expr v c -> [v]
vars e = (vars' e) []
  where vars' (Var v) = (v:)
        vars' (Const _) = id
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
alphaTrail n p = snd $ run (evalStateT (aTrail n) (Set.singleton p,Set.empty)) init
  where init = new { p = Set.singleton p, key = p+1 }
          

type ATrailBuilder a = StateT (Set PTPlace,Set PTPlace) PTConstrM a

aTrail :: Eq v => SNet l v c -> ATrailBuilder ()
aTrail n = do
  (rest, proc) <- get
  unless (Set.null (rest Set.\\ proc)) $ do
  let pCur = head $ Set.toList $ rest Set.\\ proc
      ppost = postP pCur (net n)
  forM_ ppost $ \t -> do
    let (MSExpr pre') = pre (net n) t
        (MSExpr post') = post (net n) t
        (expr,_) = fromJust $ F.find ((==pCur) . snd) pre'
        vs = vars expr
        ps = filterF (\(e',p') -> intersects vs (vars e')) post'
    forM_ ps $ \(e,p) -> do
      lift $ addP pCur p t
      addRest p
  addProc pCur
  aTrail n    

addRest :: PTPlace -> ATrailBuilder ()
addRest p = do
  (rest, proc) <- get
  put $ (p `Set.insert` rest, proc)

addProc :: PTPlace -> ATrailBuilder ()
addProc p = do
  (rest, proc) <- get
  put $ (rest, p `Set.insert` proc)
   
addP :: PTPlace -> PTPlace -> Trans -> PTConstrM PTPlace
addP p p' t = do
  insPlace p'
  let t' = Trans $ (show t) ++ (show p')
  arc p t'
  arc t' p'
  return p'

intersects :: (Eq a) => [a] -> [a] -> Bool   
intersects x y = (x `intersect` y) /= [] 
      
filterF :: F.Foldable f => (a -> Bool) -> f a -> [a]
filterF f = filter f . F.toList 
  

