module NPNTool.AlphaTrail (
  alphaTrail
  ) where

import NPNTool.PetriNet
import NPNTool.NPNet
import NPNTool.PTConstr
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.List (intersect)
import Control.Monad
import Control.Monad.State
import qualified Data.Foldable as F

-- | Generate an alpha-trail net for a specific place in a SNet
alphaTrail :: Eq v => SNet l v c -> PTPlace -> PTNet
alphaTrail n p = snd $ run (evalStateT (aTrail n) (Set.singleton p,Set.empty)) init
  where init = new { p = Set.singleton p, key = p+1 }
          

type ATrailBuilder l a = StateT (Set PTPlace,Set PTPlace) (PTConstrM l) a

aTrail :: Eq v => SNet l v c -> ATrailBuilder l ()
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
      t' <- lift $ addP pCur p t
      case labelling n t of
        Nothing -> return ()
        Just l  -> lift $ label t' l
      addRest p
  addProc pCur
  aTrail n    

addRest :: PTPlace -> ATrailBuilder l ()
addRest p = do
  (rest, proc) <- get
  put (p `Set.insert` rest, proc)

addProc :: PTPlace -> ATrailBuilder l ()
addProc p = do
  (rest, proc) <- get
  put (rest, p `Set.insert` proc)
   
addP :: PTPlace -> PTPlace -> Trans -> PTConstrM l Trans
addP p p' t = do
  insPlace p'
  let t' = Trans $ show t ++ show p'
  arc p t'
  arc t' p'
  return t'

intersects :: (Eq a) => [a] -> [a] -> Bool   
intersects x y = (x `intersect` y) /= [] 
      
filterF :: F.Foldable f => (a -> Bool) -> f a -> [a]
filterF f = filter f . F.toList 
