module NPNTool.AlphaTrail (
  alphaTrail
  ) where

import NPNTool.PetriNet
import NPNTool.NPNet
import NPNTool.PTConstr
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.List (intersect)
import Control.Monad
import Control.Monad.State
import qualified Data.Foldable as F

-- | Generate an alpha-trail net for a specific place in a NPNet
alphaTrail :: Eq v => NPNet l v c -> PTPlace -> (PTNet, Labelling l)
alphaTrail n p = sndthrd $
                 runL aTrailCode init
  where init = new { p = M.singleton p 1, key = p+1 }
        aTrailCode = evalStateT (aTrail n) (Set.singleton p,Set.empty)
        sndthrd (_,x,y) = (x,y)
          

type ATrailBuilder l a = StateT (Set PTPlace,Set PTPlace) (PTConstrM l) a

aTrail :: Eq v => NPNet l v c -> ATrailBuilder l ()
aTrail n = do
  (rest, proc) <- get
  unless (Set.null (rest Set.\\ proc)) $ do
  let pCur = head $ Set.toList $ rest Set.\\ proc
      ppost = postP pCur (net n)
  forM_ ppost $ \t -> do
    let (SArc pre') = pre (net n) t
        (SArc post') = post (net n) t
        (expr,_) = fromJust $ F.find ((==pCur) . snd) pre'
        vs = vars expr
        ps = filterF (\(e',p') -> intersects vs (vars e')) post'
    forM_ ps $ \(e,p) -> do
      t' <- lift $ addP pCur p
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
   
addP :: PTPlace -> PTPlace -> PTConstrM l Trans
addP p p' = do
  insPlace p'
  t' <- mkTrans
  arc p t'
  arc t' p'
  return t'

intersects :: (Eq a) => [a] -> [a] -> Bool   
intersects x y = (x `intersect` y) /= [] 
      
filterF :: F.Foldable f => (a -> Bool) -> f a -> [a]
filterF f = filter f . F.toList 
