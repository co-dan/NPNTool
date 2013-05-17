{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import NPNTool.CTL
import NPNTool.Bisimilarity
import NPNTool.Liveness
import NPNTool.AlphaTrail
import NPNTool.PetriNet
import NPNTool.Graphviz
import qualified Data.Map as M
import qualified Data.IntMap as IM
import NPNTool.XMLReader
import NPNTool.NPNet
import Control.Monad
import Data.Maybe
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import System.Environment
  
main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  if (length args /= 1)
    then putStrLn $ "Usage: " ++ progName ++ " <xml-file>"
    else do
         result <- runMaybeT $ test (head args)
         print (isJust result)


testLive :: (Ord (m1 p), Ord p, Ord t, DynNet (Net p t n m1) p t m1) => Net p t n m1 -> MaybeT IO ()
testLive n = unless (isLive (reachabilityGraph n) n) $ do
  lift (putStrLn "One of the nets is not live")
  mzero
             

test :: FilePath -> MaybeT IO ()
test fp = do
  npn <- lift $ runConstr fp
  testLive (net npn)
  forM_ (M.toList $ unMark (initial (net npn))) $ \(p,m) -> do
    forM_ m $ \x -> do
      case x of
        Left _ -> return ()
        Right (ElemNetId id) -> do
          let (en,enLab,enMark) = (elementNets npn) IM.! id
          testLive (en { initial = enMark })
          when (isNothing (isMBisim (alphaTrail npn p) (en,enLab))) $ do
            lift . putStrLn $ "Net " ++ show id ++ " is not m-bisimilar to its alpha-trail net"
            mzero
