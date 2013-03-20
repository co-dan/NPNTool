module PNAction where

import Control.Monad
import System.Random
import Data.Set (Set)
import qualified Data.Set as S
import PetriNet
import NPNet

data HANet m = HANet { ptnet :: PTNet, actions :: Trans -> m () }

execute :: HANet IO -> IO ()
execute h = go (initial (ptnet h))
  where 
    go mark = 
      let
        ms = step h mark
        l = length ms
      in
       unless (l == 0) $ do
         x <- randomRIO (0, l-1)
         m <- ms !! x
         go m     
       

  
step :: Monad m => HANet m -> PTMark -> [m PTMark]
step (HANet net actions) mark = 
  let 
    enabledTrans = do
      t <- S.toList $ trans net
      guard $ enabled net mark t
      return t
  in     
   map (\t -> actions t >> return (fire net mark t)) enabledTrans
       

  
