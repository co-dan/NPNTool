module Main (main) where

import CTL
import PTTest

main = do
  print $ verifyPT pn1 formula
  print $ verifyPT pn2 formula
