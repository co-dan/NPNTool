{-# LANGUAGE Arrows #-}
module NPNTool.XMLReader where

import Control.Arrow
import Text.XML.HXT.Core
import NPNTool.PetriNet
import NPNTool.NPNet
import NPNTool.NPNConstr

type Label = String
type Variable = String

idToPlace :: String -> PTPlace
idToPlace = (+1) . read . tail . dropWhile (/= '.')

idToTrans :: String -> Trans
idToTrans = Trans . ("t" ++ ) . show . (+1) . read . tail . dropWhile (/= '.')

getPlace :: ArrowXml a => a XmlTree (NPNConstrM Variable Label ())
getPlace =   hasName "places"
         >>> arr (const (mkPlace >> return ()))

getTrans :: IOSArrow XmlTree (NPNConstrM Variable Label ())
getTrans =   hasName "transitions"
         >>> getAttrValue "name"
         >>> traceValue 1 id
         >>> arr (const (mkTrans >> return ()))

getInscr :: ArrowXml a => a XmlTree (Expr Variable Int)
getInscr =   hasName "inscription"
         >>> proc inscr -> do
           ms <- hasName "monoms" <<< getChildren -< inscr
           var <- getAttrValue "name" <<< hasName "var" <<< getChildren -< ms
           power <- arr read <<< getAttrValue "power" -< ms
           returnA -< exprMult (Var var) power
           
getArcPT :: ArrowXml a => a XmlTree (NPNConstrM Variable Label ())
getArcPT =   hasName "arcsPT"
         >>> proc a -> do
           p <- getAttrValue "inPlace" -< a
           t <- getAttrValue "outTransition" -< a
           inscr <- getInscr <<< hasName "inscription" <<< getChildren -< a
           returnA -< inT (idToPlace p) inscr (idToTrans t)

getArcTP :: ArrowXml a => a XmlTree (NPNConstrM Variable Label ())
getArcTP =   hasName "arcsTP"
         >>> proc a -> do
           t <- getAttrValue "inTransition" -< a
           p <- getAttrValue "outPlace" -< a
           inscr <- getInscr <<< hasName "inscription" <<< getChildren -< a
           returnA -< outT (idToTrans t) inscr (idToPlace p) 

getNetConstr :: FilePath -> IO [NPNConstrM Variable Label ()]
getNetConstr doc = runX $ configSysVars [withTrace 1]
                   >>> readDocument [] doc
                   >>> multi (isElem >>> (getPlace <+> getTrans
                                          <+> getArcPT <+> getArcTP))

runConstr :: FilePath -> IO (SNet Variable Label Int)
runConstr doc = do
  constr <- getNetConstr doc
  let (_,sn) = run (sequence constr) new
  return sn
