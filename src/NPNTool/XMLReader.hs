{-# LANGUAGE Arrows #-}
module NPNTool.XMLReader where

import Control.Arrow
import Text.XML.HXT.Core
import NPNTool.PetriNet
import NPNTool.NPNet
import NPNTool.NPNConstr
import NPNTool.PTConstr (PTConstrM)
import qualified NPNTool.PTConstr as PTC
import Control.Monad

type Label = Int
type Variable = String

idToPlace :: String -> PTPlace
idToPlace = read . tail

idToTrans :: String -> Trans
idToTrans = Trans . tail

getPlace :: ArrowXml a => a XmlTree (PTConstrM Label ())
getPlace =   hasName "places"
         >>> getAttrValue "id"
         >>> arr (PTC.insPlace . read)

getTrans :: IOSArrow XmlTree (PTConstrM Label ())
getTrans =   hasName "transitions"
         >>> hasAttr "synchronization"
         >>> proc t -> do
           sync <- getAttrValue "synchronization" -< t
           tid <- getAttrValue "id" -< t
           returnA -< PTC.label (Trans tid) (read (tail sync))

getInscr :: ArrowXml a => a XmlTree (Expr Variable Int)
getInscr =   hasName "inscription"
         >>> proc inscr -> do
           ms <- hasName "monoms" <<< getChildren -< inscr
           var <- getAttrValue "variable" -< ms
           -- power <- arr read <<< getAttrValue "power" -< ms
           -- returnA -< exprMult (Var var) power
           returnA -< Var var
           
getArcPT :: IOSArrow XmlTree (NPNConstrM Label Variable ())
getArcPT =   hasName "arcsPT"
         >>> proc a -> do
           p <- getAttrValue "inPlace" -< a
           t <- getAttrValue "outTransition" -< a
           inscr <- getInscr <<< hasName "inscription" <<< getChildren -< a
           returnA -< inT (idToPlace p) inscr (idToTrans t)

getArcPTPTC :: IOSArrow XmlTree (PTConstrM Label ())
getArcPTPTC =  hasName "arcsPT"
            >>> proc a -> do
              p <- getAttrValue "inPlace" -< a
              t <- getAttrValue "outTransition" -< a
              returnA -< PTC.inT (idToPlace p) (idToTrans t)
           
getArcTP :: ArrowXml a => a XmlTree (NPNConstrM Label Variable ())
getArcTP =   hasName "arcsTP"
         >>> proc a -> do
           t <- getAttrValue "inTransition" -< a
           p <- getAttrValue "outPlace" -< a
           inscr <- getInscr <<< hasName "inscription" <<< getChildren -< a
           returnA -< outT (idToTrans t) inscr (idToPlace p) 

getArcTPPTC :: ArrowXml a => a XmlTree (PTConstrM Label ())
getArcTPPTC =  hasName "arcsTP"
            >>> proc a -> do
              t <- getAttrValue "inTransition" -< a
              p <- getAttrValue "outPlace" -< a
              returnA -< PTC.outT (idToTrans t) (idToPlace p) 


sysNet :: IOSArrow XmlTree (NPNConstrM Label Variable ())
sysNet = multi (getPlaceNPN <+> getTransNPN <+> getArcPT <+> getArcTP)
         <<< getChildren <<< hasName "netSystem"
  where getPlaceNPN = arr liftPTC <<< getPlace
        getTransNPN = arr liftPTC <<< getTrans

elemMarking :: IOSArrow XmlTree (PTConstrM Label ())
elemMarking = multi $ hasName "map" >>> proc m -> do
  place <- getAttrValue "place" -< m
  weight <- deep (getAttrValue "weigth" <<< hasName "weight") -< m
  returnA -< replicateM_ (read weight) (PTC.mark (idToPlace place))

elemNet :: IOSArrow XmlTree (NPNConstrM Label Variable ())
elemNet = hasName "typeElementNet" >>> getChildren
          >>> proc en -> do
            traceValue 1 id -< "Hi"
            enPTC <- --deep (elemMarking <<< getChildren <<< hasName "elemNetMarkeds")
                      multi ((getPlace <+> getTrans <+> getArcPTPTC <+> getArcTPPTC)
                                <<< hasName "net") -< en
            tokens <- getAttrValue "id" <<< hasName "tokenNets" -< en
            returnA -< insElemNet (read tokens) enPTC 
  
getNetConstr :: FilePath -> IO [NPNConstrM Label Variable ()]
getNetConstr doc = runX $ configSysVars [withTrace 1]
                   >>> readDocument [] doc
                   >>> deep sysNet <+> multi elemNet

runConstr :: FilePath -> IO (NPNet Label Variable Int)
runConstr doc = do
  constr <- getNetConstr doc
  let (_,sn) = run (sequence constr) new
  return sn

npn = runConstr "./test/Drones.npnets"
