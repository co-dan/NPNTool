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

import Control.Monad.Writer

import qualified Data.IntMap as IM
  
type Label = Int
type Variable = String

idToPlace :: String -> PTPlace
idToPlace = read . tail

idToTrans :: String -> Trans
idToTrans = Trans . tail

idToElemNet :: String -> ElemNetId
idToElemNet = ElemNetId . read . tail

getPlace :: IOSArrow XmlTree (PTConstrM Label ())
getPlace =   hasName "places"
         >>> getAttrValue "id"
         >>> arr (PTC.insPlace . read)
         
getTrans :: IOSArrow XmlTree (PTConstrM Label ())
getTrans =   hasName "transitions"
         >>> hasAttr "synchronization"
         >>> listA (proc t -> do
           sync <- getAttrValue "synchronization" -< t
           tid <- getAttrValue "id" -< t
           returnA -< PTC.label (Trans tid) (read (tail sync)))
         >>> arr sequence_
         
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
         >>> (proc a -> do
           p <- getAttrValue "inPlace" -< a
           t <- getAttrValue "outTransition" -< a
           inscr <- getInscr <<< hasName "inscription" <<< getChildren -< a
           returnA -< inT (idToPlace p) inscr (idToTrans t))
         >. sequence_

getArcPTPTC :: IOSArrow XmlTree (PTConstrM Label ())
getArcPTPTC =  hasName "arcsPT"
            >>> (proc a -> do
              p <- getAttrValue "inPlace" -< a
              t <- getAttrValue "outTransition" -< a
              returnA -< PTC.inT (idToPlace p) (idToTrans t))
            >. sequence_
           
getArcTP :: ArrowXml a => a XmlTree (NPNConstrM Label Variable ())
getArcTP =   hasName "arcsTP"
         >>> (proc a -> do
           t <- getAttrValue "inTransition" -< a
           p <- getAttrValue "outPlace" -< a
           inscr <- getInscr <<< hasName "inscription" <<< getChildren -< a
           returnA -< outT (idToTrans t) inscr (idToPlace p))
         >. sequence_

getArcTPPTC :: IOSArrow XmlTree (PTConstrM Label ())
getArcTPPTC =  hasName "arcsTP"
            >>> (proc a -> do
              t <- getAttrValue "inTransition" -< a
              p <- getAttrValue "outPlace" -< a
              returnA -< PTC.outT (idToTrans t) (idToPlace p))
            >. sequence_


sysNet :: IOSArrow XmlTree (NPNConstrM Label Variable ())
sysNet = hasName "netSystem" 
     >>> listA (getChildren 
                >>> getPlaceNPN
                <+> getTransNPN 
                <+> getArcPT
                <+> getArcTP)
     >>> arr sequence_
  where getPlaceNPN = arr liftPTC <<< getPlace
        getTransNPN = arr liftPTC <<< getTrans

sysNetMarking :: IOSArrow XmlTree (NPNConstrM Label Variable ())
sysNetMarking = getChildren >>> hasName "marking"
            >>> (getChildren >>>    
                 hasName "map" >>> proc m -> do
                   pl <- getAttrValue "place" -< m
                   marking <- getChildren <<< hasName "marking" <<< getChildren -< m
                   t <- getAttrValue "token" <<< hasName "weight" -< marking
                   w <- getAttrValue "weight" <<< hasName "weight"  -< marking
                   returnA -< replicateM_ (read w) (mark (idToPlace pl) (Right (idToElemNet t))))
            >. sequence_

elemMarking :: IOSArrow XmlTree (PTConstrM Label ())
elemMarking = hasName "marking"
          >>> (getChildren >>>
               hasName "map" >>> proc m -> do
                 place <- getAttrValue "place" -< m
                 weight <- getAttrValue "weight" <<< deep (hasName "weight") -< m
                 traceValue 1 show -< weight
                 returnA -< replicateM_ (read weight) (PTC.mark (idToPlace place)))
          >. sequence_

elemToken :: IOSArrow XmlTree Int
elemToken = arr read <<< traceValue 1 id <<< getAttrValue "id" <<< hasName "tokenNets" 

elemNetPTC :: IOSArrow XmlTree (PTConstrM Label ())
elemNetPTC = hasName "net" 
         >>> listA (getChildren 
                    >>> getPlace 
                    <+> getTrans 
                    <+> getArcPTPTC
                    <+> getArcTPPTC)
         >>> arr sequence_

elemNet :: IOSArrow XmlTree (NPNConstrM Label Variable ())
elemNet = hasName "typeElementNet"
      >>> proc en -> do
        enPTC <- getChildren >>> elemNetPTC -< en
        enMark <- getChildren >>> deep elemMarking -< en
        t <- getChildren >>> elemToken -< en
        returnA -< insElemNet t (enPTC >> enMark)
  
getNetConstr :: FilePath -> IO [NPNConstrM Label Variable ()]
getNetConstr doc = runX $ configSysVars [withTrace 1]
                   >>> readDocument [] doc
                   -- >>> listA (deep elemNet)
                   -- >>> arr sequence_
                   >>> deep elemNet <+> deep sysNet <+> deep sysNetMarking

runConstr :: FilePath -> IO (NPNet Label Variable Int)
runConstr doc = do
  constr <- getNetConstr doc
  let (_,sn) = run (sequence constr) new
--  return . snd $ PTC.run (sequence constr) PTC.new
  return sn
