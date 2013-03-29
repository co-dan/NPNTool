module NPNTool.XMLReader where

import Control.Arrow
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.Pickle
import NPNTool.PetriNet
import NPNTool.PTConstr

import NPNTool.Graphviz

newtype Place = Place { placeName :: String }
                
xpPlace :: PU Place
xpPlace = xpElem "places" $
          xpWrap (Place, placeName) $
          xpAttr "name" xpText

xpTrans :: PU Trans          
xpTrans = xpElem "transitions" $
          xpWrap (Trans, name) $
          xpAttr "name" xpText


idToPlace :: String -> PTPlace
idToPlace = (+1) . read . tail . dropWhile (/= '.')

idToTrans :: String -> Trans
idToTrans = Trans . ("t" ++ ) . show . (+1) . read . tail . dropWhile (/= '.')

doc = "/Users/dan/projects/nctlmodelchecker/test/My.npnets"

getPlace :: ArrowXml a => a XmlTree (PTConstrM String ())
getPlace =   hasName "places"
--         >>> getAttrValue "name"
         >>> arr (const (mkPlace >> return ()))

getTrans :: IOSArrow XmlTree (PTConstrM String ())
getTrans =   hasName "transitions"
--         >>> getAttrValue "name"
         >>> traceMsg 1 "getTrans"
         >>> arr (const (mkTrans >> return ()))

getArcPT :: ArrowXml a => a XmlTree (PTConstrM String ())
getArcPT =   hasName "arcsPT"
         >>> getAttrValue "inPlace" &&& getAttrValue "outTransition"
         >>> arr (\(p,t) -> inT (idToPlace p) (idToTrans t))

getArcTP :: ArrowXml a => a XmlTree (PTConstrM String ())
getArcTP =   hasName "arcsTP"
         >>> getAttrValue "inTransition" &&& getAttrValue "outPlace" 
         >>> arr (\(t,p) -> outT (idToTrans t) (idToPlace p))

getNet :: IO [PTConstrM String ()]
getNet = runX $ configSysVars [withTrace 1]
         >>> readDocument [] doc
         >>> multi (isElem >>> (getPlace <+> getTrans
                                <+> getArcPT <+> getArcTP))

runConstr :: IO PTNet
runConstr = do
  constr <- getNet
  let (_,pt) = run (sequence constr) new
  return pt
