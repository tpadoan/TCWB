{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Model where

import Data.Aeson
import GHC.Generics
import qualified Data.Set as Set

-- Reads Petri nets from JSON files

data Place = Place {pid::Int} deriving (Show,Generic)
data TRANS = TRANS {tidT::Int, labelT::[Char], preT::[Place], postT::[Place]} deriving (Show,Generic)
data NET = NET {placesN::[Place], transN::[TRANS], initmarkN::[Place]} deriving (Show,Generic)
data Trans = Trans {tid::Int, label::[Char], pre::Set.Set Place, post::Set.Set Place} deriving (Show)
data Net = Net {places::Set.Set Place, trans::[Trans], initmark::Set.Set Place} deriving (Show)

instance FromJSON Place where
  parseJSON (Object o) = do
      parsepid <- o .: "id"
      return Place{pid=parsepid}
instance FromJSON TRANS where
  parseJSON (Object o) = do
      parsetidT <- o .: "id"
      parselabelT <- o .: "label"
      parsepreT <- o .: "pre"
      parsepostT <- o .: "post"
      return TRANS{tidT=parsetidT,labelT=parselabelT,preT=parsepreT,postT=parsepostT}
instance FromJSON NET where
  parseJSON (Object o) = do
      parseplacesN <- o .: "places"
      parsetransN <- o .: "transitions"
      parseinitmarkN <- o .: "initmarking"
      return NET{placesN=parseplacesN, transN=parsetransN, initmarkN=parseinitmarkN}

instance Eq Place where
  p1 == p2 = pid p1 == pid p2
instance Ord Place where
  compare p1 p2 = compare (pid p1) (pid p2)

instance Eq Trans where
  t1 == t2 = tid t1 == tid t2
instance Ord Trans where
  compare t1 t2 = compare (tid t1) (tid t2)

convertTrans :: TRANS -> Trans
convertTrans t = Trans {tid = (tidT t), label = (labelT t), pre = (Set.fromList (preT t)), post = (Set.fromList (postT t))}

convertNet :: Maybe NET -> Maybe Net
convertNet (Just n) =
  let net = Net {places = (Set.fromList (placesN n)), trans = (map (convertTrans) (transN n)), initmark = (Set.fromList (initmarkN n))}
  in if checkNetCorrect net
    then Just net
    else Nothing
convertNet Nothing = Nothing

extractNetFromJSON json = convertNet (decode json) :: Maybe Net

checkPreAndPost :: Set.Set Place -> Trans -> Bool -> Bool
checkPreAndPost p t b = (Set.isSubsetOf (pre t) p) && (Set.isSubsetOf (post t) p) && b

checkNetCorrect :: Net -> Bool
checkNetCorrect n = (foldr (checkPreAndPost (places n)) True (trans n)) && (Set.isSubsetOf (initmark n) (places n))

enables :: (Set.Set Place) -> Trans -> Bool
enables m t = Set.isSubsetOf (pre t) m

getEnabledTrans :: Net -> Set.Set Place -> [Trans]
getEnabledTrans n m = filter (enables m) (trans n)

-- before applying use 'enables m t' to check if 't' is enabled in 'm' or 'getEnabledTrans Net m'
execAndUpdate :: Trans -> Set.Set Place -> Set.Set Place
execAndUpdate t m = Set.union (Set.difference m (pre t)) (post t)

-- returns the Set of reachable states of the Net 'n' and its max branching
states :: Net -> (Set.Set (Set.Set Place), Int)
states n =
  let ts = getEnabledTrans n (initmark n)
  in statesAfterTrans n (initmark n) ((Set.singleton (initmark n)), (length ts)) ts

statesAfterTrans :: Net -> Set.Set Place -> (Set.Set (Set.Set Place), Int) -> [Trans] -> (Set.Set (Set.Set Place), Int)
statesAfterTrans n m (rs, b) (t:l) =
  let s = execAndUpdate t m
  in
    if Set.member s rs
      then statesAfterTrans n m (rs, b) l
      else
        let
          ts = getEnabledTrans n s
          b2 = length ts
        in
          if b2 > b
            then statesAfterTrans n m (statesAfterTrans n s ((Set.insert s rs), b2) ts) l
            else statesAfterTrans n m (statesAfterTrans n s ((Set.insert s rs), b) ts) l
statesAfterTrans n m (rs, b) [] = (rs, b)

-- prints a Set of states
showStates :: Set.Set (Set.Set Place) -> [Char]
showStates s = Set.foldr (\x -> \y -> "[" ++ showPlaceSet x ++ "]\n" ++ y) "" s

showPlaceSet :: Set.Set Place -> [Char]
showPlaceSet p =
  case Set.foldl (\x -> \y -> x ++ ", " ++ show (pid y)) "" p of
    ',':(' ':str) -> str
    str -> str
