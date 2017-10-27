module Logic where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

-- Parser of Lhp formulae
-- Lexical analyser

data B = T | F
data F = Nu | Mu
data Token = Sym Char | Boo B | Fix F | Id [Char]
data VAR = VAR Int
data ACT = ACT [Char]
data LHP = BOO B | PROP (Int,[VAR]) | AND (LHP,LHP) | OR (LHP,LHP) | DIA ([VAR],[VAR],ACT,VAR,LHP) | BOX ([VAR],[VAR],ACT,VAR,LHP) | NU (LHP,LHP) | MU (LHP,LHP)

instance Eq B where
  T == T = True
  F == F = True
  _ == _ = False
instance Ord B where
  compare T F = GT
  compare F T = LT
  compare _ _ = EQ

instance Eq VAR where
  VAR v1 == VAR v2 = v1 == v2
instance Ord VAR where
  compare (VAR v1) (VAR v2) = compare v1 v2
  
instance Eq ACT where
  ACT a1 == ACT a2 = a1 == a2
instance Ord ACT where
  compare (ACT a1) (ACT a2) = compare a1 a2

instance Eq LHP where
  BOO b1 == BOO b2 = b1 == b2
  PROP (p1,vs1) == PROP (p2,vs2) = p1 == p2 && vs1 == vs2
  AND (f1,g1) == AND (f2,g2) = f1 == f2 && g1 == g2 -- ignoring case (f1 == g2 && g1 == f2)
  OR (f1,g1) == OR (f2,g2) = f1 == f2 && g1 == g2   -- ignoring case (f1 == g2 && g1 == f2)
  DIA (vs1,us1,a1,v1,f1) == DIA (vs2,us2,a2,v2,f2) = vs1 == vs2 && us1 == us2 && a1 == a2 && v1 == v2 && f1 == f2
  BOX (vs1,us1,a1,v1,f1) == BOX (vs2,us2,a2,v2,f2) = vs1 == vs2 && us1 == us2 && a1 == a2 && v1 == v2 && f1 == f2
  NU (p1,f1) == NU (p2,f2) = p1 == p2 && f1 == f2
  MU (p1,f1) == MU (p2,f2) = p1 == p2 && f1 == f2
  _ == _ = False

instance Ord LHP where
  compare (BOO b1) (BOO b2) = compare b1 b2
  compare (BOO b) _ = LT
  compare _ (BOO b) = GT
  compare (PROP (p1,vs1)) (PROP (p2,vs2)) =
    case compare p1 p2 of
      GT -> GT
      LT -> LT
      EQ -> compare vs1 vs2
  compare (PROP (p,vs)) _ = LT
  compare _ (PROP (p,vs)) = GT
  compare (AND (f1,g1)) (AND (f2,g2)) =
    case compare f1 f2 of
      GT -> GT
      LT -> LT
      EQ -> compare g1 g2
  compare (AND (f,g)) _ = LT
  compare _ (AND (f,g)) = GT
  compare (OR (f1,g1)) (OR (f2,g2)) =
    case compare f1 f2 of
      GT -> GT
      LT -> LT
      EQ -> compare g1 g2
  compare (OR (f,g)) _ = LT
  compare _ (OR (f,g)) = GT
  compare (DIA (vs1,us1,a1,v1,f1)) (DIA (vs2,us2,a2,v2,f2)) =
    case compare vs1 vs2 of
      GT -> GT
      LT -> LT
      EQ ->
        case compare us1 us2 of
          GT -> GT
          LT -> LT
          EQ ->
            case compare a1 a2 of
              GT -> GT
              LT -> LT
              EQ ->
                case compare v1 v2 of
                  GT -> GT
                  LT -> LT
                  EQ -> compare f1 f2
  compare (DIA (vs,us,a,v,f)) _ = LT
  compare _ (DIA (vs,us,a,v,f)) = GT
  compare (BOX (vs1,us1,a1,v1,f1)) (BOX (vs2,us2,a2,v2,f2)) =
    case compare vs1 vs2 of
      GT -> GT
      LT -> LT
      EQ ->
        case compare us1 us2 of
          GT -> GT
          LT -> LT
          EQ ->
            case compare a1 a2 of
              GT -> GT
              LT -> LT
              EQ ->
                case compare v1 v2 of
                  GT -> GT
                  LT -> LT
                  EQ -> compare f1 f2
  compare (BOX (vs,us,a,v,f)) _ = LT
  compare _ (BOX (vs,us,a,v,f)) = GT
  compare (NU (p1,f1)) (NU (p2,f2)) =
    case compare p1 p2 of
      GT -> GT
      LT -> LT
      EQ -> compare f1 f2
  compare (NU (p,f)) _ = LT
  compare _ (NU (p,f)) = GT
  compare (MU (p1,f1)) (MU (p2,f2)) =
    case compare p1 p2 of
      GT -> GT
      LT -> LT
      EQ -> compare f1 f2

instance Show Token where
  show (Sym '.') = "\'.\'"
  show (Sym ',') = "\',\'"
  show (Sym c) = [c]
  show (Boo T) = "T"
  show (Boo F) = "F"
  show (Fix Nu) = "nu"
  show (Fix Mu) = "mu"
  show (Id s) = "id "++s

showPrefixedSeq :: Show a => [a] -> [Char] -> [Char]
showPrefixedSeq (a:[]) prefix = prefix++show a
showPrefixedSeq (a:l) prefix = prefix++show a++" "++showPrefixedSeq l prefix
showPrefixedSeq [] _ = ""

instance Show VAR where
  show (VAR s) = show s
instance Show ACT where
  show (ACT s) = s
instance Show LHP where
  show (PROP (p,vs)) = show p++"("++showPrefixedSeq vs ""++")"
  show (BOO T) = "T"
  show (BOO F) = "F"
  show (AND (f,g)) = "AND("++show f++", "++show g++")"
  show (OR (f,g)) = "OR("++show f++", "++show g++")"
  show (DIA (vs,us,a,v,f)) = "DIA({"++showPrefixedSeq vs ""++" "++showPrefixedSeq us "!"++" < "++show a++" "++show v++"}, "++show f++")"
  show (BOX (vs,us,a,v,f)) = "BOX(["++showPrefixedSeq vs ""++" "++showPrefixedSeq us "!"++" < "++show a++" "++show v++"], "++show f++")"
  show (NU (p,f)) = "NU("++show p++", "++show f++")"
  show (MU (p,f)) = "MU("++show p++", "++show f++")"

isAlphaChar :: Char -> Bool
isAlphaChar c
  | (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') = True
  | otherwise = False

isIdChar :: Char -> Bool
isIdChar c
  | (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') = True
  | c >= '0' && c <= '9' = True
  | otherwise = False

isSym :: Char -> Bool
isSym c
  | c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}' || c == '|' || c == '&' || c == '.' || c == '<' || c == '!' = True
  | otherwise = False

isSpace :: Char -> Bool
isSpace c
  | c == ' ' || c == '\t' || c == '\n' || c == '\f' || c == '\r' = True
  | otherwise = False

extractWord :: [Char] -> Token
extractWord "T" = Boo T
extractWord "F" = Boo F
extractWord "nu" = Fix Nu
extractWord "mu" = Fix Mu
extractWord s = Id s

idStr :: ([Char],[Char]) -> (Maybe Token,[Char])
idStr ([],r) = (Nothing,[])
idStr (c:t,r)
  | isIdChar c = idStr (t,r++[c])
  | otherwise = (Just (extractWord r),c:t)

lexi :: [Char] -> Maybe [Token]
lexi [] = Nothing
lexi (c:t)
  | c == '$' = Just [Sym '$']
  | isSpace c = lexi t
  | isSym c =
    case lexi t of
      Just l -> Just (Sym c : l)
      _ -> Nothing
  | c == '_' =
    case lexi t of
      Just l -> Just (Id [c] : l)
      _ -> Nothing
  | isAlphaChar c =
    let (mbtok,s) = idStr (c:t,[])
    in case mbtok of
      Nothing -> Nothing
      Just tok ->
        case lexi s of
          Just l -> Just (tok : l)
          _ -> Nothing
  | otherwise = Nothing


-- Syntactic analyser

vars :: Map.Map [Char] Int -> Maybe [Token] -> Maybe ([VAR],[Token])
vars mv (Just ((Id v):l)) =
  case Map.lookup v mv of
    Nothing -> Nothing
    Just n ->
      case vars mv (Just l) of
        Nothing -> Nothing
        Just (vs,r) -> Just ((VAR n):vs,r)
vars _ (Just l) = Just ([],l)
vars _ Nothing = Nothing

depend :: Map.Map [Char] Int -> Maybe [Token] -> Maybe ([VAR],[VAR],[Token])
depend mv (Just ((Id v):l)) =
  case Map.lookup v mv of
    Nothing -> Nothing
    Just n ->
      case depend mv (Just l) of
        Nothing -> Nothing
        Just (vs,us,r) -> Just ((VAR n):vs,us,r)
depend mv (Just ((Sym '!'):((Id v):l))) =
  case Map.lookup v mv of
    Nothing -> Nothing
    Just n ->
      case depend mv (Just l) of
        Nothing -> Nothing
        Just (vs,us,r) -> Just (vs,(VAR n):us,r)
depend _ (Just l) = Just ([],[],l)
depend _ Nothing = Nothing

modf :: Map.Map [Char] Int -> Int -> Map.Map [Char] Int -> Int -> Maybe [Token] -> (Maybe (LHP,[Token]), Int)
modf mv kv mp kp (Just ((Sym '{'):((Id a):((Id v):((Sym '}'):l))))) =
  case Map.lookup v mv of
    Just n ->
      case modf mv kv mp kp (Just l) of
        (Just (f,r2), kpNew) -> (Just (DIA ([],[],ACT a,VAR n,f),r2), kpNew)
        (_,_) -> (Nothing, kp)
    Nothing ->
      case modf (Map.insert v kv mv) (kv+1) mp kp (Just l) of
        (Just (f,r2), kpNew) -> (Just (DIA ([],[],ACT a,VAR kv,f),r2), kpNew)
        (_,_) -> (Nothing, kp)
modf mv kv mp kp (Just ((Sym '{'):l)) =
  case depend mv (Just l) of
    Just (vs,us,(Sym '<'):((Id a):((Id v):((Sym '}'):r)))) ->
      case Map.lookup v mv of
        Just n ->
          case modf mv kv mp kp (Just r) of
            (Just (f,r2), kpNew) -> (Just (DIA (vs,us,ACT a,VAR n,f),r2), kpNew)
            (_,_) -> (Nothing, kp)
        Nothing ->
          case modf (Map.insert v kv mv) (kv+1) mp kp (Just r) of
            (Just (f,r2), kpNew) -> (Just (DIA (vs,us,ACT a,VAR kv,f),r2), kpNew)
            (_,_) -> (Nothing, kp)
    _ -> (Nothing, kp)
modf mv kv mp kp (Just ((Sym '['):((Id a):((Id v):((Sym ']'):l))))) =
  case Map.lookup v mv of
    Just n ->
      case modf mv kv mp kp (Just l) of
        (Just (f,r2), kpNew) -> (Just (BOX ([],[],ACT a,VAR n,f),r2), kpNew)
        (_,_) -> (Nothing, kp)
    Nothing ->
      case modf (Map.insert v kv mv) (kv+1) mp kp (Just l) of
        (Just (f,r2), kpNew) -> (Just (BOX ([],[],ACT a,VAR kv,f),r2), kpNew)
        (_,_) -> (Nothing, kp)
modf mv kv mp kp (Just ((Sym '['):l)) =
  case depend mv (Just l) of
    Just (vs,us,(Sym '<'):((Id a):((Id v):((Sym ']'):r)))) ->
      case Map.lookup v mv of
        Just n ->
          case modf mv kv mp kp (Just r) of
            (Just (f,r2), kpNew) -> (Just (BOX (vs,us,ACT a,VAR n,f),r2), kpNew)
            (_,_) -> (Nothing, kp)
        Nothing ->
          case modf (Map.insert v kv mv) (kv+1) mp kp (Just r) of
            (Just (f,r2), kpNew) -> (Just (BOX (vs,us,ACT a,VAR kv,f),r2), kpNew)
            (_,_) -> (Nothing, kp)
    _ -> (Nothing, kp)
modf _ _ _ kp (Just ((Boo b):l)) = (Just (BOO b,l), kp)
modf mv kv mp kp (Just ((Id p):(tok:l))) =
  case Map.lookup p mp of
    Nothing -> (Nothing, kp)
    Just n ->
      case tok of
        Sym '(' ->
          case vars mv (Just l) of
            Just (vs,(Sym ')'):r) -> (Just (PROP (n,vs),r), kp)
            _ -> (Nothing, kp)
        _ -> (Just (PROP (n,[]),tok:l), kp)
modf mv kv mp kp (Just ((Sym '('):l)) =
  case formula mv kv mp kp (Just l) of
    (Just (f,(Sym ')'):r), kpNew) -> (Just (f,r), kpNew)
    (_,_) -> (Nothing, kp)
modf _ _ _ kp _ = (Nothing, kp)

boolf :: Map.Map [Char] Int -> Int -> Map.Map [Char] Int -> Int -> Maybe (LHP,[Token]) -> (Maybe (LHP,[Token]), Int)
boolf mv kv mp kp (Just (f,(Sym '&'):l)) =
  case modf mv kv mp kp (Just l) of
    (Just (g,r), kpNew) -> (Just (AND (f,g),r), kpNew)
    (_,_) -> (Nothing,kp)
boolf mv kv mp kp (Just (f,(Sym '|'):l)) =
  case modf mv kv mp kp (Just l) of
    (Just (g,r), kpNew) -> (Just (OR (f,g),r), kpNew)
    (_,_) -> (Nothing,kp)
boolf _ _ _ kp mblt = (mblt, kp)

formula :: Map.Map [Char] Int -> Int -> Map.Map [Char] Int -> Int -> Maybe [Token] -> (Maybe (LHP,[Token]), Int)
formula mv kv mp kp (Just ((Fix Nu):((Id p):((Sym '('):l)))) =
  case vars mv (Just l) of
    Just (vs,(Sym ')'):((Sym '.'):r)) ->
      case formula mv kv (Map.insert p kp mp) (kp+1) (Just r) of
        (Just (f,r2), kpNew) -> (Just (NU (PROP (kp,vs),f),r2), kpNew)
        (_,_) -> (Nothing, kp)
    _ -> (Nothing, kp)
formula mv kv mp kp (Just ((Fix Nu):((Id p):((Sym '.'):l)))) =
  case formula mv kv (Map.insert p kp mp) (kp+1) (Just l) of
    (Just (f,r), kpNew) -> (Just (NU (PROP (kp,[]),f),r), kpNew)
    (_,_) -> (Nothing, kp)
formula mv kv mp kp (Just ((Fix Mu):((Id p):((Sym '('):l)))) =
  case vars mv (Just l) of
    Just (vs,(Sym ')'):((Sym '.'):r)) ->
      case formula mv kv (Map.insert p kp mp) (kp+1) (Just r) of
        (Just (f,r2), kpNew) -> (Just (MU (PROP (kp,vs),f),r2), kpNew)
        (_,_) -> (Nothing, kp)
    _ -> (Nothing, kp)
formula mv kv mp kp (Just ((Fix Mu):((Id p):((Sym '.'):l)))) =
  case formula mv kv (Map.insert p kp mp) (kp+1) (Just l) of
    (Just (f,r), kpNew) -> (Just (MU (PROP (kp,[]),f),r), kpNew)
    (_,_) -> (Nothing, kp)
formula mv kv mp kp mblt =
  let (mblt2,kpNew) = modf mv kv mp kp mblt
  in boolf mv kv mp kpNew mblt2

-- Tests on parsing tree for formulae

checkEnd :: [Token] -> Bool
checkEnd ((Sym '$'):[]) = True
checkEnd _ = False

varId :: VAR -> Int
varId (VAR v) = v

freeVars :: LHP -> Set.Set Int
freeVars (PROP (s,vs)) = Set.fromList (map varId vs)
freeVars (AND (f,g)) = Set.union (freeVars f) (freeVars g)
freeVars (OR (f,g)) = Set.union (freeVars f) (freeVars g)
freeVars (DIA (vs,us,_,v,f)) = Set.union (Set.union (Set.delete (varId v) (freeVars f)) (Set.fromList (map varId vs))) (Set.fromList (map varId us))
freeVars (BOX (vs,us,_,v,f)) = Set.union (Set.union (Set.delete (varId v) (freeVars f)) (Set.fromList (map varId vs))) (Set.fromList (map varId us))
freeVars (NU (p,f)) = freeVars p
freeVars (MU (p,f)) = freeVars p
freeVars _ = Set.empty

delFromList :: Eq a => a -> [a] -> [a]
delFromList _ [] = []
delFromList x (h:t) =
  if x == h
    then delFromList x t
    else h:(delFromList x t)

freeVarsOccurs :: LHP -> [Int]
freeVarsOccurs (PROP (s,vs)) = map varId vs
freeVarsOccurs (AND (f,g)) = (freeVarsOccurs f)++(freeVarsOccurs g)
freeVarsOccurs (OR (f,g)) = (freeVarsOccurs f)++(freeVarsOccurs g)
freeVarsOccurs (DIA (vs,us,_,VAR v,f)) =
  let newFVs = freeVarsOccurs f
  in (map varId vs)++(map varId us)++(delFromList v newFVs)
freeVarsOccurs (BOX (vs,us,_,VAR v,f)) =
  let newFVs = freeVarsOccurs f
  in (map varId vs)++(map varId us)++(delFromList v newFVs)
freeVarsOccurs (NU (p,f)) = freeVarsOccurs p
freeVarsOccurs (MU (p,f)) = freeVarsOccurs p
freeVarsOccurs _ = []

freeProps :: LHP -> Set.Set Int
freeProps (PROP (s,_)) = Set.singleton s
freeProps (AND (f,g)) = Set.union (freeProps f) (freeProps g)
freeProps (OR (f,g)) = Set.union (freeProps f) (freeProps g)
freeProps (DIA (_,_,_,_,f)) = freeProps f
freeProps (BOX (_,_,_,_,f)) = freeProps f
freeProps (NU (PROP (s,_),f)) = Set.delete s (freeProps f)
freeProps (MU (PROP (s,_),f)) = Set.delete s (freeProps f)
freeProps _ = Set.empty

isVarClosed :: LHP -> Bool
isVarClosed f = Set.null (freeVars f)

isPropClosed :: LHP -> Bool
isPropClosed f = Set.null (freeProps f)

isClosed :: LHP -> Bool
isClosed f = Set.null (freeVars f) && Set.null (freeProps f)

delPropFromList :: Int -> [LHP] -> [LHP]
delPropFromList p ((PROP (s,vs)):l) =
  if p == s
  then delPropFromList p l
  else (PROP (s,vs)):(delPropFromList p l)
delPropFromList p (_:l) = delPropFromList p l
delPropFromList _ [] = []

propOccurrsIn :: LHP -> LHP -> [LHP]
propOccurrsIn (PROP (p,_)) (PROP (s,vs)) =
  if p == s
    then [PROP (s,vs)]
    else []
propOccurrsIn p (AND (f,g)) = propOccurrsIn p f ++ propOccurrsIn p g
propOccurrsIn p (OR (f,g)) = propOccurrsIn p f ++ propOccurrsIn p g
propOccurrsIn p (DIA (_,_,_,_,f)) = propOccurrsIn p f
propOccurrsIn p (BOX (_,_,_,_,f)) = propOccurrsIn p f
propOccurrsIn p (NU (PROP (s,_),f)) = delPropFromList s (propOccurrsIn p f)
propOccurrsIn p (MU (PROP (s,_),f)) = delPropFromList s (propOccurrsIn p f)
propOccurrsIn _ _ = []

checkVarsInProps :: Int -> [LHP] -> Bool
checkVarsInProps sz ((PROP (_,vs)):l) = length vs == sz && checkVarsInProps sz l
checkVarsInProps _ [] = True
checkVarsInProps _ _ = False

areFixWellFormed :: LHP -> Bool
areFixWellFormed (AND (f,g)) = areFixWellFormed f && areFixWellFormed g
areFixWellFormed (OR (f,g)) = areFixWellFormed f && areFixWellFormed g
areFixWellFormed (DIA (_,_,_,_,f)) = areFixWellFormed f
areFixWellFormed (BOX (_,_,_,_,f)) = areFixWellFormed f
areFixWellFormed (NU (PROP (s,vs),f)) =
  let
    fv = freeVars (PROP (s,vs))
    sz = length vs
  in fv == (freeVars f) && Set.size fv == sz && checkVarsInProps sz (propOccurrsIn (PROP (s,vs)) f) && areFixWellFormed f
areFixWellFormed (MU (PROP (s,vs),f)) =
  let
    fv = freeVars (PROP (s,vs))
    sz = length vs
  in fv == (freeVars f) && Set.size fv == sz && checkVarsInProps sz (propOccurrsIn (PROP (s,vs)) f) && areFixWellFormed f
areFixWellFormed _ = True

parseFormula :: [Char] -> Maybe (LHP,[Token])
parseFormula s =
  case formula Map.empty 0 Map.empty 0 (lexi (s++"$")) of
    (Just (treeF, resTok), _) ->
      if checkEnd resTok && areFixWellFormed treeF
        then Just (treeF, resTok)
        else Nothing
    (Nothing, _) -> Nothing
