module MC where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Model
import Logic

-- Automata state (node)
data Node = Node (Set.Set Place) (Map.Map Int (Set.Set Place)) LHP
-- Propositions log entry
data PropLog = LogEntry (Node, Map.Map Int PropLog, Int) | EmptyLog

eqEnv :: [Int] -> Map.Map Int (Set.Set Place) -> [Int] -> Map.Map Int (Set.Set Place) -> Bool
eqEnv (v1:t1) eta1 (v2:t2) eta2 = (Map.lookup v1 eta1) == (Map.lookup v2 eta2) && eqEnv t1 eta1 t2 eta2
eqEnv [] _ [] _ = True
eqEnv _ _ _ _ = False

compareEnvs :: [Int] -> Map.Map Int (Set.Set Place) -> [Int] -> Map.Map Int (Set.Set Place) -> Ordering
compareEnvs [] _ [] _ = EQ
compareEnvs _ _ [] _ = GT
compareEnvs [] _ _ _ = LT
compareEnvs (v1:t1) eta1 (v2:t2) eta2 =
  case compare (Map.lookup v1 eta1) (Map.lookup v2 eta2) of
    GT -> GT
    LT -> LT
    EQ -> compareEnvs t1 eta1 t2 eta2

instance Eq Node where
  Node m1 eta1 (PROP (p1,vs1)) == Node m2 eta2 (PROP (p2,vs2)) = p1 == p2 && m1 == m2 && eqEnv (map varId vs1) eta1 (map varId vs2) eta2
  Node m1 eta1 f1 == Node m2 eta2 f2 = f1 == f2 && m1 == m2 && eqEnv (Set.toList (freeVars f1)) eta1 (Set.toList (freeVars f2)) eta2

instance Ord Node where
  compare (Node m1 eta1 (PROP (p1,vs1))) (Node m2 eta2 (PROP (p2,vs2))) =
    case compare p1 p2 of
      GT -> GT
      LT -> LT
      EQ ->
        case compare m1 m2 of
          GT -> GT
          LT -> LT
          EQ -> compareEnvs (map varId vs1) eta1 (map varId vs2) eta2
  compare (Node m1 eta1 f1) (Node m2 eta2 f2) =
    case compare f1 f2 of
      GT -> GT
      LT -> LT
      EQ ->
        case compare m1 m2 of
          GT -> GT
          LT -> LT
          EQ -> compareEnvs (Set.toList (freeVars f1)) eta1 (Set.toList (freeVars f2)) eta2

updateCausedBy :: Trans -> Set.Set Place -> Set.Set Place
updateCausedBy t ps =
  let diff = Set.difference ps (pre t)
  in
    if Set.size diff < Set.size ps
      then Set.union (diff) (post t)
      else ps

etaAfterTrans :: Map.Map Int (Set.Set Place) -> VAR -> Trans -> Map.Map Int (Set.Set Place)
etaAfterTrans eta (VAR v) t = Map.insert v (post t) (Map.map (updateCausedBy t) eta)

isCausedBy :: Map.Map Int (Set.Set Place) -> Trans -> VAR -> Bool
isCausedBy eta t (VAR v) =
  case Map.lookup v eta of
    Just ps -> not (Set.null (Set.intersection (pre t) ps))
    _ -> False

isConcur :: Map.Map Int (Set.Set Place) -> Trans -> VAR -> Bool
isConcur eta t (VAR v) =
  case Map.lookup v eta of
    Just ps -> Set.null (Set.intersection (pre t) ps)
    _ -> False

isCausedByList :: Map.Map Int (Set.Set Place) -> Trans -> [VAR] -> Bool
isCausedByList eta t (v:l) =
  if isCausedBy eta t v
    then isCausedByList eta t l
    else False
isCausedByList _ _ [] = True

isConcurList :: Map.Map Int (Set.Set Place) -> Trans -> [VAR] -> Bool
isConcurList eta t (v:l) =
  if isConcur eta t v
    then isConcurList eta t l
    else False
isConcurList _ _ [] = True

checkCondOnTrans :: Map.Map Int (Set.Set Place) -> ACT -> [VAR] -> [VAR] -> Trans -> Bool
checkCondOnTrans eta (ACT a) cs cc t = (a == "_" || label t == a) && isCausedByList eta t cs && isConcurList eta t cc

hashVars :: Map.Map Int (Set.Set Place) -> [VAR] -> Int -> Int -> Int
hashVars eta ((VAR v):vs) base exp =
  case Map.lookup v eta of
    Just ps -> (base^exp) * (rem (Set.foldr (\x -> \y -> (pid x) + y) 0 ps) base) + hashVars eta vs base (exp+1)
    _ -> hashVars eta vs base exp
hashVars _ [] _ _ = 0

hashNode :: Set.Set Place -> Map.Map Int (Set.Set Place) -> [VAR] -> Int
hashNode m eta vs = (rem (Set.foldr (\x -> \y -> pid x + y) 0 m) 10) + hashVars eta vs 10 1

testAncestors :: Node -> Map.Map Int PropLog -> Int -> Bool
testAncestors this@(Node m eta (PROP (p,vs))) beta h =
  case (Map.lookup p beta) of
    Just (LogEntry (anc, betaA, hA)) ->
      if h == hA
        then
          if this == anc
            then True
            else testAncestors this betaA h
        else testAncestors this betaA h
    _ -> False
testAncestors _ _ _ = False

renameVars :: Map.Map Int (Set.Set Place) -> [VAR] -> [VAR] -> Map.Map Int (Set.Set Place)
renameVars eta ((VAR v):vs) ((VAR u):us) =
  case Map.lookup v eta of
    Just ps -> renameVars (Map.insert u ps eta) vs us
    _ -> eta
renameVars eta _ _ = eta

unfold :: Node -> Map.Map Int LHP -> Map.Map Int PropLog -> Int -> (Node, Map.Map Int PropLog)
unfold this@(Node m eta (PROP (p,vs))) pi beta h =
  case Map.lookup p pi of
    Just (NU (PROP (_,vsA),f)) -> (Node m (renameVars eta vs vsA) f, (Map.insert p (LogEntry (this,beta,h)) beta))
    Just (MU (PROP (_,vsA),f)) -> (Node m (renameVars eta vs vsA) f, (Map.insert p (LogEntry (this,beta,h)) beta))
    _ -> (this, beta)
unfold nd _ beta _ = (nd,beta)

existsBranch :: Net -> [Node] -> Map.Map Int LHP -> Map.Map Int PropLog -> Map.Map Node Bool -> (Bool, Map.Map Node Bool)
existsBranch n ((Node m eta f):l) pi beta gamma =
  case Map.lookup (Node m eta f) gamma of
    Just True -> (True, gamma)
    Just False -> existsBranch n l pi beta gamma
    Nothing ->
      let (b, gammaNew) = nonempty n (Node m eta f) pi beta gamma
      in
        if b
          then (True, Map.insert (Node m eta f) True gammaNew)
          else existsBranch n l pi beta (Map.insert (Node m eta f) False gammaNew)
existsBranch _ [] _ _ gamma = (False, gamma)

forallBranches :: Net -> [Node] -> Map.Map Int LHP -> Map.Map Int PropLog -> Map.Map Node Bool -> (Bool, Map.Map Node Bool)
forallBranches n ((Node m eta f):l) pi beta gamma =
  case Map.lookup (Node m eta f) gamma of
    Just True -> forallBranches n l pi beta gamma
    Just False -> (False, gamma)
    Nothing ->
      let (b, gammaNew) = nonempty n (Node m eta f) pi beta gamma
      in
        if b
          then forallBranches n l pi beta (Map.insert (Node m eta f) True gammaNew)
          else (False, Map.insert (Node m eta f) False gammaNew)
forallBranches _ [] _ _ gamma = (True, gamma)

-- Check automaton non-emptiness
nonempty :: Net -> Node -> Map.Map Int LHP -> Map.Map Int PropLog -> Map.Map Node Bool -> (Bool, Map.Map Node Bool)
nonempty n (Node _ _ (BOO T)) _ _ gamma= (True, gamma)
nonempty n (Node _ _ (BOO F)) _ _ gamma = (False, gamma)
nonempty n this@(Node m eta (AND (f,g))) pi beta gamma =
  case Map.lookup this gamma of
    Just b -> (b, gamma)
    Nothing ->
      let (b1, gamma1) = nonempty n (Node m eta f) pi beta gamma
      in
        if b1
          then
            let (b2, gamma2) = nonempty n (Node m eta g) pi beta gamma1
            in (b2, Map.insert (Node m eta (AND (f,g))) b2 gamma2)
          else (False, Map.insert (Node m eta (AND (f,g))) False gamma1)
nonempty n this@(Node m eta (OR (f,g))) pi beta gamma =
  case Map.lookup this gamma of
    Just b -> (b, gamma)
    Nothing ->
      let (b1, gamma1) = nonempty n (Node m eta f) pi beta gamma
      in
        if b1
          then (True, Map.insert (Node m eta (OR (f,g))) True gamma1)
          else
            let (b2, gamma2) = nonempty n (Node m eta g) pi beta gamma1
            in (b2, Map.insert (Node m eta (OR (f,g))) b2 gamma2)
nonempty n this@(Node m eta (DIA (cs,cc,a,v,f))) pi beta gamma =
  case Map.lookup this gamma of
    Just b -> (b, gamma)
    Nothing ->
      let (b, gammaNew) = existsBranch n (map (\t -> Node (execAndUpdate t m) (etaAfterTrans eta v t) f) (filter (checkCondOnTrans eta a cs cc) (getEnabledTrans n m))) pi beta gamma
      in (b, Map.insert (Node m eta (DIA (cs,cc,a,v,f))) b gammaNew)
nonempty n this@(Node m eta (BOX (cs,cc,a,v,f))) pi beta gamma =
  case Map.lookup this gamma of
    Just b -> (b, gamma)
    Nothing ->
      let (b, gammaNew) = forallBranches n (map (\t -> Node (execAndUpdate t m) (etaAfterTrans eta v t) f) (filter (checkCondOnTrans eta a cs cc) (getEnabledTrans n m))) pi beta gamma
      in (b, Map.insert (Node m eta (BOX (cs,cc,a,v,f))) b gammaNew)
nonempty n this@(Node m eta (NU (PROP (p,vs),f))) pi beta gamma =
  case Map.lookup this gamma of
    Just b -> (b, gamma)
    Nothing ->
      let
        nd = Node m eta (PROP (p,vs))
        pi2 = Map.insert p (NU (PROP (p,vs),f)) pi
        beta2 = Map.insert p EmptyLog beta
        h = hashNode m eta vs
        (b, gammaNew) = nonempty n (Node m eta f) pi2 (Map.insert p (LogEntry (nd,beta2,h)) beta) gamma
      in (b, Map.insert (Node m eta (NU (PROP (p,vs),f))) b gammaNew)
nonempty n this@(Node m eta (MU (PROP (p,vs),f))) pi beta gamma =
  case Map.lookup this gamma of
    Just b -> (b, gamma)
    Nothing ->
      let
        nd = Node m eta (PROP (p,vs))
        pi2 = Map.insert p (MU (PROP (p,vs),f)) pi
        beta2 = Map.insert p EmptyLog beta
        h = hashNode m eta vs
        (b, gammaNew) = nonempty n (Node m eta f) pi2 (Map.insert p (LogEntry (nd,beta2,h)) beta) gamma
      in (b, Map.insert (Node m eta (MU (PROP (p,vs),f))) b gammaNew)
nonempty n this@(Node m eta (PROP (p,vs))) pi beta gamma =
  case Map.lookup this gamma of
    Just b -> (b, gamma)
    Nothing ->
      let h = hashNode m eta vs
      in
        if testAncestors this beta h
          then
            case Map.lookup p pi of
              Just (NU _) -> (True, Map.insert (Node m eta (PROP (p,vs))) True gamma)
              Just (MU _) -> (False, Map.insert (Node m eta (PROP (p,vs))) False gamma)
              _ -> (False, Map.insert (Node m eta (PROP (p,vs))) False gamma) -- branch should be never reached
          else
            let
              (nd,beta2) = unfold this pi beta h
              (b, gammaNew) = nonempty n nd pi beta2 gamma
            in (b, Map.insert (Node m eta (PROP (p,vs))) b gammaNew)
nonempty _ _ _ _ gamma = (False, gamma) -- branch should be never reached

satisfies :: Net -> LHP -> Bool
satisfies n f =
  let (b, _) = nonempty n (Node (initmark n) Map.empty f) Map.empty Map.empty Map.empty
  in b
