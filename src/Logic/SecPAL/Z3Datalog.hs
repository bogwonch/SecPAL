module Logic.SecPAL.Z3Datalog (toDatalog,pShow,Rule,Clause) where

import           Data.List
import           Data.Maybe
import           Logic.General.Entities
import           Logic.General.Named
import           Logic.SecPAL.AssertionSafety (flat)
import           Logic.SecPAL.Pretty
import qualified Logic.SecPAL.Language as SP

-- import           Debug.Trace

{- Datalog format for RULEs and Horn CLAUSEs -}
data Rule = Rule { predicate :: Clause , body :: [Clause] }
          | Annotation Clause AnnotationKind -- Z3 typing information... mostly
  deriving (Eq, Show, Ord)

data Clause = Clause { cname :: String , args :: [E] }
  deriving (Ord, Show)

data AnnotationKind = Input | PrintTuples
  deriving (Eq, Show, Ord)

instance PShow Rule where
  pShow r@Rule{}
    | null (body r) = pShow (predicate r) ++ "."
    | otherwise     = pShow (predicate r) 
                      ++ " :- " ++
                      intercalate ", " (map pShow $ body r) 
                      ++ "."

  pShow (Annotation clause kind) =
    let n = cname clause
        types = take (length . args $ clause) xs
    in
      n ++ "(" ++ intercalate ", " types ++ ") " ++ pShow kind
    where
      xs = map (\x -> "x"++show x++" : Z") [0..] 

instance PShow Clause where
  pShow c@Clause{} = cname c 
                     ++ "(" ++
                     intercalate ", " (map eShow $ args c)
                     ++ ")"
    where 
      eShow e@Variable{} = varName e
      eShow e@Constant{} = "\"" ++ constName e ++ "\""

instance PShow AnnotationKind where
  pShow Input = "input"
  pShow PrintTuples = "printtuples"

instance Eq Clause where
  Clause{cname=a} == Clause{cname=b} = a == b

{- Variable and constant delegation depths for use in Datalog -}
zero, inf :: E
zero = Constant "0" "secpal"
inf  = Constant "inf" "secpal"

k ::  E
k = Variable "k" "secpal"

{- ExplicitFacts are SecPAL facts with an explicit speaker and delegation depth.
 - This makes the translation to Datalog easier and is step 1 in the translation
 - algorithm from Becker's paper.
 -
 - An ExplicitClaim is a SecPAL claim that uses explicit facts.
 -
 - NOTE: Conditions seem to have disappeared here...
 -}
data ExplicitFact = ExplicitFact { speaker :: E , depth :: E , stated :: SP.Fact }
  deriving (Eq, Show)

data ExplicitClaim  = ExplicitClaim { fact :: ExplicitFact , conditions :: [ ExplicitFact ] }
  deriving (Eq, Show)

instance PShow ExplicitFact where
  pShow ef@ExplicitFact{} = pShow (speaker ef) 
                            ++ " says_"
                            ++ pShow (depth ef)
                            ++ " "
                            ++ pShow (stated ef)

instance PShow ExplicitClaim where
  pShow ExplicitClaim{ fact=f, conditions=[] } = 
    pShow f ++ ";"
  pShow ExplicitClaim{ fact=f, conditions=cs } = 
    pShow f 
    ++ " if "
    ++ intercalate ", " (map pShow cs)
    ++ ";"

toEClaim :: SP.Assertion -> ExplicitClaim
toEClaim SP.Assertion{ SP.who=w, SP.says=claim } = 
  let f  = SP.fact claim
      cs = SP.conditions claim
      toEFact = ExplicitFact w k
  in
    ExplicitClaim{ fact = toEFact f
                 , conditions = map toEFact cs
                 }

{- A SecPAL series of facts need to be translated to a single predicate -}
factToName :: SP.Fact -> String
factToName = vpToName . SP.verb

vpToName :: SP.VerbPhrase -> String
vpToName SP.Predicate{ SP.predicate=n } = n
vpToName SP.CanActAs{} = "canactas"
vpToName SP.CanSay{ SP.delegation=d, SP.what=f } =
  let d' = if d==SP.Zero then "zero_" else "infinity_"
    in "cansay_"++d'++factToName f

factToArgs :: SP.Fact -> [E]
factToArgs SP.Fact{ SP.subject=a, SP.verb=v } = a : vpToArgs v

vpToArgs :: SP.VerbPhrase -> [E]
vpToArgs SP.Predicate{ SP.args=es } = es
vpToArgs SP.CanActAs{ SP.whom=e } = [e]
vpToArgs SP.CanSay{ SP.what=f } = factToArgs f

{-- Translation algorithm --}
toClause :: ExplicitFact -> Clause
toClause ExplicitFact{ speaker=s, depth=d, stated=f } =
  let n = "Says_" ++ factToName f
      xs = [s,d] ++ factToArgs f
  in 
    Clause{ cname=n, args=xs }


toRule :: ExplicitClaim -> [Rule]
toRule ec
  | isACanSay ec = toRule_cansay ec
  | isACanActAs ec = error "Not implemented can-act-as yet"
  | otherwise = toRule_cond ec
  where 
    isACanSay = not . flat . stated . fact
    isACanActAs x = 
      case SP.verb . stated . fact $ x of
        SP.CanActAs{} -> True
        _ -> False

{- When we translate a cond statement we can convert it pretty much directly
 - into Datalog
 -}
toRule_cond :: ExplicitClaim -> [Rule]
toRule_cond ec = 
  let predicate' = toClause . fact $ ec
      body' = map toClause . conditions $ ec
      gfs = groundFacts predicate'
  in
    return Rule{ predicate=predicate', body=gfs++body' }

{- A CanSay Statement is a little more complex.
 - First we translate the cansay as we would for a cond statement.  We then add
 - a second rule that implements the can-say inference rule: namely that we say
 - something if we someone else can say it and they do say it. 
 -
 - Plus the delegation depth has to be right...
 -}
toRule_cansay :: ExplicitClaim -> [Rule]
toRule_cansay ec = --trace ("toRule_cansay: "++pShow ec) $
  let cansay = toRule_cond ec
      d = delegationDepth ec
      i = speaker . fact $ ec
      u = delegatee ec
      t = targetFact ec
      uCanSay = predicate . head $ cansay
      uSay = ExplicitFact{ speaker=u, depth=d, stated=t }
      iSay = ExplicitFact{ speaker=i, depth=inf, stated=t }
      iClaim = head $ toRule_cond ExplicitClaim{ fact=iSay, conditions=[uSay] }
      iClaim' = addUCanSay iClaim uCanSay
  in cansay++[iClaim']
  where
    delegationDepth x = 
      case SP.delegation . SP.verb . stated . fact $ x of
        SP.Zero -> zero
        SP.Infinity -> inf
    delegatee = SP.subject . stated . fact
    targetFact = SP.what . SP.verb . stated . fact
    addUCanSay x@Rule{ body=cs } c = x{ body=c:cs }


{- The ground facts are simply that all variables in the arguments are actually
 - entities we know about and not unknown unknowns.  The delegation depth k is a
 - special case and we check its a secpal_depth.
 -}
groundFacts :: Clause -> [Clause]
groundFacts Clause{ args=(x: (k' : xs)) } = 
  catMaybes $ isDelegationDepth k' : map isEntity (x:xs)
groundFacts _ = error "found an odd looking clause"

isEntity :: E -> Maybe Clause
isEntity Constant{} = Nothing
isEntity v@Variable{} = Just $ Clause "SecPAL_Entity" [v]

isDelegationDepth :: E -> Maybe Clause
isDelegationDepth x
  | x == zero = Nothing
  | x == inf  = Nothing
  | x == k    = Just $ Clause "SecPAL_Depth" [k]
  | otherwise = error "found a delegation depth that isn't a delegation depth"

{- Conversion of a full AC -}
toDatalog :: SP.AC -> String
toDatalog = unlines . (header ++) . (++ footer) . map pShow . toDatalog'

toDatalog' :: SP.AC -> [Rule]
toDatalog' (SP.AC ac) =
  let rules = concatMap (toRule . toEClaim) ac
      types = rmdups $ typingRules rules
      ents = entities rules
  in
    types ++ rules ++ ents


{- Preamble stuff -}
header :: [String]
header = 
  [ "Z 64"  -- I have no idea what this does
  , ""
  , "SecPAL_Depth(x : Z) input"
  , "SecPAL_Entity(x : Z) input"
  ]

footer :: [String]
footer =
  [ "SecPAL_Depth("++name zero++")."
  , "SecPAL_Depth("++name inf++")."
  ]

typingRules :: [Rule] -> [Rule]
typingRules = rmdups . mapMaybe typingRule 
  where
    typingRule :: Rule -> Maybe Rule
    typingRule Rule{predicate=p} = Just $ Annotation p Input
    typingRule _ = Nothing

entities :: [Rule] -> [Rule]
entities rs = map entity (rmdups . constants $ rs)
  where
    entity :: E -> Rule
    entity e = Rule{ predicate=Clause{ cname="SecPAL_Entity", args=[e] }, body=[] }


rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

{- Expand to Vars too and refactor? -}
class EntityHolding x where
  constants :: x -> [E]

instance EntityHolding E where
  constants x@Constant{} = [x]
  constants _ = []

instance EntityHolding t => EntityHolding [t] where
  constants = concatMap constants

instance EntityHolding Clause where
  constants Clause{ args=xs } = constants xs
    
instance EntityHolding Rule where
  constants r@Rule{} = constants (predicate r) ++ constants (body r)
  constants _ = []