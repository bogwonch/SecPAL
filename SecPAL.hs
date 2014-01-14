module SecPAL where


data E = Variable { varName :: String }
       | Constant { constName :: String }
       deriving (Eq, Show)

data D = Zero
       | Infinity
       deriving (Eq, Show)

data VerbPhrase = Predicate { predicate :: String, args :: [E] }
                | CanSay { delegation :: D, what :: Fact }
                | CanActAs { whom :: E }
                deriving (Eq, Show)
  
data Fact = Fact { subject :: E, verb :: VerbPhrase }
          deriving (Eq, Show)

data Claim = Claim { fact :: Fact, conditions :: [Fact], constraint :: C }
           deriving (Eq, Show)

data Assertion = Assertion { who :: E, says :: Claim }
               deriving (Eq, Show)

data AC = AC [Assertion]
        deriving (Eq, Show)

data F = F { fName :: String }
       deriving (Eq, Show)

data Ec = Entity E
        | Apply F [E]
        deriving (Eq, Show)

data C = Boolean Bool
       | Equals Ec Ec
       | Not C
       | Conj C C
       deriving (Eq, Show)


class Named a where
    name :: a -> String

instance Named E where 
    name Variable {varName=n} = n
    name Constant {constName=n} = n

instance Named F where
    name F {fName=n} = n


class Vars a where
    vars :: a -> [E]

instance Vars E where
    vars e@(Variable _) = [e]
    vars e@(Constant _) = []

instance Vars VerbPhrase where
    vars Predicate {args=a} = concatMap vars a
    vars CanSay {what=w} = vars w
    vars CanActAs {} = []

instance Vars Fact where
    vars Fact {subject=s, verb=v} = vars s ++ vars v

instance Vars Ec where
    vars (Entity e) = vars e
    vars (Apply _ es) = concatMap vars es

instance Vars C where
    vars Boolean {} = []
    vars (Equals e1 e2) = vars e1 ++ vars e2
    vars (Not c) = vars c
    vars (Conj c1 c2) = vars c1 ++ vars c2
      



-- We say that a fact is flat when it does not contain can say, and is
-- nested otherwise. Facts are of the general form e1 can sayD1 ... en can
-- sayDn fact , where n ≥ 0 and fact is flat. For example, the fact Bob can
-- read f is flat, but Charlie can say0 Bob can read
-- f is not.

flat :: Fact -> Bool
flat Fact {verb=v} = case v of
                       CanSay {} -> False
                       otherwise -> True

testIsFlat1 = flat Fact { subject = Constant "Bob"
                        , verb = Predicate "can read" [Variable "f"]
                        } 
testIsFlat2 = not . flat $ 
  Fact { subject = Constant "Charlie"
       , verb = 
          CanSay Zero Fact { subject = Constant "Bob"
                           , verb = Predicate "can read" [Variable "f"]
                           }
       }

-- Definition 2.6. (Assertion safety) 
-- Let A says fact if fact1,...,factn,c be an assertion. 
-- A variable x ∈ vars(fact) is safe iff x ∈ vars(fact1) ∪ ... ∪ vars(factn).
-- The assertion A says fact if fact1, ..., factn, c is safe iff
--   1. if fact is flat, all variables in vars(fact) are safe;
--      otherwise (i.e. fact is of the form e can sayD fact′) e is either a constant or a safe variable;
--   2. vars(c) ⊆ vars(fact) ∪ vars(fact1) ∪ ... ∪ vars(factn);
--   3. fact1,...,factn are flat.
safeIn :: E -> Claim -> Bool
safeIn Constant {} _ = True
safeIn v@(Variable {}) Claim { fact=f, conditions=fs, constraint=c }
  | v `elem` vars f = v `elem` concatMap vars fs
  | otherwise = True 

safe :: Assertion -> Bool
safe Assertion { says = claim@Claim{ fact=f@(Fact{subject=subj})
                                   , conditions=fs
                                   , constraint=c }
               } = 
    condition1 && condition2 && condition3
  where
    condition1 = if flat f 
                   then all (`safeIn` claim) (vars f)
                   else subj `safeIn` claim
    condition2 = all (`elem` concatMap vars fs) (vars c)
    condition3 = all flat fs


testSafe1 = safe $ 
  Assertion 
    (Constant "A") 
    (Claim (Fact (Constant "B") 
                 (Predicate "can read" [ Constant "Foo" ])) 
           [] (Boolean True))

testSafe2 = safe $ 
  Assertion 
    (Constant "A") 
    (Claim (Fact (Constant "B") 
                 (Predicate "can read" [ Constant "Foo" ])) 
           [ Fact (Constant "B")
                  (Predicate "can" [Variable "x", Variable "y"])
           ] (Boolean True))

testSafe3 = safe $ 
  Assertion 
    (Constant "A") 
    (Claim (Fact (Constant "B") 
                 (Predicate "can read" [ Constant "Foo" ])) 
           [ Fact (Constant "B")
                  (Predicate "can" [Variable "x", Variable "y"])
           ] 
           (Not (Equals (Entity $ Variable "x") 
                        (Entity $ Variable "y"))))
 
testSafe4 = safe $ 
  Assertion 
    (Constant "A") 
    (Claim (Fact (Constant "B") 
                 (Predicate "can" [ Variable "x" 
                                  , Variable "y"
                                  ])) 
           [ Fact (Constant "B")
                  (Predicate "can" [Variable "x", Variable "y"])
           ] (Boolean True))
 
testSafe5 = safe $ 
  Assertion 
    (Constant "A") 
    (Claim (Fact (Variable "z") 
                 (Predicate "can" [ Variable "x" 
                                  , Variable "y"
                                  ])) 
           [ Fact (Variable "z")
                  (Predicate "can" [Variable "x", Constant "Foo"])
           , Fact (Variable "z")
                  (Predicate "can read" [Variable "y"])
           ] (Boolean True))
 
testSafe6 = safe $ 
  Assertion 
    (Constant "A") 
    (Claim (Fact (Constant "B") 
                 (CanSay Zero 
                         (Fact (Variable "x")
                               (Predicate "can" [ Variable "y"
                                                , Variable "z"
                                                ]))))
           [] (Boolean True))

 
testUnSafe1 = not . safe $
  Assertion 
    (Constant "A") 
    (Claim (Fact (Constant "B") 
                 (Predicate "can" [ Variable "x"
                                  , Constant "Foo"
                                  ])) 
           [] (Boolean True))

testUnSafe2 = not . safe $ 
  Assertion 
    (Constant "A") 
    (Claim (Fact (Variable "z") 
                 (Predicate "can read" [ Constant "Foo" ])) 
           [ Fact (Constant "B")
                  (Predicate "can" [Variable "x", Variable "y"])
           ] (Boolean True))

testUnSafe3 = not . safe $ 
  Assertion (Constant "A") 
            (Claim (Fact (Constant "B") 
                         (Predicate "can read" [ Constant "Foo" ])) 
                   [ Fact (Constant "B")
                          (Predicate "can" [Variable "x", Variable "y"])
                   ] 
                   (Not (Equals (Entity $ Variable "w") 
                                (Entity $ Variable "y"))))
 
testUnSafe4 = not . safe $ 
  Assertion 
    (Constant "A") 
    (Claim (Fact (Constant "B") 
                 (Predicate "can" [ Variable "x" 
                                  , Variable "y"
                                  ])) 
           [ Fact (Constant "B")
                  (CanSay Zero
                          (Fact (Constant "C")
                                (Predicate "can" [Variable "x", Variable "y"])))
           ] (Boolean True))

testUnSafe5 = not . safe $ 
  Assertion 
    (Constant "A") 
    (Claim (Fact (Variable "w") 
                 (CanSay Zero 
                         (Fact (Variable "x")
                               (Predicate "can" [ Variable "y"
                                                , Variable "z"
                                                ]))))
           [] (Boolean True))


tests = and [ testIsFlat1
            , testIsFlat2
            , testSafe1
            , testSafe2
            , testSafe3
            , testSafe4
            , testSafe5
            , testSafe6
            , testUnSafe1
            , testUnSafe2
            , testUnSafe3
            , testUnSafe4
            , testUnSafe5
            ]

-- The assertion context from using-the-rules.tex
agTest1 = safe $
  Assertion 
    (Constant "Phone")
    (Claim (Fact (Variable "app")
                 (Predicate "meets" [Variable "policy"]))
           [ Fact (Variable "evidence")
                  (Predicate "shows meets"
                             [ Variable "app"
                             , Variable "policy"
                             ])
           ] (Boolean True))

agTest2 = safe $
  Assertion
    (Constant "Phone")
    (Claim (Fact (Variable "app")
                 (Predicate "installable" []))
           [ Fact (Variable "app")
                  (Predicate "meets" [Constant "NotMalware"])
           , Fact (Variable "app")
                  (Predicate "meets" [Constant "NoInfoLeaks"])
           ] (Boolean True))

agTest3 = safe $
  Assertion
    (Constant "Phone")
    (Claim (Fact (Constant "Google")
                 (CanSay Infinity
                         (Fact (Variable "app")
                               (Predicate "meets" [ Constant "NotMalware" ]))))
           [] (Boolean True))

agTest4 = safe $
  Assertion
    (Constant "Google")
    (Claim (Fact (Constant "AVChecker")
                 (CanSay Zero
                         (Fact (Variable "app")
                               (Predicate "meets" [ Constant "NotMalware" ]))))
           [] (Boolean True))

agTest5 = safe $
  Assertion
    (Constant "Phone")
    (Claim (Fact (Constant "NILInferer")
                 (CanSay Zero
                         (Fact (Variable "app")
                               (Predicate "meets" [ Constant "NoInfoLeaks" ]))))
           [] (Boolean True))

agTest6 = safe $
  Assertion
    (Constant "AVChecker")
    (Claim (Fact (Constant "Game")
                 (Predicate "meets" [ Constant "NotMalware" ]))
           [] (Boolean True))

agTest7 = safe $
  Assertion
    (Constant "NilInferer")
    (Claim (Fact (Constant "Evidence")
                 (Predicate "shows meets"
                            [ Constant "Game"
                            , Constant "Policy"
                            ]))
           [] (Boolean True))
