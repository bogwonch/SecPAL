module Tests.AssertionSafety where

import Logic.SecPAL.AssertionSafety
import Logic.SecPAL.Language

import Tests.Testable


testFlatness = [ Test{ description="flat (Bob can read f)"
                     , result=test testIsFlat1
                     }
               , Test{ description="not flat (Charlie can say-0 Bob can read f)"
                     , result=test testIsFlat2
                     }
               ]

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


testSafety = [ Test{ description="testSafe1", result=test testSafe1 }
             , Test{ description="testSafe2", result=test testSafe2 }
             , Test{ description="testSafe3", result=test testSafe3 }
             , Test{ description="testSafe4", result=test testSafe4 }
             , Test{ description="testSafe5", result=test testSafe5 }
             , Test{ description="testSafe6", result=test testSafe6 }
             , Test{ description="testUnSafe1", result=test testUnSafe1 }
             , Test{ description="testUnSafe2", result=test testUnSafe2 }
             , Test{ description="testUnSafe3", result=test testUnSafe3 }
             , Test{ description="testUnSafe4", result=test testUnSafe4 }
             , Test{ description="testUnSafe5", result=test testUnSafe5 }
             ]

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

{-
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
-}

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
