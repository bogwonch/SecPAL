module Tests.AssertionSafety where

import Logic.SecPAL.AssertionSafety
import Logic.General.Constraints
import Logic.General.Entities
import Logic.General.Pretty
import Logic.SecPAL.Language
import Logic.SecPAL.Pretty

import Tests.Testable

testFlatness :: [Test] 
testFlatness = [ testIsFlat1
               , testIsFlat2
               ]

testIsFlat1 :: Test 
testIsFlat1 = 
  let secpal = Fact { subject = Constant "Bob"
                    , verb = Predicate "can read" [Variable "f"]
                    } 
  in
    Test{ description=pShow secpal, result=test .flat $ secpal }

testIsFlat2 :: Test 
testIsFlat2 = 
  let secpal = Fact { subject = Constant "Charlie"
                    , verb = CanSay Zero Fact { subject = Constant "Bob"
                                              , verb = Predicate "can read" [Variable "f"]
                                              }
                    }
  in
    Test{ description=pShow secpal, result=test . not . flat $ secpal }

testSafe :: [Test] 
testSafe = [ testSafe1
           , testSafe2
           , testSafe3
           , testSafe4
           , testSafe5
           , testSafe6
           ]

testUnsafe :: [Test] 
testUnsafe = [ testUnSafe1
             , testUnSafe2
             , testUnSafe3
             , testUnSafe4
             , testUnSafe5
             ]

testSafe1 :: Test 
testSafe1 = 
  let secpal = Assertion 
                 (Constant "A") 
                 (Claim (Fact (Constant "B") 
                              (Predicate "can read" [ Constant "Foo" ])) 
                        [] (Boolean True))
  in Test{ description=pShow secpal, result=test . safe $ secpal }

testSafe2 :: Test 
testSafe2 = 
  let secpal = Assertion 
                 (Constant "A") 
                 (Claim (Fact (Constant "B") 
                              (Predicate "can read" [ Constant "Foo" ])) 
                        [ Fact (Constant "B")
                               (Predicate "can" [Variable "x", Variable "y"])
                        ] (Boolean True))
  in Test{description=pShow secpal, result=test . safe $ secpal }

testSafe3 :: Test 
testSafe3 = 
  let secpal = Assertion 
                 (Constant "A") 
                 (Claim (Fact (Constant "B") 
                              (Predicate "can read" [ Constant "Foo" ])) 
                        [ Fact (Constant "B")
                               (Predicate "can" [Variable "x", Variable "y"])
                        ] 
                        (Not (Equals (Entity $ Variable "x") 
                                     (Entity $ Variable "y"))))
  in Test{description=pShow secpal, result=test . safe $ secpal }


testSafe4 :: Test 
testSafe4 = 
  let secpal = Assertion 
                 (Constant "A") 
                 (Claim (Fact (Constant "B") 
                              (Predicate "can" [ Variable "x" 
                                               , Variable "y"
                                               ])) 
                        [ Fact (Constant "B")
                               (Predicate "can" [Variable "x", Variable "y"])
                        ] (Boolean True))
              
  in Test{description=pShow secpal, result=test . safe $ secpal }

testSafe5 :: Test 
testSafe5 = 
  let secpal = Assertion 
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
              
  in Test{description=pShow secpal, result=test . safe $ secpal }

testSafe6 :: Test 
testSafe6 = 
  let secpal = Assertion 
                 (Constant "A") 
                 (Claim (Fact (Constant "B") 
                              (CanSay Zero 
                                      (Fact (Variable "x")
                                            (Predicate "can" [ Variable "y"
                                                             , Variable "z"
                                                             ]))))
                        [] (Boolean True))
  in Test{description=pShow secpal, result=test . safe $ secpal }

 
testUnSafe1 :: Test 
testUnSafe1 =
  let secpal = Assertion 
                 (Constant "A") 
                 (Claim (Fact (Constant "B") 
                              (Predicate "can" [ Variable "x"
                                               , Constant "Foo"
                                               ])) 
                        [] (Boolean True))
  in Test{description=pShow secpal, result=test . not . safe $ secpal }

testUnSafe2 :: Test 
testUnSafe2 = 
  let secpal = Assertion 
                 (Constant "A") 
                 (Claim (Fact (Variable "z") 
                              (Predicate "can read" [ Constant "Foo" ])) 
                        [ Fact (Constant "B")
                               (Predicate "can" [Variable "x", Variable "y"])
                        ] (Boolean True))
  in Test{description=pShow secpal, result=test . not . safe $ secpal }

testUnSafe3 :: Test 
testUnSafe3 = 
  let secpal = Assertion (Constant "A") 
                         (Claim (Fact (Constant "B") 
                                      (Predicate "can read" [ Constant "Foo" ])) 
                                [ Fact (Constant "B")
                                       (Predicate "can" [Variable "x", Variable "y"])
                                ] 
                                (Not (Equals (Entity $ Variable "w") 
                                             (Entity $ Variable "y"))))
              
  in Test{description=pShow secpal, result=test . not . safe $ secpal }


testUnSafe4 :: Test 
testUnSafe4 = 
  let secpal = Assertion 
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
  in Test{description=pShow secpal, result=test . not . safe $ secpal }


testUnSafe5 :: Test 
testUnSafe5 = 
  let secpal = Assertion 
                 (Constant "A") 
                 (Claim (Fact (Variable "w") 
                              (CanSay Zero 
                                      (Fact (Variable "x")
                                            (Predicate "can" [ Variable "y"
                                                             , Variable "z"
                                                             ]))))
                        [] (Boolean True))
  in Test{description=pShow secpal, result=test . not . safe $ secpal }


-- The assertion context from ESSoS paper
testESSoS :: [Test] 
testESSoS = [ agTest1
            , agTest2
            , agTest3
            , agTest4
            , agTest5
            , agTest6
            , agTest7
            ]

agTest1 :: Test 
agTest1 = 
  let secpal = Assertion 
--                 (Constant "Phone")
                 (Variable "anyone")
                 (Claim (Fact (Variable "app")
                              (Predicate "meets" [Variable "policy"]))
                        [ Fact (Variable "evidence")
                               (Predicate "pShows meets"
                                          [ Variable "app"
                                          , Variable "policy"
                                          ])
                        ] (Boolean True))
  in Test{description=pShow secpal, result=test . safe $ secpal }

agTest2 :: Test 
agTest2 =
  let secpal = Assertion
                 (Constant "Phone")
                 (Claim (Fact (Variable "app")
                              (Predicate "installable" []))
                        [ Fact (Variable "app")
                               (Predicate "meets" [Constant "NotMalware"])
                        , Fact (Variable "app")
                               (Predicate "meets" [Constant "NoInfoLeaks"])
                        ] (Boolean True))
  in Test{description=pShow secpal, result = test . safe $ secpal }

agTest3 :: Test 
agTest3 = 
  let secpal = Assertion
                 (Constant "Phone")
                 (Claim (Fact (Constant "Google")
                              (CanSay Infinity
                                      (Fact (Variable "app")
                                            (Predicate "meets" [ Constant "NotMalware" ]))))
                        [] (Boolean True))
  in Test{description=pShow secpal, result=test . safe $ secpal }

agTest4 :: Test 
agTest4 = 
  let secpal = Assertion
                 (Constant "Google")
                 (Claim (Fact (Constant "AVChecker")
                              (CanSay Zero
                                      (Fact (Variable "app")
                                            (Predicate "meets" [ Constant "NotMalware" ]))))
                        [] (Boolean True))
  in Test{description=pShow secpal, result=test . safe $ secpal }

agTest5 :: Test 
agTest5 = 
  let secpal = Assertion
                 (Constant "Phone")
                 (Claim (Fact (Constant "NILInferer")
                              (CanSay Zero
                                      (Fact (Variable "app")
                                            (Predicate "meets" [ Constant "NoInfoLeaks" ]))))
                        [] (Boolean True))
  in Test{description=pShow secpal, result=test . safe $ secpal }

agTest6 :: Test 
agTest6 = 
  let secpal = Assertion
                 (Constant "AVChecker")
                 (Claim (Fact (Constant "Game")
                              (Predicate "meets" [ Constant "NotMalware" ]))
                        [] (Boolean True))
  in Test{description=pShow secpal, result=test . safe $ secpal }

agTest7 :: Test 
agTest7 =
  let secpal = Assertion
                 (Constant "NilInferer")
                 (Claim (Fact (Constant "Evidence")
                              (Predicate "pShows meets"
                                         [ Constant "Game"
                                         , Constant "Policy"
                                         ]))
                        [] (Boolean True))
  in Test{description=pShow secpal, result=test . safe $ secpal }



