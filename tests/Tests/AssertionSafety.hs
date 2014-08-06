{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Tests.AssertionSafety where

import Logic.SecPAL.AssertionSafety
import Logic.SecPAL.Language
import Logic.General.Pretty
import Text.Parsec
import Logic.SecPAL.Parser
import Data.Functor.Identity

import Tests.Testable

testFlatness :: [Test] 
testFlatness = [ testIsFlat1
               , testIsFlat2
               ]
make :: forall s t c. Stream s Identity t => Parsec s () c -> s -> c
make p str = case parse p "" str of
  (Left err) -> error . show $ err
  (Right a) -> a

makeFact :: String -> Fact
makeFact = make pFact

makeAssertion :: String -> Assertion
makeAssertion = make pAssertionUnsafe

testIsFlat1 :: Test 
testIsFlat1 = 
  let secpal = makeFact "Bob can-read(f)"
  in
    Test{ description=pShow secpal, result=test .flat $ secpal }

testIsFlat2 :: Test 
testIsFlat2 = 
  let secpal = makeFact "Charlie can-say 0 Bob can-read(f)"
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
  let secpal = makeAssertion "A says B can-read(Foo)."
  in Test{ description=pShow secpal, result=test . safe $ secpal }

testSafe2 :: Test 
testSafe2 = 
  let secpal = makeAssertion "A says B can-read(Foo) if B can(x,y)."
  in Test{description=pShow secpal, result=test . safe $ secpal }

testSafe3 :: Test 
testSafe3 = 
  let secpal = makeAssertion "A says B can-read(Foo) if B can(x,y); ! x = y."
  in Test{description=pShow secpal, result=test . safe $ secpal }


testSafe4 :: Test 
testSafe4 = 
  let secpal = makeAssertion "A says B can(x,y) if B can(x,y)."
  in Test{description=pShow secpal, result=test . safe $ secpal }

testSafe5 :: Test 
testSafe5 = 
  let secpal = makeAssertion "A says z can(x,y) if z can(x,Foo), z can-read(y)."
  in Test{description=pShow secpal, result=test . safe $ secpal }

testSafe6 :: Test 
testSafe6 = 
  let secpal = makeAssertion "A says B can-say 0 x can(y,z)."
  in Test{description=pShow secpal, result=test . safe $ secpal }

 
testUnSafe1 :: Test 
testUnSafe1 =
  let secpal = makeAssertion "A says B can(x,Foo)."
  in Test{description=pShow secpal, result=test . not . safe $ secpal }

testUnSafe2 :: Test 
testUnSafe2 = 
  let secpal = makeAssertion "A says z can-read(Foo) if B can(x,y)."
  in Test{description=pShow secpal, result=test . not . safe $ secpal }

testUnSafe3 :: Test 
testUnSafe3 = 
  let secpal = makeAssertion "A says B can-read(Foo) if B can(x,y); ! w = y."
  in Test{description=pShow secpal, result=test . not . safe $ secpal }


testUnSafe4 :: Test 
testUnSafe4 = 
  let secpal = makeAssertion "A says B can(x,y) if B can-say 0 C can(x,y)."
  in Test{description=pShow secpal, result=test . not . safe $ secpal }


testUnSafe5 :: Test 
testUnSafe5 = 
  let secpal = makeAssertion "A says w can-say 0 x can(y,z)."
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
  let secpal = makeAssertion "anyone says app meets(policy) if evidence shows-meets(app, policy)."
  in Test{description=pShow secpal, result=test . safe $ secpal }

agTest2 :: Test 
agTest2 =
  let secpal = makeAssertion "Phone says app is-installable if app meets(NotMalware), app meets(NoInfoLeaks)."
  in Test{description=pShow secpal, result = test . safe $ secpal }

agTest3 :: Test 
agTest3 = 
  let secpal = makeAssertion "Phone says Google can-say inf app meets(NotMalware)."
  in Test{description=pShow secpal, result=test . safe $ secpal }

agTest4 :: Test 
agTest4 = 
  let secpal = makeAssertion "Google says AVChecker can-say 0 app meets(NotMalware)."
  in Test{description=pShow secpal, result=test . safe $ secpal }

agTest5 :: Test 
agTest5 = 
  let secpal = makeAssertion "Phone says NILInferer can-say 0 app meets(NoInfoLeaks)."
  in Test{description=pShow secpal, result=test . safe $ secpal }

agTest6 :: Test 
agTest6 = 
  let secpal = makeAssertion "AVChecker says Game meets(NotMalware)."
  in Test{description=pShow secpal, result=test . safe $ secpal }

agTest7 :: Test 
agTest7 =
  let secpal = makeAssertion "NILInferer says Evidence shows-meets(Game, Policy)."
  in Test{description=pShow secpal, result=test . safe $ secpal }



