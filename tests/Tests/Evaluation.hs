module Tests.Evaluation where

import Logic.SecPAL.Language
import Logic.SecPAL.Pretty
import Logic.General.Pretty
import Logic.SecPAL.Parser
import Text.Parsec
import Logic.SecPAL.Evaluable
import Logic.SecPAL.Context
import Tests.Testable
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)

makeAssertionUnsafe :: String -> Assertion
makeAssertionUnsafe x = case parse pAssertionUnsafe "" x of
  (Left err) -> error . show $ err
  (Right a) -> a

makeAssertion :: String -> Assertion
makeAssertion x = case parse pAssertion "" x of
  (Left err) -> error . show $ err
  (Right a) -> a

testEvaluationTruths :: [Test] 
testEvaluationTruths = [inACTest1, condNoRename1]
testEvaluationFalsehoods :: [Test] 
testEvaluationFalsehoods = [falseInACTest1, falseCondNoRename1]

testCanSay :: [Test] 
testCanSay = [canSay01, canSayInf1]
testCanSayF :: [Test] 
testCanSayF = [canSayInfFalse1]

testRenamingEval :: [Test] 
testRenamingEval = [ condRename1, condRename2 , canSayRename1, testESSoSExample ]

testFunctions :: [Test] 
testFunctions = [ testHasPermission]--, testHasntPermission, testHasntPermission2 ]

testCanActAs :: [Test]
testCanActAs = [ testCanActAs1 ]

-- An assertion is true if it is in the assertion context
inACTest1 :: Test 
inACTest1 = 
  let 
    a = makeAssertionUnsafe "Alice says Bob is-cool;"
    ctx = stdCtx{ ac=AC [a], d=Infinity }
    prf = unsafePerformIO $ ctx ||- a
    pPrf = maybe "" (\p -> '\n':pShow p) prf
  in
    Test{ description = pShow ctx ++" |= "++pShow a ++ pPrf
        , result = test . isJust $ prf
        }

falseInACTest1 :: Test 
falseInACTest1 = 
  let 
    a = makeAssertionUnsafe "Alice says Alice is-cool;"
    b = makeAssertionUnsafe "Alice says Bob is-cool;"
    ctx = stdCtx{ ac=AC [a], d=Infinity }
    prf = unsafePerformIO $ ctx ||- b
    pPrf = maybe "" (\p -> '\n':pShow p) prf
  in
    Test{ description = pShow ctx ++" |= "++pShow b++pPrf
        , result = test . isNothing $ prf
        }

-- Can we use the cond variable without renaming
condNoRename1 :: Test 
condNoRename1 =
  let 
    a = makeAssertionUnsafe "Alice says Bob is-cool;"
    a' = makeAssertionUnsafe "Alice says Bob is-cool if Bob likes-jazz;"
    b = makeAssertionUnsafe "Alice says Bob likes-jazz;"
    ctx = stdCtx{ ac=AC [a', b], d=Infinity }
    prf = unsafePerformIO $ ctx ||- a
    pPrf = maybe "" (\p -> '\n':pShow p) prf
  in
    Test{ description = pShow ctx ++" |= "++pShow a++pPrf
        , result = test . isJust $ prf
        }
        --

falseCondNoRename1 :: Test 
falseCondNoRename1 =
  let 
    a = makeAssertionUnsafe "Alice says Bob is-cool;"
    a' = makeAssertionUnsafe "Alice says Bob is-cool if Bob likes-jazz;"
    b = makeAssertionUnsafe "Alice says Bob likes-jazz; False;"
    ctx = stdCtx{ ac=AC [a', b], d=Infinity }
    prf = unsafePerformIO $ ctx ||- a
    pPrf = maybe "" (\p -> '\n':pShow p) prf
  in
    Test{ description = pShow ctx ++" |= "++pShow a++pPrf
        , result = test . isNothing $ prf
        }

canSay01 :: Test 
canSay01 =
  let q  = makeAssertionUnsafe "Bob says Alice likes-jazz;"
      a1 = makeAssertionUnsafe "Bob says Alice can-say 0 Alice likes-jazz;"
      a2 = makeAssertionUnsafe "Alice says Alice likes-jazz;"
      ctx = stdCtx{ ac=AC [a1,a2], d=Infinity }
      prf = unsafePerformIO $ ctx ||- q
      pPrf = maybe "" (\p -> '\n':pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isJust $ prf
          }

canSayInf1 :: Test 
canSayInf1 =
  let q  = makeAssertionUnsafe "Bob says Alice likes-jazz;"
      a1 = makeAssertionUnsafe "Bob says Alice can-say inf Alice likes-jazz;"
      a2 = makeAssertionUnsafe "Alice says Clive can-say 0 Alice likes-jazz;"
      a3 = makeAssertionUnsafe "Clive says Alice likes-jazz;"
      ctx = stdCtx{ ac=AC [a1,a2,a3], d=Infinity }
      prf = unsafePerformIO $ ctx ||- q
      pPrf = maybe "" (\p -> '\n':pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isJust $ prf
          }

canSayInfFalse1 :: Test 
canSayInfFalse1 =
  let q  = makeAssertionUnsafe "Bob says Alice likes-jazz;"
      a1 = makeAssertionUnsafe "Bob says Alice can-say 0 Alice likes-jazz;"
      a2 = makeAssertionUnsafe "Alice says Clive can-say 0 Alice likes-jazz;"
      a3 = makeAssertionUnsafe "Clive says Alice likes-jazz;"
      ctx = stdCtx{ ac=AC [a1,a2,a3], d=Infinity }
      prf = unsafePerformIO $ ctx ||- q
      pPrf = maybe "" (\p -> '\n':pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isNothing $ prf
          }


condRename1 :: Test 
condRename1 = 
  let q    = makeAssertionUnsafe "Bob says Alice likes-jazz;"
      a1   = makeAssertionUnsafe "Bob says everyone likes-jazz;"
      ctx  = stdCtx{ ac=AC [a1], d=Infinity }
      prf  = unsafePerformIO $ ctx ||- q
      pPrf = maybe "" (\p -> '\n':pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isJust $ prf
          }

condRename2 :: Test 
condRename2 = 
  let q = makeAssertionUnsafe "Bob says Alice likes-jazz;"
      a1 = makeAssertionUnsafe "Bob says everyone likes-jazz if everyone is-human;"
      a2 = makeAssertionUnsafe "Bob says Alice is-human;"
      ctx = stdCtx{ ac=AC [a1, a2], d=Infinity }
      prf = unsafePerformIO $ ctx ||- q
      pPrf = maybe "" (\p -> '\n':pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isJust $ prf
          }

canSayRename1 :: Test 
canSayRename1 = 
  let q = makeAssertionUnsafe "Bob says Alice likes-jazz;"
      a1 = makeAssertionUnsafe "Bob says anyone can-say 0 anyone likes-jazz;"
      a2 = makeAssertionUnsafe "Alice says Alice likes-jazz;"
      ctx = stdCtx{ ac=AC [a1, a2], d=Infinity }
      prf = unsafePerformIO $ ctx ||- q
      pPrf = maybe "" (\p -> '\n':pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isJust $ prf
          }

testESSoSExample :: Test 
testESSoSExample =
  let q  = makeAssertion "Phone says Game is-installable;"
      a3 = makeAssertion "Phone says app is-installable if app meets(NotMalware), app meets(NoInfoLeaks);"
      a4 = makeAssertion "anyone says app meets(policy) if evidence shows-meets(app, policy);"
      a5 = makeAssertion "Phone says NILInferer can-say 0 app meets(NoInfoLeaks);"
      a6 = makeAssertion "Phone says Google can-say inf app meets(NotMalware);"
      a7 = makeAssertion "Google says AVChecker can-say 0 app meets(NotMalware);"
      a8 = makeAssertion "AVChecker says Game meets(NotMalware);"
      a9 = makeAssertion "NILInferer says Evidence shows-meets(Game,NoInfoLeaks);" -- Bit simplified
      ctx = stdCtx{ac=AC [a3, a4, a5, a6, a7, a8, a9]}
      prf = unsafePerformIO $ ctx ||- q
      pPrf = maybe "" (\p -> '\n':pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isJust $ prf
          }


testHasPermission :: Test 
testHasPermission = 
  let q  = makeAssertion "User says App can-access-internet;"
      a1 = makeAssertionUnsafe "anyone says app can-access-internet: permissionsCheck(apk#app, \"INTERNET\") = True;"
      ctx = stdCtx{ac=AC [a1]}
      prf = unsafePerformIO $ ctx ||- q
      pPrf = maybe "" (\p -> '\n':pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isJust $ prf
          }

testHasntPermission :: Test 
testHasntPermission = 
  let q  = makeAssertion "User says App cannot-access-internet;"
      a1 = makeAssertionUnsafe "anyone says app cannot-access-internet: permissionsCheck(apk#app, \"INTERNET\") = False;"
      ctx = stdCtx{ac=AC [a1]}
      prf = unsafePerformIO $ ctx ||- q
      pPrf = maybe "" (\p -> '\n':pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isNothing $ prf
          }

testHasntPermission2 :: Test 
testHasntPermission2 = 
  let q  = makeAssertion "User says App cannot-dance;"
      a1 = makeAssertionUnsafe "anyone says app cannot-dance: permissionsCheck(apk#app, \"BOOGIE\") = False;"
      ctx = stdCtx{ac=AC [a1]}
      prf = unsafePerformIO $ ctx ||- q
      pPrf = maybe "" (\p -> '\n':pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isJust $ prf
          }
        

-- Tests for Can-act-as
testCanActAs1 :: Test
testCanActAs1 = 
  let q = makeAssertion "A says B okay;"
      a = makeAssertion "A says B can-act-as C;"
      b = makeAssertion "A says C okay;"
      ctx = stdCtx{ac=AC [a,b]}
      prf = unsafePerformIO $ ctx ||- q
      pPrf = maybe "" (\p -> '\n':pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isJust $ prf
          }
