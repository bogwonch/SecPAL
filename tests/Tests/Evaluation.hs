module Tests.Evaluation where

import Logic.SecPAL.Language
import Logic.SecPAL.Pretty
import Logic.General.Pretty()
import Logic.SecPAL.Parser
import Logic.SecPAL.Proof
import Text.Parsec
import Logic.SecPAL.Evaluable
import Logic.SecPAL.Context
import Tests.Testable
import System.IO.Unsafe (unsafePerformIO)

ppProof :: PShow x => [Proof x] -> String
ppProof [] = ""
ppProof p  = ('\n':) . pShow . head $ p

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
    pPrf = ppProof prf
  in
    Test{ description = pShow ctx ++" |= "++pShow a ++ pPrf
        , result = test . not . null $ prf
        }

falseInACTest1 :: Test 
falseInACTest1 = 
  let 
    a = makeAssertionUnsafe "Alice says Alice is-cool;"
    b = makeAssertionUnsafe "Alice says Bob is-cool;"
    ctx = stdCtx{ ac=AC [a], d=Infinity }
    prf = unsafePerformIO $ ctx ||- b
    pPrf = ppProof prf
  in
    Test{ description = pShow ctx ++" |= "++pShow b++pPrf
        , result = test . null $ prf
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
    pPrf = ppProof prf
  in
    Test{ description = pShow ctx ++" |= "++pShow a++pPrf
        , result = test . not . null $ prf
        }
        --

falseCondNoRename1 :: Test 
falseCondNoRename1 =
  let 
    a = makeAssertionUnsafe "Alice says Bob is-cool;"
    a' = makeAssertionUnsafe "Alice says Bob is-cool if Bob likes-jazz;"
    b = makeAssertionUnsafe "Alice says Bob likes-jazz: False;"
    ctx = stdCtx{ ac=AC [a', b], d=Infinity }
    prf = unsafePerformIO $ ctx ||- a
    pPrf = ppProof prf
  in
    Test{ description = pShow ctx ++" |= "++pShow a++pPrf
        , result = test . null $ prf
        }

canSay01 :: Test 
canSay01 =
  let q  = makeAssertionUnsafe "Bob says Alice likes-jazz;"
      a1 = makeAssertionUnsafe "Bob says Alice can-say 0 Alice likes-jazz;"
      a2 = makeAssertionUnsafe "Alice says Alice likes-jazz;"
      ctx = stdCtx{ ac=AC [a1,a2], d=Infinity }
      prf = unsafePerformIO $ ctx ||- q
      pPrf = ppProof prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . not . null $ prf
          }

canSayInf1 :: Test 
canSayInf1 =
  let q  = makeAssertionUnsafe "Bob says Alice likes-jazz;"
      a1 = makeAssertionUnsafe "Bob says Alice can-say inf Alice likes-jazz;"
      a2 = makeAssertionUnsafe "Alice says Clive can-say 0 Alice likes-jazz;"
      a3 = makeAssertionUnsafe "Clive says Alice likes-jazz;"
      ctx = stdCtx{ ac=AC [a1,a2,a3], d=Infinity }
      prf = unsafePerformIO $ ctx ||- q
      pPrf = ppProof prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . not . null $ prf
          }

canSayInfFalse1 :: Test 
canSayInfFalse1 =
  let q  = makeAssertionUnsafe "Bob says Alice likes-jazz;"
      a1 = makeAssertionUnsafe "Bob says Alice can-say 0 Alice likes-jazz;"
      a2 = makeAssertionUnsafe "Alice says Clive can-say 0 Alice likes-jazz;"
      a3 = makeAssertionUnsafe "Clive says Alice likes-jazz;"
      ctx = stdCtx{ ac=AC [a1,a2,a3], d=Infinity }
      prf = unsafePerformIO $ ctx ||- q
      pPrf = ppProof prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . null $ prf
          }


condRename1 :: Test 
condRename1 = 
  let q    = makeAssertionUnsafe "Bob says Alice likes-jazz;"
      a1   = makeAssertionUnsafe "Bob says everyone likes-jazz;"
      ctx  = stdCtx{ ac=AC [a1], d=Infinity }
      prf  = unsafePerformIO $ ctx ||- q
      pPrf = ppProof prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . not . null $ prf
          }

condRename2 :: Test 
condRename2 = 
  let q = makeAssertionUnsafe "Bob says Alice likes-jazz;"
      a1 = makeAssertionUnsafe "Bob says everyone likes-jazz if everyone is-human;"
      a2 = makeAssertionUnsafe "Bob says Alice is-human;"
      ctx = stdCtx{ ac=AC [a1, a2], d=Infinity }
      prf = unsafePerformIO $ ctx ||- q
      pPrf = ppProof prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . not . null $ prf
          }

canSayRename1 :: Test 
canSayRename1 = 
  let q = makeAssertionUnsafe "Bob says Alice likes-jazz;"
      a1 = makeAssertionUnsafe "Bob says anyone can-say 0 anyone likes-jazz;"
      a2 = makeAssertionUnsafe "Alice says Alice likes-jazz;"
      ctx = stdCtx{ ac=AC [a1, a2], d=Infinity }
      prf = unsafePerformIO $ ctx ||- q
      pPrf = ppProof prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . not . null $ prf
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
      pPrf = ppProof prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . not . null $ prf
          }


testHasPermission :: Test 
testHasPermission = 
  let q  = makeAssertion "User says apk#App can-access-internet;"
      a1 = makeAssertionUnsafe "anyone says apk#app can-access-internet: permissionsCheck(apk#app, \"INTERNET\") = True;"
      ctx = stdCtx{ac=AC [a1]}
      prf = unsafePerformIO $ ctx ||- q
      pPrf = ppProof prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . not . null $ prf
          }

testHasntPermission :: Test 
testHasntPermission = 
  let q  = makeAssertion "User says apk#App cannot-access-internet;"
      a1 = makeAssertionUnsafe "anyone says apk#app cannot-access-internet: permissionsCheck(apk#app, \"INTERNET\") = False;"
      ctx = stdCtx{ac=AC [a1]}
      prf = unsafePerformIO $ ctx ||- q
      pPrf = ppProof prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . null $ prf
          }

testHasntPermission2 :: Test 
testHasntPermission2 = 
  let q  = makeAssertion "User says apk#App cannot-dance;"
      a1 = makeAssertionUnsafe "anyone says#apk@app cannot-dance: permissionsCheck(apk#app, \"BOOGIE\") = False;"
      ctx = stdCtx{ac=AC [a1]}
      prf = unsafePerformIO $ ctx ||- q
      pPrf = ppProof prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . not . null $ prf
          }
        

-- Tests for Can-act-as
testCanActAs1 :: Test
testCanActAs1 = 
  let q = makeAssertion "A says B okay;"
      a = makeAssertion "A says B can-act-as C;"
      b = makeAssertion "A says C okay;"
      ctx = stdCtx{ac=AC [a,b]}
      prf = unsafePerformIO $ ctx ||- q
      pPrf = ppProof prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . not . null $ prf
          }
