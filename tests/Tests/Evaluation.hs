module Tests.Evaluation where

import Logic.SecPAL.Language
import Logic.SecPAL.Pretty
import Logic.SecPAL.Parser
import Text.Parsec
import Logic.SecPAL.Evaluable
import Logic.SecPAL.Context
import Tests.Testable
import Data.Either
import Data.Maybe


makeAssertion :: String -> Assertion
makeAssertion x = head . rights $ [ parse pAssertion "" x ]

testEvaluationTruths = [inACTest1, condNoRename1]
testEvaluationFalsehoods = [falseInACTest1, falseCondNoRename1]

testCanSay = [canSay01, canSayInf1]
testCanSayF = [canSayInfFalse1]

testRenamingEval = [ condRename1, condRename2 , canSayRename1, testESSoSExample ]

-- An assertion is true if it is in the assertion context
inACTest1 = 
  let 
    a = makeAssertion "Alice says Bob is-cool."
    ctx = stdCtx{ ac=AC [a], d=Infinity }
    prf = ctx ||- a
    pPrf = maybe "" (\p -> '\n':pShow p) prf
  in
    Test{ description = pShow ctx ++" |= "++pShow a ++ pPrf
        , result = test . isJust $ prf
        }

falseInACTest1 = 
  let 
    a = makeAssertion "Alice says Alice is-cool."
    b = makeAssertion "Alice says Bob is-cool."
    ctx = stdCtx{ ac=AC [a], d=Infinity }
    prf = ctx ||- b
    pPrf = maybe "" (\p -> '\n':pShow p) prf
  in
    Test{ description = pShow ctx ++" |= "++pShow b++pPrf
        , result = test . isNothing $ prf
        }

-- Can we use the cond variable without renaming
condNoRename1 =
  let 
    a = makeAssertion "Alice says Bob is-cool."
    a' = makeAssertion "Alice says Bob is-cool if Bob likes-jazz."
    b = makeAssertion "Alice says Bob likes-jazz."
    ctx = stdCtx{ ac=AC [a', b], d=Infinity }
    prf = ctx ||- a
    pPrf = maybe "" (\p -> '\n':pShow p) prf
  in
    Test{ description = pShow ctx ++" |= "++pShow a++pPrf
        , result = test . isJust $ prf
        }
        --

falseCondNoRename1 =
  let 
    a = makeAssertion "Alice says Bob is-cool."
    a' = makeAssertion "Alice says Bob is-cool if Bob likes-jazz."
    b = makeAssertion "Alice says Bob likes-jazz; False."
    ctx = stdCtx{ ac=AC [a', b], d=Infinity }
    prf = ctx ||- a
    pPrf = maybe "" (\p -> '\n':pShow p) prf
  in
    Test{ description = pShow ctx ++" |= "++pShow a++pPrf
        , result = test . isNothing $ prf
        }

canSay01 =
  let q  = makeAssertion "Bob says Alice likes-jazz."
      a1 = makeAssertion "Bob says Alice can-say 0 Alice likes-jazz."
      a2 = makeAssertion "Alice says Alice likes-jazz."
      ctx = stdCtx{ ac=AC [a1,a2], d=Infinity }
      prf = ctx ||- q
      pPrf = maybe "" (\p -> '\n':pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isJust $ prf
          }

canSayInf1 =
  let q  = makeAssertion "Bob says Alice likes-jazz."
      a1 = makeAssertion "Bob says Alice can-say inf Alice likes-jazz."
      a2 = makeAssertion "Alice says Clive can-say 0 Alice likes-jazz."
      a3 = makeAssertion "Clive says Alice likes-jazz."
      ctx = stdCtx{ ac=AC [a1,a2,a3], d=Infinity }
      prf = ctx ||- q
      pPrf = maybe "" (\p -> '\n':pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isJust $ prf
          }

canSayInfFalse1 =
  let q  = makeAssertion "Bob says Alice likes-jazz."
      a1 = makeAssertion "Bob says Alice can-say 0 Alice likes-jazz."
      a2 = makeAssertion "Alice says Clive can-say 0 Alice likes-jazz."
      a3 = makeAssertion "Clive says Alice likes-jazz."
      ctx = stdCtx{ ac=AC [a1,a2,a3], d=Infinity }
      prf = ctx ||- q
      pPrf = maybe "" (\p -> '\n':pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isNothing $ prf
          }


condRename1 = 
  let q    = makeAssertion "Bob says Alice likes-jazz."
      a1   = makeAssertion "Bob says everyone likes-jazz."
      ctx  = stdCtx{ ac=AC [a1], d=Infinity }
      prf  = ctx ||- q
      pPrf = maybe "" (\p -> '\n':pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isJust $ prf
          }

condRename2 = 
  let q = makeAssertion "Bob says Alice likes-jazz."
      a1 = makeAssertion "Bob says everyone likes-jazz if everyone is-human."
      a2 = makeAssertion "Bob says Alice is-human."
      ctx = stdCtx{ ac=AC [a1, a2], d=Infinity }
      prf = ctx ||- q
      pPrf = maybe "" (\p -> '\n':pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isJust $ prf
          }

canSayRename1 = 
  let q = makeAssertion "Bob says Alice likes-jazz."
      a1 = makeAssertion "Bob says anyone can-say 0 anyone likes-jazz."
      a2 = makeAssertion "Alice says Alice likes-jazz."
      ctx = stdCtx{ ac=AC [a1, a2], d=Infinity }
      prf = ctx ||- q
      pPrf = maybe "" (\p -> '\n':pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isJust $ prf
          }

testESSoSExample =
  let q = makeAssertion "Phone says Game is-installable."
      a3 = makeAssertion "Phone says app is-installable if app meets(NotMalware), app meets(NoInfoLeaks)."
      a4 = makeAssertion "anyone says app meets(policy) if evidence shows-meets(app, policy)."
      a5 = makeAssertion "Phone says NILInferer can-say 0 app meets(NoInfoLeaks)."
      a6 = makeAssertion "Phone says Google can-say inf app meets(NotMalware)."
      a7 = makeAssertion "Google says AVChecker can-say 0 app meets(NotMalware)."
      a8 = makeAssertion "AVChecker says Game meets(NotMalware)."
      a9 = makeAssertion "NILInferer says Evidence shows-meets(Game,NoInfoLeaks)." -- Bit simplified
      ctx = stdCtx{ac=AC [a3, a4, a5, a6, a7, a8, a9]}
      prf = ctx ||- q
      pPrf = maybe "" (\p -> '\n':pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isJust $ prf
          }
