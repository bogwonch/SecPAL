module Tests.Evaluation where

import Logic.SecPAL.Language
import Logic.SecPAL.Pretty
import Logic.SecPAL.Parser
import Text.Parsec
import Logic.SecPAL.Evaluable
import Logic.SecPAL.Context
import Logic.SecPAL.Proof
import Tests.Testable
import Data.Either
import Data.Maybe


makeAssertion x = head . rights $ [ parse pAssertion "" x ]

testEvaluationTruths = [inACTest1, condNoRename1]
testEvaluationFalsehoods = [falseInACTest1, falseCondNoRename1]

testCanSay = [canSay01, canSayInf1]
testCanSayF = [canSayInfFalse1]

-- An assertion is true if it is in the assertion context
inACTest1 = 
  let 
    a = makeAssertion "Alice says Bob is-cool."
    ctx = Context { ac=AC [a], d=Infinity }
    prf = ctx ||- a
    pPrf = maybe "" (\p -> "\n"++pShow p) prf
  in
    Test{ description = pShow ctx ++" |= "++pShow a ++ pPrf
        , result = test . isJust $ prf
        }

falseInACTest1 = 
  let 
    a = makeAssertion "Alice says Alice is-cool."
    b = makeAssertion "Alice says Bob is-cool."
    ctx = Context { ac=AC [a], d=Infinity }
    prf = ctx ||- b
    pPrf = maybe "" (\p -> "\n"++pShow p) prf
  in
    Test{ description = pShow ctx ++" |= "++pShow b++pPrf
        , result = test . not . isJust $ prf
        }

-- Can we use the cond variable without renaming
condNoRename1 =
  let 
    a = makeAssertion "Alice says Bob is-cool."
    a' = makeAssertion "Alice says Bob is-cool if Bob likes-jazz."
    b = makeAssertion "Alice says Bob likes-jazz."
    ctx = Context { ac=AC [a', b], d=Infinity }
    prf = ctx ||- a
    pPrf = maybe "" (\p -> "\n"++pShow p) prf
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
    ctx = Context { ac=AC [a', b], d=Infinity }
    prf = ctx ||- a
    pPrf = maybe "" (\p -> "\n"++pShow p) prf
  in
    Test{ description = pShow ctx ++" |= "++pShow a++pPrf
        , result = test . not . isJust $ prf
        }

canSay01 =
  let q  = makeAssertion "Bob says Alice likes-jazz."
      a1 = makeAssertion "Bob says Alice can-say 0 Alice likes-jazz."
      a2 = makeAssertion "Alice says Alice likes-jazz."
      ctx = Context{ ac=AC [a1,a2], d=Infinity }
      prf = ctx ||- q
      pPrf = maybe "" (\p -> "\n"++pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isJust $ prf
          }

canSayInf1 =
  let q  = makeAssertion "Bob says Alice likes-jazz."
      a1 = makeAssertion "Bob says Alice can-say inf Alice likes-jazz."
      a2 = makeAssertion "Alice says Clive can-say 0 Alice likes-jazz."
      a3 = makeAssertion "Clive says Alice likes-jazz."
      ctx = Context{ ac=AC [a1,a2,a3], d=Infinity }
      prf = ctx ||- q
      pPrf = maybe "" (\p -> "\n"++pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . isJust $ prf
          }

canSayInfFalse1 =
  let q  = makeAssertion "Bob says Alice likes-jazz."
      a1 = makeAssertion "Bob says Alice can-say 0 Alice likes-jazz."
      a2 = makeAssertion "Alice says Clive can-say 0 Alice likes-jazz."
      a3 = makeAssertion "Clive says Alice likes-jazz."
      ctx = Context{ ac=AC [a1,a2,a3], d=Infinity }
      prf = ctx ||- q
      pPrf = maybe "" (\p -> "\n"++pShow p) prf
  in Test { description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
          , result = test . not . isJust $ prf
          }

