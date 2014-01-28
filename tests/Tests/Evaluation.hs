module Tests.Evaluation where

import Logic.SecPAL.Language
import Logic.SecPAL.Pretty
import Logic.SecPAL.Parser
import Text.Parsec
import Logic.SecPAL.Evaluable
import Logic.SecPAL.Context
import Tests.Testable
import Data.Either

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
  in
    Test{ description = pShow ctx ++" |= "++pShow a
        , result = test $ ctx ||- a
        }

falseInACTest1 = 
  let 
    a = makeAssertion "Alice says Alice is-cool."
    b = makeAssertion "Alice says Bob is-cool."
    ctx = Context { ac=AC [a], d=Infinity }
  in
    Test{ description = pShow ctx ++" |= "++pShow b
        , result = test . not $ ctx ||- b
        }

-- Can we use the cond variable without renaming
condNoRename1 =
  let 
    a = makeAssertion "Alice says Bob is-cool."
    a' = makeAssertion "Alice says Bob is-cool if Bob likes-jazz."
    b = makeAssertion "Alice says Bob likes-jazz."
    ctx = Context { ac=AC [a', b], d=Infinity }
  in
    Test{ description = pShow ctx ++" |= "++pShow a
        , result = test $ ctx ||- a
        }
        --

falseCondNoRename1 =
  let 
    a = makeAssertion "Alice says Bob is-cool."
    a' = makeAssertion "Alice says Bob is-cool if Bob likes-jazz."
    b = makeAssertion "Alice says Bob likes-jazz; False."
    ctx = Context { ac=AC [a', b], d=Infinity }
  in
    Test{ description = pShow ctx ++" |= "++pShow a
        , result = test . not $ ctx ||- a
        }

canSay01 =
  let q  = makeAssertion "Bob says Alice likes-jazz."
      a1 = makeAssertion "Bob says Alice can-say 0 Alice likes-jazz."
      a2 = makeAssertion "Alice says Alice likes-jazz."
      ctx = Context{ ac=AC [a1,a2], d=Infinity }
  in Test { description = pShow ctx ++ " |= " ++ pShow q
          , result = test $ ctx ||- q
          }

canSayInf1 =
  let q  = makeAssertion "Bob says Alice likes-jazz."
      a1 = makeAssertion "Bob says Alice can-say inf Alice likes-jazz."
      a2 = makeAssertion "Alice says Clive can-say 0 Alice likes-jazz."
      a3 = makeAssertion "Clive says Alice likes-jazz."
      ctx = Context{ ac=AC [a1,a2,a3], d=Infinity }
  in Test { description = pShow ctx ++ " |= " ++ pShow q
          , result = test $ ctx ||- q
          }

canSayInfFalse1 =
  let q  = makeAssertion "Bob says Alice likes-jazz."
      a1 = makeAssertion "Bob says Alice can-say 0 Alice likes-jazz."
      a2 = makeAssertion "Alice says Clive can-say 0 Alice likes-jazz."
      a3 = makeAssertion "Clive says Alice likes-jazz."
      ctx = Context{ ac=AC [a1,a2,a3], d=Infinity }
  in Test { description = pShow ctx ++ " |= " ++ pShow q
          , result = test . not $ ctx ||- q
          }

