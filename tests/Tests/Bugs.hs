module Tests.Bugs where
{- This is for testcases where I know something will go wrong later! -}

import Tests.Evaluation (makeAssertion)

import Logic.SecPAL.Language
import Logic.SecPAL.Pretty
import Logic.General.Pretty
import Logic.SecPAL.Evaluable
import Logic.SecPAL.Context
import Tests.Testable
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)

testBugs :: [Test]
testBugs = [infLoop]

infLoop :: Test
infLoop =
  let
    a = makeAssertion "Alice says Bob can-say inf I am-cool."
    b = makeAssertion "Bob says Alice can-say inf I am-cool."
    q = makeAssertion "Alice says I am-cool."
    ctx = stdCtx{ ac=AC [a,b], d=Infinity }
    prf = unsafePerformIO $ ctx ||- q
    pPrf = maybe "" (\p -> '\n':pShow p) prf
  in
    Test{ description = pShow ctx ++ " |= "++pShow q++pPrf
        , result = test . isNothing $ prf
        }

infLoop2 :: Test
infLoop2 =
  let 
    a = makeAssertion "Alice says Alice likes-jazz if Alice likes-jazz."
    q = makeAssertion "Alice says Alice likes-jazz."
    ctx = stdCtx{ ac=AC [a], d=Infinity }
    prf = unsafePerformIO $ ctx ||- q
    pPrf = maybe "" (\p -> '\n':pShow p) prf
  in
    Test{ description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
        , result = test . isNothing $ prf
        }
