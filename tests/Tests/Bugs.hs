module Tests.Bugs where
{- This is for testcases where I know something will go wrong later! -}

import Tests.Evaluation (makeAssertion, ppProof)

import Logic.SecPAL.Language
import Logic.SecPAL.Pretty
import Logic.General.Pretty()
import Logic.SecPAL.Evaluable
import Logic.SecPAL.Context
import Tests.Testable
import System.IO.Unsafe (unsafePerformIO)

infLoop :: Test
infLoop =
  let
    a = makeAssertion "Alice says Bob can-say inf I am-cool;"
    b = makeAssertion "Bob says Alice can-say inf I am-cool;"
    q = makeAssertion "Alice says I am-cool;"
    ctx = stdCtx{ ac=AC [a,b], d=Infinity }
    prf = unsafePerformIO $ ctx ||- q
    pPrf = ppProof prf
  in
    Test{ description = pShow ctx ++ " |= "++pShow q++pPrf
        , result = test . null $ prf
        }

infLoop2 :: Test
infLoop2 =
  let 
    a = makeAssertion "Alice says Alice likes-jazz if Alice likes-jazz;"
    q = makeAssertion "Alice says Alice likes-jazz;"
    ctx = stdCtx{ ac=AC [a], d=Infinity }
    prf = unsafePerformIO $ ctx ||- q
    pPrf = ppProof prf
  in
    Test{ description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
        , result = test . null $ prf
        }

renaming1 :: Test
renaming1 = 
  let a = makeAssertion "I says A is-cool if x is-cool, x is-rad;"
      b = makeAssertion "I says B is-cool;"
      c = makeAssertion "I says C is-rad;"
      q = makeAssertion "I says A is-cool;"
      ctx = stdCtx{ ac=AC [a, b, c] }
      prf = unsafePerformIO $ ctx ||- q
      pPrf = ppProof prf
  in
    Test{ description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
        , result = test . null $ prf
        }

