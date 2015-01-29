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

canSayInterference :: Test
canSayInterference =
  let a0 = makeAssertion "I says U can-say 0 a can(b);"
      a1 = makeAssertion "U says A can(X);"
      a2 = makeAssertion "U says A can(Y);"
      a3 = makeAssertion "I says This Works if A can(X), A can(Y);"
      q = makeAssertion "I says This Works;"
      ctx = stdCtx{ ac=AC [a0, a1, a2, a3] }
      prf = unsafePerformIO $ ctx ||- q
      pPrf = ppProof prf
  in
    Test{ description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
        , result = test . not . null $ prf
        }

justWeird0 :: Test
justWeird0 = 
  let a0 = makeAssertion "I says x is-good if x likes(A), x likes(B);"
      a1 = makeAssertion "I says A likes(y) if y is-cool;"
      a2 = makeAssertion "I says A is-cool;"
      a3 = makeAssertion "I says B is-cool;"
      q = makeAssertion "I says A is-good;"
      ctx = stdCtx{ ac=AC [a0,a1,a2,a3] }
      prf = unsafePerformIO $ ctx ||- q
      pPrf = ppProof prf
  in
    Test{ description = pShow ctx ++ " |= " ++ pShow q ++ pPrf
        , result = test . not . null $ prf
        }
