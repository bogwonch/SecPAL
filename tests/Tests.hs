module Main where

import Tests.AssertionSafety
import Tests.Evaluation
import Tests.TestResults
import Tests.Parser
import Tests.Testable
import Tests.ArbitraryParser
import System.Console.ANSI
import Test.QuickCheck
import Data.Function

cTitle = setSGRCode [ SetColor Foreground Vivid Black]
cSome = setSGRCode [ SetColor Foreground Dull Yellow]
cPass = setSGRCode [ SetColor Foreground Dull Green]
cFail = setSGRCode [ SetColor Foreground Dull Red]

cNormal = setSGRCode [Reset]

runTests :: String -> [Test] -> IO ()
runTests name ts = 
  let passes = length [ t | t <- ts, result t == TestPassed ]
      fails  = length [ t | t <- ts, result t /= TestPassed ]
  in do
    putStr $  cTitle ++ "✱ "++cNormal
    showScore passes fails
    putStrLn $ ' ':name
    mapM_ print ts
    putStrLn ""

showScore p f = 
  let t = p + f
      len = 10
      pp = (len * p) `quot` t
      fp = len - pp
  in do
    putStr $ cPass ++ replicate pp '█'
    putStr $ cFail ++ replicate fp '█'
    putStr cNormal


main = do
  runTests "AssertionSafety/flat" testFlatness
  runTests "AssertionSafety/safe" testSafe
  runTests "AssertionSafety/unsafe" testUnsafe
  runTests "AssertionSafety/ESSoS" testESSoS
  runTests "Evaluation/Truths" testEvaluationTruths
  runTests "Evaluation/Falsehoods" testEvaluationFalsehoods
  runTests "Parser/All" testParser


  quickCheckResult propParsableE
  quickCheck propParsableD
  {-
  quickCheck propParsableVerbPhrase
  quickCheck propParsableFact
  quickCheck propParsableClaim
  quickCheck propParsableAssertion
  quickCheck propParsableEc
  quickCheck propParsableC
  quickCheck propParsableValue
-}
