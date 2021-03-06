module Main where

import System.Console.ANSI
import Tests.ArbitraryParser
import Tests.ArbitraryDatalog
import Tests.AssertionSafety
import Tests.Bugs
import Tests.Evaluation
import Tests.TestResults
import Tests.Testable

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

runTestsM name ts = ts >>= runTests name
showScore p f = 
  let t = p + f
      len = 10
      pp = (len * p) `quot` t
      fp = len - pp
  in do
    putStr $ cPass ++ replicate pp '█'
    putStr $ cFail ++ replicate fp '█'
    putStr cNormal

main :: IO ()
main = do
  runTests "AssertionSafety/flat" testFlatness
  runTests "AssertionSafety/safe" testSafe
  runTests "AssertionSafety/unsafe" testUnsafe
  runTests "AssertionSafety/ESSoS" testESSoS
  runTests "Evaluation/Truths" testEvaluationTruths
  runTests "Evaluation/Falsehoods" testEvaluationFalsehoods
  runTests "Evaluation/Can-Say" testCanSay
  runTests "Evaluation/Can-Say Falsehoods" testCanSayF
  runTests "Evaluation/Renaming" testRenamingEval
  runTests "Evaluation/Can-Act-As" testCanActAs
  runTests "Evaluation/Functions" testFunctions
  runTestsM "Parser/QuickCheck" testParserQC
  runTestsM "Datalog/Parser" testDatalogQC
  runTests "Bugs/Infinite-Loop-cansay" [infLoop]
  --runTests "Bugs/Infinite-Loop-cond" [infLoop2]
  runTests "Bugs/Renaming-cond" [renaming1]
  runTests "Bugs/Can-say-interference" [canSayInterference]
  runTests "Bugs/Weird" [justWeird0]
