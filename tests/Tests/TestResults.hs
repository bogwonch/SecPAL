module Tests.TestResults where

import System.Console.ANSI

data TestResult = TestPassed
                | TestFailed (Maybe String)
  deriving (Eq)

cReset :: String
cReset = setSGRCode [Reset]
cGreen :: String
cGreen = setSGRCode [ SetColor Foreground Dull Green ]
cRed :: String
cRed = setSGRCode [ SetColor Foreground Dull Red ]

instance Show TestResult where
  show TestPassed = cGreen ++ "✔" ++ cReset
  show (TestFailed Nothing) = cRed ++ "✘" ++ cReset
  show (TestFailed (Just reason)) = cRed ++ "✘" ++ reason ++ cReset

