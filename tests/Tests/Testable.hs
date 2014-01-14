module Tests.Testable where

import Tests.TestResults

data Test = Test
  { description :: String
  , result :: TestResult
  }

instance Show Test where
  show t = (show.result $ t) ++ " " ++ description t

class Testable a where
  test :: a -> TestResult

instance Testable Bool where
  test True = TestPassed
  test False = TestFailed Nothing
