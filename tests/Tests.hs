module Main where

import Tests.AssertionSafety
import Tests.Testable

tests :: [Test]
tests = testFlatness ++ testSafety

main = do
  mapM_ print tests
