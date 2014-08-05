module Tests.ArbitraryDatalog where

import Logic.General.Entities as D
import Logic.General.Parser
import Logic.DatalogC.Pretty
import Logic.General.Pretty
import Logic.DatalogC.Language as D
import Logic.DatalogC.Parser
import Logic.DatalogC.Safety
import Control.Applicative
import Test.QuickCheck
import Tests.ArbitraryParser ( qc
                             , arbitraryTokenChar
                             , propParsable
                             )
import Tests.Testable hiding (Testable)
import Text.Parsec

import Debug.Trace
import System.IO.Unsafe

testDatalogQC :: IO [Test]
testDatalogQC = sequence [ qc "Entity" propParsableEntity
                         , qc "Predicate" propParsablePredicate
                         , qc "Clause" propParsableClause
                         ]

propParsableEntity :: D.E -> Bool
propParsableEntity = propParsable pE

propParsablePredicate :: D.Predicate -> Bool
propParsablePredicate = propParsable pPredicate

instance Arbitrary Predicate where
  arbitrary = Predicate <$> listOf1 arbitraryTokenChar <*> arbitrary

propParsableClause :: D.Clause -> Bool
propParsableClause = propParsable pClause

instance Arbitrary Clause where
  arbitrary = suchThat (Clause <$> arbitrary <*> arbitrary <*> arbitrary) safe 
