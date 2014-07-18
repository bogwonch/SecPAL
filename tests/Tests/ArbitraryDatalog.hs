module Tests.ArbitraryDatalog where

import Logic.DatalogC.Language as D
import Logic.DatalogC.Parser
import Logic.DatalogC.Safety
import Control.Applicative
import Test.QuickCheck
import Tests.ArbitraryParser ( qc
                             , arbitraryUpper
                             , arbitraryLower
                             , arbitraryTokenChar
                             )
import Tests.Testable hiding (Testable)
import Text.Parsec

propParsable :: Show a => Parsec String () a -> a -> Bool
propParsable parser datalog = 
  case parse parser "" (show datalog) of 
    (Left _)         -> False
    (Right datalog') -> show datalog == show datalog'

testDatalogQC :: IO [Test]
testDatalogQC = sequence [ qc "Entity" propParsableEntity
                         , qc "Predicate" propParsablePredicate
                         , qc "Clause" propParsableClause
                         ]

propParsableEntity :: D.Entity -> Bool
propParsableEntity = propParsable pEntity

instance Arbitrary Entity where
  arbitrary = oneof [ Variable <$> ((:) <$> arbitraryLower <*> listOf arbitraryTokenChar)
                    , Constant <$> ((:) <$> arbitraryUpper <*> listOf arbitraryTokenChar)
                    -- TODO: String constants
                    ]

propParsablePredicate :: D.Predicate -> Bool
propParsablePredicate = propParsable pPredicate

instance Arbitrary Predicate where
  arbitrary = Predicate <$> listOf1 arbitraryTokenChar <*> arbitrary

propParsableClause :: D.Clause -> Bool
propParsableClause = propParsable pClause

instance Arbitrary Clause where
  arbitrary = suchThat (Clause <$> arbitrary <*> arbitrary <*> arbitrary) safe 

instance Arbitrary Constraint where
  arbitrary = oneof [ Boolean <$> arbitrary ]
