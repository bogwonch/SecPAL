module Tests.ArbitraryParser where

import Logic.SecPAL.Language
import Test.QuickCheck
import Control.Applicative
import Data.Char
import Text.Parsec
import Logic.SecPAL.Parser

propParsable spp sp = 
    case parse spp "" (show sp) of
      (Left _) -> False
      (Right sp') -> sp == sp'


arbitraryTokenChar = oneof [ arbitraryUpper
                           , arbitraryLower
                           , suchThat arbitrary (`elem` "-~_0987654321")
                           ]
arbitraryUpper = suchThat arbitrary isAsciiUpper
arbitraryLower = suchThat arbitrary isAsciiLower

propParsableE = propParsable pE
instance Arbitrary E where
    arbitrary = oneof [ Variable <$> ((:) <$> arbitraryLower <*> listOf arbitraryTokenChar)
                      , Constant <$> ((:) <$> arbitraryUpper <*> listOf arbitraryTokenChar)
                      ]

propParsableD = propParsable pD
instance Arbitrary D where
    arbitrary = elements [ Zero, Infinity ]

propParsableVerbPhrase = propParsable pVerbPhrase
instance Arbitrary VerbPhrase where
    arbitrary = oneof [ CanSay <$> arbitrary <*> arbitrary 
                      , Predicate <$> listOf1 arbitraryTokenChar <*> arbitrary
                      ]

propParsableFact = propParsable pFact
instance Arbitrary Fact where
    arbitrary = Fact <$> arbitrary <*> arbitrary

propParsableClaim = propParsable pClaim
instance Arbitrary Claim where
    arbitrary = Claim <$> arbitrary <*> arbitrary <*> arbitrary

propParsableAssertion = propParsable pAssertion
instance Arbitrary Assertion where
    arbitrary = Assertion <$> arbitrary <*> arbitrary

instance Arbitrary F where
    arbitrary = F <$> listOf1 arbitraryTokenChar

propParsableEc = propParsable pEc
instance Arbitrary Ec where
    arbitrary = oneof [ Entity <$> arbitrary
                      , Apply <$> arbitrary <*> arbitrary
                      , Value <$> arbitrary
                      ]

propParsableC = propParsable pC
instance Arbitrary C where
    arbitrary = oneof [ arbitraryC'
                      , Conj <$> arbitraryC' <*> arbitrary 
                      ]
      where
        arbitraryC' = oneof [ Boolean <$> arbitrary
                            , Equals <$> arbitrary <*> arbitrary
                            , Not <$> arbitraryC'
                            ]

propParsableValue = propParsable pValue
instance Arbitrary Value where
    arbitrary = oneof [ Int' <$> arbitrary
                      , Float' <$> arbitrary
                      , String' <$> listOf (suchThat arbitrary (\x -> isAscii x && 
                                                                      x /= '"'))
                      ]


