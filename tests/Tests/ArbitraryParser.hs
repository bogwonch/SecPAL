module Tests.ArbitraryParser where

import Logic.SecPAL.Language
import Logic.SecPAL.Pretty
import Test.QuickCheck
import Control.Applicative
import Data.Char
import Text.Parsec
import Logic.SecPAL.Parser
import Tests.Testable
import Tests.TestResults

propParsable :: (PShow a) => Parsec String () a -> a -> Bool
propParsable spp sp = 
    case parse spp "" (pShow sp) of
      (Left _) -> False
      (Right sp') -> pShow sp == pShow sp'

testParserQC = sequence [ qc "E" propParsableE
                        , qc "D" propParsableD
                        , qc "VerbPhrase" propParsableVerbPhrase
                        , qc "Fact" propParsableFact
                        , qc "Claim" propParsableClaim
                        , qc "Assertion" propParsableAssertion
                        , qc "Ec" propParsableEc
                        , qc "C" propParsableC
                        , qc "Value" propParsableValue
                        ]

qc name p = do
    r <- quickCheckWithResult stdArgs{maxSize=32, chatty=False} p
    return $ case r of
               Success{} -> Test name TestPassed
               _ -> Test name $ TestFailed (Just $ output r)

arbitraryTokenChar = oneof [ arbitraryUpper
                           , arbitraryLower
                           , suchThat arbitrary (`elem` "-~_0987654321")
                           ]
arbitraryUpper = suchThat arbitrary isAsciiUpper
arbitraryLower = suchThat arbitrary isAsciiLower
arbitraryLetter = oneof [arbitraryUpper, arbitraryLower]

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
      where
        arbVP 0 = Predicate <$> listOf1 arbitraryTokenChar <*> arbitrary
        arbVP n = oneof [ CanSay <$> arbitrary <*> arbitrary 
                        , Predicate <$> listOf1 arbitraryTokenChar <*> resize (n`div`2) arbitrary
                        ]


propParsableFact = propParsable pFact
instance Arbitrary Fact where
    arbitrary = Fact <$> arbitrary <*> arbitrary

propParsableClaim = propParsable pClaim
instance Arbitrary Claim where
    arbitrary = Claim <$> arbitrary <*> arbitrary <*> arbitrary

propParsableAssertion = propParsable pAssertionUnsafe
instance Arbitrary Assertion where
    arbitrary = Assertion <$> arbitrary <*> arbitrary

instance Arbitrary F where
    arbitrary = F <$> ((:) <$> arbitraryLetter <*> listOf arbitraryTokenChar)

propParsableEc = propParsable pEc
instance Arbitrary Ec where
    arbitrary = sized arbEc

arbEc 0 = oneof [ Entity <$> arbitrary
                , Value <$> arbitrary
                ]

arbEc n = oneof [ Entity <$> arbitrary
                , Value <$> arbitrary
                , Apply <$> arbitrary <*> listOf (resize (n`div`2) (sized arbEc))
                ]

propParsableC = propParsable pC
instance Arbitrary C where
    arbitrary = oneof [ sized arbC'
                      , Conj <$> sized arbC' <*> arbitrary 
                      ]
      where
        arbC' 0 = oneof [ Boolean <$> arbitrary
                        , Equals <$> arbitrary <*> arbitrary
                        ]
        arbC' n = oneof [ Boolean <$> arbitrary
                        , Equals <$> arbitrary <*> arbitrary
                        , Not <$> resize (n`div`2) (sized arbC')
                        ]

propParsableValue = propParsable pValue
instance Arbitrary Value where
    arbitrary = oneof [ Int' <$> arbitrary
                      , Float' <$> arbitrary
                      , String' <$> listOf (suchThat arbitrary (\x -> isAscii x && 
                                                                     x `notElem` "\"\\" &&
                                                                     isPrint x))
                      ]


parseDebug spp sp = 
  case parse spp "" (pShow sp) of
    (Left err) -> putStrLn $ ":-( " ++ show err
    (Right sp') -> parseDebug sp sp' 
  where
    parseDebug sp sp' = do
      putStrLn $ "INPUT:  "++pShow sp
      putStrLn $ "OUTPUT: "++pShow sp'
      putStrLn   ""
      putStrLn $ "INPUT:  "++show sp
      putStrLn $ "OUTPUT: "++show sp'

