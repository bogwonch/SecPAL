module Tests.ArbitraryParser where

import Logic.General.Entities
import Logic.General.Constraints
import Logic.General.Parser
import Logic.SecPAL.Language
import Logic.SecPAL.Pretty
import Logic.General.Pretty
import Test.QuickCheck
import Control.Applicative
import Data.Char
import Text.Parsec
import Logic.SecPAL.Parser
import Tests.Testable hiding (Testable)
import Tests.TestResults

propParsable :: (Show a, PShow a) => Parsec String () a -> a -> Bool
propParsable spp sp = 
    case parse spp "" (pShow sp) of
      (Left _) -> False
      (Right sp') -> pShow sp == pShow sp'

testParserQC :: IO [Test] 
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

qc :: Testable prop => String -> prop -> IO Test 
qc name p = do
    r <- quickCheckWithResult stdArgs{maxSize=32, chatty=False} p
    return $ case r of
               Success{} -> Test name TestPassed
               _ -> Test name $ TestFailed (Just $ output r)

arbitraryTokenChar :: Gen Char 
arbitraryTokenChar = oneof [ arbitraryUpper
                           , arbitraryLower
                           , suchThat arbitrary (`elem` "-_0987654321'")
                           ]
arbitraryUpper :: Gen Char 
arbitraryUpper = suchThat arbitrary isAsciiUpper
arbitraryLower :: Gen Char 
arbitraryLower = suchThat arbitrary isAsciiLower
arbitraryLetter :: Gen Char 
arbitraryLetter = oneof [arbitraryUpper, arbitraryLower]

propParsableE :: E -> Bool 
propParsableE = propParsable pE

instance Arbitrary E where
    arbitrary = oneof [ (`Variable` []) <$> ((:) <$> arbitraryLower <*> listOf arbitraryTokenChar)
                      , (`Constant` []) <$> ((:) <$> arbitraryUpper <*> listOf arbitraryTokenChar)
                      ]

propParsableD :: D -> Bool 
propParsableD = propParsable pD

instance Arbitrary D where
    arbitrary = elements [ Zero, Infinity ]

propParsableVerbPhrase :: VerbPhrase -> Bool 
propParsableVerbPhrase = propParsable pVerbPhrase

instance Arbitrary VerbPhrase where
    arbitrary = oneof [ CanSay <$> arbitrary <*> arbitrary 
                      , Predicate <$> listOf1 arbitraryTokenChar <*> arbitrary
                      , CanActAs <$> arbitrary
                      ]

propParsableFact :: Fact -> Bool 
propParsableFact = propParsable pFact
instance Arbitrary Fact where
    arbitrary = Fact <$> arbitrary <*> arbitrary

propParsableClaim :: Claim -> Bool 
propParsableClaim = propParsable pClaim
instance Arbitrary Claim where
    arbitrary = Claim <$> arbitrary <*> arbitrary <*> arbitrary

propParsableAssertion :: Assertion -> Bool 
propParsableAssertion = propParsable pAssertionUnsafe

instance Arbitrary Assertion where
    arbitrary = Assertion <$> arbitrary <*> arbitrary

instance Arbitrary F where
    arbitrary = F <$> ((:) <$> arbitraryLetter <*> listOf arbitraryTokenChar)

propParsableEc :: Ec -> Bool 
propParsableEc = propParsable pEc

instance Arbitrary Ec where
    arbitrary = sized arbEc

arbEc :: Int -> Gen Ec 
arbEc 0 = oneof [ Entity <$> arbitrary
                , Value <$> arbitrary
                ]

arbEc n = oneof [ Entity <$> arbitrary
                , Value <$> arbitrary
                , Apply <$> arbitrary <*> listOf (resize (n`div`2) (sized arbEc))
                ]

propParsableC :: C -> Bool 
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

propParsableValue :: Value -> Bool 
propParsableValue = propParsable pValue

instance Arbitrary Value where
    arbitrary = oneof [ Int' <$> arbitrary
                      , Float' <$> arbitrary
                      , String' <$> listOf (suchThat arbitrary (\x -> isAscii x && 
                                                                     x `notElem` "\"\\" &&
                                                                     isPrint x))
                      , Bool' <$> arbitrary
                      ]

{-
parseDebug :: (Show a, Show a1, PShow a, PShow a1) => Parsec String () a1 -> a -> IO () 
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
-}
