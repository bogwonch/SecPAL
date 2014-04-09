{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
module Logic.SecPAL.Parser where

import Logic.SecPAL.Language
import Logic.SecPAL.AssertionSafety
import Logic.SecPAL.Pretty
import Text.Parsec
import Control.Applicative ((*>), (<*))
import Control.Monad 

pWs :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m ()
pWs = spaces *> optional eof

pListSep :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Char
pListSep = spaces *> char ',' <* spaces

pTokenChar :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Char
pTokenChar = alphaNum <|> oneOf "-_~" <?> "token character"

pE :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m E
pE = pVariable <|> pConstant <?> "entity"

pVariable :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m E
pVariable = do
  n <- lower
  ns <- many pTokenChar
  return Variable{varName=n:ns}

pConstant :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m E
pConstant = do
  n <- upper
  ns <- many pTokenChar
  return Constant{constName=n:ns}


pD :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m D
pD = pZero <|> pInf <?> "delegation level"

pZero :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m D
pZero = do
  string "0"
  return Zero

pInf :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m D
pInf = do
  string "inf"
  return Infinity


pVerbPhrase :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m VerbPhrase
pVerbPhrase = try pCanSay <|> pPredicate <?> "verb phrase"

pPredicate :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m VerbPhrase
pPredicate = do
  p <- many1 pTokenChar
  spaces
  as <- option  [] (try pPredicateArg)
  return Predicate{ predicate=p, args=as }

pPredicateArg :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m [E]
pPredicateArg = do
  char '('
  as <- pE `sepBy` pListSep
  char ')'
  return as

pCanSay :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m VerbPhrase
pCanSay = do
  string "can-say"
  spaces
  d <- pD
  spaces
  f <- pFact -- No nested says statements (probs shouldnt do this but parsing is easier)
  return CanSay{ delegation=d, what=f }


pFact :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Fact
pFact = do
  s <- pE
  spaces
  v <- pVerbPhrase
  return Fact{ subject=s, verb=v } 


pClaim :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Claim
pClaim = do
  f <- pFact
  spaces
  fs <- option [] pClaimConditional
  spaces
  c <- option (Boolean True) pClaimConstraint
  return Claim{ fact=f, conditions=fs, constraint=c }
  

pClaimConditional :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m [Fact]
pClaimConditional = do
  string "if"
  spaces
  pFact `sepBy1` pListSep

pClaimConstraint :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m C
pClaimConstraint = do
  char ';' -- Yeah not strictly SecPAL but it makes the parsing easier
  spaces
  pC


pAssertion :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Assertion
pAssertion = do
  assertion <- pAssertionUnsafe
  if safe assertion
    then return assertion
    else fail $ "Unsafe assertion: "++pShow assertion

pAssertionUnsafe :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Assertion
pAssertionUnsafe = do
  e <- pE
  string " says "
  c <- pClaim
  spaces
  char '.'
  return Assertion{ who=e, says=c }


pAC :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m AC
pAC = liftM AC (pAssertion `sepBy` spaces)


pEc :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Ec
pEc = try pApply <|> try pEValue <|> pEntity <?> "constraint entity"

pApply :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Ec
pApply = do
  n <- letter
  name <- many pTokenChar
  spaces
  char '('
  spaces
  as <- pEc `sepBy` try pListSep
  spaces
  char ')'
  return $ Apply (F $ n:name) as

pEntity :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Ec
pEntity = liftM Entity pE

pEValue :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Ec
pEValue = liftM Value pValue


pConj :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m C
pC :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m C
pC = try pConj <|> pC' <?> "conjugation or constraint"
pC' :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m C
pC' = try pEquals <|> pNot <|> pBoolean <?> "constraint"

pBoolean :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m C
pBoolean = pTrue <|> pFalse <?> "boolean"
  where 
    pTrue =  string "True" *> return (Boolean True)
    pFalse = string "False" *> return (Boolean False)

pEquals :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m C
pEquals = do
  a <- pEc
  spaces *> string "=" <* spaces
  b <- pEc 
  return $ Equals a b

pNot :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m C
pNot = do
  char '!'
  spaces
  x <- pC'
  return $ Not x

pConj = do
  a <- pC'
  spaces *> string "," <* spaces
  b <- pC
  return $ Conj a b


pValue :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Value
pValue = try pFloat <|> pInt <|> pString <|> pBool <?> "value"

-- TODO: Refactor duplicate code
pBool :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Value
pBool = pTrue <|> pFalse <?> "boolean"
  where 
    pTrue =  string "True" *> return (Bool' True)
    pFalse = string "False" *> return (Bool' False)


pInt :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Value
pInt = try pHex <|> pDec <?> "integer"

pDec :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Value
pDec = do
  sign <- option "" (string "-")  
  n <- many1 digit
  return $ Int' (read $ sign++n)

pHex :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Value
pHex = do
  char '0'
  oneOf "xX"
  n <- many1 hexDigit
  return $ Int' (read $ "0x"++n)

pFloat :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Value
pFloat = do
  sign <- option "" (string "-")  
  int <- many1 digit
  char '.'
  frac <- many1 digit
  e <- option "" pExponent
  let float = sign ++ int ++ "." ++ frac ++ e
  return $ Float' (read float)

pExponent :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m String
pExponent = do
  char 'e'
  s <- option "" (string "-")
  n <- many1 digit
  return $ "e" ++ s ++ n

pString :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Value
pString = do
  char '"'
  word <- manyTill anyChar (try $ char '"')
  return $ String' word
