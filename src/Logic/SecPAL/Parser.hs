{-# LANGUAGE NoMonomorphismRestriction #-}
module Logic.SecPAL.Parser where

import Logic.SecPAL.Language
import Text.Parsec
import Control.Applicative ((*>), (<*>), (<*))
import Control.Monad 
import Data.Maybe

import Debug.Trace

pWs = spaces *> optional eof
pListSep = spaces *> char ',' <* spaces
pTokenChar = alphaNum <|> oneOf "-_~" <?> "token character"


pE = pVariable <|> pConstant <?> "entity"

pVariable = do
  n <- lower
  ns <- many pTokenChar
  return Variable{varName=n:ns}

pConstant = do
  n <- upper
  ns <- many pTokenChar
  return Constant{constName=n:ns}


pD = pZero <|> pInf <?> "delegation level"

pZero = do
  string "0"
  return Zero

pInf = do
  string "inf"
  return Infinity


pVerbPhrase = try pCanSay <|> pPredicate <?> "verb phrase"

pPredicate = do
  pred <- many1 pTokenChar
  spaces
  args <- option  [] (try pPredicateArg)
  return Predicate{ predicate=pred, args=args }

pPredicateArg = do
  char '('
  args <- pE `sepBy` pListSep
  char ')'
  return args

pCanSay = do
  string "can-say"
  spaces
  d <- pD
  spaces
  fact <- pFact -- No nested says statements (probs shouldnt do this but parsing is easier)
  return CanSay{ delegation=d, what=fact }


pFact = do
  subject <- pE
  spaces
  verb <- pVerbPhrase
  return Fact{ subject=subject, verb=verb }


pClaim = do
  f <- pFact
  spaces
  fs <- option [] pClaimConditional
  spaces
  c <- option (Boolean True) pClaimConstraint
  return Claim{ fact=f, conditions=fs, constraint=c }
  

pClaimConditional = do
  string "if"
  spaces
  conds <- pFact `sepBy1` pListSep
  return conds

pClaimConstraint = do
  char ';' -- Yeah not strictly SecPAL but it makes the parsing easier
  spaces
  c <- pC
  return c


pAssertion = do
  who <- pE
  string " says "
  what <- pClaim
  spaces
  char '.'
  return Assertion{ who=who, says=what }


pAC = liftM AC (pAssertion `sepBy` spaces)


pEc = try pApply <|> pEntity <|> pEValue <?> "constraint entity"

pApply = do
  name <- many1 pTokenChar
  spaces
  char '('
  spaces
  args <- pEc `sepBy` try pListSep
  spaces
  char ')'
  return $ Apply (F name) args

pEntity = liftM Entity pE

pEValue = liftM Value pValue


pC = try pConj <|> pC' <?> "constraint"
pC' = try pEquals <|> pNot <|> pBoolean <?> "constraint"

pBoolean = pTrue <|> pFalse <?> "boolean"
  where 
    pTrue =  string "True" *> return (Boolean True)
    pFalse = string "False" *> return (Boolean False)

pEquals = do
  a <- pEc
  spaces *> string "=" <* spaces
  b <- pEc 
  return $ Equals a b

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


pValue = try pFloat <|> pInt <|> pString <?> "value"

pInt = pHex <|> pDec <?> "integer"

pDec = do
  n <- many1 digit
  return $ Int' (read n)

pHex = do
  char '0'
  oneOf "xX"
  n <- many1 hexDigit
  return $ Int' (read $ "0x"++n)

pFloat = do
  int <- many1 digit
  char '.'
  frac <- many1 digit
  return $ Float' (read $ int ++ "." ++ frac)

pString = do
  char '"'
  word <- manyTill anyChar (try $ char '"')
  return $ String' word
