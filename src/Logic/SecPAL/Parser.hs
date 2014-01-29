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
  pFact `sepBy1` pListSep

pClaimConstraint = do
  char ';' -- Yeah not strictly SecPAL but it makes the parsing easier
  spaces
  pC


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
  n <- letter
  name <- many pTokenChar
  spaces
  char '('
  spaces
  args <- pEc `sepBy` try pListSep
  spaces
  char ')'
  return $ Apply (F $ n:name) args

pEntity = liftM Entity pE

pEValue = liftM Value pValue


pC = try pConj <|> pC' <?> "conjugation or constraint"
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

pInt = try pHex <|> pDec <?> "integer"

pDec = do
  sign <- option "" (string "-")  
  n <- many1 digit
  return $ Int' (read $ sign++n)

pHex = do
  char '0'
  oneOf "xX"
  n <- many1 hexDigit
  return $ Int' (read $ "0x"++n)

pFloat = do
  sign <- option "" (string "-")  
  int <- many1 digit
  char '.'
  frac <- many1 digit
  exp <- option "" pExponent
  let float = sign ++ int ++ "." ++ frac ++ exp
  return $ Float' (read float)

pExponent = do
  char 'e'
  s <- option "" (string "-")
  n <- many1 digit
  return $ "e" ++ s ++ n

pString = do
  char '"'
  word <- manyTill anyChar (try $ char '"')
  return $ String' word