{-# LANGUAGE NoMonomorphismRestriction #-}
module Logic.SecPAL.Parser where

import Logic.SecPAL.Language
import Text.Parsec
import Control.Applicative ((*>), (<*>), (<*))

import Debug.Trace

pWs = spaces *> optional eof

pTokenChar = alphaNum <|> oneOf "-_~" <?> "token character"


pE = pVariable <|> pConstant <?> "entity"

pVariable = do
  n <- lower
  ns <- many pTokenChar
  return $ Variable{varName=n:ns}

pConstant = do
  n <- upper
  ns <- many pTokenChar
  return $ Constant{constName=n:ns}


pD = pZero <|> pInf <?> "delegation level"

pZero = do
  string "0"
  return Zero

pInf = do
  string "inf"
  return Infinity


pEc = try pApply <|> pEntity <|> pEValue <?> "constraint entity"

pApply = do
  name <- many1 pTokenChar
  spaces
  char '('
  spaces
  args <- pEc `sepBy` (try $ spaces *> char ',' <* spaces)
  spaces
  char ')'
  return $ Apply (F name) args

pEntity = pE >>= return . Entity

pEValue = pValue >>= return . Value


pC = try pConj <|> pC <?> "constraint"
pC' = pBoolean <|> pNot <|> pEquals <?> "constraint"

pBoolean = pTrue <|> pFalse <?> "boolean"
  where 
    pTrue =  string "True" *> return (Boolean True)
    pFalse = string "False" *> return (Boolean False)

pEquals = do
  a <- pEc
  spaces *> string "==" <* spaces
  b <- pEc 
  return $ Equals a b

pNot = do
  char '!'
  spaces
  x <- pC
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
