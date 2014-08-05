{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
module Logic.General.Parser where

import Control.Applicative ((<*), (<$>), (*>))
import Control.Monad (void)
import Logic.General.Entities
import Logic.General.Constraints
import Text.Parsec

pTokenChar :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Char
pTokenChar = alphaNum <|> oneOf "-_'" <?> "token character"

pE :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m E
pE = pVariable <|> pConstant <|> pString <?> "entity"

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

pString :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m E
pString = Constant <$> (char '"' *> many quotedChar <* char '"')
  where quotedChar = try (string "\\\"" >> return '"') <|> noneOf "\""

pComment :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m String
pComment = char '%' *> manyTill anyChar (char '\n') <?> "comment"

pWs :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m ()
pWs = void $ spaces >> optional pComment 

pListSep :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Char
pListSep = try $ pWs *> char ',' <* pWs

pEc :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Ec
pEc = try pApply <|> try pEValue <|> pEntity <?> "constraint entity"

pApply :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Ec
pApply = do
  n <- letter
  name <- many pTokenChar
  spaces
  _ <- char '('
  spaces
  as <- pEc `sepBy` try pListSep
  spaces
  _ <- char ')'
  return $ Apply (F $ n:name) as

pEntity :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Ec
pEntity = Entity <$> pE

pEValue :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Ec
pEValue = Value <$> pValue


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
  _ <- spaces *> string "=" <* spaces
  b <- pEc 
  return $ Equals a b

pNot :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m C
pNot = do
  _ <- char '!'
  spaces
  x <- pC'
  return $ Not x

pConj = do
  a <- pC'
  _ <- spaces *> string "," <* spaces
  b <- pC
  return $ Conj a b


pValue :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Value
pValue = try pFloat <|> pInt <|> pCString <|> pBool <?> "value"

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
  _ <- char '0'
  _ <- oneOf "xX"
  n <- many1 hexDigit
  return $ Int' (read $ "0x"++n)

pFloat :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Value
pFloat = do
  sign <- option "" (string "-")  
  int <- many1 digit
  _ <- char '.'
  frac <- many1 digit
  e <- option "" pExponent
  let float = sign ++ int ++ "." ++ frac ++ e
  return $ Float' (read float)

pExponent :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m String
pExponent = do
  _ <- char 'e'
  s <- option "" (string "-")
  n <- many1 digit
  return $ "e" ++ s ++ n

pCString :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Value
pCString = do
  _ <- char '"'
  word <- manyTill anyChar (try $ char '"')
  return $ String' word
