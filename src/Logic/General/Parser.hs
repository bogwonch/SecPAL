{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
module Logic.General.Parser where

import Control.Applicative ((<*), (<$>), (*>))
import Control.Monad (void, unless)
import Logic.General.Entities
import Logic.General.Constraints
import Text.Parsec
import System.IO
import System.IO.Unsafe (unsafePerformIO)

warning :: forall (m :: * -> *). Monad m => String -> m ()
warning = (return $!) . unsafePerformIO . hPutStrLn stderr . ("%%% WARNING: "++)

pTokenChar :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Char
pTokenChar = alphaNum <|> oneOf "-._'" <?> "token character"

pE :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m E
pE = try pVariable <|> try pConstant <|> pString <?> "entity"

pType :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m String
pType = option [] (try $ many1 pTokenChar <* char '#')

pVariable :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m E
pVariable = do
  t <- pType
  u <- many (char '_')
  n <- lower
  ns <- many pTokenChar
  let var = if null t then u++n:ns else t++"#"++ u++n:ns
  _ <- unless (null u) (warning "don't start variables with an underscore")
  return Variable{varName=var, varType=t}

pConstant :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m E
pConstant = do
  t <- pType
  u <- many (char '_')
  n <- upper
  ns <- many pTokenChar
  let var = if null t then u++n:ns else t++"#"++ u++n:ns
  _ <- unless (null u) (warning "don't start constants with an underscore")
  return Constant{constName=var, constType=t}

pString :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m E
pString = (`Constant` []) <$> (char '"' *> many quotedChar <* char '"')
  where quotedChar = try (string "\\\"" >> return '"') <|> noneOf "\""

pComment :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m ()
pComment = do { char '%' ; manyTill anyChar (char '\n') ; return () } <?> "comment"

pWs :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m ()
pWs = void (many $ try pWs')

pWs' = pComment <|> void (many1 space) <?> "whitespace"

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

pConj :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m C
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
