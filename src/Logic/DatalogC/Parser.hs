{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

module Logic.DatalogC.Parser where

import Logic.DatalogC.Language as L
import Logic.DatalogC.Safety
import Text.Parsec
import Control.Applicative ((*>), (<*))
import Control.Monad
import Data.Maybe
import Data.List

-- This should really all be taken from SecPAL or refactored into a new
-- generalised logic parser
pTokenChar :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Char 
pTokenChar = letter <|> digit <|> oneOf "-_'"

pComment :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m String
pComment = char '%' *> manyTill anyChar (char '\n')

pWs :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m ()
pWs = void $ spaces >> optional pComment 

pListSep :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Char
pListSep = pWs *> char ',' <* pWs

-- Start of language proper
pEntity :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Entity 
pEntity = pVariable <|> pConstant <|> pString <?> "variable, constant or string"

pToken :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m String
pToken = many1 pTokenChar

pVariable :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Entity
pVariable = do
  n <- lower
  ns <- many pTokenChar
  return . Variable $ n:ns

pConstant :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Entity
pConstant = do 
  n <- upper
  ns <- many pTokenChar
  return . Constant $ n:ns

pString :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Entity
pString = liftM Constant $ char '"' *> many quotedChar <* char '"'
  where quotedChar = try (string "\\\"" >> return '"') <|> noneOf "\""

pPredicate :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Predicate
pPredicate = do
  n <- pToken
  arity' <- optionMaybe pArity
  _ <- pWs
  _ <- char '('
  xs <- pEntity `sepBy` pListSep
  _ <- char ')'
  let arity = length xs
  -- Check if an explicit arity is correct wrt the arguments
  if maybe False (/= arity) arity'  
    then fail $ "predicate \""++n++"\" has wrong arity:"++
                " declared "++show (fromJust arity')++
                " but used "++show arity++"."
    else return Predicate{ name=n, args=xs }

pArity :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Int
pArity = liftM read $ char '/' *> many1 digit 

pClause :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Clause
pClause = do
  _ <- pWs
  h <- pPredicate
  _ <- pWs
  b <- option [] pRule
  _ <- pWs
  c <- option (Boolean True) pConstraint
  _ <- pWs
  _ <- char '.'
  let clause = Clause{ L.head=h, body=b, constraint=c }
  if safe clause 
    then return clause
    else fail $
      "unsafe variables ("
      ++intercalate ", " (map show $ unsafeVars clause)
      ++") in clause \""
      ++show clause
      ++"\""
   
pRule :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m [Predicate]
pRule = (string ":-" >> pWs) *> pPredicate `sepBy` pListSep

pConstraint :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Constraint
pConstraint = char '[' *> pWs *> pConstraintTerm <* pWs <* char ']'
 
pConstraintTerm :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Constraint
pConstraintTerm = pBoolean

pBoolean :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Constraint
pBoolean = pTrue <|> pFalse
  where pTrue = string "True" >> return (Boolean True)
        pFalse = string "False" >> return (Boolean False)

