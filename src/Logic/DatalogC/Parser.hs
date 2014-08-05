{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

module Logic.DatalogC.Parser where

import Logic.General.Parser
import Logic.General.Constraints
import Logic.DatalogC.Language as L
import Logic.DatalogC.Safety
import Text.Parsec
import Control.Applicative ((*>), (<*))
import Control.Monad
import Data.Maybe
import Data.List

pToken :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m String
pToken = many1 pTokenChar

pPredicate :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Predicate
pPredicate = try $ do
  n <- pToken
  arity' <- optionMaybe pArity
  _ <- pWs
  _ <- char '('
  xs <- pE `sepBy` pListSep
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
pRule = try $ (string ":-" >> pWs) *> pPredicate `sepBy` pListSep

pConstraint :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m C
pConstraint = try $ char '[' *> pWs *> pC <* pWs <* char ']'
 
