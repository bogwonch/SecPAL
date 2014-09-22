{-- Parser for SecPAL -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
module Logic.SecPAL.Parser where

import Control.Monad 
import Control.Applicative ((*>))
import Logic.General.Entities
import Logic.General.Constraints
import Logic.General.Parser
import Logic.SecPAL.AssertionSafety
import Logic.SecPAL.Language
import Logic.General.Pretty
import Logic.SecPAL.Pretty()
import Text.Parsec

pD :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m D
pD = pZero <|> pInf <?> "delegation level"

pZero :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m D
pZero = do
  _ <- string "0"
  return Zero

pInf :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m D
pInf = do
  _ <- string "inf"
  return Infinity


pVerbPhrase :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m VerbPhrase
pVerbPhrase = try pCanSay <|> try pCanActAs <|> pPredicate <?> "verb phrase"

pPredicate :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m VerbPhrase
pPredicate = do
  p <- many1 pTokenChar
  spaces
  as <- option  [] (try pPredicateArg)
  return Predicate{ predicate=p, args=as }

pPredicateArg :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m [E]
pPredicateArg = do
  _ <- char '('
  as <- pE `sepBy` pListSep
  _ <- char ')'
  return as

pCanSay :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m VerbPhrase
pCanSay = do
  _ <- string "can-say"
  spaces
  d <- pD
  spaces
  f <- pFact -- No nested says statements (probs shouldnt do this but parsing is easier)
  return CanSay{ delegation=d, what=f }

pCanActAs :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m VerbPhrase
pCanActAs = do
  _ <- string "can-act-as"
  spaces
  w <- pE
  return CanActAs{ whom=w }

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
  _ <- string "if"
  spaces
  pFact `sepBy1` pListSep

pClaimConstraint :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m C
pClaimConstraint = do
  _ <- char ':' -- Yeah not strictly SecPAL but it makes the parsing easier
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
  pWs
  e <- pE
  _ <- string " says "
  c <- pClaim
  spaces
  _ <- char ';'
  pWs
  return Assertion{ who=e, says=c }


pAC :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m AC
pAC = pWs *> liftM AC (pAssertion `sepBy` pWs)


