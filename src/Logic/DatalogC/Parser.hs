module Logic.DatalogC.Parser where

import Logic.DatalogC.Language
import Logic.DatalogC.Safety
import Text.Parsec
import Control.Applicative ((*>), (<*))
import Control.Monad
import Data.Maybe

-- This should really all be taken from SecPAL or refactored into a new
-- generalised logic parser
pTokenChar = alpha <|> oneOf "-_'"
pComment = char '%' *> manyTill anyChar (char '\n')
pWs = do { spaces; optional pComment; pWs }

pListSep :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Char
pListSep = pWs *> char ',' <* pWs

-- Start of language proper
pEntity = pVariable <|> pConstant <|> pString <?> "variable, constant or string"

pToken = many1 pTokenChar

pVariable = do
  n <- lower
  ns <- many pTokenChar
  return . Variable $ n:ns

pConstant = do 
  n <- upper
  ns <- many pTokenChar
  return . Constant $ n:ns

pString = liftM Constant $ char '"' *> many quotedChar <* char '"'
  where quotedChar = try (string "\\\"" >> return '"') <|> noneOf "\""

pPredicate = do
  name <- pToken
  arity' <- optionMaybe pArity
  pWs
  char '('
  args <- pEntity `sepBy` pListSep
  char ')'
  let arity = length args
  -- Check if an explicit arity is correct wrt the arguments
  if maybe False (/= arity) arity'  
    then fail $ "predicate \""++name++"\" has wrong arity:"++
                " declared "++show (fromJust arity')++
                " but used "++show arity++"."
    else return Predicate{ name=name, args=args }

pArity = liftM read $ char '/' *> many1 digit 

pClause = do
  pWs
  h <- pPredicate
  pWs
  b <- option [] pRule
  pWs
  c <- option (Boolean True) pConstraint
  pWs
  char '.'
  let clause = Clause{ head=h, body=b, constraint=c }
  if safe clause 
    then return clause
    else fail $
      "unsafe variables ("++intercalate ", " (map show $ unsafeVars c)++") in clause \""++show c++"\""
   
pRule = (string ":-" >> pWs) *> pPredicate `sepBy` pListSep
pConstraint = char '[' *> pWs *> pConstraintTerm `sepBy1` pListSep <* pWs <* char ']'
 
pConstraintTerms = pBoolean

pBoolean = pTrue <|> pFalse
  where pTrue = string "True" >> return (Boolean True)
        pFalse = string "False" >> return (Boolean False)

