module Logic.General.ConstraintEvaluation (evaluate) where

import Data.Char
import Logic.General.Constraints
import Logic.General.ConstraintFunctions
import Logic.General.Parser
import Logic.General.Pretty
import Logic.SecPAL.Context
import qualified Logic.General.Named as N
import Text.Parsec

evaluate :: Context -> Ec -> IO Ec
evaluate ctx v = 
  case v of
    (Apply _ _) -> evaluateFunction ctx v
    _           -> return v


evaluateFunction :: Context -> Ec -> IO Ec
evaluateFunction = functionInterpreter

functionInterpreter :: Context -> Ec -> IO Ec
functionInterpreter _ (Apply f xs) =
  runConstraint f (map eShow xs)

functionInterpreter _ (Entity _) = fail "attempted to interpret entity as a function"
functionInterpreter _ (Value _) = fail "attempted to interpret value as a function"

valueParser :: Context -> SourceName -> String -> IO Ec
valueParser _ n str =
  case parse pEc n str of
    (Left err) -> putStrLn ("failed to parse: " ++ show err) >> fail "parser error"
    (Right v) -> return v

  
getPkgName :: String -> String
getPkgName (f:fs) = toUpper f : fs
getPkgName [] = error "nameless package"

eShow :: Ec -> String
eShow (Value (String' x)) = x
eShow x = pShow x


