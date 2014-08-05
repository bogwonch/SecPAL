module Logic.General.ConstraintEvaluation where

import Data.Char
import Logic.General.Constraints
import Logic.General.Parser
import Logic.General.Pretty
import Logic.SecPAL.Context
import qualified Logic.General.Named as N
import System.IO
import Text.Parsec
import Language.Haskell.Interpreter 

evaluate :: Context -> Ec -> IO Ec
evaluate ctx v = 
  case v of
    (Apply _ _) -> evaluateFunction ctx v
    _           -> return v


evaluateFunction :: Context -> Ec -> IO Ec
evaluateFunction ctx v = do
  result <- runInterpreter (functionInterpreter ctx v)
  case result of
    Left err -> hPutStrLn stderr ("! failed to evaluate function: " ++ show err) >> fail "function evaluation"
    Right ans  -> return ans


functionInterpreter :: Context -> Ec -> Interpreter Ec
functionInterpreter ctx (Apply f xs) = do
  -- Load the function code
  set [ searchPath := [pluginDir ctx] ]
  let pkg = getPkgName . N.name $ f
  loadModules [ pluginDir ctx ++ "/" ++ pkg ++ ".hs" ]
  setTopLevelModules [ pkg ]
  setImportsQ [("Prelude", Nothing)]

  -- Evaluate the query
  let expr = "(" ++ N.name f ++ " " ++ unwords (map eShow xs) ++ ") >>= return . show"
  ans <- interpret expr (as :: IO String)

  -- Parse the answer
  liftIO (ans >>= valueParser ctx (N.name f))

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
eShow e@(Entity _) = '"': pShow e ++ "\""
eShow x = pShow x


