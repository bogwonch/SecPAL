module Support where

import Data.List
import Network.Curl
import Control.Monad
import qualified Utilities as T

type App = String

support :: App -> App -> IO Float
support lhs rhs = do
  unless (T.typeof lhs == T.app) (fail $ "type of '"++lhs++"' is not '"++T.app++"'")
  unless (T.typeof rhs == T.app) (fail $ "type of '"++rhs++"' is not '"++T.app++"'")

  let lhs' = T.remove lhs
  let rhs' = T.remove rhs

  let url = "http://localhost:5000/support"
            ++"?lhs="++lhs'
            ++"&rhs="++rhs'
  (code, str) <- curlGetString url []
  if "Error:" `isPrefixOf` str
    then fail "arguments"
    else return $ read str

