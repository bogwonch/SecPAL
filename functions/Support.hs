module Support where

import Data.List
import Network.Curl

type App = String

support :: App -> App -> IO Float
support lhs rhs = do
  let url = "http://localhost:5000/support"
            ++"?lhs="++lhs 
            ++"&rhs="++rhs
  (code, str) <- curlGetString url []
  if "Error:" `isPrefixOf` str
    then fail "arguments"
    else return $ read str

