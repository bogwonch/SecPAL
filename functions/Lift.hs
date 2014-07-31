module Lift where

import Data.List
import Network.Curl

type App = String

lift :: App -> App -> IO Float
lift lhs rhs = do
  let url = "http://localhost:5000/lift"
            ++"?lhs="++lhs 
            ++"&rhs="++rhs
  (code, str) <- curlGetString url []
  if "Error:" `isPrefixOf` str
    then fail "arguments"
    else return $ read str

