module Confidence where

import Data.List
import Network.Curl

type App = String

confidence :: App -> App -> IO Float
confidence lhs rhs = do
  let url = "http://localhost:5000/confidence"
            ++"?lhs="++lhs 
            ++"&rhs="++rhs
  (code, str) <- curlGetString url []
  if "Error:" `isPrefixOf` str
    then fail "arguments"
    else return $ read str

