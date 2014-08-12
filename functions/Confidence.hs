module Confidence where

import Data.List
import Data.Char
import Network.Curl
import qualified Utilities as T
import Control.Monad

type App = String

confidence :: App -> App -> IO Float
confidence lhs rhs = do
  unless (T.typeof lhs == T.app) (fail $ "type of '"++lhs++"' is not '"++T.app++"'")
  unless (T.typeof rhs == T.app) (fail $ "type of '"++rhs++"' is not '"++T.app++"'")

  let lhs' = map toLower . T.remove $ lhs
  let rhs' = map toLower . T.remove $ rhs

  let url = "http://localhost:5000/confidence"
            ++"?lhs="++lhs'
            ++"&rhs="++rhs'
  (code, str) <- curlGetString url []
  if "Error:" `isPrefixOf` str
    then fail "arguments"
    else return $ read str

