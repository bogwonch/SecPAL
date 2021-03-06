module Lift where

import Data.List
import Data.Char
import Network.Curl
import Control.Monad
import qualified Utilities as T

type App = String

lift :: App -> App -> IO Float
lift lhs rhs = do
  lhs `T.shouldHaveType` T.app
  rhs `T.shouldHaveType` T.app

  let lhs' = map toLower . T.remove $ lhs
  let rhs' = map toLower . T.remove $ rhs

  let url = "http://localhost:5000/lift"
            ++"?lhs="++lhs'
            ++"&rhs="++rhs'
  (code, str) <- curlGetString url []
  if "Error:" `isPrefixOf` str
    then fail "arguments"
    else return $ read str

