module Category where

import Data.List
import Data.Char
import Network.Curl
import Control.Monad
import qualified Utilities as T

type App = String

category :: App -> IO String
category app = do
  app `T.shouldHaveType` T.app

  let app' = map toLower . T.remove $ app

  let url = "http://localhost:5001/category"
            ++"?package="++app'
  (code, str) <- curlGetString url []
  if "Error:" `isPrefixOf` str
    then fail "arguments"
    else return $ read str

