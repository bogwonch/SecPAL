module HasPermission where

import Data.List
import Data.Char
import Network.Curl
import Control.Monad
import Control.Applicative
import qualified Utilities as T
import System.IO

type App = String
type Permission = String

hasPermission :: App -> Permission -> IO Bool
hasPermission app permission = do
  app `T.shouldHaveType` T.app

  let app' = map toLower . T.remove $ app

  let url = "http://localhost:5001/permission"
            ++"?package="++app'
            ++"&permission="++permission
  (code, str) <- curlGetString url []
  if "Error:" `isPrefixOf` str
    then fail "arguments"
    else return $ read str 
{-hasntPermission :: App -> Permission -> IO Bool-}
{-hasntPermission = not <$> hasPermission-}
