module Logic.General.ConstraintFunctions (runConstraint) where

import Data.List
import Data.Char
import Network.Curl
import System.Process
import System.Exit
import Control.Applicative
import Logic.General.Constraints
import qualified Logic.General.Types as T

runConstraint :: F -> [String] -> IO String

runConstraint F{fName="category"} [app] 
 = show <$> category app
runConstraint F{fName="confidence"} [a,b] 
 = show <$> confidence a b
runConstraint F{fName="lift"} [a,b] 
 = show <$> lift a b
runConstraint F{fName="support"} [a,b] 
 = show <$> support a b
runConstraint F{fName="permissionsCheck"} [app,p]
 = show <$> permissionsCheck app p
runConstraint F{fName="hasPermission"} [app,p] 
 = show <$> hasPermission app p

runConstraint _ _ = fail "Unknown constraint function"


type App = String
type Permission = String

category :: App -> IO String
category app = do
  app `T.shouldHaveType` T.app

  let app' = map toLower . T.remove $ app

  let url = "http://localhost:5001/category"
            ++"?package="++app'
  (_, str) <- curlGetString url []
  if "Error:" `isPrefixOf` str
    then fail "arguments"
    else return $ read str

hasPermission :: App -> Permission -> IO Bool
hasPermission app permission = do
  app `T.shouldHaveType` T.app

  let app' = map toLower . T.remove $ app

  let url = "http://localhost:5001/permission"
            ++"?package="++app'
            ++"&permission="++permission
  (_, str) <- curlGetString url []
  if "Error:" `isPrefixOf` str
    then fail "arguments"
    else return $ read str


caratData :: String -> App -> App -> IO Float
caratData kind lhs rhs =  do
  lhs `T.shouldHaveType` T.app
  rhs `T.shouldHaveType` T.app

  let lhs' = map toLower . T.remove $ lhs
  let rhs' = map toLower . T.remove $ rhs

  let url = "http://localhost:5000/"++kind
            ++"?lhs="++lhs'
            ++"&rhs="++rhs'
  (_, str) <- curlGetString url []
  if "Error:" `isPrefixOf` str
    then fail "arguments"
    else return $ read str

confidence, lift, support :: App -> App -> IO Float
lift       = caratData "lift"
support    = caratData "support"
confidence = caratData "confidence"


permissionsCheck :: String
                 -> String
                 -> IO Bool
permissionsCheck apk permission = do
  apk `T.shouldHaveType` T.app

  let app' = T.remove apk

  ret <- system $ unwords
    [ "functions/permissionsCheck"
    , "apps/"++app'
    , permission
    ]
  case ret of
    ExitSuccess -> return True
    ExitFailure _ -> return False
