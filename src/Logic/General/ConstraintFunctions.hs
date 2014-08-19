module Logic.General.ConstraintFunctions (runConstraint) where

import Data.List
import Data.Char
import Network.Curl
import System.Process
import System.Exit
import Control.Concurrent.ParallelIO.Global
import Logic.General.Constraints
import qualified Logic.General.Types as T

runConstraint :: F -> [String] -> IO Ec
runConstraint f xs = extraWorkerWhileBlocked $ runConstraint' f xs
--runConstraint f xs =  runConstraint' f xs

runConstraint' F{fName="category"} [app] 
 = category app
runConstraint' F{fName="confidence"} [a,b] 
 = confidence a b
runConstraint' F{fName="lift"} [a,b] 
 = lift a b
runConstraint' F{fName="support"} [a,b] 
 = support a b
runConstraint' F{fName="permissionsCheck"} [app,p]
 = permissionsCheck app p
runConstraint' F{fName="hasPermission"} [app,p] 
 = hasPermission app p

runConstraint' _ _ = fail "Unknown constraint function"


type App = String
type Permission = String

category :: App -> IO Ec
category app = do
  app `T.shouldHaveType` T.app

  let app' = map toLower . T.remove $ app

  let url = "http://localhost:5001/category"
            ++"?package="++app'
  (_, str) <- curlGetString url []
  if "Error:" `isPrefixOf` str
    then fail "arguments"
    else return . Value . String' . read $ str

hasPermission :: App -> Permission -> IO Ec
hasPermission app permission = do
  app `T.shouldHaveType` T.app

  let app' = map toLower . T.remove $ app

  let url = "http://localhost:5001/permission"
            ++"?package="++app'
            ++"&permission="++permission
  (_, str) <- curlGetString url []
  if "Error:" `isPrefixOf` str
    then fail "arguments"
    else return . Value . Bool' . read $ str


caratData :: String -> App -> App -> IO Ec
caratData kind lhs rhs =  do
  lhs `T.shouldHaveType` T.app
  rhs `T.shouldHaveType` T.app

  let lhs' = map toLower . T.remove $ lhs
  let rhs' = map toLower . T.remove $ rhs

  let url = "http://localhost:5000/"++kind
            ++"?lhs="++lhs'
            ++"&rhs="++rhs'
  (code, str) <- curlGetString url []
  print code
  putStrLn str
  if "Error:" `isPrefixOf` str
    then fail "arguments"
    else return . Value . Float' . read $ str

confidence, lift, support :: App -> App -> IO Ec
lift       = caratData "lift"
support    = caratData "support"
confidence = caratData "confidence"


permissionsCheck :: String
                 -> String
                 -> IO Ec
permissionsCheck apk permission = do
  apk `T.shouldHaveType` T.app

  let app' = T.remove apk

  ret <- system $ unwords
    [ "functions/permissionsCheck"
    , "apps/"++app'
    , permission
    ]
  case ret of
    ExitSuccess   -> return . Value . Bool' $ True
    ExitFailure _ -> return . Value . Bool' $ False
