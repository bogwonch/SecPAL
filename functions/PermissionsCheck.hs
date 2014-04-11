module PermissionsCheck where

import System.Process
import System.Exit

permissionsCheck :: String
                 -> String
                 -> IO Bool
permissionsCheck app permission = do
  ret <- system $ unwords 
    [ "functions/permissionsCheck"
    , "apps/"++app
    , permission
    ]
  case ret of
    ExitSuccess -> return True
    ExitFailure _ -> return False

