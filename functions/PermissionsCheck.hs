{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module PermissionsCheck where

import System.Process
import System.Exit
import Control.Monad
import Data.List
import qualified Utilities as T

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

