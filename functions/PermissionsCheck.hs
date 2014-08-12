{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module PermissionsCheck where

import System.Process
import System.Exit
import Control.Monad
import Data.List
import qualified Utilities as T

type Type = String

class Typed x where
  typeof :: x -> Type
  remove :: x -> x

instance Typed String where
  typeof str
    | '#' `notElem` str = none
    | otherwise = takeWhile (not . (== '#')) str

  remove str =
    case elemIndex '#' str of
      (Just idx) -> let (_,'#':str') = idx `splitAt` str in str'
      Nothing    -> str


none :: Type
none = ""

app :: Type
app = "apk"

{- -}


permissionsCheck :: String
                 -> String
                 -> IO Bool
permissionsCheck apk permission = do
  unless (T.typeof apk == T.app) $ 
    fail $ apk ++ " does not have type " ++ T.app

  let app' = T.remove apk
  
  ret <- system $ unwords 
    [ "functions/permissionsCheck"
    , "apps/"++app'
    , permission
    ]
  case ret of
    ExitSuccess -> return True
    ExitFailure _ -> return False

