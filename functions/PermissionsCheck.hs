{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module PermissionsCheck where

import System.Process
import System.Exit
import Control.Monad
import Data.List
{-
import qualified Logic.General.Types as T
 -}

type Type = String

class Typed x where
  typeof :: x -> Type
  removeType :: x -> x

instance Typed String where
  typeof str
    | ':' `notElem` str = none
    | otherwise = takeWhile (not . (== ':')) str

  removeType str =
    case elemIndex ':' str of
      (Just idx) -> let (_,':':str') = idx `splitAt` str in str'
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
  unless (typeof apk == app) $ 
    fail $ apk ++ " does not have type " ++ app

  let app' = removeType apk
  
  ret <- system $ unwords 
    [ "functions/permissionsCheck"
    , "apps/"++app'
    , permission
    ]
  case ret of
    ExitSuccess -> return True
    ExitFailure _ -> return False

