{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Utilities where

import System.IO
import Control.Monad
import Data.List

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
      (Just idx) -> let (_, '#':str') = idx `splitAt` str in str'
      Nothing    -> str
    
shouldHaveType, mustHaveType :: (Typed x, Show x) => x -> Type -> IO ()
x `shouldHaveType` t = unless (typeof x == t) $ 
  hPutStrLn stderr ("@ "++ show x ++ "is not of type "++ show t)
x `mustHaveType` t = unless (typeof x == t) $ 
  error ("! "++ show x ++ "is not of type "++ show t)

none :: Type
none = ""

app :: Type
app = "apk"


