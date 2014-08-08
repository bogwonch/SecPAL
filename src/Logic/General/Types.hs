{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Logic.General.Types where

import Data.List

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
      (Just idx) -> let (_, ':':str') = idx `splitAt` str in str'
      Nothing    -> str
    

none :: Type
none = ""

app :: Type
app = "apk"


