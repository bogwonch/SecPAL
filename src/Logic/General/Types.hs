{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Logic.General.Types where

type Type = String

class Typed x where
  typeof :: x -> Type

instance Typed String where
  typeof str
    | ':' `notElem` str = none
    | otherwise = takeWhile (not . (== ':')) str

none :: Type
none = ""

app :: Type
app = "apk"


