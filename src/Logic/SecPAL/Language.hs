module Logic.SecPAL.Language where

import System.Process
import System.IO.Unsafe
import Control.Monad
import System.FilePath.Find
import System.Exit

data E = Variable { varName :: String }
       | Constant { constName :: String }
       deriving (Show)

instance Eq E where
    (Variable _) == (Constant _) = False
    (Constant _) == (Variable _) = False
    (Variable x) == (Variable y) = x == y
    (Constant x) == (Constant y) = x == y

data D = Zero
       | Infinity
       deriving (Eq,Show)

data VerbPhrase = Predicate { predicate :: String, args :: [E] }
                | CanSay { delegation :: D, what :: Fact }
                | CanActAs { whom :: E }
                deriving (Eq,Show)

data Fact = Fact { subject :: E, verb :: VerbPhrase }
          deriving (Eq,Show)

data Claim = Claim { fact :: Fact, conditions :: [Fact], constraint :: C }
           deriving (Eq,Show)

data Assertion = Assertion { who :: E, says :: Claim }
               deriving (Eq,Show)

data AC = AC [Assertion]
        deriving (Eq,Show)

acs :: AC -> [Assertion]
acs (AC as) = as

data F = F { fName :: String }
       deriving (Eq,Show)

data Ec = Entity E
        | Apply F [Ec]
        | Value Value
        deriving (Show)

instance Eq Ec where
  (Entity a) == (Entity b) = a == b
  (Value a) == (Value b) = a == b

  v'@(Value _) == f'@(Apply _ _) = f' == v'
  f'@(Apply f args) == v'@(Value v) 
    | typeF f == typeV v = evaluate f args == v -- We're going to have to improve this later
    | otherwise = error $ "type error: "++show f'++" and "++ show v

  a == b = error $ "comparing '"++show a++"' with '"++show b++"'"

data C = Boolean Bool
       | Equals Ec Ec
       | Not C
       | Conj C C
       deriving (Eq,Show)

data Value 
    = Int' Int
    | Float' Float
    | String' String
    | Bool' Bool
  deriving (Eq,Show)


{- This is dire code, shouldn't be in this file at all, needs urgent refactoring
 - and will need to be extensible plus non crap... but later 
 -
 - TODO: See above
 -}
baseInt = Int' 0
baseFloat = Float' 0
baseString = String' ""
baseBool = Bool' True

typeV :: Value -> Value
typeV (Int' _) = baseInt
typeV (Float' _) = baseFloat
typeV (String' _) = baseString
typeV (Bool' _) = baseBool

typeF :: F -> Value
typeF (F "permissionsCheck") = baseBool
typeF f = error $ "undefined function type: "++show f

evaluate :: F -> [Ec] -> Value
evaluate (F "permissionsCheck") = unsafePerformIO . fPermissionsCheck -- FIXME: stop being lazy

fPermissionsCheck :: [Ec] -> IO Value
fPermissionsCheck [Entity (Constant app), Value (String' perm)] = 
  do
    command <- findCommand "permissionsCheck"
    apk <- findAPK app
    exitcode <- rawSystem command [apk, perm]
    return $ case exitcode of
      ExitSuccess   -> Bool' True
      ExitFailure 1 -> Bool' False
      _             -> error "failure in permissionsCheck"
fPermissionsCheck _ = error "permissionsCheck usage: [apk :: Constant, perm :: String]"

findCommand name = 
  do
    fs <- find always (fileName ==? name) "functions" 
    case fs of 
      (f:_) -> return f
      _ -> fail $ "the command '"++show name++"' could not be found"

findAPK name = 
  do
    fs <- find always (fileName ~~? ('*':name++"*.apk")) "apps" 
    case fs of 
      (f:_) -> return f
      _ -> fail $ "the app '"++show name++"' could not be found"

