module Logic.DatalogC.Pretty (pShow) where

import Logic.General.Pretty
import Logic.General.Constraints
import Logic.DatalogC.Language as D
import Data.Char
import Data.List

instance PShow Clause where
  pShow Clause{ D.head=h, D.body=b, D.constraint=c } = 
    let h' = pShow h
        b' = if null b
               then ""
               else " :- "++intercalate ", " (map pShow b)
        c' = if c == Boolean True
               then ""
               else " ["++pShow c++"]"

    in h' ++ b' ++ c' ++ "."

instance PShow Predicate where
  pShow p = predName p ++ arity ++ arguments
    where
      arity     = "" --"/" ++ (show . length . args $ p)
      arguments = "(" ++ intercalate ", " (map pShow (args p)) ++ ")"

      predName = map toLower . name
