module Logic.SecPAL.Context where

import Logic.SecPAL.Language
import Logic.SecPAL.Substitutions
import Logic.SecPAL.Pretty

data Context = Context { ac :: AC
                       , d :: D
                       , theta :: [Substitution]
                       , debug :: Bool
                       }
  deriving (Eq,Show)

stdCtx :: Context
stdCtx = Context { ac=AC [], d=Infinity, theta=[], debug=False }

instance PShow Context where
  --pShow ctx = "(" ++ pShow (ac ctx) ++ ", " ++ pShow (d ctx) ++ ")"
  pShow ctx = "AC, "++pShow (d ctx)++(if null (theta ctx) then "" else ' ':pShow (theta ctx))
