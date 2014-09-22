{-- Code defining what is in an assertion context
 -}
module Logic.SecPAL.Context where

import Logic.SecPAL.Language
import Logic.SecPAL.Substitutions
import Logic.SecPAL.Pretty

data Context = Context { ac :: AC
                       , localAC :: AC
                       , d :: D
                       , theta :: [Substitution]
                       , debug :: Bool
                       , pluginDir :: String
                       }
  deriving (Eq,Show)

stdCtx :: Context
stdCtx = Context 
  { ac        = AC []
  , localAC   = AC []
  , d         = Infinity
  , theta     = []
  , debug     = False
  , pluginDir = "functions"
  }

instance PShow Context where
  --pShow ctx = "(" ++ pShow (ac ctx) ++ ", " ++ pShow (d ctx) ++ ")"
  pShow ctx = "AC, "++pShow (d ctx)++(if null (theta ctx) then "" else ' ':pShow (theta ctx))


getAC :: Context -> AC
getAC it = 
  let (AC x) = ac it
      (AC y) = localAC it
  in AC $ x ++ y
