module Logic.SecPAL.Context where

import Logic.SecPAL.Language

data Context = Context { ac :: AC
                       , d :: D
                       }
  deriving (Eq)

instance Show Context where
  show ctx = "(" ++ show (ac ctx) ++ ", " ++ show (d ctx) ++ ")"

