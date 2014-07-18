module Logic.DatalogC.Safety where

import qualified Logic.DatalogC.Language as L
import Logic.DatalogC.Vars

safe :: L.Clause -> Bool
safe = null . unsafeVars

unsafeVars :: L.Clause -> [L.Entity]
unsafeVars c =
  let hVars = vars . L.head $ c
      bVars = (vars . L.body $ c) ++ (vars . L.constraint $ c)
  in [ v | v <- hVars, v `notElem` bVars ]
  
