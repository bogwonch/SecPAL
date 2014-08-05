module Logic.DatalogC.Safety where

import Logic.General.Entities
import qualified Logic.DatalogC.Language as L
import Logic.General.Vars

safe :: L.Clause -> Bool
safe = null . unsafeVars

unsafeVars :: L.Clause -> [E]
unsafeVars c =
  let hVars = vars . L.head $ c
      bVars = (vars . L.body $ c) ++ (vars . L.constraint $ c)
  in [ v | v <- hVars, v `notElem` bVars ]
  
