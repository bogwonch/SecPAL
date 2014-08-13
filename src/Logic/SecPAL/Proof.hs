module Logic.SecPAL.Proof where

import Logic.SecPAL.Language (Assertion)
import Logic.General.Constraints (C)
import Logic.SecPAL.Pretty
import Logic.General.Pretty()
import Logic.SecPAL.Context
import Data.Maybe
import Data.List

data Proof a = PStated { conclusion :: (Context, a) }
             | PCond { conclusion :: (Context, a) 
                     , ifs :: [[Proof Assertion]]
                     , constraint :: [Proof C]
                     , flatness :: Bool
                     }
             | PCanSay { conclusion :: (Context, a)
                       , delegation :: [Proof Assertion]
                       , action :: [Proof Assertion]
                       }
             | PCanActAs { conclusion :: (Context, a)
                         , renaming :: [Proof Assertion]
                         , renamed :: [Proof Assertion]
                         }
  deriving (Show)


instance (PShow a) => PShow (Proof a) where
  pShow prf = 
    let ac' = ac . fst . conclusion $ prf
    in "AC := " ++ pShow ac' ++ "\n" ++ pShow' 0 prf
  
showCtx :: (PShow a, PShow b) => (a, b) -> String
showCtx (ctx, a) = pShow ctx ++" |= "++pShow a

pShow' :: (PShow a) => Int -> Proof a -> String
pShow' n (PStated stm) = 
    let statement = showCtx stm
        proven = replicate (length statement) '-'
    in 
      intercalate "\n" $ map (replicate (n*2) ' ' ++) [statement, proven]

pShow' n PCond{conclusion=cc, ifs=[], constraint=c} = 
    intercalate "\n" [ replicate (n*2) ' ' ++ showCtx cc
                     , pShow' (n+1) (head c)
                     ]

pShow' n PCond{conclusion=cc, ifs=is, constraint=c} = 
    intercalate "\n" [ replicate (n*2) ' ' ++ showCtx cc
                     , intercalate "\n" $ map (pShow' (n+1). head) is 
                     , pShow' (n+1) (head c)
                     ]

pShow' n PCanSay{conclusion=cc, delegation=de, action=a} = 
    intercalate "\n" [ replicate (n*2) ' ' ++ showCtx cc
                     , pShow' (n+1) $ head de
                     , pShow' (n+1) $ head a
                     ]

pShow' n PCanActAs{conclusion=cc, renaming=r, renamed=q} = 
    intercalate "\n" [ replicate (n*2) ' ' ++ showCtx cc
                     , pShow' (n+1) $ head r
                     , pShow' (n+1) $ head q
                     ]

makeCond :: (Context, Assertion)
         -> [[Proof Assertion]]
         -> [Proof C]
         -> Bool
         -> [Proof Assertion]
makeCond cc is cs flat
  | any null is = []
  | null cs     = []
  | not flat    = []
  | otherwise   = [PCond cc is cs flat]

makeCanSay :: (Context, Assertion)
           -> [ Proof Assertion ]
      	   -> [ Proof Assertion ]
	         -> [ Proof Assertion ]
makeCanSay cc de a
  | null de   = []
  | null a    = []
  | otherwise = [PCanSay cc de a]


makeCanActAs :: (Context, Assertion)
             -> [ Proof Assertion ]
             -> [ Proof Assertion ]
             -> [ Proof Assertion ]
makeCanActAs cc delta q
  | null delta = []
  | null q     = []
  | otherwise  = [PCanActAs cc delta q]
