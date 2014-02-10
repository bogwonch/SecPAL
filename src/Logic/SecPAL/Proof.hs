module Logic.SecPAL.Proof where

import Logic.SecPAL.Language (Assertion, C)
import Logic.SecPAL.Pretty
import Logic.SecPAL.Context
import Data.Maybe
import Data.List

data Proof a = PStated { conclusion :: (Context, a) }
             | PCond { conclusion :: (Context, a) 
                     , ifs :: [ Proof Assertion ]
                     , constraint :: Proof C
                     , flatness :: Bool
                     }
             | PCanSay { conclusion :: (Context, a)
                       , delegation :: Proof Assertion
                       , action :: Proof Assertion
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

pShow' n PCond{conclusion=cc, ifs=is, constraint=c} = 
    intercalate "\n" [ replicate (n*2) ' ' ++ showCtx cc
                     , intercalate "\n" $ map (pShow' (n+1)) is 
                     , pShow' (n+1) c
                     ]

pShow' n PCanSay{conclusion=cc, delegation=de, action=a} = 
    intercalate "\n" [ replicate (n*2) ' ' ++ showCtx cc
                     , pShow' (n+1) de
                     , pShow' (n+1) a
                     ]

makeCond :: (Context, Assertion)
         -> [Maybe (Proof Assertion)]
         -> Maybe (Proof C)
         -> Bool
         -> Maybe (Proof Assertion)
makeCond cc is cs flat
  | any isNothing is = Nothing
  | isNothing cs = Nothing
  | not flat = Nothing
  | otherwise = Just $ PCond cc (map fromJust is) (fromJust cs) flat

makeCanSay :: (Context, Assertion)
           -> Maybe (Proof Assertion)
	   -> Maybe (Proof Assertion)
	   -> Maybe (Proof Assertion)
makeCanSay cc de a
  | isNothing de = Nothing
  | isNothing a = Nothing
  | otherwise = Just $ PCanSay cc (fromJust de) (fromJust a)
