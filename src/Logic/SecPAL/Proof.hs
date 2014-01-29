module Logic.SecPAL.Proof where

import Logic.SecPAL.Language (Assertion, C)
import Logic.SecPAL.Pretty
import Logic.SecPAL.Context

data Proof a = PStated { conclusion :: (Context, a) }
             | PCond { conclusion :: (Context, a) 
                     , ifs :: [ Proof Assertion ]
                     , constraint :: Proof C
                     , flatness :: Proof Bool
                     }
             | PCanSay { conclusion :: (Context, a)
                       , delegation :: Proof Assertion
                       , action :: Proof Assertion
                       }
  deriving (Show)


instance (PShow a) => PShow (Proof a) where
  pShow = pShow' 0

showCtx (ctx, a) = pShow ctx ++" |= "++pShow a

pShow' :: (PShow a) => Int -> Proof a -> String
pShow' n (PStated stm) = 
    let statement = showCtx stm
        proven = replicate (length statement) '-'
    in 
      unlines $ map (replicate (n*2) ' ' ++) [statement, proven]

pShow' n PCond{conclusion=cc, ifs=ifs, constraint=c} = 
    unlines [ replicate (n*2) ' ' ++ showCtx cc
            , concatMap (pShow' (n+1)) ifs 
            , pShow' (n+1) c
            ]

pShow' n PCanSay{conclusion=cc, delegation=d, action=a} = 
    unlines [ replicate (n*2) ' ' ++ showCtx cc
            , pShow' (n+1) d
            , pShow' (n+1) a
            ]

