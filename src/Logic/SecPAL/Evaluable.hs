{-# OPTIONS -Wall #-}
module Logic.SecPAL.Evaluable where

import Control.Applicative
import Control.Monad (when, forM)
import Data.Array.IO
import Data.List
import Data.Maybe
import Logic.General.Entities
import Logic.General.Constraints
import Logic.SecPAL.AssertionSafety (flat)
import Logic.SecPAL.Context
import Logic.General.ConstraintEvaluation
import Logic.SecPAL.Language
import Logic.SecPAL.Pretty
import Logic.SecPAL.Proof hiding (constraint, delegation)
import Logic.SecPAL.Substitutions
import System.Console.ANSI
import System.Random

class Evaluable x where 
    (||-) :: Context -> x -> IO (Maybe (Proof x))

instance Evaluable C where
  ctx ||- c@(Boolean True) = return . Just $ PStated (ctx,c)
  _ ||- (Boolean False) = return Nothing

  ctx ||- c@(Equals a b) = do
    a' <- evaluate ctx a
    b' <- evaluate ctx b
    return $ if a' == b' 
      then Just . PStated $ (ctx, c)
      else Nothing

  ctx ||- c@(Not c') = do
    p <- isJust <$> ctx ||- c'
    if p 
      then return . Just $ PStated (ctx,c)
      else return Nothing

  ctx ||- c@(Conj x y) = do
    pX <- isJust <$> ctx ||- x
    pY <- isJust <$> ctx ||- y
    return $ if pX && pY 
      then Just $ PStated (ctx,c)
      else Nothing


type Result = Maybe (Proof Assertion)
instance Evaluable Assertion where
  ctx ||- x = do
    {- We're shuffling them so that we try and avoid infinite loops where a
     - cond statement will loop but a cansay will terminate. 
     -
     - Probably a neater way of doing this but randomization is cheap and
     - usually good!
     -}
    strategies <- shuffle [statedStrategy, condStrategy, canSayStrategy, canActAsStrategy]
    tryStrategies ctx x strategies 
    where
      -- | Randomly shuffle a list
      --   /O(N)/
      shuffle :: [a] -> IO [a]
      shuffle xs = do
              ar <- makeArray len xs
              forM [1..len] $ \i -> do
                  j <- randomRIO (i,len)
                  vi <- readArray ar i
                  vj <- readArray ar j
                  writeArray ar j vi
                  return vj
        where
          len = length xs
          makeArray :: Int -> [a] -> IO (IOArray Int a)
          makeArray l = newListArray (1,l)


type Strategy = Context -> Assertion -> IO Result

statedStrategy :: Strategy
statedStrategy = strategy tryStated "known fact"

condStrategy :: Strategy
condStrategy   = strategy tryCond   "derivable fact"

canSayStrategy :: Strategy
canSayStrategy = strategy tryCanSay "delegatable"

canActAsStrategy :: Strategy
canActAsStrategy = strategy tryCanActAs "renameable"

strategy :: (Context -> Assertion -> IO Result)
         -> String
         -> Context
         -> Assertion
         -> IO Result
strategy f msg ctx x = do
  result <- f ctx x
  let isSuccessful = isJust result

  when (debug ctx) $
    putStrLn . useColor isSuccessful $ 
      "@ '"++pShow x++"' "++usePrefix isSuccessful++" "++msg

  return $ if isSuccessful then result else Nothing
  where 
    inColor c str = setSGRCode [SetColor Foreground Dull c] ++ str ++ setSGRCode[Reset]
    
    useColor True   = inColor Green
    useColor False  = inColor Red
    usePrefix True  = "is"
    usePrefix False = "is not"


tryStrategies :: Context -> Assertion -> [Strategy] -> IO Result
tryStrategies _ _ [] = return Nothing
tryStrategies ctx x (s:ss) = do
  result <- s ctx x
  if isJust result 
    then return result 
    else tryStrategies ctx x ss 


isIn :: Assertion -> AC -> Bool
x `isIn` (AC xs) = x `elem` xs

-- The Glorious Cond Rule!
--
-- (A says f if f_1, .., f_n, c) ∈ AC
-- AC,D |= A says f_i, ∀i ∈ (1..n)    |= c    flat(f)
-- --------------------------------------------------
--                  AC, D |= A says f
--
cond' :: Context -> Assertion -> Assertion -> IO Result
cond' ctx result query =
  let w = who query
      whoSays = asserts w
      fs = conditions (says query)
      aSaysFs = map whoSays fs
      AC theAC = ac ctx
      ac' = AC $ query `delete` theAC -- removes cond infinite loop
      ctx' = ctx{theta=[], ac=ac'}
  in do
    ifStatements <- mapM (ctx' ||-) aSaysFs
    cs <- ctx' ||- (constraint . says $ query)
    return $
      makeCond 
        (ctx,result) 
        ifStatements
        cs
        (flat . fact . says $ query)


cond :: Context -> Assertion -> Assertion -> IO Result
cond ctx result query =
  let query'        = simplify query
      delta         = fromJust $ result ==? query'
      renamedQuery  = subAll query delta
      renamedResult = subAll result delta
  in 
      cond' ctx{theta=delta} renamedResult renamedQuery
  where
    simplify q = 
      let says' = (says q){ conditions=[], constraint=Boolean True }
          q' = q{ says=says' }
      in q'

-- The Mysterious Can-Say Rule!
--
-- AC, oo |= A says B can-say D fact    AC, D |= B says fact
-- ---------------------------------------------------------
--                     AC, oo |= A says fact
canSay' :: Context -> Assertion -> Assertion -> Assertion -> IO Result
canSay' Context{d=Zero} _ _ _ = return Nothing
canSay' ctx query canSayStm origCanSay = 
  let w = who query
      f = fact . says $ query
      cs = fact . says $ canSayStm
      f' = what . verb $ cs 
      de = delegation . verb $ cs
      b = subject cs
      ctx' = ctx{theta=[]}
      AC theAC = ac ctx
      ac' = AC $ origCanSay `delete` theAC -- to avoid infinite loop
  in if f == f'
       then do
         del <- ctx' ||- delegates w b f de 
         ass <- ctx'{d=de, ac=ac'} ||- (b `asserts` f)
         return $
           makeCanSay 
             (ctx,query) 
             del
             ass
       else return Nothing

canSay :: Context -> Assertion -> Assertion -> IO Result
canSay ctx result canSayStm = 
  let canSayStm'    = simplify canSayStm
      pRenaming     = result ==? canSayStm'
      delta      = fromMaybe (error "can say statement failed to simplify") pRenaming 
      renamedCSS    = subAll canSayStm delta
      renamedResult = subAll result delta
  in
    canSay' ctx{theta=delta} renamedResult renamedCSS canSayStm
  where
    simplify q = 
      let f = what . verb . fact . says $ q
          claim' = (says q){ fact = f, conditions=[], constraint=Boolean True }
      in q{ says=claim' }

-- Sublime Can-act-as Rule
--
-- AC, D |= A says B can-act-as C    AC, D |= A says C verbphrase
-- --------------------------------------------------------------
--                  AC, D |= A says B verbphrase
canActAs' :: Context -> Assertion -> Assertion -> Assertion -> IO Result
canActAs' ctx query canActAsStm renamedQuery = do
  delta <- ctx ||- canActAsStm
  result <- ctx ||- renamedQuery
  return $
    makeCanActAs
      (ctx,query)
      delta
      result

canActAs :: Context -> Assertion -> Assertion -> IO Result
canActAs ctx query canActAsStm =
  let delta = whom . verb . fact . says $ canActAsStm
      c = says query
      f = fact c
      renamedQuery = query{ says=c{ fact=f{ subject=delta } } }
  in
    canActAs' ctx query canActAsStm renamedQuery

asserts :: E -> Fact -> Assertion
a `asserts` f = Assertion { who=a
                          , says = Claim { fact=f
                                         , conditions=[]
                                         , constraint=Boolean True
                                         }
                          }

delegates :: E -> E -> Fact -> D -> Assertion
delegates from to w level =
    Assertion { who=from
              , says = Claim { fact = Fact { subject = to
                                           , verb = CanSay level w
                                           }
                             , conditions=[]
                             , constraint=Boolean True
                             }
              }

tryStated :: Context -> Assertion -> IO Result
tryStated ctx a =  
  let as = filter (isJust . (==? a)) (acs $ ac ctx)
  in case as of
    [] -> return Nothing
    (p:_) -> 
      let ts = fromJust $ p ==? a
      in 
        return . Just $ PStated (ctx{theta=ts}, p)

tryCond :: Context -> Assertion -> IO Result
tryCond ctx a = 
    let as = filter (isSpecific a) (acs (ac ctx))
    in do
      candidates <- mapM (cond ctx a) as
      return $ case filter isJust candidates of
         [] -> Nothing
         (p:_) -> p

tryCanSay :: Context -> Assertion -> IO Result
tryCanSay ctx a = 
    let as = filter (isDelegation a) (acs (ac ctx))
    in do
      candidates <- mapM (canSay ctx a) as
      return $ case filter isJust candidates of
         [] -> Nothing
         (p:_) -> p

tryCanActAs :: Context -> Assertion -> IO Result
tryCanActAs ctx a = 
  let as = filter (isRenaming a) (acs $ ac ctx)
  in do
    candidates <- mapM (canActAs ctx a) as
    return $ case filter isJust candidates of
      [] -> Nothing
      (p:_) -> p

isSpecific :: Assertion -> Assertion -> Bool
x `isSpecific` y = 
    let 
        who_x = who x
        who_y = who y
        says_x = says x
        says_y = says y
        fact_x = fact says_x
        fact_y = fact says_y

        whose = isJust $ who_x ==? who_y
        facts = isJust $ fact_x ==? fact_y
        result = whose && facts
    in 
        --trace (show x ++ (if result then " <=== " else " <=/= ") ++ show y) $ 
        result


isDelegation :: Assertion -> Assertion -> Bool
isDelegation 
  Assertion{ who=a
           , says=Claim{fact=f}
           } 
  Assertion{ who=a'
           , says=Claim{ fact=Fact{ verb=CanSay{what=f'} } }
           }
    = isJust (a ==? a') && isJust (f ==? f')
isDelegation _ _ = False


isRenaming :: Assertion -> Assertion -> Bool
isRenaming
  Assertion{ who=a
           , says=Claim{ fact=Fact{ subject=b } }
           }
  Assertion{ who=a'
           , says=Claim{ fact=Fact{ subject=b'
                                  , verb=CanActAs{ whom=_ }
                                  }
                       }
           }
    = isJust (a ==? a') && isJust (b ==? b')

isRenaming _ _ = False
