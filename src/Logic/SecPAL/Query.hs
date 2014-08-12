{-# LANGUAGE FlexibleContexts, KindSignatures, NoMonomorphismRestriction, RankNTypes #-}
module Logic.SecPAL.Query where

import Control.Applicative ((<*), (*>), (<$>))
import Data.Maybe
import Logic.General.Entities
import Logic.General.Parser
import Logic.General.Pretty
import Logic.SecPAL.Language
import Logic.SecPAL.Parser
import qualified Logic.SecPAL.Substitutions as S
import qualified Logic.General.Types as T
import System.Directory (getDirectoryContents)
import System.FilePath.Posix
import Text.Parsec

data QueryOptions = QueryOptions
  { existentials :: [QueryOption]
  , queryTimeout :: Maybe QueryOption
  }
  deriving (Show, Eq)

instance PShow QueryOptions where
  pShow QueryOptions{ existentials=[], queryTimeout=Nothing } 
    = ""
  pShow QueryOptions{ existentials=[], queryTimeout=Just t } 
    = unlines [pShow t]
  pShow QueryOptions{ existentials=e, queryTimeout=Nothing } 
    = unlines . map pShow $ e
  pShow QueryOptions{ existentials=e, queryTimeout=Just t }
    = unlines $ map pShow e ++ [pShow t]

hasExistentials :: Query -> Bool
hasExistentials = not . null . existentials . options
 
data QueryOption = Existential { var::E, consts::Maybe [E] }
                 | Timeout Int
  deriving (Eq, Show)

instance PShow QueryOption where
  pShow Existential{ var=v, consts=Nothing } = ":for "++pShow v
  pShow Existential{ var=v, consts=vs } = ":for "++pShow v++" in "++pShow vs
  pShow (Timeout t) = ":timeout "++show t

defaultOptions :: QueryOptions
defaultOptions = QueryOptions
  { existentials = []
  , queryTimeout = Nothing
  }

joinOptions :: [QueryOption] -> QueryOptions
joinOptions = foldl joinOptions' defaultOptions

joinOptions' :: QueryOptions -> QueryOption -> QueryOptions
joinOptions' opts@QueryOptions{existentials=es} e@Existential{} = opts{existentials=e:es}
joinOptions' opts t@(Timeout _) = opts{queryTimeout=Just t}

data Query = Query
  { options :: QueryOptions
  , query   :: Assertion
  }
  deriving (Show)

instance PShow Query where
  pShow Query{options=o, query=q}
    = unlines [ pShow o, "?- " ++ pShow q ]

pQuery :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m Query
pQuery = do
  _  <- pWs
  o  <- many pOptions
  _  <- pWs
  q  <- pAssertionUnsafe -- Allowed for existential queries and safety checked later
  _  <- pWs
  o' <- many pOptions
  return Query{ options=joinOptions (o++o')
              , query=q
              }


pOptions :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m QueryOption
pOptions = try pExistential <|> try pTimeout <?> "query option"
  
pQueryOpt :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m ()
pQueryOpt = char ':' *> return ()

pExistential :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m QueryOption
pExistential = do
  pQueryOpt
  _ <- string "for "
  pWs
  v <- pVariable
  _ <- pWs
  vs <- optionMaybe (string "in " *> pExistentialList)
  pWs
  return Existential{var=v, consts=vs}

pExistentialList :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m [E]
pExistentialList = pWs *> char '[' *> pConstant `sepBy1` pListSep <* char ']'

pTimeout :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m QueryOption
pTimeout = Timeout <$> (pQueryOpt *> string "timeout " *> pWs *> (read <$> many1 digit) <* pWs)


queryableApps :: IO [E]
queryableApps = mapMaybe toE <$> getDirectoryContents "apps"
  where 
    toE f = 
      case splitExtension f of
        (app, ".apk") -> Just Constant{ constName=T.app++"#"++app, constType=T.app }
        _ -> Nothing

populateExistentials :: Query -> IO Query
populateExistentials q@Query{ options=os@QueryOptions{ existentials=es }} = do
  es' <- mapM populateExistential es
  let os' = os{ existentials=es' }
  return q{options=os'}

populateExistential :: QueryOption -> IO QueryOption
populateExistential e@Existential{consts=Nothing} = populateExistential' e
populateExistential x = return x

populateExistential' :: QueryOption -> IO QueryOption
populateExistential' e
  | T.typeof (var e) == T.app = queryableApps >>= \cs -> return e{consts=Just cs}
  | otherwise = fail $ "I can't figure what the possible values of "++pShow (var e)++" might be"


getSubs :: [QueryOption] -> [[S.Substitution]]
getSubs [] = []
getSubs [e] = map (:[]) $ getSubs' e
getSubs (es:fs) = [ e:f | e <- getSubs' es, f <- getSubs fs ]

getSubs' :: QueryOption -> [S.Substitution]
getSubs' Existential{ consts=Nothing } = error "trying to substitute unpopulated existential"
getSubs' Existential{ var=v, consts=Just cs } = 
  map (v `S.rename`) cs
getSubs' _ = []
