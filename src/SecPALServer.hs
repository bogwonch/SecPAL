{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Applicative
import           Data.Aeson
import           Data.Text.Lazy
import           GHC.Generics (Generic)
import           Web.Scotty
import qualified Data.ByteString.Lazy as B
import qualified Logic.SecPAL.Language as SecPAL
import           Logic.SecPAL.Parser
import           Text.Parsec
import qualified Logic.SecPAL.Query as Q

data RemoteQuery = 
  RemoteQuery { localContext :: [String]
              , query        :: String
              }
    deriving (Show, Generic)

instance FromJSON RemoteQuery 
instance ToJSON RemoteQuery
  
main :: IO ()
main = scotty 2345 $
  post "/" $ do
    (obj :: RemoteQuery) <- jsonData
    text $ pack (show obj)

--parseQuery :: RemoteQuery -> ([SecPAL.Assertion], SecPAL.Assertion)
parseQuery :: RemoteQuery -> Either ParseError ([SecPAL.Assertion], SecPAL.Assertion)
parseQuery q = do
  ctx <- mapM (parse pAssertion "localContext") (localContext q)
  qry <- parse pAssertion "query" (query q)
  return (ctx, qry)
