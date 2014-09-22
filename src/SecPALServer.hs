{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-- Listening server for SecPAL queries -}
module Main where

import           Control.Applicative
import           Control.Monad (when, unless)
import           Data.Aeson
import           Data.Either 
import           Data.List
import           GHC.Generics (Generic)
import           Logic.SecPAL.Context
import           Logic.SecPAL.Parser
import           Logic.SecPAL.Pretty
import           System.IO
import           Text.Parsec
import           Text.Parsec.Error
import           Web.Scotty
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Logic.SecPAL.Language as SecPAL
import qualified Logic.SecPAL.Query as Q

data RemoteQuery = 
  RemoteQuery { localContext :: [String]
              , query        :: String
              }
    deriving (Show, Generic)

instance FromJSON RemoteQuery 
instance ToJSON RemoteQuery
  
main :: IO ()
main = webapp

webapp :: IO ()
webapp = scotty 2345 $
  post "/" $ do
    (obj :: RemoteQuery) <- jsonData
    text (T.pack . show . query $ obj)
    case parseQuery obj of
      (Left err) -> raise . T.pack . parseErrors $ err
      (Right sp) -> text . T.pack . handleSecPAL $ sp

handleSecPAL :: ([SecPAL.Assertion], Q.Query) -> String
handleSecPAL (ctx, q) = 
  "<p>Context: " ++ pShow ctx ++ "</p><p>Query: "++pShow q

parseErrors :: ParseError -> String
parseErrors = showErrorMessages "or" "unknown" "expecting" "unexpected" "end-of-input" . errorMessages

--parseQuery :: RemoteQuery -> ([SecPAL.Assertion], SecPAL.Assertion)
parseQuery :: RemoteQuery -> Either ParseError ([SecPAL.Assertion], Q.Query)
parseQuery q = do
  ctx <- mapM (parse pAssertion "") (localContext q)
  qry <- parse Q.pQuery "" (query q)
  return (ctx, qry)
