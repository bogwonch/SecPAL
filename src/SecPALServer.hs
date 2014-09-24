{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-- Listening server for SecPAL queries -}
module Main where

import           Control.Applicative
import           Control.Concurrent.ParallelIO.Global
import           Control.Monad (when, unless)
import           Data.Aeson
import           Data.Either.Unwrap
import           Data.List
import           Data.Maybe
import           GHC.Generics (Generic)
import           Logic.SecPAL.AssertionSafety
import           Logic.SecPAL.Context
import           Logic.SecPAL.Evaluable
import           Logic.SecPAL.Parser
import           Logic.SecPAL.Pretty
import           Logic.SecPAL.Proof
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Parsec
import           Text.Parsec.Error
import           Text.Parsec.String
import qualified Data.Text.Lazy             as T
import qualified Logic.SecPAL.Language      as SecPAL
import qualified Logic.SecPAL.Query         as Q
import qualified Logic.SecPAL.Substitutions as S
import qualified Web.Scotty                 as Server

import           System.IO.Unsafe

{- Command line argument handling -}
data Options = Options
  { optACFile :: Maybe String
  , optHelp   :: Bool
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { optACFile = Nothing
  , optHelp   = False
  }

options :: [OptDescr (Options -> IO Options)]
options = 
  [ Option "f" ["file"]
      (ReqArg (\f opts -> return opts{optACFile = Just f}) "FILE")
      "assertion context file"
  , Option "h" ["help"]
      (NoArg (\opts -> return opts{optHelp = True}))
      "show this message"
  ]

usage :: IO ()
usage = do
  prog <- getProgName
  let header = "Usage: "
               ++ prog
               ++ " -f FILE [options]"
  putStrLn $ usageInfo header options

{- Query received by Scotty -}
data RemoteQuery = 
  RemoteQuery { localContext :: [String]
              , query        :: String
              }
    deriving (Show, Generic)

instance FromJSON RemoteQuery 
instance ToJSON RemoteQuery
  
main :: IO ()
main = do
  argv <- getArgs
  let (actions, _, errors) = getOpt RequireOrder options argv
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optACFile   = acfile
              , optHelp     = help
              } = opts

  unless (null errors)    $ mapM_ (hPutStrLn stderr) errors >> exitFailure
  when help               $ usage >> exitSuccess
  when (isNothing acfile) $ usage >> exitFailure

  parsedAC <- parseFromFile (many1 pAssertionUnsafe) (fromJust acfile)

  whenLeft parsedAC $ \err -> hPrint stderr err >> exitFailure
  let theAC = fromRight parsedAC
  let ctx = stdCtx{ ac=SecPAL.AC theAC }
  webapp ctx

webapp :: Context -> IO ()
webapp ctx = Server.scotty 2345 $
  Server.post "/" $ do
    (obj :: RemoteQuery) <- Server.jsonData
    case parseQuery obj of
      (Left err) -> Server.raise . T.pack . parseErrors $ err
      (Right sp) -> do
        -- UNCLEAN UNCLEAN!!!!!
        let out = unsafePerformIO $ handleSecPAL ctx sp
        Server.text . T.pack $ out

handleSecPAL :: Context -> ([SecPAL.Assertion], Q.Query) -> IO String
handleSecPAL ctx (localCtx, q) = 
  --return $ "<p>Context: " ++ pShow ctx ++ "</p><p>Query: "++pShow q
  runQuery ctx{localAC=SecPAL.AC localCtx} q False False

parseErrors :: ParseError -> String
parseErrors = showErrorMessages "or" "unknown" "expecting" "unexpected" "end-of-input" . errorMessages

--parseQuery :: RemoteQuery -> ([SecPAL.Assertion], SecPAL.Assertion)
parseQuery :: RemoteQuery -> Either ParseError ([SecPAL.Assertion], Q.Query)
parseQuery q = do
  ctx <- mapM (parse pAssertion "") (localContext q)
  qry <- parse Q.pQuery "" (query q)
  return (ctx, qry)


{- OH FOR THE LOVE OF GOD REFACTOR MAN -}
runQuery :: Context -> Q.Query -> Bool -> Bool -> IO String 
runQuery ctx q _ _ = 
  if Q.hasExistentials q
    then runExistentialQuery ctx q False False
    else do 
      p <- runAssertion ctx (Q.query q) False False 
      printResult False False p

runAssertion :: Context -> SecPAL.Assertion -> Bool -> Bool -> IO [ Proof SecPAL.Assertion ]
runAssertion ctx a _ _ = do
  unless (safe a) ( fail $ "query "++pShow a++" is unsafe" )
  ctx ||- a

printResult :: Bool -> Bool -> [Proof SecPAL.Assertion] -> IO String
printResult _ _ decision =
  case decision of
    []      -> return "! No."
    _       -> return "! Yes."

runExistentialQuery :: Context  -> Q.Query -> Bool -> Bool -> IO String
runExistentialQuery ctx q verbose debugging = do
  q' <- Q.populateExistentials q
  let ss = Q.getSubs (Q.existentials . Q.options $ q') 
  let a = Q.query q'
  let as = [ (a `S.subAll` s, s) | s <- ss ]

  results <- parallel $ map (runExistentialQuery' verbose debugging ctx) as
  return $ header ++ intercalate "," results ++ footer

  where
    header = "["
    footer = "]"

runExistentialQuery' :: Bool -> Bool -> Context -> (SecPAL.Assertion, [S.Substitution]) -> IO String
runExistentialQuery' verbose debugging ctx (a,_) = do
  decision <- runAssertion ctx a verbose debugging
  return . result2JSON a $ decision

result2JSON :: SecPAL.Assertion -> [Proof SecPAL.Assertion] -> String
result2JSON a decision =
 "{ \"query\": \""++pShow a++"\", \"result\": \""++result++"\" }"
 where
  result = show . not . null $ decision
