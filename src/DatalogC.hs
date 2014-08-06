module Main where

import Control.Monad
import Data.Either
import Logic.DatalogC.Parser
import Logic.DatalogC.Pretty
import System.Console.GetOpt
import System.Console.Readline
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import qualified Logic.DatalogC.Language as L


import Debug.Trace

data Options = Options
  { optScripts :: [FilePath]
  , optHelp :: Bool
  }
  deriving Show

defaultOptions :: Options
defaultOptions = Options
  { optScripts = []
  , optHelp    = False
  }

options :: [OptDescr (Options -> IO Options)]
options = 
  [ Option "f" ["file"]
      (ReqArg (\f opts -> return opts{optScripts = f : optScripts opts}) "FILE")
      "DatalogC knowledge base"
  , Option "h" ["help"]
      (NoArg (\opts -> return opts{optHelp=True}))
      "show this message"
  ]

usage :: IO ()
usage = do 
  prog <- getProgName
  let header = "Usage: " 
               ++ prog
               ++ " -f FILE [options]"
  putStrLn $ usageInfo header options

main :: IO ()
main = do
  argv <- getArgs
  let (actions, _, errors) = getOpt RequireOrder options argv
  opts <- foldl (>>=) (return defaultOptions) actions

  let Options { optScripts = scripts
              , optHelp    = help
              } = opts
              
  unless (null errors) $ mapM_ (hPutStrLn stderr) errors >> exitFailure
  when help            $ usage >> exitSuccess
  when (null scripts)  $ usage >> exitFailure

  parsed <- mapM (parseFromFile pProgram) scripts
  let (errs, datalog) = partitionEithers parsed

  traceIO $ "parsed " ++ show scripts
  unless (null errs) $ mapM_ (hPrint stderr) errs >> exitFailure

  mapM_ (mapM_ (putStrLn . pShow)) datalog
