module Main where

import Control.Monad hiding (forM_)
import Data.Either.Unwrap
import Data.Foldable (forM_)
import Data.Maybe
import Logic.SecPAL.AssertionSafety
import Logic.SecPAL.Context
import Logic.SecPAL.Evaluable
import Logic.SecPAL.Language
import Logic.SecPAL.Named
import Logic.SecPAL.Parser
import Logic.SecPAL.Pretty
import Logic.SecPAL.Proof
import Logic.SecPAL.Substitutions
import Logic.SecPAL.Vars
import System.Console.GetOpt
import System.Console.Readline
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

data Options = Options
  { optACFile  :: Maybe String
  , optDebug   :: Bool
  , optHelp    :: Bool
  , optVerbose :: Bool
  } deriving Show

defaultOptions = Options
  { optACFile  = Nothing
  , optDebug   = False
  , optHelp    = False
  , optVerbose = False
  }

options =
  [ Option "f" ["file"] 
      (ReqArg (\f opts -> return opts{optACFile = Just f}) "FILE") 
      "assertion context file"
  , Option "d" ["debug"] 
      (NoArg (\opts -> return opts{optDebug = True})) 
      "debug (unimplemented)"
  , Option "h" ["help"]
      (NoArg (\opts -> return opts{optHelp = True}))
      "show this message"
  , Option "v" ["verbose"]
      (NoArg (\opts -> return opts{optVerbose = True}))
      "be more chatty"
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

  -- Parse options, getting a list of option actions
  let (actions, nonOptions, errors) = getOpt RequireOrder options argv

  -- Here we thread startOptions through all supplied option actions
  opts <- foldl (>>=) (return defaultOptions) actions

  let Options { optACFile  = acfile
              , optDebug   = debug   
              , optHelp    = help
              , optVerbose = verbose
              } = opts

  unless (null errors)    $ mapM_ (hPutStrLn stderr) errors >> exitFailure
  when help               $ usage >> exitSuccess
  when (isNothing acfile) $ usage >> exitFailure

  parsedAC <- parseFromFile (many1 pAssertion) (fromJust acfile)

  whenLeft parsedAC $ \err -> hPrint stderr err >> exitFailure
  let ac = fromRight parsedAC
  
  when verbose $ do
    putStrLn "The assertion context is:"
    mapM_ (putStrLn . ("  "++) . pShow) ac 
    putStrLn ""

  replWelcome

  repl opts ac


replWelcome = do
  putStrLn "Loaded assertion context."
  putStrLn "Enter query or :h to see the help"
  
repl opts ac = do
  let Options { optACFile  = acfile
              , optDebug   = debug   
              , optHelp    = help
              , optVerbose = verbose
              } = opts

  input <- readline "? "
  forM_ input addHistory
  case input of
    -- Quiting functions
    Nothing -> return () -- EOF or ^D
    Just "exit" -> return ()
    Just "quit" -> return ()
    Just ":q"   -> return ()
    Just ":qa"  -> return ()
    
    -- Show AC
    Just ":s"    -> doShow
    Just ":show" -> doShow

    Just ":v!"        -> doToggleVerbose
    Just ":verbose!"  -> doToggleVerbose
    Just ":v"         -> doSetVerbose
    Just ":verbose"   -> doSetVerbose
    Just ":nov"       -> doSetNotVerbose
    Just ":noverbose" -> doSetNotVerbose

    Just ":h"    -> doHelp
    Just ":help" -> doHelp
    Just "?"     -> doHelp

    Just query -> doQuery query

  where
    recur = repl opts ac

    doHelp = do
      putStrLn "Enter a SecPAL query or a REPL command"
      putStrLn ""
      putStrLn "Commands:"
      putStrLn "  exit, quit, :q, :qa  -> Quit"
      putStrLn "  :s, :show            -> Show the Assertion Context"
      putStrLn "  :v, :verbose         -> Turn on verbose mode"
      putStrLn "  :nov, :noverbose     -> Turn off verbose mode"
      putStrLn "  :v!, :verbose!       -> Toggle verbose mode"
      putStrLn "  :h, :help, ?         -> Show this message"
      recur

    doShow = do
      mapM_ (putStrLn . pShow) ac
      recur 


    doQuery q = do
      case parse pAssertion "" q of
        (Left err) -> hPutStrLn stderr ("@ " ++ show err)
        (Right assertion) -> runQuery ac assertion (optVerbose opts)
      recur


    doToggleVerbose = repl opts{optVerbose = not (optVerbose opts)} ac
    doSetVerbose    = repl opts{optVerbose = True} ac
    doSetNotVerbose = repl opts{optVerbose = False} ac


runQuery c q verbose = 
  let ctx = stdCtx{ac=AC c}
      decision = ctx ||- q
  in case decision of
    Nothing  -> putStrLn "! No."
    (Just proof) -> do when verbose $ (putStrLn . pShow) proof 
                       unless verbose $ putStrLn "! Yes."



