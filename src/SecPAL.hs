module Main where

import Control.Monad hiding (forM_)
import Data.Either.Unwrap
import Data.Foldable (forM_)
import Data.Maybe
import Logic.SecPAL.Context
import Logic.SecPAL.Evaluable
import Logic.SecPAL.Language
import Logic.SecPAL.Parser
import Logic.SecPAL.Pretty
import Logic.SecPAL.DatalogC
import Logic.DatalogC.Pretty
import System.Console.GetOpt
import System.Console.Readline
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

data Options = Options
  { optACFile   :: Maybe String
  , optDebug    :: Bool
  , optHelp     :: Bool
  , optVerbose  :: Bool
  , optCheck    :: Bool
  , optPDatalog :: Bool
  } deriving Show

defaultOptions :: Options
defaultOptions  = Options
  { optACFile   = Nothing
  , optDebug    = False
  , optHelp     = False
  , optVerbose  = False
  , optCheck    = False
  , optPDatalog = False
  }

options :: [OptDescr (Options -> IO Options)] 
options =
  [ Option "f" ["file"] 
      (ReqArg (\f opts -> return opts{optACFile = Just f}) "FILE") 
      "assertion context file"
  , Option "d" ["debug"] 
      (NoArg (\opts -> return opts{optDebug = True})) 
      "debug mode"
  , Option "h" ["help"]
      (NoArg (\opts -> return opts{optHelp = True}))
      "show this message"
  , Option "v" ["verbose"]
      (NoArg (\opts -> return opts{optVerbose = True}))
      "be more chatty"
  , Option "c" ["check"]
      (NoArg (\opts -> return opts{optCheck = True}))
      "just check the file: no prompt"
  , Option "p" ["print-datalog"]
      (NoArg (\opts -> return opts{optPDatalog = True}))
      "just print the DatalogC translation of the SecPAL"
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
  let (actions, _, errors) = getOpt RequireOrder options argv

  -- Here we thread startOptions through all supplied option actions
  opts <- foldl (>>=) (return defaultOptions) actions

  let Options { optACFile   = acfile
              , optHelp     = help
              , optVerbose  = verbose
              , optCheck    = checking
              , optPDatalog = pdatalog
              } = opts

  unless (null errors)    $ mapM_ (hPutStrLn stderr) errors >> exitFailure
  when help               $ usage >> exitSuccess
  when (isNothing acfile) $ usage >> exitFailure

  parsedAC <- parseFromFile (many1 pAssertion) (fromJust acfile)

  whenLeft parsedAC $ \err -> hPrint stderr err >> exitFailure
  let theAC = fromRight parsedAC
  
  when verbose $ do
    putStrLn "The assertion context is:"
    mapM_ (putStrLn . ("  "++) . pShow) theAC 
    putStrLn ""

  when pdatalog $ do
    putStrLn "% vim: set ft=prolog:"
    mapM_ (mapM_ (putStrLn . pShow) . toDatalog) theAC
    exitSuccess

  unless checking $ do
    replWelcome
    repl opts theAC


replWelcome :: IO () 
replWelcome = do
  putStrLn "Loaded assertion context."
  putStrLn "Enter query or :h to see the help"
  
repl :: Options -> [Assertion] -> IO () 
repl opts c = do
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

    Just ":d!"      -> doToggleDebug
    Just ":debug!"  -> doToggleDebug
    Just ":d"       -> doSetDebug
    Just ":debug"   -> doSetDebug
    Just ":nod"     -> doSetNotDebug
    Just ":nodebug" -> doSetNotDebug

    Just ":h"    -> doHelp
    Just ":help" -> doHelp
    Just "?"     -> doHelp

    Just query -> doQuery query

  where
    recur = repl opts c

    doHelp = do
      putStrLn "Enter a SecPAL query or a REPL command"
      putStrLn ""
      putStrLn "Commands:"
      putStrLn "  exit, quit, :q, :qa  -> Quit"
      putStrLn "  :s, :show            -> Show the Assertion Context"
      putStrLn "  :v, :verbose         -> Turn on verbose mode"
      putStrLn "  :nov, :noverbose     -> Turn off verbose mode"
      putStrLn "  :v!, :verbose!       -> Toggle verbose mode"
      putStrLn "  :d, :debug           -> Turn on debug mode"
      putStrLn "  :nod, :nodebug       -> Turn off debug mode"
      putStrLn "  :d!, :debug!         -> Toggle debug mode"
      putStrLn "  :h, :help, ?         -> Show this message"
      recur

    doShow = do
      mapM_ (putStrLn . pShow) c
      recur 

    doQuery q = do
      case parse pAssertion "" q of
        (Left err) -> hPutStrLn stderr ("@ " ++ show err)
        (Right assertion) -> runQuery c assertion (optVerbose opts) (optDebug opts)
      recur


    doToggleVerbose = repl opts{optVerbose = not (optVerbose opts)} c
    doSetVerbose    = repl opts{optVerbose = True} c
    doSetNotVerbose = repl opts{optVerbose = False} c

    doToggleDebug = repl opts{optDebug = not (optDebug opts)} c
    doSetDebug    = repl opts{optDebug = True} c
    doSetNotDebug = repl opts{optDebug = False} c


runQuery :: (PShow x, Evaluable x) => [Assertion] -> x -> Bool -> Bool -> IO () 
runQuery c q verbose debugging = do
  let ctx = stdCtx{ac=AC c, debug=debugging}
  decision <- ctx ||- q
  case decision of
    Nothing      -> putStrLn "! No."
    (Just proof) -> do when verbose $ (putStrLn . pShow) proof 
                       unless verbose $ putStrLn "! Yes."



