module Main where

import Control.Applicative ((<$>))
import Control.Monad hiding (forM_)
import Control.Concurrent.ParallelIO.Global
import Data.Either.Unwrap
import Data.Foldable (forM_)
import Data.Maybe
import Logic.SecPAL.Context
import Logic.SecPAL.Evaluable
import Logic.SecPAL.Language
import Logic.SecPAL.Parser
import Logic.SecPAL.Proof
import Logic.SecPAL.AssertionSafety
import Logic.SecPAL.Pretty
import qualified Logic.SecPAL.Query as Q
import qualified Logic.SecPAL.Substitutions as S
import Logic.SecPAL.Z3Datalog
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

  parsedAC <- parseFromFile (many1 pAssertionUnsafe) (fromJust acfile)

  whenLeft parsedAC $ \err -> hPrint stderr err >> exitFailure
  let theAC = fromRight parsedAC
  
  when verbose $ do
    putStrLn "The assertion context is:"
    mapM_ (putStrLn . ("  "++) . pShow) theAC 
    putStrLn ""

  when pdatalog $ do
    let dl = toDatalog $ AC theAC
    putStrLn dl
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

    Just "" -> repl opts c

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
      case parse Q.pQuery "" q of
        (Left err) -> hPutStrLn stderr ("@ " ++ show err)
        (Right query) -> runQuery c query (optVerbose opts) (optDebug opts)
      recur


    doToggleVerbose = repl opts{optVerbose = not (optVerbose opts)} c
    doSetVerbose    = repl opts{optVerbose = True} c
    doSetNotVerbose = repl opts{optVerbose = False} c

    doToggleDebug = repl opts{optDebug = not (optDebug opts)} c
    doSetDebug    = repl opts{optDebug = True} c
    doSetNotDebug = repl opts{optDebug = False} c


runQuery :: [Assertion] -> Q.Query -> Bool -> Bool -> IO () 
runQuery c q verbose debugging = 
  let ctx = stdCtx{ac=AC c, debug=debugging}
  in if Q.hasExistentials q
       then runExistentialQuery ctx q verbose debugging
       else do 
          p <- runAssertion ctx (Q.query q) verbose debugging 
          printResult verbose debugging p

runAssertion :: Context -> Assertion -> Bool -> Bool -> IO [ Proof Assertion ]
runAssertion ctx a _ _ = do
  unless (safe a) ( fail $ "query "++pShow a++" is unsafe" )
  ctx ||- a

printResult :: Bool -> Bool -> [Proof Assertion] -> IO ()
printResult verbose _ decision =
  case decision of
    []      -> putStrLn "! No."
    proof   -> do when verbose $ (putStrLn . pShow . head) proof 
                  unless verbose $ putStrLn "! Yes."

runExistentialQuery :: Context  -> Q.Query -> Bool -> Bool -> IO ()
runExistentialQuery ctx q verbose debugging = do
  q' <- Q.populateExistentials q
  let ss = Q.getSubs (Q.existentials . Q.options $ q') 
  let a = Q.query q'
  let as = [ (a `S.subAll` s, s) | s <- ss ]

  parallel_ $ map (runExistentialQuery' verbose debugging ctx) as
  return ()

runExistentialQuery' :: Bool -> Bool -> Context -> (Assertion, [S.Substitution]) -> IO ()
runExistentialQuery' verbose debugging ctx (a,s) = do
  decision <- runAssertion ctx a verbose debugging
  unless (null decision) $ do
    --putStrLn $ pShow s ++ " ?- " ++ pShow a
    putStrLn $ pShow a
    when verbose $ (putStrLn . pShow . head) decision
    --printResult verbose debugging decision
  return ()

