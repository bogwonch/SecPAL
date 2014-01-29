module Main where

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
import Text.Parsec
import System.IO
import System.Exit
import Data.Maybe


getQuery = do
    putStrLn "Enter the query:"
    putStr " > " >> hFlush stdout
    line <- getLine
    let query = parse pAssertion "" line
    case query of
      (Left err) -> showErr line err >> return Nothing
      (Right sp) -> return . Just $ sp


getAC = do
    putStrLn "Enter the assertion context line by line."
    putStrLn "When done enter a blank line"
    acs <- getAC' []
    return . AC $ acs


getAC' ac = do
    putStr " > " >> hFlush stdout
    line <- getLine
    if line == "" 
      then return ac
      else let query = parse pAssertion "" line
           in case query of 
             (Left err) -> showErr line err >> return []
             (Right sp) -> getAC' (sp:ac) 

showErr line err =
    let y = sourceLine . errorPos $ err
        x = sourceColumn . errorPos $ err
        ls = lines line
        l = ls !! (y-1)
        arrow = replicate (x-1) ' '++"👆"
    in do
    print err
    putStrLn l
    putStrLn arrow


test ac = (Context{ac=ac, d=Infinity} ||-)

main = do
  q <- getQuery
  case q of 
    Nothing -> putStrLn ":-(" >> exitFailure
    Just sp -> putStrLn $ "Okay your query is: "++pShow sp
  ac <- getAC
  case ac of
    (AC []) -> putStrLn ":-(" >> exitFailure
    (AC (_:_)) -> putStrLn $ "Okay your assertion context contains: " ++ pShow ac
  
  print $ test ac (fromJust q) 

