{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Algebra
import Model
import Interpreter
import Lexer
import System.Exit
import Parser
import System.Environment (getArgs)

spaceToStdOut :: Space -> IO ()
spaceToStdOut s = putStr (printSpace s)

programCompletionToStdOut :: Space -> Pos -> Heading -> IO ()
programCompletionToStdOut s p h = do putStrLn "Finished program succesfully!"
                                     putStrLn ("Final position: " ++ show p ++ ". Final heading: " ++ show h ++ ". Final space:")
                                     putStrLn ""
                                     spaceToStdOut s

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive env as@ArrowState{space, pos, heading} =
  do putStrLn ""
     putStrLn "Current space:" 
     spaceToStdOut space
     putStrLn ""
     putStrLn ("Current position:" ++ show pos)
     putStrLn ("Current heading:" ++ show heading)
     putStrLn ""
     putStrLn "Next command:"
     print (head $ stack as)
     putStrLn "Continue? Press y (yes) or n (no)."
     key <- getLine
     case key of
      "y" -> executeStep env as
      "n" -> die "Execution stopped."
  where
    executeStep e a = let nextStep = step e a
                       in case nextStep of
                            (Done s p h) -> programCompletionToStdOut s p h
                            (Fail m)     -> do putStrLn "Failed with the following error: "
                                               putStrLn m
                            (Ok as')     -> interactive env as'

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch e a = case step e a of
              Done s p h -> (s, p, h)
              Fail m -> error m
              Ok as -> batch e as

main :: IO ()
main = do args <- getArgs
          case args of
            [spacePath, programPath, startPosStr, headingStr, mode] -> do spaceStr <- readFile spacePath
                                                                          let space = run parseSpace spaceStr
                                                                          programStr <- readFile programPath
                                                                          let program = toEnvironment programStr
                                                                          let pos = initializePos space startPosStr
                                                                          let heading = read headingStr
                                                                          let stack = initializeStack program
                                                                          let arrowState = ArrowState space pos heading stack
                                                                          case mode of
                                                                            "interactive" -> interactive program arrowState
                                                                            "batch" -> let (s, p, h) = batch program arrowState
                                                                                        in s `seq` programCompletionToStdOut s p h
                                                                            _ -> error "Mode not recognized."
            _ -> error "Wrong amount of arguments specified."

