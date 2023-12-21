module Main where

import Algebra
import Model
import Interpreter
import Lexer
import Parser

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive e a = undefined

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch e a = case step e a of
              Done s p h -> (s, p , h)
              Fail m -> error m
              Ok as -> batch e as

-- This function is just here to play around with and test your lexer/parser.
-- When implementing exercise 11, delete this comment and this function,
-- and write a new main function.
main :: IO ()
main = do
  chars <- readFile "examples/Add.arrow"
  putStrLn "Input program:"
  putStrLn ""
  putStrLn chars
  putStrLn ""
  let tokens = alexScanTokens chars
  putStrLn "Tokens:"
  putStrLn ""
  print tokens
  let arr = parser tokens
  putStrLn "Parsed program:"
  putStrLn ""
  print arr
  print (checkProgram arr)

-- main = do
--   chars <- readFile "examples/Add.arrow"
--   print (toEnvironment chars)
