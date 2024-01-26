{-
Submitted by:
Bram Boogaarts, 2600617
Annemae van de Hoef, 5048184
-}

module Main where

import CSharp.Algebra
import CSharp.Analysis
import CSharp.CodeGen
import CSharp.AbstractSyntax
import CSharp.Parser

import SSM

import ParseLib.Abstract.Derived
import ParseLib.Error (ErrorsPretty)

import System.Environment
import System.FilePath
import Prelude hiding ((*>), (<$), (<*))
import CSharp.Parser (lexicalScanner)
import CSharp.CodeGen (codeAlgebra)

main :: IO ()
main = do
  -- get command line arguments
  args <- getArgs
  files <- case args of
    [] -> do
      putStrLn "no argument given; assuming example.cs"
      return ["example.cs"]
    xs -> return xs
  -- translate each of the files
  mapM_ processFile files

-- processFile compiles one file;
-- it take the name of the input file
processFile :: FilePath -> IO ()
processFile infile = do
  let outfile = addExtension (dropExtension infile) "ssm"
  xs <- readFile infile
  let program = Main.run "parser" (pClass <* eof) . Main.run "lexer" lexicalScanner $ xs
  print program
  case foldCSharp checkAlgebra program of
    [] -> do let ssm = formatCode $ foldCSharp codeAlgebra program
             writeFile outfile ssm
             putStrLn (outfile ++ " written")
    es -> do putStrLn "Compiler error(s):"
             mapM_ putStrLn es


run :: (ErrorsPretty s, Ord s, Show a) => String -> Parser s a -> [s] -> a
run s p x = fst . headOrError . parse (p <* eof) $ x
    where
  headOrError (x : xs) = x
  headOrError [] = error $ "The " <> s <> " returned no full parses."
