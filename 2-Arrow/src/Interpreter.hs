module Interpreter where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Char (isSpace)
import Control.Monad (replicateM)
import Data.List (find)

import Lexer
import Parser
import Model
import Algebra


data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary
  deriving (Show, Eq)

type Size      =  Int
type Pos       =  (Int, Int)
type Space     =  Map Pos Contents



-- | Parses a space file, such as the ones in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
    (mr, mc) <- parenthesised ((,) <$> natural <* symbol ',' <*> natural)
                <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <- replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ M.fromList $ concat $
            zipWith (\r cs ->
              zipWith (\c d -> ((r, c), d)) [0..] cs) [0..] css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)

    contents :: Parser Char Contents
    contents = choice (Prelude.map (\(f,c) -> f <$ symbol c) contentsTable)
      <* spaces


-- | Conversion table
contentsTable :: [ (Contents, Char)]
contentsTable =  [ (Empty   , '.' )
                 , (Lambda  , '\\')
                 , (Debris  , '%' )
                 , (Asteroid, 'O' )
                 , (Boundary, '#' )]


run :: Parser Char Space -> [Char] -> Space
run p s = maybe M.empty fst (find (\(_, r) -> null r) (parse p s))

-- Exercise 7
printSpace :: Space -> String
printSpace s = show m ++ "\n" ++ printSpace
  where 
    m@(mr, mc) = fst (M.findMax s)
    
    contentsChar :: Contents -> Char
    contentsChar c = (snd . head) (filter (\(c', _) -> c' == c) contentsTable)

    printSpace = concat [
        [contentsChar (s M.! (r, c)) | c <- [0..mc]] ++ "\n"
        | r <- [0..mr]
      ]
    

-- These three should be defined by you
type Commands = [Command]
type Heading = ()

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment s = let tokens = alexScanTokens s
                      p@(Program rs) = parser tokens
                   in if checkProgram p then M.fromList [(i, cs) | (Rule i cs) <- rs]
                      else error "Checking the program failed!"

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step = undefined


