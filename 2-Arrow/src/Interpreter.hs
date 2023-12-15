{-# LANGUAGE NamedFieldPuns #-}
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
data Heading = L | R | U | D

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState { space :: Space
                               , pos :: Pos
                               , heading :: Heading
                               , stack :: Stack }

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
step _ (ArrowState space pos heading []) = Done space pos heading 
step env state@ArrowState{space, pos, heading, stack = (com:coms)} = case com of
  Go -> Ok state{pos = moveIfPossible, stack = coms}
  Take -> Ok state{space = pickUp, stack = coms}
  Mark -> Ok state{space = mark, stack = coms}
  Nothing' -> Ok state{stack = coms}
  Turn d -> Ok state{heading = changeHeading d, stack = coms}
  Case d as -> maybe (Fail "No sensor reading found.") (\cs -> Ok (state{stack = cs ++ coms})) (getCommandsFromBranch d as)
  RuleCall i -> maybe (Fail "Call to undefined rule.") (\cs -> Ok (state{stack = cs ++ coms})) (M.lookup i env)

  where
    moveIfPossible :: Pos
    moveIfPossible= let posToMove = calculatePos pos heading
                     in maybe pos (\c -> if c `elem` [Empty, Lambda, Debris] then posToMove else pos) (M.lookup posToMove space)

    pickUp :: Space
    pickUp = changeContents takeIfPossible pos Empty space
      where
        takeIfPossible :: Contents -> Contents -> Contents
        takeIfPossible new old = if old == Lambda || old == Debris 
                                   then Empty 
                                   else old

    mark :: Space 
    mark = changeContents markWithLambda pos Lambda space
      where 
        markWithLambda :: Contents -> Contents -> Contents
        markWithLambda new _ = new

    changeHeading :: Dir -> Heading
    changeHeading d = case (heading, d) of
      (_, ToFront) -> heading
      (U, ToLeft)  -> L
      (U, ToRight) -> R
      (L, ToLeft)  -> D
      (L, ToRight) -> U
      (D, ToLeft)  -> R
      (D, ToRight) -> L
      (R, ToLeft)  -> U
      (R, ToRight) -> D

    getCommandsFromBranch :: Dir -> [Alt] -> Maybe Stack
    getCommandsFromBranch d as = do let headingToLook = changeHeading d
                                        posToLook = calculatePos pos headingToLook
                                    con <- M.lookup posToLook space
                                    -- find the first alternative matching con, and grab the corresponding Commands
                                    (Alt _ cs) <- find (\(Alt pat cs) -> matchPatToContents pat con) as
                                    return cs
      where
        matchPatToContents :: Pat -> Contents -> Bool
        matchPatToContents p c = case (p, c) of
          (Underscore, _) -> True
          (EmptyP, Empty) -> True
          (LambdaP, Lambda) -> True
          (DebrisP, Debris) -> True
          (AsteroidP, Asteroid) -> True
          (BoundaryP, Boundary) -> True
          _ -> False

    calculatePos :: Pos -> Heading -> Pos
    calculatePos (x,y) h = case h of
      L -> (x-1, y)
      R -> (x+1, y)
      U -> (x, y+1)
      D -> (x, y-1)

    -- Change Contents of a position in Space, using a function that takes both the new and old Contents
    changeContents :: (Contents -> Contents -> Contents) -> Pos -> Contents -> Space -> Space
    changeContents = M.insertWith
