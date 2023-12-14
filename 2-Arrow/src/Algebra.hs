module Algebra where

import Model

import Prelude hiding (take)
import Data.List (nub)

-- Exercise 5

data ProgramAlgebra p r c d a t = ProgramAlgebra { program :: [r] -> p
                                                 , rule :: Ident -> [c] -> r
                                                 , go :: c
                                                 , take :: c
                                                 , mark :: c
                                                 , nothing' :: c
                                                 , turn :: d -> c
                                                 , case' :: d -> [a] -> c
                                                 , identCmd :: Ident -> c
                                                 , left' :: d
                                                 , right' :: d
                                                 , front :: d
                                                 , alt :: t -> [c] -> a
                                                 , empty' :: t 
                                                 , lambda' :: t 
                                                 , debris' :: t
                                                 , asteroid' :: t
                                                 , boundary' :: t
                                                 , underscore :: t
                                                 }

fold :: ProgramAlgebra p r c d a t -> Program -> p
fold
  (ProgramAlgebra program rule go take mark nothing' turn case' identCmd left' right' front alt empty' lambda' debris' asteroid' boundary' underscore)
  = fP
    where
        fP (Program rs) = program (map fR rs)
        fR (Rule i cs) = rule i (map fC cs)
        fC Go = go
        fC Take = take
        fC Mark = mark
        fC Nothing' = nothing'
        fC (Turn d) = turn (fD d)
        fC (Case d as) = case' (fD d) (map fA as)
        fC (IdentCmd i) = identCmd i 
        fD Left' = left' 
        fD Right' = right' 
        fD Front = front
        fA (Alt p cs) = alt (fPat p) (map fC cs)
        fPat Empty' = empty' 
        fPat Lambda' = lambda' 
        fPat Debris' = debris' 
        fPat Asteroid' = asteroid' 
        fPat Boundary' = boundary' 
        fPat Underscore = underscore


-- Exercise 6

noUndefinedCalls :: ProgramAlgebra Bool (Ident, [Ident]) [Ident] () [Ident] ()
noUndefinedCalls = ProgramAlgebra {
  program = checkCallsMade,
  rule = \i cs -> (i, concat cs),
  go = [],
  take = [],
  mark = [],
  nothing' = [],
  turn = const [],
  case' = \_ cs -> concat cs,
  identCmd = \i -> [i],
  alt = \_ cs -> concat cs
}
  where
    checkCallsMade :: [(Ident, [Ident])] -> Bool
    checkCallsMade xs = let rules = map fst xs
                            calls = concatMap snd xs
                         in all (`elem` rules) calls

existsStartRule :: ProgramAlgebra Bool Bool () () () ()
existsStartRule = ProgramAlgebra {
  program = or,
  rule = \i _ -> i == "start"
}

noDoublyDefinedRule :: ProgramAlgebra Bool Ident () () () ()
noDoublyDefinedRule = ProgramAlgebra {
  program = \rs -> length rs == length (nub rs),
  rule = \i _ -> i
}

completePatternMatches :: ProgramAlgebra Bool Bool Bool () (Pat, Bool) Pat
completePatternMatches = ProgramAlgebra {
  program = and,
  rule = \_ cs -> and cs,
  go = True,
  take = True,
  mark = True,
  nothing' = True,
  turn = const True,
  case' = \_ as -> isComplete (map fst as) && and (map snd as),
  identCmd = const True,
  alt = \pat cs -> (pat, and cs),
  empty' = Empty',
  lambda' = Lambda',
  debris' = Debris',
  asteroid' = Asteroid',
  boundary' = Boundary',
  underscore = Underscore
}
  where
    isComplete :: [Pat] -> Bool
    isComplete pats = Underscore `elem` pats || all (`elem` pats) [Empty', Lambda', Debris', Asteroid', Boundary']

checkProgram :: Program -> Bool
checkProgram p = fold noUndefinedCalls p && fold existsStartRule p && fold noDoublyDefinedRule p && fold completePatternMatches p


