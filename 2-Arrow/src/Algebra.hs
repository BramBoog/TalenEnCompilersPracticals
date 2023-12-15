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
                                                 , ruleCall :: Ident -> c
                                                 , toLeft :: d
                                                 , toRight :: d
                                                 , toFront :: d
                                                 , alt :: t -> [c] -> a
                                                 , emptyP :: t 
                                                 , lambdaP :: t 
                                                 , debrisP :: t
                                                 , asteroidP :: t
                                                 , boundaryP :: t
                                                 , underscore :: t
                                                 }

fold :: ProgramAlgebra p r c d a t -> Program -> p
fold
  (ProgramAlgebra program rule go take mark nothing' turn case' ruleCall toLeft toRight toFront alt emptyP lambdaP debrisP asteroidP boundaryP underscore)
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
        fC (RuleCall i) = ruleCall i 
        fD ToLeft = toLeft 
        fD ToRight = toRight 
        fD ToFront = toFront
        fA (Alt p cs) = alt (fPat p) (map fC cs)
        fPat EmptyP = emptyP 
        fPat LambdaP = lambdaP 
        fPat DebrisP = debrisP 
        fPat AsteroidP = asteroidP 
        fPat BoundaryP = boundaryP 
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
  ruleCall = \i -> [i],
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
  ruleCall = const True,
  alt = \pat cs -> (pat, and cs),
  emptyP = EmptyP,
  lambdaP = LambdaP,
  debrisP = DebrisP,
  asteroidP = AsteroidP,
  boundaryP = BoundaryP,
  underscore = Underscore
}
  where
    isComplete :: [Pat] -> Bool
    isComplete pats = Underscore `elem` pats || all (`elem` pats) [EmptyP, LambdaP, DebrisP, AsteroidP, BoundaryP]

checkProgram :: Program -> Bool
checkProgram p = fold noUndefinedCalls p && fold existsStartRule p && fold noDoublyDefinedRule p && fold completePatternMatches p
