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
  = foldP
    where
        foldP (Program rs) = program (map foldR rs)
        foldR (Rule i cs) = rule i (map foldC cs)
        foldC Go = go
        foldC Take = take
        foldC Mark = mark
        foldC Nothing' = nothing'
        foldC (Turn d) = turn (foldD d)
        foldC (Case d as) = case' (foldD d) (map foldA as)
        foldC (RuleCall i) = ruleCall i 
        foldD ToLeft = toLeft 
        foldD ToRight = toRight 
        foldD ToFront = toFront
        foldA (Alt p cs) = alt (foldPat p) (map foldC cs)
        foldPat EmptyP = emptyP 
        foldPat LambdaP = lambdaP 
        foldPat DebrisP = debrisP 
        foldPat AsteroidP = asteroidP 
        foldPat BoundaryP = boundaryP 
        foldPat Underscore = underscore


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
