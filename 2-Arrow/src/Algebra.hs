module Algebra where

import Model


-- Exercise 5

data ProgramAlgebra p r c d a t = ProgramAlgebra { program :: [r] -> p
                                                 , rule :: Ident' -> [c] -> r
                                                 , go :: c
                                                 , take :: c
                                                 , mark :: c
                                                 , nothing' :: c
                                                 , turn :: d -> c
                                                 , case' :: d -> [a] -> c
                                                 , indentCmd :: Ident' -> c
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
fold (ProgramAlgebra program rule go take mark nothing turn case' indentCmd left' right' front alt empty' lambda' debris' asteroid' boundary' underscore) = fP
    where
        fP (Program rs) = program (map fR rs)
        fR (Rule i cs) = rule i (map fC cs)
        fC Go = go
        fC Take = take
        fC Mark = mark
        fC Nothing' = nothing
        fC (Turn d) = turn (fD d)
        fC (Case d as) = case' (fD d) (map fA as)
        fC (IdentCmd i) = indentCmd i 
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

-- noUndefinedCalls -- c = [Ident'] alle rules die gecalled worden door command, r = (Ident', [Ident']) rulenaam en alle rules die gecalled worden binnen de rule

existsStartRule :: ProgramAlgebra Bool Bool Bool Bool Bool Bool
existsStartRule = ProgramAlgebra {
  program = or,
  rule = \i _ -> i == "start"
}

checkProgram :: Program -> Bool
checkProgram p = fold existsStartRule p
