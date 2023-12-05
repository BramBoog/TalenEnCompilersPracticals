{
module Lexer where

import Model
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-
  $white+                        ;
  "--".*\r\n                     ;
  "->"                           { const ArrowT }
  \.                             { const PeriodT }
  ","                            { const CommaT }
  "go"                           { const GoT }
  "take"                         { const TakeT }
  "mark"                         { const MarkT }
  "nothing"                      { const NothingT }
  "turn"                         { const TurnT }
  "case"                         { const CaseT }
  "of"                           { const OfT }
  "end"                          { const EndT }
  "left"                         { const LeftT }
  "right"                        { const RightT }
  "front"                        { const FrontT }
  ";"                            { const SemicolonT }
  "Empty"                        { const EmptyT }
  "Lambda"                       { const LambdaT }
  "Debris"                       { const DebrisT }
  "Asteroid"                     { const AsteroidT }
  "Boundary"                     { const BoundaryT }
  "_"                            { const UnderscoreT }
  [$alpha $digit \+\-]+          { IdentT }

