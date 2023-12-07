{
module Parser where

import Model
}

%name parser
%tokentype { Token }

%token
  "->"                                { ArrowT }
  '.'                                 { PeriodT }
  ','                                 { CommaT }
  go                                  { GoT }
  take                                { TakeT }
  mark                                { MarkT }
  nothing                             { NothingT }
  turn                                { TurnT } 
  case                                { CaseT } 
  of                                  { OfT } 
  end                                 { EndT } 
  left                                { LeftT }
  right                               { RightT }
  front                               { FrontT }
  ';'                                 { SemicolonT }
  empty                               { EmptyT }
  lambda                              { LambdaT }
  debris                              { DebrisT }
  asteroid                            { AsteroidT }
  boundary                            { BoundaryT } 
  '_'                                 { UnderscoreT } 
  ident                               { IdentT $$ }

%%

Program : Rules                       { Program $1 }

Rules : {- empty -}                   { [] }
      | Rule Rules                    { $1 : $2 }

Rule : ident "->" Cmds '.'            { Rule $1 $3 }

Cmds : {- empty -}                    { [] }
     | Cmd CmdsWithSep                { $1 : $2 }

CmdsWithSep : {- empty -}             { [] }
            | ',' Cmd CmdsWithSep     { $2 : $3 }

Cmd : go                              { Go }
    | take                            { Take }
    | mark                            { Mark }
    | nothing                         { Nothing' }
    | turn Dir                        { Turn $2 }
    | case Dir of Alts end            { Case $2 $4 }
    | ident                           { IdentCmd $1 }
    
Dir : left                            { Left' }
    | right                           { Right' }
    | front                           { Front }

Alts : {- empty -}                    { [] }
     | Alt AltsWithSep                { $1 : $2 }

AltsWithSep : {- empty -}             { [] }
            | ';' Alt AltsWithSep     { $2 : $3 }

Alt : Pat "->" Cmds                   { Alt $1 $3 }

Pat : empty                           { Empty' }
    | lambda                          { Lambda' }
    | debris                          { Debris' }
    | asteroid                        { Asteroid' }
    | boundary                        { Boundary' }
    | '_'                             { Underscore }

{

happyError ts = error (show ts)

}