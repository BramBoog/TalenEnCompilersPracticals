{
module Parser where

import Model
}

%name parser
%tokentype { Token }

%token
  x { PeriodT }

%%

Program : { Program }

{

happyError _ = error "parse error"

}