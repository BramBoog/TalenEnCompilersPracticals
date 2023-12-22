module Model where

-- Exercise 1
data Token = ArrowT
           | PeriodT
           | CommaT
           | GoT
           | TakeT
           | MarkT 
           | NothingT
           | TurnT 
           | CaseT 
           | OfT 
           | EndT 
           | LeftT
           | RightT
           | FrontT 
           | SemicolonT 
           | EmptyT 
           | LambdaT 
           | DebrisT 
           | AsteroidT 
           | BoundaryT
           | UnderscoreT 
           | IdentT Ident
    deriving Show

type Ident = String

-- Exercise 2
newtype Program = Program [Rule] 
    deriving Show

data Rule = Rule Ident [Command]
    deriving Show

data Command = Go | Take | Mark | Nothing' | Turn Dir | Case Dir [Alt] | RuleCall Ident
    deriving Show

data Dir = ToLeft | ToRight | ToFront 
    deriving Show

data Alt = Alt Pat [Command]
    deriving Show

data Pat = EmptyP | LambdaP | DebrisP | AsteroidP | BoundaryP | Underscore
    deriving (Show, Eq)
