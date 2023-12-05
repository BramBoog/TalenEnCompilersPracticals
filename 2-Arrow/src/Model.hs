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
           | IdentT Ident'
    deriving Show

type Ident' = String

-- Exercise 2
data Program = Program [Rule] 
    deriving Show

data Rule = Rule Ident' [Command]
    deriving Show

data Command = Go | Take | Mark | Nothing' | Turn Dir | Case Dir [Alt]
    deriving Show

data Dir = Left' | Right' | Front 
    deriving Show

data Alt = Alt Pat [Command]
    deriving Show

data Pat = Empty' | Lambda' | Debris' | Asteroid' | Boundary' | Underscore
    deriving Show
