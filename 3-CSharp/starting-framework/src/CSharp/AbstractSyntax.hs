-- This module defined an abstract syntax tree type for
-- (a subset of) the C# programming language

module CSharp.AbstractSyntax where

type ClassName = String -- Class names
type Ident = String     -- Variable names

data Class    -- Classes (top-level C# programs)
  = Class ClassName [Member]
  deriving (Eq, Ord, Show)

data Member   -- Class members
  = MemberD Decl                      -- global variable declaration
  | MemberM RetType Ident [Decl] Stat -- function (aka "method") defintions
  deriving (Eq, Ord, Show)

data Stat     -- Statements
  = StatDecl   Decl
  | StatExpr   Expr
  | StatIf     Expr Stat Stat
  | StatWhile  Expr Stat
  | StatReturn Expr
  | StatBlock  [Stat]
  deriving (Eq, Ord, Show)

data Literal = LitInt Int | LitBool Bool
  deriving (Eq, Ord, Show)

data Expr   -- Expressions
  = ExprLit   Literal
  | ExprVar   Ident
  | ExprOper  Operator Expr Expr
  | ExprMeth  Ident [Expr]
  deriving (Eq, Ord, Show)

-- todo operator order (and/or)
data Operator -- Binary operators, in descending order of priority (same line is same priority)
  = OpMul | OpDiv | OpMod 
  | OpAdd | OpSub 
  | OpLeq | OpLt | OpGeq | OpGt
  | OpEq  | OpNeq
  | OpAnd  
  | OpXor 
  | OpOr 
  | OpAsg 
  deriving (Eq, Show, Ord, Enum, Bounded)

data Decl = Decl Type Ident  -- Variable declarations
  deriving (Eq, Ord, Show)

-- (Simplified) types of C#, not including "void"
data Type = TyBool | TyInt
  deriving (Eq, Show, Ord, Enum, Bounded)

-- (Simplified) types of C#, including "void"
data RetType
  = TyVoid      -- "void"
  | NV Type     -- "not void"
  deriving (Eq, Ord)

instance Show RetType where 
  show TyVoid      = "void"
  show (NV TyBool) = "bool"
  show (NV TyInt)  = "int"
