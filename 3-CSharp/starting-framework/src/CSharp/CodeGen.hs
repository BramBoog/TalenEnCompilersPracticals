{-# LANGUAGE BlockArguments #-}
module CSharp.CodeGen where

import CSharp.AbstractSyntax
import CSharp.Algebra

import SSM
import Control.Monad.State

import Prelude hiding (LT, GT, EQ)
import qualified Data.Map as M

{-
  This file contains a starting point for the code generation.
-}

-- The types that we generate for each datatype: our type variables for the algebra.
-- Change these definitions instead of the function signatures to get better type errors.
type C = Code                   -- Class
type M = Code                   -- Member
type S = Code                   -- Statement
type E = ValueOrAddress -> Code -- Expression

type Env = M.Map Ident Int -- voor 11, maak van Int (RetType, Int)

codeAlgebra :: CSharpAlgebra C (State Env M) (State Env S) (Env -> E)
codeAlgebra = CSharpAlgebra
  fClass
  fMembDecl
  fMembMeth
  fStatDecl
  fStatExpr
  fStatIf
  fStatWhile
  fStatReturn
  fStatBlock
  fExprLit
  fExprVar
  fExprOp

-- E --> (Env -> E)
-- M -> State Env M
-- S --> Env -> (S, Env)

fClass :: ClassName -> [State Env M] -> C
fClass c ms = let stateToRun = do ms' <- sequence ms
                                  return ([Bsr "main", HALT] ++ concat ms')
               in evalState stateToRun M.empty

fMembDecl :: Decl -> State Env M
fMembDecl (Decl t i) = do modify (\env -> M.insert i (M.size env) env)
                          return []

fMembMeth :: RetType -> Ident -> [Decl] -> State Env S -> State Env M
fMembMeth t x ps s = do mapM_ fStatDecl ps -- misschien algemene functie voor maken en die toepassen hier en in fStatDecl
                        s' <- s
                        return ([LABEL x] ++ s' ++ [RET])
                        -- misschien parameters weer uit de env met modify?

-- TODO Controleren dat local vars ook local blijven 
fStatDecl :: Decl -> State Env S
fStatDecl (Decl t i) = do modify (\env -> M.insert i (M.size env) env)
                          return []

fStatExpr :: (Env -> E) -> State Env S
fStatExpr e = state (\env -> (e env Value ++ [pop], env))

fStatIf :: (Env -> E) -> State Env S -> State Env S -> State Env S
fStatIf e s1 s2 = do env <- get
                     s1' <- s1
                     s2' <- s2
                     let c        = e env Value
                         (n1, n2) = (codeSize s1', codeSize s2')
                     return (c ++ [BRF (n1 + 2)] ++ s1' ++ [BRA n2] ++ s2')

fStatWhile :: (Env -> E) -> State Env S -> State Env S
fStatWhile e s1 = do env <- get 
                     s1' <- s1
                     let c = e env Value
                         (n, k) = (codeSize s1', codeSize c)
                     return ([BRA n] ++ s1' ++ c ++ [BRT (-(n + k + 2))])

fStatReturn :: (Env -> E) -> State Env S
fStatReturn e = state (\env -> (e env Value ++ [pop, RET], env))

fStatBlock :: [State Env S] -> State Env S
fStatBlock = fmap concat . sequence

fExprLit :: Literal -> (Env -> E)
fExprLit l va env = [LDC n] where
  n = case l of
    LitInt n -> n
    LitBool b -> bool2int b

fExprVar :: Ident -> (Env -> E)
fExprVar x env va = case va of
    Value   ->  [LDL  loc]
    Address ->  [LDLA loc]
  where loc = env M.! x

fExprOp :: Operator -> (Env -> E) -> (Env -> E) -> (Env -> E)
fExprOp OpAsg e1 e2 env va = e2 env Value ++ [LDS 0] ++ e1 env Address ++ [STA 0]
fExprOp op    e1 e2 env va = e1 env Value ++ e2 env Value ++ [
   case op of
    { OpAdd -> ADD; OpSub -> SUB; OpMul -> MUL; OpDiv -> DIV;
    ; OpMod -> MOD
    ; OpAnd -> AND; OpOr -> OR; OpXor -> XOR;
    ; OpLeq -> LE; OpLt -> LT;
    ; OpGeq -> GT; OpGt -> GT;
    ; OpEq  -> EQ; OpNeq -> NE;}
  ]

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show

-- Encode a C# bool as an int, for the SSM
bool2int :: Bool -> Int
bool2int True  = -1
bool2int False = 0
