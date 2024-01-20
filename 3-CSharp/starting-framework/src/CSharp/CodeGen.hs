{-# LANGUAGE BlockArguments #-}
module CSharp.CodeGen where

import CSharp.AbstractSyntax
import CSharp.Algebra

import SSM

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

type Scope = M.Map Ident Int -- voor 11, maak van Int (Type, Int)
type Env = [Scope] -- local2 : local1 : global : []

{-
1. Global variables
2. 1 local variable
3. 2 local variables
4. calling global variable
5. nested local scope
6. calling local scope above -- put scope addresses on stack with local variables?
7. calling global scope from nested local
-}

codeAlgebra :: CSharpAlgebra C (Env -> M, [Ident]) (Env -> S, [Ident]) (Env -> E)
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
-- M --> (Env -> M)
-- S --> (Env -> S)

fClass :: ClassName -> [(Env -> M, [Ident])] -> C
fClass c ts = let (fs, is) = (map fst ts, concatMap snd ts)
                  globalScopeEnv = [M.fromList $ zip is [0..(length is - 1)]] -- allocate an address to each declared global var
                  code = concatMap ($ globalScopeEnv) fs
               in [AJS (length is), Bsr "main", HALT] ++ code -- reserve space for global vars at the bottom of the stack
                  
fMembDecl :: Decl -> (Env -> M, [Ident])
fMembDecl (Decl t i) = (const [], [i])

fMembMeth :: RetType -> Ident -> [Decl] -> (Env -> S, [Ident]) -> (Env -> M, [Ident])
fMembMeth t x ps s = (\env -> [LABEL x] ++ fst s env ++ [RET], [])

fStatDecl :: Decl -> (Env -> S, [Ident])
fStatDecl (Decl t i) = (const [], [i])

fStatExpr :: (Env -> E) -> (Env -> S, [Ident])
fStatExpr e = (\env -> e env Value ++ [pop], [])

fStatIf :: (Env -> E) -> (Env -> S, [Ident]) -> (Env -> S, [Ident]) -> (Env -> S, [Ident])
fStatIf e st sf = (\env -> c env ++ [BRF (nt env + 2)] ++ tb env ++ [BRA (nf env)] ++ fb env, [])
  where
    tb      = fst st
    fb      = fst sf
    c env'  = e env' Value
    nt env' = codeSize $ tb env'
    nf env' = codeSize $ fb env'

fStatWhile :: (Env -> E) -> (Env -> S, [Ident]) -> (Env -> S, [Ident])
fStatWhile e s = (\env -> [BRA (n env)] ++ b env ++ c env ++ [BRT (-(n env + k env + 2))], [])
  where
    b      = fst s
    c env' = e env' Value
    n env' = codeSize $ b env'
    k env' = codeSize $ c env'

fStatReturn :: (Env -> E) -> (Env -> S, [Ident])
fStatReturn e = (\env -> e env Value ++ [pop] ++ [RET], [])

fStatBlock :: [(Env -> S, [Ident])] -> (Env -> S, [Ident])
fStatBlock ts = let (fs, is) = (map fst ts, concatMap snd ts)
                    initLocalScope = [LDR MP, LDRR MP SP, AJS (length is)] -- mark pointer points to adress of previous scope, reserve space for local vars
                    returnToPrevScope = [LDRR SP MP, STR MP]
                    localScope = M.fromList $ zip is [1..(length is)] -- start allocating adresses for local vars from 1, since 0 is always the adress of the previous scope
                 in (\env -> initLocalScope ++ concatMap ($ (localScope : env)) fs ++ returnToPrevScope, [])

fExprLit :: Literal -> (Env -> E)
fExprLit l env va = [LDC n] where
  n = case l of
    LitInt n -> n
    LitBool b -> bool2int b

fExprVar :: Ident -> Env -> E
fExprVar x (s:ss) va = maybe (LDL 0 : loadFromPrevScopes ss)
                             loadValueOrAddress
                             (M.lookup x s)
  where
    loadValueOrAddress loc = case va of
      Value   ->  [LDL  loc]
      Address ->  [LDLA loc]

    loadFromPrevScopes :: Env -> Code
    loadFromPrevScopes (ps:pss) = maybe (LDA 0 : loadFromPrevScopes pss)
                                        loadValueOrAddress'
                                        (M.lookup x ps)
      where
        loadValueOrAddress' loc = case va of
          Value   ->  [LDA  loc]
          Address ->  [LDAA loc]
        

fExprOp :: Operator -> (Env -> E) -> (Env -> E) -> (Env -> E)
fExprOp OpAsg e1 e2 env va = e2 env Value ++ [LDS 0] ++ e1 env Address ++ [STA 0] -- moet rekening houden met scoping
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
