{-# LANGUAGE BlockArguments #-}
module CSharp.CodeGen where

import CSharp.AbstractSyntax
import CSharp.Algebra

import SSM

import Prelude hiding (LT, GT, EQ)
import qualified Data.Map as M
import Data.List (intercalate)

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
  fExprMeth

fClass :: ClassName -> [(Env -> M, [Ident])] -> C
fClass c ts = let (fs, is) = (map fst ts, concatMap snd ts)
                  -- allocate an address to each declared global var, skipping the "dummy" starting position 0 of the MP
                  globalScopeEnv = [M.fromList $ zip is [1..(length is)]]
                  code = concatMap ($ globalScopeEnv) fs
               in [AJS (length is), Bsr "main", HALT] ++ code -- reserve space for global vars at the bottom of the stack first

fMembDecl :: Decl -> (Env -> M, [Ident])
fMembDecl (Decl t i) = (const [], [i])

fMembMeth :: RetType -> Ident -> [Decl] -> (Env -> S, [Ident]) -> (Env -> M, [Ident])
fMembMeth rt x ps sb = (go, [])
  where
    go env = let is = map (\(Decl t i) -> i) ps
                 nParam = length is
                 -- Method parameters are found from -2 downward relative to the MP when it is placed on the method local scope,
                 -- since -1 is always occupied by the return address originating from the method call
                 envWithParams = M.fromList (zip is [-(nParam + 1)..(-2)]) : removeLocalVars env
              in [LABEL x] ++ fst sb envWithParams ++ [STS (-nParam), AJS (-(nParam - 1)), RET]
      where
        -- A method body cannot refer to variables from the local scope the method is called in, only "global" class members.
        -- So we need to remove variables from the local scopes when entering the method body,
        -- while retaining a way to jump back to the "global" scope across all the local scopes in between.
        -- Therefore, we keep the 0th element of each local scope, since this is the adress of the scope before (see fExprVar)
        removeLocalVars :: Env -> Env
        removeLocalVars [s] = [s]
        removeLocalVars (s:ss) = M.filter (/= 0) s : removeLocalVars ss
              
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
fStatBlock ts = (go, [])
  where
    go env = let (fs, is) = (map fst ts, concatMap snd ts)
                 initLocalScope = [LDR MP, LDRR MP SP, AJS (length is)] -- let MP point to address of previous scope, reserve space for local vars
                 returnToPrevScope = [LDRR SP MP, STR MP]
                 localScope = M.fromList $ zip is [1..(length is)] -- start allocating adresses for local vars from 1, since 0 is always the address of the previous scope
                 -- Statblock creates a new local scope;
                 -- however, if we're in a method body, we've already been supplied a partial local scope with space for the method parameters in fMembMeth.
                 -- So if that's the case, we need to amend the first local scope in env, not create a new one
                 newEnv = if (not . null) (M.filter (< 0) (head env)) -- Method calls have their parameters on negative adresses in a partial local scope
                            then M.union (head env) localScope : tail env
                            else localScope : env
              in initLocalScope ++ concatMap ($ newEnv) fs ++ returnToPrevScope

fExprLit :: Literal -> (Env -> E)
fExprLit l env va = [LDC n] where
  n = case l of
    LitInt n -> n
    LitBool b -> bool2int b

fExprVar :: Ident -> Env -> E
fExprVar x (s:ss) va = maybe (LDL 0 : loadFromPrevScopes ss) -- for first scope, check if var in current scope, else go back one scope (by loading its address)
                             loadValueOrAddress
                             (M.lookup x s)
  where
    loadValueOrAddress loc = case va of -- loading from current scope is done by loading from MP
      Value   ->  [LDL  loc]
      Address ->  [LDLA loc]

    -- we always start this code block with SP pointing to the address of the scope previous to the one we just checked
    -- check if var is in said previous scope, else load address of scope before (which sits on place 0 of previous scope), and try again
    loadFromPrevScopes :: Env -> Code
    loadFromPrevScopes (ps:pss) = maybe (LDA 0 : loadFromPrevScopes pss)
                                        loadValueOrAddress'
                                        (M.lookup x ps)
      where
        loadValueOrAddress' loc = case va of -- loading from previous scope is always done by loading via an address
          Value   ->  [LDA  loc]
          Address ->  [LDAA loc]

fExprOp :: Operator -> (Env -> E) -> (Env -> E) -> (Env -> E)
fExprOp OpAsg e1 e2 env va = e2 env Value ++ [LDS 0] ++ e1 env Address ++ [STA 0]
-- Shortcutting/lazy evaluation via BRF/BRT
-- Since BRF/BRT consumes it, add the first bool an extra time to the stack, so it still remains after BRF/BRT for its original use
fExprOp OpAnd e1 e2 env va = e1 env Value ++ [LDS 0, BRF (codeSize (e2 env Value) + 1)] ++ e2 env Value ++ [AND]
fExprOp OpOr  e1 e2 env va = e1 env Value ++ [LDS 0, BRT (codeSize (e2 env Value) + 1)] ++ e2 env Value ++ [OR]
fExprOp op    e1 e2 env va = e1 env Value ++ e2 env Value ++ [
   case op of
    { OpAdd -> ADD; OpSub -> SUB; OpMul -> MUL; OpDiv -> DIV;
    ; OpMod -> MOD
    ; OpXor -> XOR;
    ; OpLeq -> LE; OpLt -> LT;
    ; OpGeq -> GT; OpGt -> GT;
    ; OpEq  -> EQ; OpNeq -> NE;}
  ]

fExprMeth :: Ident -> [Env -> E] -> Env -> E
fExprMeth "print" es env va = intercalate [TRAP 0] (map (\e -> e env Value) es) ++ [TRAP 0]
fExprMeth i es env va = concatMap (\e -> e env Value) es ++ [Bsr i]

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show

-- Encode a C# bool as an int, for the SSM
bool2int :: Bool -> Int
bool2int True  = -1
bool2int False = 0
