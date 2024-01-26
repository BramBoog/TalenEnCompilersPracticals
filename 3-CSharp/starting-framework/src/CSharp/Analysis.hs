{-# LANGUAGE NamedFieldPuns #-}
module CSharp.Analysis where

import CSharp.AbstractSyntax
import CSharp.Algebra
import CSharp.CodeGen (ScopeType (Normal, OnlyMethodParams, Method))
import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe (maybeToList)

data Scope = Scope{scopeType :: ScopeType, scopeContent :: M.Map Ident RetType} deriving Show
type VarEnv = [Scope] -- The list of scopes has the following structure, local2 : local1 : global : []
type MethodEnv = M.Map Ident RetType
data Env = Env{varEnv :: VarEnv, methodEnv :: MethodEnv}

type ErrorMsg = String
type C = [ErrorMsg]                                                                          -- Class
data M = M{varDecls :: [(Ident, RetType)], methDecls :: [(Ident, RetType)], run :: Env -> C} -- Member
type S = State Env C                                                                         -- Statement
type E = Env -> (C, Maybe RetType)                                                           -- Expression

checkAlgebra :: CSharpAlgebra C M S E
checkAlgebra = CSharpAlgebra
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

  where
    fClass :: ClassName -> [M] -> C
    fClass _ ms = let (vds, mds, fs) = (concatMap (\M{varDecls} -> varDecls) ms, concatMap (\M{methDecls} -> methDecls) ms, map (\M{run} -> run) ms)
                      globalScopeVarEnv = [Scope Normal (M.fromList vds)]
                      methodEnv = M.fromList mds
                      env = Env globalScopeVarEnv methodEnv
                      -- Use of the global (= class level) variables and methods is not dependent on order; they can be used by all class members,
                      -- so they are passed to all checks of member code
                   in concatMap ($ env) fs

    fMembDecl :: Decl -> M
    fMembDecl (Decl t i) = M [(i, NV t)] [] (const [])

    fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
    fMembMeth rt i ds s = M [] [(i, rt)] go
      where
                                -- Initialize a local scope for the method body, to which we can already add the method parameters;
                                -- any local vars declared in the method body, which is a StatBlock, are added to this scope in fStatBlock.
        go env@Env{varEnv} = let varEnvWithParams = Scope OnlyMethodParams (M.fromList $ map (\(Decl t i) -> (i, NV t)) ds) : varEnv
                              in evalState s env{varEnv = varEnvWithParams}

    fStatDecl :: Decl -> S
    fStatDecl (Decl t i) = do modify update -- a declaration updates the state, so that any statements after can use the declared var
                              return []
      where
        update env@Env{varEnv} = let currentScope@Scope{scopeContent} = head varEnv
                                     updatedCurrentScope = currentScope{scopeContent = M.insert i (NV t) scopeContent}
                                  in env{varEnv = updatedCurrentScope : tail varEnv}

    fStatExpr :: E -> S
    fStatExpr e = state (\env -> (fst $ e env, env))

    fStatIf :: E -> S -> S -> S
    fStatIf e st sf = do env <- get
                         let re = fst $ e env
                         rtb <- st
                         rfb <- sf
                         return (re ++ rtb ++ rfb)

    fStatWhile :: E -> S -> S
    fStatWhile e s = do env <- get
                        let re = fst $ e env
                        rb <- s
                        return (re ++ rb)

    fStatReturn :: E -> S
    fStatReturn e = state (\env -> (fst $ e env, env))

    fStatBlock :: [S] -> S
    fStatBlock sts = do env@Env{varEnv} <- get
                        -- Statblock creates a new local scope;
                        -- however, if we're in a method body, we've already been supplied a partial local scope with the method parameters from fMembMeth.
                        -- So if that's the case, we need to keep the first local scope in varEnv so that we can update it with any local declarations;
                        -- otherwise, we create a new (empty) one.
                        let currentScope@Scope{scopeType, scopeContent} = head varEnv
                            (newVarEnv, varEnvOnScopeExit) = case scopeType of
                                                OnlyMethodParams -> let prevScopes = tail varEnv
                                                                    in (currentScope{scopeType = Normal} : prevScopes, prevScopes)
                                                _                -> (Scope Normal M.empty : varEnv, varEnv)
                        put env{varEnv = newVarEnv}
                        cs <- sequence sts -- any of these may be declarations; this way, their order is taken into account, the state updating with every new declaration
                        put env{varEnv = varEnvOnScopeExit} -- when exiting the local scope, reset the varEnv to remove all local vars/method parameters
                        return (concat cs)

    fExprLit :: Literal -> E
    fExprLit l env = case l of
                       LitInt _  -> ([], Just $ NV TyInt)
                       LitBool _ -> ([], Just $ NV TyBool)

    fExprVar :: Ident -> E
    fExprVar x env@Env{varEnv} = case varEnv of
                                   [] -> (["Reference to undeclared variable: " ++ x], Nothing)
                                   (s:ss) -> maybe (fExprVar x env{varEnv = ss})
                                                   (\t -> ([], Just t))
                                                   (M.lookup x (scopeContent s))

    fExprOp :: Operator -> E -> E -> E
    fExprOp OpAsg e1 e2 env = let (errors1, declaredTypeM) = e1 env
                                  (errors2, inferredTypeM) = e2 env
                                  typeError = (concat . maybeToList) $ do declaredType <- declaredTypeM
                                                                          inferredType <- inferredTypeM
                                                                          let typeError = ["Cannot assign variable due to type mismatch; declared type " 
                                                                                         ++ show declaredType 
                                                                                         ++ " does not match " 
                                                                                         ++ show inferredType
                                                                                          | declaredType /= inferredType]
                                                                          return typeError
                               in (errors1 ++ errors2 ++ typeError, declaredTypeM) -- type of an assignment expression is the declared type of the variable being assigned to
    fExprOp op    e1 e2 env = let (errors1, _) = e1 env     -- Since we only need to type check assignments, and not operator applications,
                                  (errors2, _) = e2 env     -- we can ignore the type of subexpressions
                                  exprType = NV (case op of -- and infer the type of the expression based on the operator used
                                    { OpAdd -> TyInt; OpSub -> TyInt; OpMul -> TyInt; OpDiv -> TyInt;
                                    ; OpMod -> TyInt;
                                    ; OpXor -> TyBool; OpAnd -> TyBool; OpOr -> TyBool;
                                    ; OpLeq -> TyBool; OpLt -> TyBool;
                                    ; OpGeq -> TyBool; OpGt -> TyBool;
                                    ; OpEq  -> TyBool; OpNeq -> TyBool;})
                               in (errors1 ++ errors2, Just exprType)

    fExprMeth :: Ident -> [E] -> E
    fExprMeth "print" es env = let paramErrors = concatMap (fst . ($ env)) es
                                in (paramErrors, Just TyVoid)
    fExprMeth i es env@Env{methodEnv} = let paramErrors = concatMap (fst . ($ env)) es
                                            methType = methodEnv M.! i
                                         in (paramErrors, Just methType)
