{-# LANGUAGE NamedFieldPuns #-}
module CSharp.Analysis where

import CSharp.AbstractSyntax
import CSharp.Algebra
import CSharp.CodeGen (ScopeType (Normal, OnlyMethodParams), Scope (Scope, scopeType, scopeContent))
import qualified Data.Map as M
import Control.Monad.State

type VarEnv = [Scope] -- local2 : local1 : global : []
type MethodEnv = M.Map Ident RetType
data MethodInfo = MethodInfo{name :: Ident, retType :: RetType}
data Env = Env{varEnv :: VarEnv, methodEnv :: MethodEnv}
type ErrorMsg = String

-- analysisAlgebra :: CSharpAlgebra Bool () () ()
-- analysisAlgebra = undefinedAlgebra { clas = \_ _ -> True }

-- type C = [ErrorMsg]
-- type M = ([Decl], [MethodInfo], Env -> C)
-- type S = State Env C 
-- type E = Env -> C

-- typeCheckAlgebra :: CSharpAlgebra C M S E 
-- typeCheckAlgebra = CSharpAlgebra
--   fClass
--   fMembDecl
--   fMembMeth
--   fStatDecl
--   fStatExpr
--   fStatIf
--   fStatWhile
--   fStatReturn
--   fStatBlock
--   fExprLit
--   fExprVar
--   fExprOp
--   fExprMeth

--   where
--     fClass :: ClassName -> [M] -> C
--     fClass _ ts = let (ds, mis, fs) = (concatMap (\(a, _, _) -> a) ts, concatMap (\(_, a, _) -> a) ts, map (\(_, _, a) -> a) ts)
--                       globalScopeVarEnv = [Scope Normal (M.fromList $ map (\(Decl t i) -> (i, t)) ds)]
--                       methodEnv = M.fromList $ map (\mi -> (name mi, retType mi)) mis
--                       env = Env globalScopeVarEnv methodEnv
--                    in concatMap ($ env) fs

--     fMembDecl :: Decl -> M
--     fMembDecl d = ([d], [], const [])

--     fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
--     fMembMeth rt i ds s = ([], [MethodInfo i rt], go)
--       where
--         go env@Env{varEnv} = let varEnvWithParams = Scope OnlyMethodParams (M.fromList $ map (\(Decl t i) -> (i, t)) ds) : varEnv
--                               in evalState s env{varEnv = varEnvWithParams}

--     fStatDecl :: Decl -> S
--     fStatDecl (Decl t i) = do modify update
--                               return []
--       where
--         update env@Env{varEnv} = let currentScope@Scope{scopeContent} = head varEnv
--                                      updatedCurrentScope = currentScope{scopeContent = M.insert i t scopeContent}
--                                   in env{varEnv = updatedCurrentScope : tail varEnv}

--     fStatExpr :: E -> S
--     fStatExpr e = state (\s -> (e s, s))
    
    -- fStatIf :: E -> S -> S -> S
    -- fStatIf e st sf = (\env -> c env ++ [BRF (nt env + 2)] ++ tb env ++ [BRA (nf env)] ++ fb env, [])
    --   where
    --     tb      = fst st
    --     fb      = fst sf
    --     c env'  = e env' Value
    --     nt env' = codeSize $ tb env'
    --     nf env' = codeSize $ fb env'