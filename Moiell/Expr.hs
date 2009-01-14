module Moiell.Expr where

import Moiell.AST

import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set

data Env = Env { bound :: Map.Map String Expr, free :: Set.Set String }

instance Monoid Env where
  mempty = Env Map.empty Set.empty
  Env lBound lFree `mappend` Env rBound rFree =
    Env bound free
      where
        bound = lBound `Map.union` rBound
        free  = lFree `Set.union` rFree `Set.difference` Map.keysSet bound

type Expr = [Expr1]
data Expr1 
  = Scope Env Expr 
  | AppExpr Expr Expr
  | IdtExpr String
  | StrExpr String
  | ChrExpr Char
  | NumExpr Double

data EnvExpr = EnvExpr { env :: Env, expr :: Expr }

instance Monoid EnvExpr where
  mempty = EnvExpr mempty mempty
  EnvExpr lEnv lExpr `mappend` EnvExpr rEnv rExpr =
    EnvExpr (lEnv `mappend` rEnv) (lExpr `mappend` rExpr)

ast2Expr (StringLit s) = EnvExpr mempty [StrExpr s]
ast2Expr (CharLit c)   = EnvExpr mempty [ChrExpr c]
ast2Expr (NumberLit x) = EnvExpr mempty [NumExpr x]
ast2Expr (Ident i)     = EnvExpr mempty { free = Set.singleton i } [IdtExpr i]

ast2Expr (App [App [Ident "="] [App [Ident "?"] [Ident i]]] r) = EnvExpr env []
  where
    EnvExpr rEnv rExprs = mconcat (map ast2Expr r)
    env = insert i rExprs rEnv

ast2Expr (App ops args) = EnvExpr env [AppExpr opExprs argExprs]
  where
    EnvExpr opEnv opExprs = mconcat (map ast2Expr ops)
    EnvExpr argEnv argExprs = mconcat (map ast2Expr args)
    env = opEnv `mappend` argEnv