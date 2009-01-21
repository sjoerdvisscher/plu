module Moiell.Expr where

import Moiell.AST

import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set

data Env = Env { getMap :: Map.Map String (Maybe Expr) } deriving (Eq, Show)

instance Monoid Env where
  mempty = Env Map.empty
  Env lMap `mappend` Env rMap = Env (Map.unionWithKey oneJust lMap rMap) where
    oneJust k Nothing Nothing = Nothing
    oneJust k (Just x) Nothing = Just x
    oneJust k Nothing (Just x) = Just x
    oneJust k (Just _) (Just _) = error ("Dupicate declaration of " ++ k)

type Expr = [Expr1]
data Expr1 
  = Scope (Map.Map String Expr) Expr 
  | AppExpr Expr Expr
  | VarExpr String
  | IdtExpr String
  | StrExpr String
  | ChrExpr Char
  | NumExpr Double
  deriving (Eq, Show)

data EnvExpr = EnvExpr { env :: Env, expr :: Expr } deriving (Eq, Show)

instance Monoid EnvExpr where
  mempty = EnvExpr mempty mempty
  EnvExpr lEnv lExpr `mappend` EnvExpr rEnv rExpr =
    EnvExpr (lEnv `mappend` rEnv) (lExpr `mappend` rExpr)

ast2Expr xs = mconcat (map ast12Expr xs)

ast12Expr (StringLit s) = EnvExpr mempty [StrExpr s]
ast12Expr (CharLit c)   = EnvExpr mempty [ChrExpr c]
ast12Expr (NumberLit x) = EnvExpr mempty [NumExpr x]
ast12Expr (                 Ident i ) = EnvExpr (Env (Map.singleton i Nothing)) [VarExpr i]
ast12Expr (App [Ident "?"] [Ident i]) = EnvExpr (Env (Map.singleton i Nothing)) [IdtExpr i]
ast12Expr (App [Brackets _ _ _] arg) = EnvExpr (Env frees) [Scope bounds argExprs] where
  EnvExpr (Env argEnv) argExprs = ast2Expr arg
  (frees, bounds') = Map.partition (== Nothing) argEnv
  bounds = Map.mapMaybe id bounds'

ast12Expr (App [App [Ident "="] l] r) = EnvExpr (Env env) []
  where
    EnvExpr lEnv lExprs = ast2Expr l
    EnvExpr rEnv rExprs = ast2Expr r
    Env map = lEnv `mappend` rEnv
    env = assign lExprs rExprs map
    assign [IdtExpr i]      r env = Map.insert i (Just r) env
    assign ((IdtExpr i):xs) r env = assign xs [AppExpr [IdtExpr "Tail"] r] (Map.insert i (Just [AppExpr [IdtExpr "Head"] r]) env)

ast12Expr (App ops args) = EnvExpr env [AppExpr opExprs argExprs]
  where
    EnvExpr opEnv opExprs = ast2Expr ops
    EnvExpr argEnv argExprs = ast2Expr args
    env = opEnv `mappend` argEnv