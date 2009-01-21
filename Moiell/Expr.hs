module Moiell.Expr (Expr, Expr1(..), ast2Expr, expr2comp) where

import Moiell.AST
import Moiell.Semantics

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Control.Monad.Reader hiding (msum, mapM, sequence)
import qualified Data.Map as Map

data Env = Env { getMap :: Map.Map String (Maybe Expr) } deriving (Eq, Show)

instance Monoid Env where
  mempty = Env Map.empty
  Env lMap `mappend` Env rMap = Env (Map.unionWith mappend lMap rMap)

type Expr = [Expr1]
data Expr1 
  = ObjExpr (Map.Map String Expr) Expr 
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

ast2Expr :: AST -> Expr
ast2Expr = expr . mkScope . ast2EnvExpr

ast2EnvExpr :: AST -> EnvExpr
ast2EnvExpr xs = mconcat (map ast12EnvExpr xs)

ast12EnvExpr (StringLit s) = EnvExpr mempty [StrExpr s]
ast12EnvExpr (CharLit c)   = EnvExpr mempty [ChrExpr c]
ast12EnvExpr (NumberLit x) = EnvExpr mempty [NumExpr x]
ast12EnvExpr (                 Ident i ) = EnvExpr (Env (Map.singleton i Nothing)) [VarExpr i]
ast12EnvExpr (App [Ident "?"] [Ident i]) = EnvExpr (Env (Map.singleton i Nothing)) [IdtExpr i]
ast12EnvExpr (App [Brackets '(' ')' _] arg) = mkScope $ ast2EnvExpr arg
ast12EnvExpr (App [Brackets '{' '}' _] arg) = mkObject $ ast2EnvExpr arg

ast12EnvExpr (App [App [Ident "="] l] r) = EnvExpr (Env env) []
  where
    EnvExpr lEnv lExprs = ast2EnvExpr l
    EnvExpr rEnv rExprs = ast2EnvExpr r
    Env map = lEnv `mappend` rEnv
    env = assign lExprs rExprs map
    assign [IdtExpr i]      r env = Map.insert i (Just r) env
    assign ((IdtExpr i):xs) r env = assign xs [AppExpr [IdtExpr "Tail"] r] (Map.insert i (Just [AppExpr [IdtExpr "Head"] r]) env)

ast12EnvExpr (App ops args) = EnvExpr env [AppExpr opExprs argExprs]
  where
    EnvExpr opEnv opExprs = ast2EnvExpr ops
    EnvExpr argEnv argExprs = ast2EnvExpr args
    env = opEnv `mappend` argEnv

mkScope arg = lower $ mkObject arg
mkObject arg = EnvExpr (Env frees) [ObjExpr bounds argExprs] where
  EnvExpr (Env argEnv) argExprs = arg
  (frees, bounds') = Map.partition (== Nothing) argEnv
  bounds = Map.mapMaybe id bounds'

lower (EnvExpr env expr) = EnvExpr env [AppExpr expr []]

expr2comp :: Expr -> Stream Value
expr2comp xs = asum (map expr12comp xs)

expr12comp :: Expr1 -> Stream Value
expr12comp (StrExpr s) = pure $ S s
expr12comp (NumExpr n) = pure $ N n
expr12comp (ChrExpr c) = pure $ C c
expr12comp (VarExpr i) = lookupVar i
expr12comp (ObjExpr props expr) = object (fmap expr2comp props) (expr2comp expr)
expr12comp (AppExpr ops args)   = apply (expr2comp ops) (expr2comp args)