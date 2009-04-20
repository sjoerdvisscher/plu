module Moiell.Expr (
  ast2Comp, 
  run, 
  
  Comp(..), 
  Value(..), 
  Object(..),
  
  TException,
  TWriter,
  TReader
  ) where

import Moiell.AST
import Moiell.Semantics

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Control.Monad.Reader hiding (msum, mapM, sequence)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Scope = Map.Map String Expr
data Env = Env { getExpr :: Expr, getBound :: Scope, getFree :: Set.Set String } deriving (Show)

instance Monoid Env where
  mempty = Env [] Map.empty Set.empty
  Env lExpr lBound lFree `mappend` Env rExpr rBound rFree = 
    Env (lExpr ++ rExpr) (Map.unionWithKey noDupes lBound rBound) (Set.union lFree rFree) where
      noDupes k _ _ = fail ("Duplicate variable definition:" ++ k)

type Expr = [Expr1]
data Expr1 
  = ObjExpr Scope Expr 
  | AppExpr Expr Expr
  | VarExpr String
  | IdtExpr String
  | ValExpr Value
  | ThisExpr
  deriving (Show)

ast2Comp :: CompMap -> AST -> Comp Value
ast2Comp global ast = if not(Set.null(undecls)) 
  then fail ("Undeclared variables: " ++ show env) 
  else expr2comp [global] expr 
    where
      env = mkScope $ ast2Env ast
      Env expr _ frees = env
      undecls = Set.filter (flip Map.notMember global) frees

ast2Env :: AST -> Env
ast2Env xs = mconcat (map ast12Env xs)

ast12Env :: AST1 -> Env
ast12Env (StringLit s) = Env [ValExpr $ S s] Map.empty Set.empty
ast12Env (CharLit c)   = Env [ValExpr $ C c] Map.empty Set.empty
ast12Env (NumberLit n) = Env [ValExpr $ N n] Map.empty Set.empty

ast12Env (                 Ident "$") = Env [ThisExpr ] Map.empty Set.empty
ast12Env (                 Ident  i ) = Env [VarExpr i] Map.empty (Set.singleton i)
ast12Env (App [Ident "?"] [Ident  i]) = Env [IdtExpr i] Map.empty (Set.singleton i)

ast12Env (App [Brackets '(' ')' _] arg) = mkScope  $ ast2Env arg
ast12Env (App [Brackets '{' '}' _] arg) = mkObject $ ast2Env arg

ast12Env (App [App [Ident "="] l] r) = assign lExpr rExpr (Env [] lBound lFree `mappend` Env [] rBound rFree)
  where
    Env lExpr lBound lFree = ast2Env l
    Env rExpr rBound rFree = ast2Env r
    assign [IdtExpr i]      r env = assign1 i r env
    assign ((IdtExpr i):xs) r env = assign xs [AppExpr [VarExpr "Tail"] r] (assign1 i [AppExpr [VarExpr "Head"] r] env)
    assign1 i r (Env expr bound free) = Env expr (Map.insert i r bound) free

ast12Env (App ops args) = Env [AppExpr opExprs argExprs] bounds frees
  where
    Env opExprs opB opF = ast2Env ops
    Env argExprs argB argF = ast2Env args
    bounds = opB `mappend` argB
    frees  = opF `mappend` argF

mkScope :: Env -> Env
mkScope arg = lower $ mkObject arg
mkObject :: Env -> Env
mkObject (Env argExprs bounds frees) = Env [ObjExpr bounds argExprs] Map.empty (Set.difference frees $ Map.keysSet bounds)

lower :: Env -> Env
lower (Env expr b f) = Env [AppExpr expr []] b f

expr2comp :: [CompMap] -> Expr -> Comp Value
expr2comp env xs = asum (map (expr12comp env) xs)

expr12comp :: [CompMap] -> Expr1 -> Comp Value
expr12comp _ (ThisExpr ) = this
expr12comp _ (ValExpr x) = pure x
expr12comp e (VarExpr i) = lookupVar i e
expr12comp e (ObjExpr props expr) = object compProps (expr2comp env1 expr)
  where 
    compProps = fmap (expr2comp env1) props
    env1 = compProps : e
expr12comp e (AppExpr ops args)   = apply (expr2comp e ops) (expr2comp e args)

lookupVar :: String -> [CompMap] -> Comp Value
lookupVar i [] = error ("Variable not found:" ++ i)
lookupVar i (e:p) = maybe (runInParent $ lookupVar i p) id $ Map.lookup i e