{-# LANGUAGE ScopedTypeVariables #-}
module Moiell(
  compileString,
  compileFile,
  -- debugAST,

  Moiell(..)) 
where

import Moiell.Class
import Moiell.Expr
import Moiell.Globals

import qualified Data.Map as Map
import qualified Data.Set as Set

type CompMap c = Map.Map String c
type Env c = [CompMap c]

compile :: forall c. Moiell c => (Expr, Set.Set String) -> c
compile = expr2comp [globalScope] . checkVariables (globalScope :: CompMap c)

compileString :: Moiell c => String -> c
compileString = compile . parseString

compileFile :: Moiell c => String -> IO c
compileFile fileName = do
  parseResult <- parseFile fileName
  (return . compile) parseResult

-- debugAST :: String -> IO ()
-- debugAST = putStrLn . showExpr . checkVariables . parseString
  
checkVariables :: Moiell c => CompMap c -> (Expr, Set.Set String) -> Expr
checkVariables globs (expr, frees) = 
  if Set.null undeclared then expr else 
    error ("Undeclared variables: " ++ (foldr1 (\l r -> l ++ ", " ++ r) $ Set.toList undeclared))
  where
    undeclared = Set.filter (flip Map.notMember globs) frees 

  
expr2comp :: Moiell c => Env c -> Expr -> c
expr2comp e xs = csum (map (expr12comp e) xs)

expr12comp :: Moiell c => Env c -> Expr1 -> c
expr12comp _ (ThisExpr ) = this
expr12comp _ (UrExpr   ) = urObject
expr12comp _ (StrExpr x) = string x
expr12comp _ (NumExpr x) = number x
expr12comp e (VarExpr i) = lookupVar i e
expr12comp _ (IdtExpr i) = fatal $ "Name expressions only allowed in left-hand side of assignments:" ++ i
expr12comp e (ObjExpr parExpr exprProps expr) = object (expr2comp e parExpr) compAttrs compVars (expr2comp env1 expr)
  where 
    (compVars, compAttrs) = splitProps env1 exprProps
    env1 = compVars : e
expr12comp e (AppExpr ops args)   = apply (expr2comp e ops) (expr2comp e args)

splitProps :: Moiell c => Env c -> Scope -> (CompMap c, CompMap c)
splitProps e s = foldr (splitProps' e) (Map.empty, Map.empty) (Map.assocs s)

splitProps' :: Moiell c => Env c -> (Expr1, Expr) -> (CompMap c, CompMap c) -> (CompMap c, CompMap c)
splitProps' e (VarExpr i, expr) (vars, attrs) = (vars, Map.insert i (expr2comp e expr) attrs) -- assume it is an attribute
splitProps' e (IdtExpr i, expr) (vars, attrs) = (Map.insert i (expr2comp e expr) vars, attrs)

