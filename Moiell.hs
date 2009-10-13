{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies #-}
module Moiell(
  compileString,
  compileFile,
  -- debugAST,

  Moiell(..)) 
where

import Moiell.Expr

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

type CompMap c v = Map.Map String (c v)
type Env c v = [CompMap c v]

class (MonadPlus c) => Moiell c v | c -> v where
  urObject :: c v
  object :: c v -> CompMap c v -> c v -> c v
  string :: String -> c v
  number :: Double -> c v
  
  apply :: c v -> c v -> c v
  
  this :: c v
  runInParent :: c v -> c v 
  
  run :: c v -> String
  globalScope :: Map.Map String (c v)

  lookupVar :: String -> Env c v -> c v
  lookupVar i [] = fail ("Undeclared variable: " ++ i)
  lookupVar i (e:p) = maybe (runInParent $ lookupVar i p) id $ Map.lookup i e

  compile :: (Expr, Set.Set String) -> c v
  compile = expr2comp [globalScope :: Map.Map String (c v)] . checkVariables (globalScope :: Map.Map String (c v))

compileString :: Moiell c v => String -> c v
compileString = compile . parseString

compileFile :: Moiell c v => String -> IO (c v)
compileFile fileName = do
  parseResult <- parseFile fileName
  (return . compile) parseResult

-- debugAST :: String -> IO ()
-- debugAST = putStrLn . showExpr . checkVariables . parseString
  
checkVariables :: Moiell c v => CompMap c v -> (Expr, Set.Set String) -> Expr
checkVariables globs (expr, frees) = 
  if Set.null undeclared then expr else 
    error ("Undeclared variables: " ++ (foldr1 (\l r -> l ++ ", " ++ r) $ Set.toList undeclared))
  where
    undeclared = Set.filter (flip Map.notMember globs) frees 

  
expr2comp :: Moiell c v => Env c v -> Expr -> c v
expr2comp e xs = msum (map (expr12comp e) xs)

expr12comp :: Moiell c v => Env c v -> Expr1 -> c v
expr12comp _ (ThisExpr ) = this
expr12comp _ (UrExpr   ) = urObject
expr12comp _ (StrExpr x) = string x
expr12comp _ (NumExpr x) = number x
expr12comp e (VarExpr i) = lookupVar i e
expr12comp _ (IdtExpr i) = fail $ "Name expressions only allowed in left-hand side of assignments:" ++ i
expr12comp e (ObjExpr parExpr exprProps expr) = object (expr2comp e parExpr) compAttrs (expr2comp env1 expr)
  where 
    (compVars, compAttrs) = splitProps env1 exprProps
    env1 = compVars : e
expr12comp e (AppExpr ops args)   = apply (expr2comp e ops) (expr2comp e args)

splitProps :: Moiell c v => Env c v -> Scope -> (CompMap c v, CompMap c v)
splitProps e s = foldr (splitProps' e) (Map.empty, Map.empty) (Map.assocs s)

splitProps' :: Moiell c v => Env c v -> (Expr1, Expr) -> (CompMap c v, CompMap c v) -> (CompMap c v, CompMap c v)
splitProps' e (VarExpr i, expr) (vars, attrs) = (vars, Map.insert i (expr2comp e expr) attrs) -- assume it is an attribute
splitProps' e (IdtExpr i, expr) (vars, attrs) = (Map.insert i (expr2comp e expr) vars, attrs)

