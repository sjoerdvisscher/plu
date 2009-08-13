module Moiell(
  runString,
  runFile,
  debugString,
  debugFile,

  Comp, 
  Value(..), 
  Object(..),

  showResult) 
where

import Moiell.Expr
import Moiell.Globals
import Moiell.Semantics
-- import Moiell.DebugSemantics

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set


runString :: String -> TResult
runString = runWithEnv globalObject . expr2comp [globalScope] . checkVariables . parseString
runFile :: String -> IO TResult
runFile fileName = do
  parseResult <- parseFile fileName
  return $ runWithEnv globalObject $ expr2comp [globalScope] $ checkVariables parseResult
debugString :: String -> IO ()
debugString = putStrLn . showExpr . checkVariables . parseString
debugFile :: String -> IO ()
debugFile fileName = do
  parseResult <- parseFile fileName
  putStrLn $ showExpr $ checkVariables parseResult
  
checkVariables :: (Expr, Set.Set String) -> Expr
checkVariables (expr, frees) = 
  if Set.null undeclared then expr else 
    error ("Undeclared variables: " ++ (foldr1 (\l r -> l ++ ", " ++ r) $ Set.toList undeclared))
  where
    undeclared = Set.filter (flip Map.notMember globalScope) frees 


type Env = [CompMap]

expr2comp :: Env -> Expr -> Comp Value
expr2comp e xs = msum (map (expr12comp e) xs)

expr12comp :: Env -> Expr1 -> Comp Value
expr12comp _ (ThisExpr ) = this
expr12comp _ (UrExpr   ) = urObject
expr12comp _ (StrExpr x) = return.S $ x
expr12comp _ (ChrExpr x) = return.C $ x
expr12comp _ (NumExpr x) = return.N $ x
expr12comp _ (AttExpr i) = return.A $ i
expr12comp e (VarExpr i) = lookupVar i e
expr12comp _ (IdtExpr i) = fail $ "Name expressions only allowed in left-hand side of assignments:" ++ i
expr12comp e (ObjExpr parExpr exprProps expr) = object (expr2comp e parExpr) Map.empty (expr2comp env1 expr)
  where 
    compProps = fmap (expr2comp env1) exprProps
    env1 = compProps : e
expr12comp e (AppExpr ops args)   = apply (expr2comp e ops) (expr2comp e args)

lookupVar :: String -> Env -> Comp Value
lookupVar i [] = fail ("Undeclared variable: " ++ i)
lookupVar i (e:p) = maybe (runInParent $ lookupVar i p) id $ Map.lookup i e