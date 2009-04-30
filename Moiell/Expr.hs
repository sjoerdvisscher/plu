module Moiell.Expr(Expr, Expr1(..), showExpr, Moiell.Expr.parseString, Moiell.Expr.parseFile) where

import Moiell.Parser

import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intersperse)

type Expr = [Expr1]
data Expr1 
  = ObjExpr Expr Scope Expr 
  | AppExpr Expr Expr
  | VarExpr String
  | IdtExpr String
  | StrExpr String
  | ChrExpr Char
  | NumExpr Double
  | ThisExpr
  | UrExpr

parseString :: String -> (Expr, Free)
parseString = ast2Expr . getAST . Moiell.Parser.parseString
parseFile :: String -> IO (Expr, Free)
parseFile fileName = do
  parseResult <- Moiell.Parser.parseFile fileName
  return $ ast2Expr $ getAST parseResult

type Scope = Map.Map String Expr
type Free  = Set.Set String
data Env   = Env { getExpr :: Expr, getBounds :: Scope, getFrees :: Free } deriving (Show)

instance Monoid Env where
  mempty = Env [] Map.empty Set.empty
  Env lExpr lBound lFree `mappend` Env rExpr rBound rFree = 
    Env (lExpr ++ rExpr) (Map.unionWithKey noDupes lBound rBound) (Set.union lFree rFree) where
      noDupes k _ _ = fail ("Duplicate variable definition:" ++ k)

ast2Expr :: AST -> (Expr, Free)
ast2Expr ast = (expr, free) where
  env = mkScope $ ast2Env ast
  Env expr _ free = env

ast2Env :: AST -> Env
ast2Env xs = mconcat (map ast12Env xs)

ast12Env :: AST1 -> Env
ast12Env (StringLit s) = Env [StrExpr s] Map.empty Set.empty
ast12Env (CharLit c)   = Env [ChrExpr c] Map.empty Set.empty
ast12Env (NumberLit n) = Env [NumExpr n] Map.empty Set.empty

ast12Env (                 Ident "$") = Env [ThisExpr ] Map.empty Set.empty
ast12Env (                 Ident  i ) = Env [VarExpr i] Map.empty (Set.singleton i)
ast12Env (App [Ident "?"] [Ident  i]) = Env [IdtExpr i] Map.empty (Set.singleton i)

ast12Env (App [Brackets '(' ')' _] arg) = mkScope  $ ast2Env arg
ast12Env (App [Brackets '{' '}' _] arg) = mkObject [UrExpr] $ ast2Env arg
ast12Env (App [App [Brackets '{' '}' _] arg] par) = mkObject (getExpr $ ast2Env par) $ ast2Env arg

ast12Env (App [App [Ident "="] l] r) = assign lExpr rExpr (Env [] lBound lFree `mappend` Env [] rBound rFree)
  where
    Env lExpr lBound lFree = ast2Env l
    Env rExpr rBound rFree = ast2Env r
    assign [IdtExpr i]      val env = assign1 i val env
    assign ((IdtExpr i):xs) val env = assign xs [AppExpr [VarExpr "Tail"] val] (assign1 i [AppExpr [VarExpr "Head"] val] env)
    assign1 i val (Env expr bound free) = Env expr (Map.insert i val bound) free

ast12Env (App ops args) = Env [AppExpr opExprs argExprs] bounds frees
  where
    Env opExprs opB opF = ast2Env ops
    Env argExprs argB argF = ast2Env args
    bounds = opB `mappend` argB
    frees  = opF `mappend` argF

mkScope :: Env -> Env
mkScope arg = if Map.null (getBounds arg) then arg else lower $ mkObject [UrExpr] arg
mkObject :: Expr -> Env -> Env
mkObject parent (Env argExprs bounds frees) = 
  Env [ObjExpr parent bounds argExprs] Map.empty (Set.difference frees $ Map.keysSet bounds)

lower :: Env -> Env
lower (Env expr b f) = Env [AppExpr expr []] b f

instance Show Expr1 where
  show (IdtExpr i) = "?" ++ i
  show (VarExpr i) = i
  show (NumExpr i) = show i
  show (ChrExpr c) = show c
  show (StrExpr s) = show s
  show (ThisExpr)  = "$"
  show (UrExpr)    = "Ur"
  show (ObjExpr p env e) = showExpr p ++ showListInBrackets "{" "}" True (map showBinding (Map.assocs env) ++ map show e)
  show (AppExpr [AppExpr o l] r) = showListInBrackets "(" ")" False [showExpr l ++ " " ++ showExpr o ++ " " ++ showExpr r]
  show (AppExpr o e) = showExpr o ++ showExprInBrackets e

showExpr :: Expr -> String
showExpr [x] = show x
showExpr xs  = showExprInBrackets xs

showExprInBrackets :: Expr -> String
showExprInBrackets xs = showListInBrackets "(" ")" False $ map show xs

showBinding :: (String, Expr) -> String
showBinding (i, e) = "?" ++ i ++ " = " ++ showExpr e

showListInBrackets :: String -> String -> Bool -> [String] -> String
showListInBrackets o c isBlock strs = if (isBlock && length strs > 1) || any (elem '\n') strs 
  then o ++ "\n" ++ concat (map indent strs) ++ c
  else o ++ concat (intersperse ", " strs) ++ c

indent :: String -> String
indent = unlines . map ("  " ++) . lines