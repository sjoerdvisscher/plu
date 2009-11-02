module Moiell.Expr(Expr, Expr1(..), Scope, showExpr, Moiell.Expr.parseString, Moiell.Expr.parseFile) where

import Moiell.Parser

import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intersperse)
import Data.Foldable (foldMap)

type Expr = [Expr1]
data Expr1 
  = ObjExpr Expr Scope Expr 
  | AppExpr Expr Expr
  | VarExpr String
  | IdtExpr String
  | StrExpr String
  | NumExpr Double
  | ThisExpr
  | UrExpr
  deriving (Eq, Ord)

parseString :: String -> (Expr, Free)
parseString = ast2Expr . getAST . Moiell.Parser.parseString
parseFile :: String -> IO (Expr, Free)
parseFile fileName = do
  parseResult <- Moiell.Parser.parseFile fileName
  return $ ast2Expr $ getAST parseResult

type Scope = Map.Map Expr1 Expr
type Free  = Set.Set String
data Env   = Env { getExpr :: Expr, getBounds :: Scope, getFrees :: Map.Map String Integer } deriving (Show)

instance Monoid Env where
  mempty = Env [] Map.empty Map.empty
  Env lExpr lBound lFree `mappend` Env rExpr rBound rFree = 
    Env (lExpr ++ rExpr) (Map.unionWithKey noDupes lBound rBound) (Map.unionWith (+) lFree rFree) where
      noDupes k _ _ = fail ("Duplicate variable definition:" ++ show k)

ast2Expr :: AST -> (Expr, Free)
ast2Expr ast = (expr, Map.keysSet free) where
  Env expr _ free = mkScope $ ast2Env ast

ast2Env :: AST -> Env
ast2Env = foldMap ast12Env

ast12Env :: AST1 -> Env
ast12Env (StringLit s) = Env [StrExpr s] Map.empty Map.empty
ast12Env (NumberLit n) = Env [NumExpr n] Map.empty Map.empty

ast12Env (                 Ident "$") = Env [ThisExpr ] Map.empty Map.empty
ast12Env (                 Ident  i ) = Env [VarExpr i] Map.empty (Map.singleton i 1)
ast12Env (App [Ident "?"] [Ident  i]) = Env [IdtExpr i] Map.empty Map.empty

ast12Env (App [Brackets '(' ')' _] arg) = mkScope  $ ast2Env arg
ast12Env (App [Brackets '{' '}' _] arg) = mkObject [UrExpr] $ ast2Env arg
ast12Env (App [App [Brackets '{' '}' _] arg] par) = mkObject parent $ (ast2Env arg `mappend` parentEnv') where
  Env parent bounds frees = ast2Env par
  parentEnv' = Env [] bounds frees

ast12Env (App [App [Ident "="] l] r) = assign lExpr rExpr (Env [] lBound lFree `mappend` Env [] rBound rFree)
  where
    Env lExpr lBound lFree = ast2Env l
    Env rExpr rBound rFree = ast2Env r
    assign [e]    val env  = assign1 e val env
    assign (e:xs) val env  = assign xs [AppExpr [VarExpr "Tail"] val] (assign1 e [AppExpr [VarExpr "Head"] val] env)
    assign1 e val (Env expr bound free) = Env expr (Map.insert e val bound) free

ast12Env (App ops args) = Env [AppExpr opExprs argExprs] bounds frees
  where
    opEnv@(Env opExprs _ _) = ast2Env ops
    argEnv@(Env argExprs _ _) = ast2Env args
    Env _ bounds frees = opEnv `mappend` argEnv

mkScope :: Env -> Env
mkScope arg = if Map.null (getBounds arg) then arg else lower $ mkObject [UrExpr] arg
mkObject :: Expr -> Env -> Env
mkObject parent (Env argExprs bounds frees) = obj where
  obj = Env [ObjExpr parent bounds' argExprs] Map.empty frees'
  frees' = Map.difference frees $ Map.mapKeys getVarName bounds
  bounds' = Map.fromList . concatMap toAttr . Map.toList $ bounds
  toAttr b@(e, [AppExpr [VarExpr "Attr"] _]) = [b]
  toAttr b@(VarExpr _, val) = [b]
  toAttr b@(IdtExpr e, val) = 
    if Map.findWithDefault 0 e frees <= 1
      then [b] -- let val be inlined
      else [(IdtExpr e, attrOfThis),  (attrVar, val), (attrIdt, attr)] where
        attrName = "!" ++ e
        attrVar  = VarExpr attrName
        attrIdt  = IdtExpr attrName
        attr     = [AppExpr [VarExpr "Attr"] [StrExpr attrName]]
        attrOfThis = [AppExpr [attrVar] [ThisExpr]]
  

lower :: Env -> Env
lower (Env expr b f) = Env [AppExpr expr []] b f

getVarName :: Expr1 -> String
getVarName (IdtExpr i) = i
getVarName (VarExpr i) = i
getVarName e = error $ "Cannot assign to " ++ show e

instance Show Expr1 where
  show (IdtExpr i) = "?" ++ i
  show (VarExpr i) = i
  show (NumExpr i) = show i
  show (StrExpr s) = show s
  show (ThisExpr)  = "$"
  show (UrExpr)    = "{}"
  show (ObjExpr p env e) = showExpr p ++ showListInBrackets "{" "}" True (map showBinding (Map.assocs env) ++ map show e)
  show (AppExpr [AppExpr o l] r) = showListInBrackets "(" ")" False [showExpr l ++ " " ++ showExpr o ++ " " ++ showExpr r]
  show (AppExpr o e) = showExpr o ++ showExprInBrackets e

showExpr :: Expr -> String
showExpr [x] = show x
showExpr xs  = showExprInBrackets xs

showExprInBrackets :: Expr -> String
showExprInBrackets xs = showListInBrackets "(" ")" False $ map show xs

showBinding :: (Expr1, Expr) -> String
showBinding (i, e) = show i ++ " = " ++ showExpr e

showListInBrackets :: String -> String -> Bool -> [String] -> String
showListInBrackets o c isBlock strs = if (isBlock && length strs > 1) || any (elem '\n') strs 
  then o ++ "\n" ++ concat (map indent strs) ++ c
  else o ++ concat (intersperse ", " strs) ++ c

indent :: String -> String
indent = unlines . map ("  " ++) . lines