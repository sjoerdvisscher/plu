module Moiell.AST where

import Data.List (intersperse)

type AST = [AST1]
data AST1
  = App AST AST
  | Ident String
  | Brackets Char Char Bool
  | NumberLit Double
  | CharLit Char
  | StringLit String
  deriving (Eq)

bracketPostfix :: AST -> AST -> AST
bracketPostfix a [App [Brackets '(' ')' _] arg] = mkApp a arg
bracketPostfix a [App [Brackets '[' ']' _] arg] = mkInfixApp [Ident "Filter"] a arg
bracketPostfix a [App [Brackets l r _] arg] = mkInfixApp [Brackets l r True] a arg

-- Resolve ident(...), ident[...] and ident{...} parsing ambiguity
mkPrefixApp :: AST -> AST -> AST
mkPrefixApp op@[Ident _] arg@[App [Brackets _ _ _] _] = bracketPostfix op arg
mkPrefixApp op           arg                          = mkApp op arg

mkInfixApp :: AST -> AST -> AST -> AST
mkInfixApp [Ident ","]  l       r = l ++ r
mkInfixApp [Ident "=>"] l       r = mkApp [Brackets '{' '}' False] (mkInfixApp [Ident "="] l [Ident "_"] ++ r)
mkInfixApp [Ident "->"] l       r = mkApp [Ident "Each"          ] (mkInfixApp [Ident "=>"] l r)
mkInfixApp op           l       r = mkApp (mkApp op l) r

mkApp                 :: AST -> AST -> AST
mkApp op arg          = [App op arg]
mkBracketsApp         :: Char -> Char -> AST -> AST
mkBracketsApp l r arg = mkApp [Brackets l r False] arg
mkPostfixApp          :: AST -> Maybe AST -> AST
mkPostfixApp arg      = maybe arg (mkInfixApp [Ident "*"] arg)

instance Show AST1 where
  show (Ident s) = s
  show (NumberLit i) = show i
  show (CharLit   c) = show c
  show (StringLit s) = show s
  show (Brackets l r _) = [l, r]
  show (App [App [Brackets l r _] ls] rs) = showAST ls ++ [l] ++ showAST rs ++ [r]
  show (App [App fs ls] rs) = showAST ls ++ " " ++ showAST fs ++ " " ++ showAST rs
  show (App [Ident "?"] [Ident x]) = '?':x
  show (App [Brackets l r False] xs) = [l] ++ showAST xs ++ [r]
  show (App fs xs) = showAST fs ++ showASTInBrackets xs

showAST :: AST -> String
showAST x@[(App [App _ _] _)] = showASTInBrackets x
showAST [x] = show x
showAST xs = showASTInBrackets xs

showASTInBrackets :: AST -> String
showASTInBrackets xs = "(" ++ concat (intersperse "; " (map show xs)) ++ ")" 