module Moiell.AST (AST, AST1(..)) where

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