module Moiell.Parser(AST, AST1(..), parser) where

import Moiell.Tokenizer
import ApplicativeParsec
import Data.List (intersperse)

type AST = [AST1]
data AST1
  = App AST AST
  | Ident String
  | Brackets Char Char Bool
  | NumberLit Int
  | CharLit Char
  | StringLit String
  deriving (Eq)

parseFile fileName  = parser fileName <$> readFile fileName
parseString         = parser "(unknown)"
parser fileName     = tokenizer fileName >=> runParser moiellProgram "" ""

moiellProgram       = exprs <* eof

exprs               = concat <$> infixSeq `sepBy` seqSeparator <?> "sequence"

infixSeq            = inOptWS (option [] (infixSeq' id 0)) <?> "infix expression" 
infixSeq' f prec    = do
                        value <- dotInfixExpr
                        mmaybe infixOperator 
                          (return $ f value)
                          (\op -> (\(f1, f2) -> f1 <$> infixSeq' (mkInfixApp op (f2 value)) (rPrecedence op))
                                    (if precedence op <= prec then (id, f) else (f, id)))
infixOperator       = try (inWS dotInfixExpr) <|> try (inOptWS ([Ident ","] <$ simpleToken Comma)) <?> "infix operator"

dotInfixExpr        = postfixExpr `chainl1` (flip mkApp <$ simpleToken Dot) <?> "compact expression"
postfixExpr         = mkPostfixApp <$> prefixExpr <*> optionMaybe identToken <?> "postfix expression"
prefixExpr          = try (mkPrefixApp <$> identToken <*> bracketPostfixExpr) <|> bracketPostfixExpr <?> "prefix expression"
bracketPostfixExpr  = foldl bracketPostfix <$> atomExpr <*> many bracketExpr <?> "bracket postfix expression"

atomExpr            = bracketExpr <|> atomToken <|> identToken <|> indentBlock
bracketExpr         = brackets '(' ')' <|> brackets '{' '}' <|> brackets '[' ']' <?> "bracket expression"
brackets l r        = mkBracketsApp l r <$> betweenBrackets l r exprs
indentBlock         = between (simpleToken Indent) (simpleToken Unindent) exprs <?> "indentation block"
seqSeparator        = simpleToken SemiColon <|> simpleToken NL

betweenBrackets l r = between (bracket l <* optIndent) (optUnindent *> bracket r)
bracket c           = simpleToken (Bracket c)
optIndent           = option [] (simpleToken NL *> simpleToken Indent)
optUnindent         = option [] (simpleToken Unindent <* simpleToken NL)
inWS                = between (simpleToken WS) (simpleToken WS <|> simpleToken NL)
inOptWS             = between (optionMaybe (simpleToken WS)) (optionMaybe (simpleToken WS))

bracketPostfix a [App [Brackets '(' ')' _] arg] = [App a arg]
bracketPostfix a [App [Brackets l r _] arg] = [App [App [Brackets l r True] a] arg]

-- Resolve ident(...), ident[...] and ident{...} parsing ambiguity
mkPrefixApp op@[Ident _] arg@[App [Brackets _ _ _] _] = bracketPostfix op arg
mkPrefixApp op           arg                          = mkApp op arg

mkInfixApp [Ident ","] = (++)
mkInfixApp op          = mkApp . (mkApp op)

mkApp op arg          = [App op arg]
mkBracketsApp l r arg = [App [Brackets l r False] arg]
mkPostfixApp arg      = maybe arg (mkApp [App [Ident "*"] arg])

mmaybe p a f          = optionMaybe p >>= maybe a f
 
mkTokenParser testTok
  = token showTok posFromTok testTok
  where
    showTok (pos,t)     = show t
    posFromTok (pos,t)  = pos
  
simpleToken x = mkTokenParser (\(pos,t) -> if x == t then Just [t] else Nothing)

atomToken = mkTokenParser testTok where
  testTok (pos, CharTok c)   = Just [CharLit c]
  testTok (pos, StringTok c) = Just [StringLit c]
  testTok (pos, NumberTok c) = Just [NumberLit c]
  testTok (pos, _) = Nothing

identToken = mkTokenParser testTok where
  testTok (pos, IdentTok c)  = Just [Ident c]
  testTok (pos, _) = Nothing

operatorToken p = mkTokenParser testTok where
  testTok (pos, IdentTok c) | precedence [Ident c] == p = Just [Ident c]
  testTok (pos, _) = Nothing

precedence [Ident "*"] = 120
precedence [Ident "/"] = 120
precedence [Ident "+"] = 110
precedence [Ident "-"] = 110

precedence [Ident ","] = 60

precedence [Ident "=="] = 50
precedence [Ident "!="] = 50
precedence [Ident ">" ] = 50
precedence [Ident "<" ] = 50
precedence [Ident ">="] = 50
precedence [Ident "<="] = 50

precedence [Ident "=>"] = 20
precedence [Ident "->"] = 20

precedence [Ident "="] = 10

precedence [Ident [_]] = 200
precedence [Ident _] = 100
precedence _ = 90

rPrecedence [Ident "=>"] = 19
rPrecedence [Ident "->"] = 19
rPrecedence x = precedence x

instance Show AST1 where
  show (Ident s) = s
  show (NumberLit i) = show i
  show (CharLit   c) = show c
  show (StringLit s) = show s
  show (Brackets l r _) = [l, r]
  show (App [App [Brackets l r _] ls] rs) = show01More ls ++ [l] ++ show01More rs ++ [r]
  show (App [App fs ls] rs) = show01More ls ++ " " ++ show01More fs ++ " " ++ show01More rs
  show (App [Ident "?"] [Ident x]) = '?':x
  show (App [Brackets l r False] xs) = [l] ++ show01More xs ++ [r]
  show (App fs xs) = show01More fs ++ showASTList xs
  
show01More [x@(App [App _ _] _)] = "(" ++ show x ++ ")"
show01More [x] = show x
show01More xs = showASTList xs
showASTList xs = "(" ++ (concat (intersperse "; " (map show xs))) ++ ")" 