module Moiell.Parser (parser) where

import Moiell.AST
import Moiell.Tokenizer
import ApplicativeParsec

type TokenParser a = GenParser (SourcePos, Token) String a
type ParserForAST = TokenParser AST

parseFile           :: SourceName -> IO (Either ParseError AST)
parseFile fileName  = parser fileName <$> readFile fileName

parseString         :: String -> Either ParseError AST
parseString         = parser "(unknown)"

parser              :: SourceName -> String -> Either ParseError AST
parser fileName     = tokenizer fileName >=> runParser moiellProgram "" ""

moiellProgram       :: ParserForAST
moiellProgram       = exprs <* eof

exprs               :: ParserForAST
exprs               = concat <$> infixSeq `sepBy` seqSeparator <?> "sequence"

infixSeq            :: ParserForAST
infixSeq            = inOptWS (option [] (infixSeq' id 0)) <?> "infix expression"
infixSeq'           :: (AST -> AST) -> Integer -> ParserForAST
infixSeq' f prec    = do
                        value <- dotInfixExpr
                        mmaybe infixOperator 
                          (return $ f value)
                          (\op -> (\(f1, f2) -> f1 <$> infixSeq' (mkInfixApp op (f2 value)) (rPrecedence op))
                                    (if precedence op <= prec then (id, f) else (f, id)))
infixOperator       :: ParserForAST
infixOperator       = try (inWS dotInfixExpr) <|> try (inOptWS ([Ident ","] <$ simpleToken Comma)) <?> "infix operator"

dotInfixExpr, postfixExpr, prefixExpr, bracketPostfixExpr :: ParserForAST
dotInfixExpr        = postfixExpr `chainl1` (flip mkApp <$ simpleToken Dot) <?> "compact expression"
postfixExpr         = mkPostfixApp <$> prefixExpr <*> optional identToken <?> "postfix expression"
prefixExpr          = try (mkPrefixApp <$> identToken <*> bracketPostfixExpr) <|> bracketPostfixExpr <?> "prefix expression"
bracketPostfixExpr  = foldl bracketPostfix <$> atomExpr <*> many bracketExpr <?> "bracket postfix expression"

atomExpr, bracketExpr, indentBlock, seqSeparator :: ParserForAST
atomExpr            = choice [bracketExpr, atomToken, identToken, indentBlock]
bracketExpr         = choice [brackets '(' ')', brackets '{' '}', brackets '[' ']'] <?> "bracket expression"
indentBlock         = between (simpleToken Indent) (simpleToken Unindent) exprs <?> "indentation block"
seqSeparator        = simpleToken SemiColon <|> simpleToken NL

brackets            :: Char -> Char -> ParserForAST
brackets l r        = mkBracketsApp l r <$> betweenBrackets l r exprs
betweenBrackets     :: Char -> Char -> ParserForAST -> ParserForAST
betweenBrackets l r = between (bracket l <* optIndent) (optUnindent *> bracket r)
bracket             :: Char -> ParserForAST
bracket c           = simpleToken (Bracket c)

optIndent, optUnindent :: ParserForAST
optIndent           = [] <$ optional (simpleToken NL *> simpleToken Indent)
optUnindent         = [] <$ optional (simpleToken Unindent <* simpleToken NL)
inWS, inOptWS :: ParserForAST -> ParserForAST
inWS                = between (simpleToken WS) (simpleToken WS <|> simpleToken NL)
inOptWS             = between (optionMaybe (simpleToken WS)) (optionMaybe (simpleToken WS))

bracketPostfix :: AST -> AST -> AST
bracketPostfix a [App [Brackets '(' ')' _] arg] = [App a arg]
bracketPostfix a [App [Brackets l r _] arg] = [App [App [Brackets l r True] a] arg]

-- Resolve ident(...), ident[...] and ident{...} parsing ambiguity
mkPrefixApp :: AST -> AST -> AST
mkPrefixApp op@[Ident _] arg@[App [Brackets _ _ _] _] = bracketPostfix op arg
mkPrefixApp op           arg                          = mkApp op arg

mkInfixApp :: AST -> AST -> AST -> AST
mkInfixApp [Ident ","] = (++)
mkInfixApp op          = mkApp . (mkApp op)

mkApp                 :: AST -> AST -> AST
mkApp op arg          = [App op arg]
mkBracketsApp         :: Char -> Char -> AST -> AST
mkBracketsApp l r arg = [App [Brackets l r False] arg]
mkPostfixApp          :: AST -> Maybe AST -> AST
mkPostfixApp arg      = maybe arg (mkApp [App [Ident "*"] arg])

mmaybe                :: (Alternative m, Monad m) => m a -> m b -> (a -> m b) -> m b
mmaybe p a f          = optional p >>= maybe a f

mkTokenParser :: ((SourcePos, Token) -> Maybe AST) -> ParserForAST
mkTokenParser testTok
  = token showTok posFromTok testTok
  where
    showTok (pos,t)     = show t
    posFromTok (pos,t)  = pos
  
simpleToken :: Token -> ParserForAST
simpleToken x = mkTokenParser (\(pos,t) -> if x == t then Just [] else Nothing)

atomToken :: ParserForAST
atomToken = mkTokenParser testTok where
  testTok (pos, CharTok c)   = Just [CharLit c]
  testTok (pos, StringTok c) = Just [StringLit c]
  testTok (pos, NumberTok c) = Just [NumberLit c]
  testTok (pos, _) = Nothing

identToken :: ParserForAST
identToken = mkTokenParser testTok where
  testTok (pos, IdentTok c)  = Just [Ident c]
  testTok (pos, _) = Nothing

precedence :: AST -> Integer
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

rPrecedence :: AST -> Integer
rPrecedence [Ident "=>"] = 19
rPrecedence [Ident "->"] = 19
rPrecedence x = precedence x