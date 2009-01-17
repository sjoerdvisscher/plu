module Moiell.Tokenizer (tokenizer, Token(..)) where

import ApplicativeParsec
import Numeric
import qualified Data.ByteString as B
import qualified Data.String.UTF8 as U

data Token
  = CharTok Char
  | StringTok String
  | NumberTok Double
  | IdentTok String
  | Bracket Char
  | NL | WS
  | SemiColon | Comma | Dot
  | Indent | Unindent
  deriving (Eq)

-- A parser which is indentation aware, with current indentation as state.
type IndentParser = Parsec (U.UTF8 B.ByteString) String

-- Begin with no indentation as parser state
-- Add a new line to the input so every line ends with a new line.
--tokenizer :: SourceName -> String -> Either ParseError [(SourcePos, Token)]
tokenizer fileName input = runParser tokenize "" fileName input

tokenize          :: IndentParser [(SourcePos, Token)]
tokenize          = indentedLines <* eof

withPos           :: IndentParser Token -> IndentParser (SourcePos, Token)
withPos p         = (,) <$> getPosition <*> p

-- for debugging
-- withPos           :: IndentParser Token -> IndentParser (SourcePos, String, a)
-- withPos p         = (,,) <$> getPosition <*> getState <*> p

indentedLines     = concat <$> many (emptyLine <|> indentedLine) <?> "lines"
indentedLine      = try (indentation *> (indentedBlock <|> afterIndentation)) <?> "line"
emptyLine         = try (([] <$ many (oneOf " \t")) <<:> newLine) <?> "empty line"
newLine           = withPos (NL <$ char '\n') <?> "new-line"

indentedBlock     = preserveState (
                      moreIndentation <:>> 
                      afterIndentation <++> indentedLines
                      <<:> (withPos $ return Unindent)
                      <<:> (withPos $ return NL)
                    ) <?> "indented block"
preserveState p   = do st <- getState; result <- p; setState st; return result

indentation       = (string =<< getState) <?> "indentation"
moreIndentation   = withPos (
                      Indent <$ (updateState . (flip (++)) =<< many1 (oneOf " \t"))
                    ) <?> "indent"

afterIndentation  = (actualTokens <* optional lineComment) <<:> newLine
actualTokens      = option [] (
                      nonIndentToken <:>> many (nonIndentToken <|> wsToken)
                    ) <?> "tokens"
wsToken           = withPos (
                      WS <$ many1 (oneOf " \t")
                    ) <?> "white-space"
nonIndentToken    = withPos (choice [
                        StringTok <$> stringLit
                      , CharTok   <$> charLit
                      , NumberTok <$> numberLit
                      , IdentTok  <$> identifier
                      , Bracket   <$> oneOf "(){}[]"
                      , SemiColon <$  char ';'
                      , Comma     <$  char ','
                      , Dot       <$  char '.'
                      ]) <?> "token"

stringLit         = between (char '"') (char '"') (
                      many (escChar <|> noneOf "\"\\")
                    ) <?> "string literal"
charLit           = between (char '\'') (char '\'') (
                      escChar <|> noneOf "\'\\"
                    ) <?> "character literal"
escChar           = char '\\' *> (p_escape <|> p_unicode)
p_escape          = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
                      where decode c r = r <$ char c
p_unicode         = char 'u' *> (decode <$> count 4 hexDigit)
                      where decode x = toEnum code where ((code,_):_) = readHex x
                      
numberLit         = read <$> integerPart <++> option [] fractionalPart <++> option [] exponentPart
numbers           = many (oneOf ['0'..'9'])
integerPart       = string "0" <|> oneOf ['1'..'9'] <:>> numbers
fractionalPart    = char '.' <:>> numbers
exponentPart      = char 'e' <:>> option [] (string "+" <|> string "-") <++> integerPart

identifier        = choice [identStartChar <:>> many identChar, many1 operatorChar, string "?"]
identStartChar    = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ "_$")
identChar         = noneOf "(){}[] \t\n\v,;'\".#"
operatorChar      = oneOf "!@%^&*/-+=|<>:`~"

lineComment       = string "#" <* many (noneOf "\n") <?> "line comment"

(<:>>) = liftA2 (:)
(<++>) = liftA2 (++)
(<<:>) = liftA2 append where append l r = l ++ [r]

instance Show Token where
  show (CharTok c)    = show c
  show (StringTok s)  = show s
  show (NumberTok i)  = show i
  show (IdentTok s)   = s
  show (Bracket c)    = show c
  show NL             = "new-line"
  show SemiColon      = "semi-colon"
  show Comma          = "comma"
  show Dot            = "dot"
  show Indent         = "indent"
  show Unindent       = "unindent"
  show WS             = "white-space"
