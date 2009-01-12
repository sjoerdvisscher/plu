module Moiell.Tokenizer (tokenizer, Token(..)) where

import ApplicativeParsec
import Data.Char
import Numeric

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
type IndentParser = GenParser Char String

-- Begin with no indentation as parser state
-- Add a new line to the input so every line ends with a new line.
tokenizer :: SourceName -> String -> Either ParseError [(SourcePos, Token)]
tokenizer fileName input = runParser tokenize "" fileName (input ++ "\n")

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

charLit           = between (char '\'') (char '\'') (p_char '\'')
stringLit         = between (char '"') (char '"') (many $ p_char '"')
p_char delim      =  (char '\\' >> p_esc)
                 <|> (satisfy (\x -> x /= delim && x /= '\\'))

p_esc             =  ('"'   <$ char '"')
                 <|> ('\\'  <$ char '\\')
                 <|> ('/'   <$ char '/')
                 <|> ('\b'  <$ char 'b')
                 <|> ('\f'  <$ char 'f')
                 <|> ('\n'  <$ char 'n')
                 <|> ('\r'  <$ char 'r')
                 <|> ('\t'  <$ char 't')
                 <|> (char 'u' *> p_uni)
                 <?> "escape character"

p_uni             = check =<< count 4 (satisfy isHexDigit)
          where check x | code <= max_char  = pure (toEnum code)
                        | otherwise         = empty
                  where code      = fst $ head $ readHex x
                        max_char  = fromEnum (maxBound :: Char)
                      
numberLit         = do s <- getInput
                       case readSigned readFloat s of
                         [(n,s1)] -> n <$ setInput s1
                         _        -> empty

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
