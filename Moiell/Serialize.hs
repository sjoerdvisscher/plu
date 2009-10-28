module Moiell.Serialize where

import Moiell.Class

import Data.Monoid
import qualified Data.Map as Map
import Data.List (intersperse)

data Src = S [String] | A Src Src | I String

instance Moiell Src where
  
  -- Create object from parent, map of attributes, map of local variables and body.
  object par attrs vars body = one $ 
    (if show par == "{}" then "" else show par) ++
    (showSrcInBrackets "{" "}" True $
      mconcat [attrsToSrc attrs, body])
  urObject = one $ "{}"

  -- Create attributes, strings and numbers.
  attrib a = one $ "@" ++ a
  string s = one $ show s
  number n = one $ show n
  
  -- Function application.
  apply (I "Attr") (S ['"':y]) = I (init y)
  apply f@(I ('!':y)) (I "$") = f
  apply f x = A f x
  
  -- The empty sequence.
  empty = mempty
  -- Concat sequences.
  csum = mconcat
  -- Sequence eliminator, taking:
  -- an empty value
  -- a function taking head and tail computations
  -- the computation to eliminate
  split _ _ c = one $ "split" ++ showInBrackets c
  
  -- Throw catchable errors.
  throw e = one $ "throw" ++ showInBrackets e
  -- Catch catchable errors.
  catch b h = one $ showInBrackets b ++ " catch " ++ showInBrackets h
  -- Throw fatal errors.
  fatal = error
  
  -- Get the current scope object.
  this = I "$"
  -- Evaluate the given computation in the parent scope.
  inParent = id
  
  builtIns = Map.fromList $ map (\s -> (s, one "{ Built-in function }")) $
    [ "+"
    , "-"
    , "*"
    , "/"
    , "~"
    , "div"
    , "mod"
    , "++"
    , "<"
    , "<="
    , ">"
    , ">="
    , "=="
    , "!="
    , "chars"
    , "Attr"
    ]

  -- Run the computation.
  run = show
  
  -- Look the identifier up in the environment.
  lookupVar i [] = fatal ("Undeclared variable: " ++ i)
  lookupVar i [e] = maybe (inParent $ lookupVar i []) (const (I i)) $ Map.lookup i e
  lookupVar i (e:p) = maybe (inParent $ lookupVar i p) (id) $ Map.lookup i e


one :: String -> Src
one s = S [s]

instance Show Src where
  show (S [])  = "()"
  show (S [x]) = x
  show (S xs)  = showSrcInBrackets "(" ")" False (S xs)
  show (A (A f x) y) = "(" ++ show x ++ " " ++ show f ++ " " ++ show y ++ ")"
  show (A f (S xs)) = show f ++ showSrcInBrackets "(" ")" False (S xs)
  show (A f (I i)) = i ++ "." ++ show f
  show (A f x) = let s = show x in if head s == '(' then show f ++ s else s ++ "." ++ show f
  show (I i) = i

instance Monoid Src where
  mempty = S []
  mappend l (S []) = l
  mappend (S []) r = r
  mappend l r = S (toList l ++ toList r)

toList :: Src -> [String]
toList (S xs) = xs
toList a = [show a]

attrsToSrc :: Map.Map String Src -> Src
attrsToSrc = S . map (\(i, c) -> i ++ " = " ++ show c) . Map.toList

varsToSrc :: Map.Map String Src -> Src
varsToSrc = S . map (\(i, c) -> "?" ++ i ++ " = " ++ show c) . Map.toList

showInBrackets :: Src -> String
showInBrackets = showSrcInBrackets "(" ")" False

showSrcInBrackets :: String -> String -> Bool -> Src -> String
showSrcInBrackets o c isBlock src = let strs = toList src in
  if (isBlock && length strs > 1) || any (elem '\n') strs 
  then o ++ "\n" ++ concat (map indent strs) ++ c
  else o ++ concat (intersperse "; " strs) ++ c

indent :: String -> String
indent = unlines . map ("  " ++) . lines