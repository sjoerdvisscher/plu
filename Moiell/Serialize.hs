{-# LANGUAGE TypeSynonymInstances #-}
module Moiell.Serialize where

import Moiell.Class

import qualified Data.Map as Map
import Data.List (intersperse)

instance Moiell String where
  
  -- Create object from parent, map of attributes and content.
  object "{}" attrs vars content = "{" ++ showSeq (attrsToSeq attrs ++ varsToSeq vars ++ [content]) ++ "}"
  object par  attrs vars content = par ++ object "{}" attrs vars content
  urObject = "{}"

  -- Create attributes, strings and numbers.
  attrib a = "@" ++ a
  string s = show s
  number n = show n
  
  -- Function application.
  apply f x = f ++ "(" ++ x ++ ")"
  
  -- Create call-by-value functions.
  eachC  f = "eachC(c -> c)"
  eachCS f = "eachCS(String -> c)"
  eachCN f = f 3.14
    
  -- The empty sequence.
  empty = "()"
  -- Concat sequences.
  csum = concat . intersperse "; "
  -- Sequence eliminator, taking:
  -- an empty value
  -- a function taking head and tail computations
  -- the computation to eliminate
  split e ht c = "split(" ++ c ++ ")"
  
  -- Throw catchable errors.
  throw e = "throw(" ++ e ++ ")"
  -- Catch catchable errors.
  catch b h = "(" ++ b ++ ") catch (" ++ h ++ ")"
  -- Throw fatal errors.
  fatal = error
  
  -- Get the current scope object.
  this = "$"
  -- Evaluate the given computation in the parent scope.
  inParent c = "^(" ++ c ++ ")"
  
  -- Run the computation.
  run = id
  
  -- Look the identifier up in the environment.
  lookupVar i [] = fatal ("Undeclared variable: " ++ i)
  lookupVar i (e:p) = maybe ("^" ++ lookupVar i p) (const i) $ Map.lookup i e

attrsToSeq :: Map.Map String String -> [String]
attrsToSeq = map (\(i, c) -> i ++ " = " ++ c) . Map.toList

varsToSeq :: Map.Map String String -> [String]
varsToSeq = map (\(i, c) -> "?" ++ i ++ " = " ++ c) . Map.toList

showSeq :: [String] -> String
showSeq = concat . intersperse "; "