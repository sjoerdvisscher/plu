module Moiell.Class where

import qualified Data.Map as Map

class Moiell c where
  
  -- Create object from parent, map of attributes and content.
  object :: c -> Map.Map String c -> c -> c
  urObject :: c

  -- Create attributes, strings and numbers.
  attrib :: String -> c
  string :: String -> c
  number :: Double -> c
  
  -- Function application.
  apply :: c -> c -> c
  
  -- Create call-by-value functions.
  eachC :: (c -> c) -> c
  eachCS :: (String -> c) -> c
  eachCN :: (Double -> c) -> c
    
  -- The empty sequence.
  empty :: c
  -- Concat sequences.
  csum :: [c] -> c
  -- Sequence eliminator, taking:
  -- an empty value
  -- a function taking head and tail computations
  -- the computation to eliminate
  split :: c -> (c -> c -> c) -> c -> c
  
  -- Throw catchable errors.
  throw :: c
  -- Catch catchable errors.
  catch :: c
  -- Throw fatal errors.
  fatal :: String -> c
  
  -- Get the current scope object.
  this :: c
  -- Evaluate the given computation in the parent scope.
  inParent :: c -> c 
  
  -- Run the computation.
  run :: c -> String
  
  -- Look the identifier up in the environment.
  lookupVar :: String -> [Map.Map String c] -> c
  lookupVar i [] = fatal ("Undeclared variable: " ++ i)
  lookupVar i (e:p) = maybe (inParent $ lookupVar i p) id $ Map.lookup i e
