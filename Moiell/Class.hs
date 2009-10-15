module Moiell.Class where

import qualified Data.Map as Map

type Env c = [Map.Map String c]

class Moiell c where
  urObject :: c
  object :: c -> Map.Map String c -> c -> c
  attrib :: String -> c
  string :: String -> c
  number :: Double -> c
  
  eachC :: (c -> c) -> c
  liftCS :: (String -> c) -> c
  liftCN :: (Double -> c) -> c
  eachC2 :: (c -> c -> c) -> c
  liftC2S :: (String -> String -> c) -> c
  liftC2N :: (Double -> Double -> c) -> c
  
  apply :: c -> c -> c
  
  empty :: c
  csum :: [c] -> c
  split :: c -> (c -> c -> c) -> c -> c
  
  throw :: c
  catch :: c
  err :: String -> c
  
  this :: c
  inParent :: c -> c 
  
  run :: c -> String

  lookupVar :: String -> Env c -> c
  lookupVar i [] = err ("Undeclared variable: " ++ i)
  lookupVar i (e:p) = maybe (inParent $ lookupVar i p) id $ Map.lookup i e
