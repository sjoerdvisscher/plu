{-# OPTIONS -fglasgow-exts #-}  
{-# LANGUAGE NoMonomorphismRestriction #-}
module Moiell where

import Moiell.Parser
import Moiell.Semantics
import Moiell.Expr

import Control.Applicative
import Data.Foldable
import qualified Data.Map as Map
import Control.Monad hiding (msum, mapM, sequence)

runComp comp = showResult $ eval startState comp
showResult result = case result of 
  Yield x comp s  -> (show x) ++ "\n" ++ showResult (eval s comp)
  Done s          -> ("Done.\n")
  Error e s       -> ("Error: " ++ show e)

instance Show a => Show (Stream a) where
  show = runComp

main1 = do
  (Right ast) <- parseFile "test.moi"
  print $ ast2Expr ast
  
main2 = do
  (Right ast) <- parseFile "test.moi"
  print $ (expr2comp . ast2Expr) ast
  
main3 = case (parseString "5 mod 3") of
  (Right ast) -> (expr2comp . ast2Expr) ast