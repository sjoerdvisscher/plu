{-# OPTIONS -fglasgow-exts #-}  
{-# LANGUAGE NoMonomorphismRestriction #-}
module Moiell where

import Moiell.Parser
import Moiell.Semantics
import Moiell.Expr

import Control.Applicative
import Data.Foldable
import Control.Monad hiding (msum, mapM, sequence)

qsortS xs = do
  (x, xs) <- splitS xs
  ifS x (qsortS (xs *<=* x) <|> x <|> qsortS (xs *>* x)) empty

foldrS f z xs = do
  f <- eachS f
  z <- eachS z
  (x, xs) <- splitS xs
  ifS x (f <*> x <*> (foldrS f z xs)) z

--lengthS = foldrS (pure $ const (n 1 *+*)) (n 0)
--sumS xs = foldrS (pure (*+*)) (n 0)
lastS xs = do (x, ls) <- splitS xs; lastS1 xs ls
lastS1 xs ls = do (x, xs) <- splitS xs; (l, ls) <- splitS xs; ifS l (lastS1 xs ls) x
tailS xs = do (x, xs) <- splitS xs; xs

takeS i xs = do i <- eachS i; (x, xs) <- splitS xs; ifS (i *==* n 0) empty (x <|> takeS (i *-* n 1) xs) 
dropS i xs = do i <- eachS i; ifS (i *==* n 0) xs (do (x, xs) <- splitS xs; dropS (i *-* n 1) xs)
intersperseS sep xs = do sep <- eachS sep; (x, xs) <- splitS xs; x *++* (ifS xs (sep *++* intersperseS sep xs) (s ""))

showS = liftM show
showStreamS max str = intersperseS (s ", ") (takeS max str)

toS strX strY = do
  x <- eachS strX
  y <- eachS strY
  msum [x, (x */=* y) `andS` ((x *+* n 1) `toS` y)]   

pairwiseAdd xs ys = do
  (x, xs) <- splitS xs
  (y, ys) <- splitS ys
  (x *+* y) <|> (xs `pairwiseAdd` ys)

fib = n 1 <|> n 1 <|> (fib `pairwiseAdd` (tailS fib))

main1 = asum [
    s "Quicksort:"
  , showStreamS (n 5) (qsortS (toStream [5,2,6,8,3]))
  , s "Fibonacci:"
  , showStreamS (n 10) fib
  ]
  
main2 = do
  (Right ast) <- parseFile "test.moi"
  print $ ast2Expr ast
  
main3 = case (parseString "(?x, ?xs = 3, 4; xs + x)") of
  (Right ast) -> ast2Expr ast
  