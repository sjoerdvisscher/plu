{-# OPTIONS -fglasgow-exts #-}  
{- LANGUAGE NoMonomorphismRestriction -}
module Moiell where

import Moiell.Parser

import Prelude hiding (foldr, mapM, sequence)
import Data.Foldable
import Data.Maybe
import Data.Traversable
import Control.Monad hiding (msum, mapM, sequence)
import Control.Monad.Error hiding (msum, mapM, sequence)
import Control.Monad.State hiding (msum, mapM, sequence)
import Control.Monad.Reader hiding (msum, mapM, sequence)
import Control.Applicative

type TState = [Int]
type TEnv = [(String, Int)]
type TError = Int

data Result a = Yield a (Stream a) StateRec | Done StateRec | Error TError StateRec deriving Show
data StateRec = StateRec { state :: TState, env :: TEnv } deriving Show
data Stream a = Str (StateRec -> Result a)
data Value = N Double | S String deriving (Eq, Ord, Show)

eval s (Str c)   = c s
compAp f (Str c) = Str $ f . c

instance Monad Stream where
  return x  = Str $ Yield x empty
  c >>= f   = compAp helper c where
    helper (Yield x sx s) = eval s $ (f x) <|> (sx >>= f)
    helper (Done s)       = Done s
    helper (Error e s)    = Error e s

instance MonadPlus Stream where
  mzero     = Str $ Done
  mplus a b = compAp helper a where
    helper (Yield x sa s) = Yield x (mplus sa b) s
    helper (Done s)       = eval s b 
    helper r              = r
  
instance MonadState TState Stream where
  get       = Str $ \s -> Yield (state s) empty s
  put st    = Str $ \s -> Yield ()        empty s{ state = st }

instance MonadReader TEnv Stream where
  ask       = Str $ \s -> Yield (env s) empty s
  local f c = Str $ \s -> eval s{ env = f (env s) } c

instance MonadError TError Stream where
  throwError errMsg = Str $ Error errMsg
  catchError c hnd  = compAp helper c where
    helper (Error e s) = eval s (hnd e)
    helper r           = r

instance Functor Stream where
  fmap  = liftM

instance Applicative Stream where
  pure  = return
  (<*>) = ap

instance Alternative Stream where
  empty = mzero
  (<|>) = mplus

instance Functor Result where
  fmap f (Yield x c s) = Yield (f x) (fmap f c) s
  fmap f (Done s)      = Done s
  fmap f (Error e s)   = Error e s

startState = StateRec { state = [], env = [] }

runComp comp = showResult $ eval startState comp
showResult result = case result of 
  Yield x comp s  -> (show x) ++ "\n" ++ showResult (eval s comp)
  Done s          -> ("Done.\n" ++ show s)
  Error e s       -> ("Error: " ++ show e ++ "\n" ++ show s)

instance Show a => Show (Stream a) where
  show = runComp

eachS = fmap return
    
splitS :: Stream a -> Stream (Stream a, Stream a)
splitS comp = compAp helper comp where
  helper (Yield x sa s) = Yield (pure x, sa) empty s
  helper (Done s)       = Yield (empty, empty) empty s
  helper (Error e s)    = Error e s

ifS testComp trueComp falseComp = compAp helper testComp where
  helper (Yield _ _ s) = eval s trueComp
  helper (Done s)      = eval s falseComp
  helper (Error e s)   = Error e s

oneS str = do (x, xs) <- splitS str; x
notS str = ifS str empty (return ())
andS aStr bStr = ifS aStr bStr empty
orS aStr bStr = ifS aStr aStr bStr

n = pure . N
s = pure . S

toStream :: [Double] -> Stream Value
toStream as = asum (map n as)

toDouble vStr = do
  v <- vStr
  case v of
    N n -> pure n
    S s -> maybe empty (pure . fst) (listToMaybe (reads s))

toString vStr = do
  v <- vStr
  case v of
    N n -> pure (show n)
    S s -> pure s

filterN2 :: (a -> b -> Bool) -> Stream a -> Stream b -> Stream a
filterN2 f aStr bStr = do
  a <- aStr
  b <- bStr
  if (f a b) then (pure a) else empty

(*<=*) = filterN2 (<=)
(*>=*) = filterN2 (>=)
(*<*)  = filterN2 (<)
(*>*)  = filterN2 (>)
(*==*) = filterN2 (==)
(*/=*) = filterN2 (/=)

liftN2 f aStr bStr = do
  a <- toDouble aStr
  b <- toDouble bStr
  n $ f a b

liftS2 f aStr bStr = do
  a <- toString aStr
  b <- toString bStr
  s $ f a b
  
(*+*)  = liftN2 (+)
(*-*)  = liftN2 (-)
(***)  = liftN2 (*)
(*++*)  = liftS2 (++)

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

main = asum [
    s "Quicksort:"
  , showStreamS (n 5) (qsortS (toStream [5,2,6,8,3]))
  , s "Fibonacci:"
  , showStreamS (n 10) fib
  ]