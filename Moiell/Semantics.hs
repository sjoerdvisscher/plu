{-# OPTIONS -fglasgow-exts #-}  
{-# LANGUAGE NoMonomorphismRestriction #-}
module Moiell.Semantics where

import Prelude hiding (foldr, mapM, sequence)
import Data.Foldable
import Data.Maybe
import Data.Traversable
import qualified Data.Map as Map
import Control.Monad hiding (msum, mapM, sequence)
import Control.Monad.Error hiding (msum, mapM, sequence)
import Control.Monad.Reader hiding (msum, mapM, sequence)
import Control.Applicative

type TEnv = Map.Map String (Stream Value)
type TError = String

type Object = (TEnv, Stream Value)
data Result a = Yield a (Stream a) StateRec | Done StateRec | Error TError StateRec
data StateRec = StateRec { env :: TEnv }
data Stream a = Str (StateRec -> Result a)
data Value = N Double | S String | C Char | O Object

eval s (Str c)   = c s
compAp f (Str c) = Str $ f . c

instance Monad Stream where
  return x  = Str $ Yield x empty
  fail s    = Str $ Error s
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
  
instance MonadReader TEnv Stream where
  ask       = Str $ \s -> Yield (env s) empty s
  local f c = Str $ \s -> restoreEnv s $ eval s{ env = f (env s) } c where
    restoreEnv s y@(Yield _ _ _) = y
    restoreEnv s (Done s1)       = Done s1 { env = env s }
    restoreEnv s (Error e s1)    = Error e s1 { env = env s }

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

instance Show Value where
  show (N n) = show n
  show (S s) = s
  show (C c) = [c]
  show (O o) = "{Object}"

lookupVar i = do
  env <- ask
  fromJust (Map.lookup i env)





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
  pure . N $ f a b

liftS2 f aStr bStr = do
  a <- toString aStr
  b <- toString bStr
  pure . S $ f a b
  
(*+*)  = liftN2 (+)
(*-*)  = liftN2 (-)
(***)  = liftN2 (*)
(*++*)  = liftS2 (++)

globalEnv = Map.fromList []